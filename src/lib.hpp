#ifndef KLX_LIB_HPP
#define KLX_LIB_HPP

#include <iostream>
#include <string>
#include <string_view>
#include <vector>
#include <array>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>

#include <locale.hpp>
#include <print.hpp>
#include <view.hpp>
#include <unicode.hpp>
#include <log.hpp>
#include <report.hpp>

namespace klx {

struct Error {};

template <typename... Ts>
[[noreturn]] inline void halt(Ts&&... args) {
	report(std::cerr, std::forward<Ts>(args)...);
	throw Error {};
}


using TokenTypes = size_t;
enum: TokenTypes {
	TOKENTYPE_SRC = 0b001,
	TOKENTYPE_AST = 0b010,
	TOKENTYPE_IR  = 0b100,

	TOKENTYPE_SRC_AST = TOKENTYPE_SRC | TOKENTYPE_AST,
	TOKENTYPE_AST_IR  = TOKENTYPE_AST | TOKENTYPE_IR,
	TOKENTYPE_SRC_IR  = TOKENTYPE_SRC | TOKENTYPE_IR,

	TOKENTYPE_COMMON  = TOKENTYPE_SRC | TOKENTYPE_AST | TOKENTYPE_IR,
};

#define SYMBOL_TYPES \
	X(NONE,       "none", TOKENTYPE_COMMON) \
	X(TERMINATOR, "eof",  TOKENTYPE_SRC) \
	\
	/* Special */ \
	X(IDENTIFIER, "identifier", TOKENTYPE_COMMON) \
	X(INTEGER,    "integer",    TOKENTYPE_COMMON) \
	X(CHARACTER,  "character",  TOKENTYPE_COMMON) \
	X(STRING,     "string",     TOKENTYPE_COMMON) \
	\
	/* Blocks */ \
	X(PROGRAM, "program", TOKENTYPE_AST) \
	X(BLOCK,   "block",   TOKENTYPE_IR) \
	X(RET,     "ret",     TOKENTYPE_IR) \
	X(FN,      "fn",      TOKENTYPE_AST_IR) \
	X(END,     "end",     TOKENTYPE_AST_IR) \
	\
	/* Linkage Specifiers */ \
	X(DECL,   "decl",   TOKENTYPE_COMMON) /* Implicitly Local */ \
	X(EXTERN, "extern", TOKENTYPE_COMMON) /* Implicitly Global */ \
	\
	/* Control Flow */ \
	X(WHILE,  "while",  TOKENTYPE_SRC_AST) \
	X(IF,     "if",     TOKENTYPE_SRC_AST) \
	X(ELSE,   "else",   TOKENTYPE_SRC) \
	X(CALL,   "call",   TOKENTYPE_IR) \
	X(JUMP,   "jump",   TOKENTYPE_IR) \
	X(BRANCH, "branch", TOKENTYPE_IR) \
	X(PHI,    "phi",    TOKENTYPE_IR) \
	\
	/* AST */ \
	X(EXPR, "expr", TOKENTYPE_AST) \
	\
	/* Stack Effect Annotation */ \
	X(EFFECT_SEPERATOR, "->", TOKENTYPE_SRC_IR) \
	X(EFFECT_OPEN,      "(",  TOKENTYPE_SRC_IR) \
	X(EFFECT_CLOSE,     ")",  TOKENTYPE_SRC_IR) \
	\
	/* Block Expression */ \
	X(BLOCK_OPEN,  "{", TOKENTYPE_SRC) \
	X(BLOCK_CLOSE, "}", TOKENTYPE_SRC) \
	\
	/* Stack/Register Manipulation */ \
	X(POP,     "pop",     TOKENTYPE_IR) \
	X(PUSH,    "push",    TOKENTYPE_IR) \
	X(LET,     "let",     TOKENTYPE_IR) \
	X(COPY,    "copy",    TOKENTYPE_COMMON) \
	X(MOVE,    "move",    TOKENTYPE_SRC_AST) \
	X(REMOVE,  "remove",  TOKENTYPE_SRC_AST)

	// Enum of symbols
	#define X(name, str, type) name,
		enum class Symbols { SYMBOL_TYPES };
	#undef X

	// Map enum values to string view
	#define X(name, str, type) str##_sv,
		constexpr View SYMBOL_TO_STRING[] = { SYMBOL_TYPES };
	#undef X

	// Map of string view to symbol name and mode
	#define X(name, str, type) std::pair { str##_sv , std::pair { Symbols::name , type } },
		inline std::unordered_map<View, std::pair<Symbols, TokenTypes>> STRING_TO_SYMBOL = { SYMBOL_TYPES };
	#undef X

	constexpr decltype(auto) symbol_to_string(Symbols sym) {
		return SYMBOL_TO_STRING[(int)sym];
	}

	inline decltype(auto) string_to_symbol(View sv) {
		auto it = STRING_TO_SYMBOL.find(sv);
		return std::pair { it, it != STRING_TO_SYMBOL.end() };
	}

#undef SYMBOL_TYPES

inline std::ostream& operator<<(std::ostream& os, Symbols k) {
	return (os << symbol_to_string(k));
}


struct Token {
	klx::View view = symbol_to_string(Symbols::NONE);
	klx::Symbols kind = Symbols::NONE;
};


template <TokenTypes MODE = TOKENTYPE_SRC>
struct Lexer {
	klx::View original {};
	klx::View src {};

	klx::Token peek_ {};

	constexpr Lexer(klx::View src_): original(src_), src(src_) {
		next();  // this can throw
	}

	template <typename F, typename... Ts>
	void expect(const F& fn, View sv, Ts&&... args) {
		if (not fn(peek().kind))
			halt(Phases::PHASE_SYNTACTIC, original, sv, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	void error(Phases phase, View sv, Ts&&... args) {
		halt(phase, original, sv, std::forward<Ts>(args)...);
	}

	inline Token peek() const {
		return peek_;
	}

	inline Token next() {
		Token tok {};

		auto& [sbegin, send] = src;
		auto& [view, kind] = tok;
		auto& [begin, end] = view;

		uint32_t c = klx::as_char(src);

		// Skip whitespace.
		klx::consume_char(src, c, klx::is_whitespace);

		view = klx::as_view(src); // Set view to first character.

		// Lex tokens.
		if (src.is_eof()) {
			kind = Symbols::TERMINATOR;
			view = View {};  // set to eof
		}

		// Skip comments.
		else if (c == '#') {
			klx::consume_char(src, c, not_equal((uint32_t)'\n'));  // Skip until \n and then return next token.
			return next();
		}

		// Numbers.
		else if (klx::is_number(c)) {
			kind = Symbols::INTEGER;
			view = klx::consume_char(src, c, [&] (uint32_t c) {
				if (klx::is_number(c))
					return true;

				else if (klx::is_whitespace(c))
					return false;

				// Error if we find a character that is not a valid digit. i.e: `123foo`
				halt(Phases::PHASE_LEXICAL, original, as_view(src), STR_NOT_NUMBER, as_view(src));
			});
		}

		// Words.
		else if (klx::is_visible(c)) {
			kind = Symbols::IDENTIFIER;
			view = klx::consume_char(src, c, klx::is_visible);

			if (auto [it, found] = string_to_symbol(view); found) {
				auto [sym, type] = it->second;

				if ((type & MODE) == MODE)
					kind = sym;
			}
		}

		else {
			halt(Phases::PHASE_LEXICAL, original, view, STR_UNKNOWN_CHAR, view);
		}

		Token out = peek_;
		peek_ = tok;

		return out;
	}
};


using IntermediateLexer = Lexer<TOKENTYPE_IR>;
using SourceLexer       = Lexer<TOKENTYPE_SRC>;


struct Op {
	Symbols kind = Symbols::NONE;

	View sv = ""_sv;

	size_t x = 0u;
	size_t y = 0u;
	size_t z = 0u;

	constexpr Op(Symbols kind_, View sv_, size_t x_ = 0u, size_t y_ = 0u, size_t z_ = 0u):
		kind(kind_), sv(sv_), x(x_), y(y_), z(z_) {}

	constexpr Op():
		kind(Symbols::NONE) {}
};

using IR = std::vector<Op>;


struct Effect {
	size_t in;
	size_t out;

	constexpr Effect(size_t in_ = 0u, size_t out_ = 0u):
		in(in_), out(out_) {}
};

using Decls = std::unordered_map<View, Effect>;


struct Context: public IR {
	Decls decls;

	template <typename... Ts> decltype(auto) instruction(Ts&&... args) {
		return this->emplace_back(std::forward<Ts>(args)...);
	}

	template <typename... Ts> decltype(auto) node(Ts&&... args) {
		return instruction(std::forward<Ts>(args)...);
	}

	template <typename... Ts> decltype(auto) decl(Ts&&... args) {
		return decls.try_emplace(std::forward<Ts>(args)...);
	}
};


constexpr auto is_stmt = partial_eq_any(
	Symbols::IDENTIFIER,
	Symbols::DECL,
	Symbols::EXTERN
);

constexpr auto is_decl = partial_eq_any(
	Symbols::DECL,
	Symbols::EXTERN
);

constexpr auto is_expr = partial_eq_any(
	Symbols::INTEGER,
	Symbols::IDENTIFIER,
	Symbols::WHILE,
	Symbols::IF,
	Symbols::BLOCK_OPEN,
	Symbols::COPY,
	Symbols::MOVE,
	Symbols::REMOVE
);

constexpr auto is_intrinsic = partial_eq_any(
	Symbols::COPY,
	Symbols::MOVE,
	Symbols::REMOVE
);

constexpr auto is_literal = partial_eq_any(
	Symbols::INTEGER,
	Symbols::CHARACTER,
	Symbols::STRING
);

constexpr auto is_annotation = equal(Symbols::EFFECT_OPEN);
constexpr auto is_block = equal(Symbols::BLOCK_OPEN);


// Parsing
inline void parse_literal    (Context&, SourceLexer&);
inline void parse_intrinsic  (Context&, SourceLexer&);
inline void parse_call       (Context&, SourceLexer&);
inline void parse_while      (Context&, SourceLexer&);
inline void parse_if         (Context&, SourceLexer&);
inline void parse_block      (Context&, SourceLexer&);
inline void parse_expression (Context&, SourceLexer&);
inline void parse_annotation (Context&, SourceLexer&, View, Symbols);
inline void parse_decl       (Context&, SourceLexer&);
inline void parse_fn         (Context&, SourceLexer&);
inline void parse_statement  (Context&, SourceLexer&);
inline Context  parse            (SourceLexer&);


// Expressions
inline void parse_literal(Context& ast, SourceLexer& lexer) {
	lexer.expect(is_literal, lexer.peek().view, STR_LITERAL);
	auto [view, kind] = lexer.next();

	switch (kind) {
		case Symbols::INTEGER: {
			ast.node(kind, view, to_int(view));
		} break;

		case Symbols::CHARACTER: {

		} break;

		case Symbols::STRING: {

		} break;

		default: break;
	}
}

inline void parse_intrinsic(Context& ast, SourceLexer& lexer) {
	lexer.expect(is_intrinsic, lexer.peek().view, STR_INTRINSIC);
	Token tok = lexer.next();

	lexer.expect(equal(Symbols::INTEGER), lexer.peek().view, STR_ARG, tok.view);
	Token arg = lexer.next();

	ast.node(tok.kind, tok.view, to_int(arg.view));
}

inline void parse_call(Context& ast, SourceLexer& lexer) {
	lexer.expect(equal(Symbols::IDENTIFIER), lexer.peek().view, STR_CALL);
	Token tok = lexer.next();
	ast.node(Symbols::IDENTIFIER, tok.view);
}

inline void parse_while(Context& ast, SourceLexer& lexer) {
	lexer.expect(equal(Symbols::WHILE), lexer.peek().view, STR_WHILE);
	Token tok = lexer.next();

	ast.node(Symbols::WHILE, tok.view);

	// Expression.
	parse_expression(ast, lexer);

	// Body.
	parse_expression(ast, lexer);
}

inline void parse_if(Context& ast, SourceLexer& lexer) {
	lexer.expect(equal(Symbols::IF), lexer.peek().view, STR_IF);
	lexer.next();

	ast.node(Symbols::IF, lexer.peek().view);

	// Expression.
	parse_expression(ast, lexer);

	// True block.
	parse_expression(ast, lexer);

	// False block.
	if (lexer.peek().kind == Symbols::ELSE) {
		lexer.next();  // Skip `else`
		parse_expression(ast, lexer);
	}

	// No false branch so we just emit an empty expression.
	else {
		ast.node(Symbols::EXPR, lexer.peek().view);
		ast.node(Symbols::END, lexer.peek().view);
	}
}

inline void parse_block(Context& ast, SourceLexer& lexer) {
	lexer.expect(equal(Symbols::BLOCK_OPEN), lexer.peek().view, STR_EXPECT, symbol_to_string(Symbols::BLOCK_OPEN));
	lexer.next();  // skip `{`

	while (eq_none(lexer.peek().kind, Symbols::BLOCK_CLOSE, Symbols::TERMINATOR))
		parse_expression(ast, lexer);

	lexer.expect(equal(Symbols::BLOCK_CLOSE), lexer.peek().view, STR_EXPECT, symbol_to_string(Symbols::BLOCK_CLOSE));
	lexer.next();  // skip `}`
}

inline void parse_expression(Context& ast, SourceLexer& lexer) {
	ast.node(Symbols::EXPR, lexer.peek().view);

	switch (lexer.peek().kind) {
		case Symbols::CHARACTER:
		case Symbols::STRING:
		case Symbols::INTEGER: {
			parse_literal(ast, lexer);
		} break;

		case Symbols::IDENTIFIER: {
			parse_call(ast, lexer);
		} break;

		case Symbols::COPY:
		case Symbols::MOVE:
		case Symbols::REMOVE: {
			parse_intrinsic(ast, lexer);
		} break;

		case Symbols::WHILE: {
			parse_while(ast, lexer);
		} break;

		case Symbols::IF: {
			parse_if(ast, lexer);
		} break;

		case Symbols::BLOCK_OPEN: {
			parse_block(ast, lexer);
		} break;

		default: {
			lexer.error(Phases::PHASE_SYNTACTIC, lexer.peek().view, STR_EXPR);
		} break;
	}

	ast.node(Symbols::END, lexer.peek().view);
}


// Statements
inline void parse_annotation(Context& ast, SourceLexer& lexer, View name, Symbols linkage) {
	lexer.expect(is_annotation, lexer.peek().view, STR_ANNOTATION);
	lexer.next();  // skip `(`

	size_t in = 0u;
	size_t out = 0u;

	// Parse input values.
	while (lexer.peek().kind != Symbols::EFFECT_SEPERATOR) {
		lexer.expect(equal(Symbols::IDENTIFIER), lexer.peek().view, STR_IDENTIFIER);
		lexer.next();  // skip identifier
		in++;
	}

	lexer.expect(equal(Symbols::EFFECT_SEPERATOR), lexer.peek().view, STR_EXPECT, symbol_to_string(Symbols::EFFECT_SEPERATOR));
	lexer.next();  // skip `->`

	// Parse output values.
	while (lexer.peek().kind != Symbols::EFFECT_CLOSE) {
		lexer.expect(equal(Symbols::IDENTIFIER), lexer.peek().view, STR_IDENTIFIER);
		lexer.next();  // skip identifier
		out++;
	}

	lexer.expect(equal(Symbols::EFFECT_CLOSE), lexer.peek().view, STR_EXPECT, symbol_to_string(Symbols::EFFECT_CLOSE));
	lexer.next();  // skip `)`

	ast.node(linkage, name, in, out);
	ast.decl(name, in, out);
}

inline void parse_decl(Context& ast, SourceLexer& lexer) {
	lexer.expect(is_decl, lexer.peek().view, STR_DECL);
	Symbols linkage = lexer.peek().kind;
	lexer.next();  // Skip decl/extern

	lexer.expect(equal(Symbols::IDENTIFIER), lexer.peek().view, STR_IDENTIFIER);
	View name = lexer.peek().view;
	lexer.next();  // Skip identifier.

	parse_annotation(ast, lexer, name, linkage);
}

inline void parse_fn(Context& ast, SourceLexer& lexer) {
	lexer.expect(equal(Symbols::IDENTIFIER), lexer.peek().view, STR_FN);
	View name = lexer.peek().view;
	lexer.next();  // Skip identifier.

	// Inline declaration.
	if (is_annotation(lexer.peek().kind))
		parse_annotation(ast, lexer, name, Symbols::DECL);

	ast.node(Symbols::FN, name);

	// Body.
	parse_expression(ast, lexer);
}

inline void parse_statement(Context& ast, SourceLexer& lexer) {
	switch (lexer.peek().kind) {
		case Symbols::DECL:
		case Symbols::EXTERN: {
			parse_decl(ast, lexer);
		} break;

		case Symbols::IDENTIFIER: {
			parse_fn(ast, lexer);
		} break;

		default: {
			lexer.error(Phases::PHASE_SYNTACTIC, lexer.peek().view, STR_STMT);
		} break;
	}
}

inline Context parse(SourceLexer& lexer) {
	Context ast;
	ast.node(Symbols::PROGRAM, lexer.peek().view);

	while (lexer.peek().kind != Symbols::TERMINATOR)
		parse_statement(ast, lexer);

	ast.node(Symbols::END, lexer.peek().view);

	return ast;
}

}

#endif
