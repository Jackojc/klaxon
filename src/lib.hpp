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
	X(FN,      "fn",      TOKENTYPE_COMMON) \
	X(RET,     "ret",     TOKENTYPE_IR) \
	X(BLOCK,   "block",   TOKENTYPE_IR) \
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
	X(POP,    "pop",    TOKENTYPE_IR) \
	X(PUSH,   "push",   TOKENTYPE_IR) \
	X(LOAD,   "load",   TOKENTYPE_IR) \
	X(COPY,   "copy",   TOKENTYPE_COMMON) \
	X(MOVE,   "move",   TOKENTYPE_SRC_AST) \
	X(REMOVE, "remove", TOKENTYPE_SRC_AST)

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


template <TokenTypes MODE>
struct Lexer {
	klx::View original {};
	klx::View src {};

	klx::Token peek_ {};

	constexpr Lexer(klx::View src_): original(src_), src(src_) {
		next();
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
	size_t in = 0u;
	size_t out = 0u;
};

struct Decl {
	Effect effect;
	Symbols linkage = Symbols::DECL;

	constexpr Decl() {}
	constexpr Decl(Effect effect_, Symbols linkage_):
		effect(effect_), linkage(linkage_) {}
};

using Declarations = std::unordered_map<View, Decl>;
using Effects = std::vector<Effect>;


template <TokenTypes MODE>
struct Context: Lexer<MODE> {
	size_t stack = 0u;

	size_t block_id = 0u;
	size_t register_id = 0u;

	Effects effects;
	IR instructions;
	Declarations decls;

	inline Context(klx::View src): Lexer<MODE>::Lexer(src) {
		instructions.reserve(instructions.capacity() + src.size());
	}

	// size_t reg_at(size_t i) const {
	// 	return (register_id - 1) - i;
	// }

	// size_t push(size_t n = 1) {
	// 	stack += n;
	// 	return stack;
	// }

	// size_t pop(size_t n = 1) {
	// 	stack -= n;
	// 	return stack;
	// }

	// size_t block() { return block_id++; }
	// size_t reg() { return register_id++; }

	// void reset() {
	// 	stack = 0u;
	// 	block_id = 0u;
	// 	register_id = 0u;
	// }

	template <typename... Ts> decltype(auto) instruction(Ts&&... args) {
		return instructions.emplace_back(std::forward<Ts>(args)...);
	}

	template <typename... Ts> decltype(auto) decl(Ts&&... args) {
		return decls.try_emplace(std::forward<Ts>(args)...);
	}

	template <typename F, typename... Ts>
	void expect_token(const F& fn, View sv, Ts&&... args) const {
		if (not fn(Lexer<MODE>::peek().kind))
			halt(Phases::PHASE_SYNTACTIC, Lexer<MODE>::original, sv, std::forward<Ts>(args)...);
	}

	template <typename F, typename... Ts>
	void expect_effect(const F& fn, View sv, Ts&&... args) const {
		if (not fn(stack))
			halt(Phases::PHASE_SEMANTIC, Lexer<MODE>::original, sv, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	void error(Phases phase, View sv, Ts&&... args) const {
		halt(phase, Lexer<MODE>::original, sv, std::forward<Ts>(args)...);
	}
};

using SourceContext       = Context<TOKENTYPE_SRC>;
using IntermediateContext = Context<TOKENTYPE_IR>;


// Predicates.
constexpr auto src_is_valid = partial_eq_any(
	Symbols::TERMINATOR,
	Symbols::IDENTIFIER,
	Symbols::INTEGER,
	Symbols::FN,
	Symbols::DECL,
	Symbols::EXTERN,
	Symbols::WHILE,
	Symbols::IF,
	Symbols::ELSE,
	Symbols::EFFECT_SEPERATOR,
	Symbols::EFFECT_OPEN,
	Symbols::EFFECT_CLOSE,
	Symbols::BLOCK_OPEN,
	Symbols::BLOCK_CLOSE,
	Symbols::COPY,
	Symbols::MOVE,
	Symbols::REMOVE
);

constexpr auto ir_is_valid = partial_eq_any(
	Symbols::IDENTIFIER,
	Symbols::INTEGER,
	Symbols::FN,
	Symbols::RET,
	Symbols::BLOCK,
	Symbols::END,
	Symbols::EXTERN,
	Symbols::CALL,
	Symbols::JUMP,
	Symbols::BRANCH,
	Symbols::EFFECT_SEPERATOR,
	Symbols::EFFECT_OPEN,
	Symbols::EFFECT_CLOSE,
	Symbols::LOAD,
	Symbols::POP,
	Symbols::PUSH,
	Symbols::COPY
);

constexpr auto src_is_stmt = partial_eq_any(
	Symbols::FN,
	Symbols::DECL,
	Symbols::EXTERN
);

constexpr auto src_is_decl = partial_eq_any(
	Symbols::DECL,
	Symbols::EXTERN
);

constexpr auto src_is_expr = partial_eq_any(
	Symbols::INTEGER,
	Symbols::IDENTIFIER,
	Symbols::WHILE,
	Symbols::IF,
	Symbols::BLOCK_OPEN,
	Symbols::COPY,
	Symbols::MOVE,
	Symbols::REMOVE
);

constexpr auto src_is_intrinsic = partial_eq_any(
	Symbols::COPY,
	Symbols::MOVE,
	Symbols::REMOVE
);

constexpr auto src_is_literal = partial_eq_any(
	Symbols::INTEGER,
	Symbols::CHARACTER,
	Symbols::STRING
);

constexpr auto common_is_type_annotation = equal(Symbols::EFFECT_OPEN);
constexpr auto src_is_block = equal(Symbols::BLOCK_OPEN);

// Is a basic instruction (i.e. not block/fn/end/ret)
constexpr auto ir_is_instruction = partial_eq_any(
	Symbols::LOAD,
	Symbols::COPY,
	Symbols::PUSH,
	Symbols::POP,
	Symbols::CALL,
	Symbols::JUMP,
	Symbols::BRANCH
);


// Parsing
inline void src_parse_literal    (SourceContext&);
inline void src_parse_intrinsic  (SourceContext&);
inline void src_parse_call       (SourceContext&);
inline void src_parse_while      (SourceContext&);
inline void src_parse_if         (SourceContext&);
inline void src_parse_block      (SourceContext&);
inline void src_parse_expression (SourceContext&);
inline Decl src_parse_annotation (SourceContext&, View, Symbols);
inline void src_parse_decl       (SourceContext&);
inline void src_parse_fn         (SourceContext&);
inline void src_parse_statement  (SourceContext&);
inline void src_parse            (SourceContext&);


// Expressions
inline void src_parse_literal(SourceContext& ctx) {
	ctx.expect_token(src_is_literal, ctx.peek().view, STR_LITERAL);
	auto [view, kind] = ctx.next();

	switch (kind) {
		case Symbols::INTEGER: {
			ctx.instruction(kind, view, to_int(view));
		} break;

		case Symbols::CHARACTER: {

		} break;

		case Symbols::STRING: {

		} break;

		default: break;
	}
}

inline void src_parse_intrinsic(SourceContext& ctx) {
	ctx.expect_token(src_is_intrinsic, ctx.peek().view, STR_INTRINSIC);
	Token tok = ctx.next();

	ctx.expect_token(equal(Symbols::INTEGER), ctx.peek().view, STR_ARG, tok.view);
	Token arg = ctx.next();

	ctx.instruction(tok.kind, tok.view, to_int(arg.view));
}

inline void src_parse_call(SourceContext& ctx) {
	ctx.expect_token(equal(Symbols::IDENTIFIER), ctx.peek().view, STR_CALL);
	Token tok = ctx.next();
	ctx.instruction(Symbols::IDENTIFIER, tok.view);
}

inline void src_parse_while(SourceContext& ctx) {
	ctx.expect_token(equal(Symbols::WHILE), ctx.peek().view, STR_WHILE);
	Token tok = ctx.next();

	ctx.instruction(Symbols::WHILE, tok.view);

	// Expression.
	ctx.instruction(Symbols::EXPR, tok.view);
	src_parse_expression(ctx);
	ctx.instruction(Symbols::END, ctx.peek().view);

	// Body.
	ctx.instruction(Symbols::EXPR, ctx.peek().view);
	src_parse_expression(ctx);
	ctx.instruction(Symbols::END, ctx.peek().view);
}

inline void src_parse_if(SourceContext& ctx) {
	ctx.expect_token(equal(Symbols::IF), ctx.peek().view, STR_IF);
	ctx.next();

	ctx.instruction(Symbols::IF, ctx.peek().view);

	// Expression.
	ctx.instruction(Symbols::EXPR, ctx.peek().view);
	src_parse_expression(ctx);
	ctx.instruction(Symbols::END, ctx.peek().view);

	// True block.
	ctx.instruction(Symbols::EXPR, ctx.peek().view);
	src_parse_expression(ctx);
	ctx.instruction(Symbols::END, ctx.peek().view);

	// False block.
	ctx.instruction(Symbols::EXPR, ctx.peek().view);

	if (ctx.peek().kind == Symbols::ELSE) {
		ctx.next();
		src_parse_expression(ctx);
	}

	ctx.instruction(Symbols::END, ctx.peek().view);
}

inline void src_parse_block(SourceContext& ctx) {
	ctx.expect_token(equal(Symbols::BLOCK_OPEN), ctx.peek().view, STR_EXPECT, symbol_to_string(Symbols::BLOCK_OPEN));
	ctx.next();  // skip `{`

	ctx.instruction(Symbols::EXPR, ctx.peek().view);

	while (eq_none(ctx.peek().kind, Symbols::BLOCK_CLOSE, Symbols::TERMINATOR))
		src_parse_expression(ctx);

	ctx.instruction(Symbols::END, ctx.peek().view);

	ctx.expect_token(equal(Symbols::BLOCK_CLOSE), ctx.peek().view, STR_EXPECT, symbol_to_string(Symbols::BLOCK_CLOSE));
	ctx.next();  // skip `}`
}

inline void src_parse_expression(SourceContext& ctx) {
	ctx.instruction(Symbols::EXPR, ctx.peek().view);

	switch (ctx.peek().kind) {
		case Symbols::INTEGER: {
			src_parse_literal(ctx);
		} break;

		case Symbols::IDENTIFIER: {
			src_parse_call(ctx);
		} break;

		case Symbols::COPY:
		case Symbols::MOVE:
		case Symbols::REMOVE: {
			src_parse_intrinsic(ctx);
		} break;

		case Symbols::WHILE: {
			src_parse_while(ctx);
		} break;

		case Symbols::IF: {
			src_parse_if(ctx);
		} break;

		case Symbols::BLOCK_OPEN: {
			src_parse_block(ctx);
		} break;

		default:
			ctx.error(Phases::PHASE_SYNTACTIC, ctx.peek().view, STR_EXPR);
	}

	ctx.instruction(Symbols::END, ctx.peek().view);
}


// Statements
inline Decl src_parse_annotation(SourceContext& ctx, View name, Symbols linkage) {
	ctx.expect_token(common_is_type_annotation, ctx.peek().view, STR_ANNOTATION);
	ctx.next();  // skip `(`

	size_t in = 0u;
	size_t out = 0u;

	while (ctx.peek().kind != Symbols::EFFECT_SEPERATOR) {
		ctx.expect_token(equal(Symbols::IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
		ctx.next();  // skip identifier
		in++;
	}

	ctx.expect_token(equal(Symbols::EFFECT_SEPERATOR), ctx.peek().view, STR_EXPECT, symbol_to_string(Symbols::EFFECT_SEPERATOR));
	ctx.next();  // skip `->`

	while (ctx.peek().kind != Symbols::EFFECT_CLOSE) {
		ctx.expect_token(equal(Symbols::IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
		ctx.next();  // skip identifier
		out++;
	}

	ctx.expect_token(equal(Symbols::EFFECT_CLOSE), ctx.peek().view, STR_EXPECT, symbol_to_string(Symbols::EFFECT_CLOSE));
	ctx.next();  // skip `)`

	// Store type signature.
	auto [it, succ] = ctx.decl(name, Effect { in, out }, linkage);

	if (not succ)
		ctx.error(Phases::PHASE_SEMANTIC, name, STR_MULTIPLE_DECLARED, name);

	ctx.instruction(linkage, name, in, out);

	return it->second;
}

inline void src_parse_decl(SourceContext& ctx) {
	ctx.expect_token(src_is_decl, ctx.peek().view, STR_DECL);
	Symbols linkage = ctx.peek().kind;
	ctx.next();

	ctx.expect_token(equal(Symbols::IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
	View name = ctx.peek().view;
	ctx.next();

	src_parse_annotation(ctx, name, linkage);
}

inline void src_parse_fn(SourceContext& ctx) {
	ctx.expect_token(equal(Symbols::FN), ctx.peek().view, STR_FN);
	ctx.next();  // skip `fn`

	ctx.expect_token(equal(Symbols::IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
	View name = ctx.peek().view;
	ctx.next();

	Decl decl;

	// Inline declaration.
	if (common_is_type_annotation(ctx.peek().kind))
		decl = src_parse_annotation(ctx, name, Symbols::DECL);

	// Split declaration.
	else {
		auto it = ctx.decls.find(name);

		if (it == ctx.decls.end())
			ctx.error(Phases::PHASE_SEMANTIC, name, STR_UNDECLARED, name);

		decl = it->second;
	}

	auto [effect, linkage] = decl;

	ctx.instruction(Symbols::FN, name);

	// Body.
	src_parse_expression(ctx);
}

inline void src_parse_statement(SourceContext& ctx) {
	switch (ctx.peek().kind) {
		case Symbols::DECL:
		case Symbols::EXTERN:
			return src_parse_decl(ctx);

		case Symbols::FN:
			return src_parse_fn(ctx);

		default:
			ctx.error(Phases::PHASE_SYNTACTIC, ctx.peek().view, STR_STMT);
	}
}

inline void src_parse(SourceContext& ctx) {
	ctx.instruction(Symbols::PROGRAM, ctx.peek().view);

	while (ctx.peek().kind != Symbols::TERMINATOR)
		src_parse_statement(ctx);

	ctx.instruction(Symbols::END, ctx.peek().view);
}

}

#endif
