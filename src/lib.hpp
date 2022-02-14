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
	TOKENTYPE_IR  = 0b010,

	TOKENTYPE_COMMON = TOKENTYPE_SRC | TOKENTYPE_IR,
};

#define SYMBOL_TYPES \
	X(NONE,       "none", TOKENTYPE_COMMON) \
	X(TERMINATOR, "eof",  TOKENTYPE_SRC) \
	\
	/* Special */ \
	X(IDENTIFIER, "identifier", TOKENTYPE_COMMON) \
	X(INTEGER,    "integer",    TOKENTYPE_COMMON) \
	\
	/* Blocks */ \
	X(DEF,   "def",   TOKENTYPE_COMMON) \
	X(RET,   "ret",   TOKENTYPE_IR) \
	X(BLOCK, "block", TOKENTYPE_IR) \
	X(END,   "end",   TOKENTYPE_IR) \
	\
	/* Linkage Specifiers */ \
	X(DECL,   "decl",   TOKENTYPE_SRC) /* Implicitly Local */ \
	X(EXTERN, "extern", TOKENTYPE_SRC) /* Implicitly Global */ \
	X(LOCAL,  "local",  TOKENTYPE_IR) \
	X(GLOBAL, "global", TOKENTYPE_IR) \
	\
	/* Control Flow */ \
	X(WHILE,  "while",  TOKENTYPE_SRC) \
	X(IF,     "if",     TOKENTYPE_SRC) \
	X(ELSE,   "else",   TOKENTYPE_SRC) \
	X(CALL,   "call",   TOKENTYPE_IR) \
	X(JUMP,   "jump",   TOKENTYPE_IR) \
	X(BRANCH, "branch", TOKENTYPE_IR) \
	\
	/* Stack Effect Annotation */ \
	X(EFFECT_SEPERATOR, "->", TOKENTYPE_COMMON) \
	X(EFFECT_OPEN,      "(",  TOKENTYPE_COMMON) \
	X(EFFECT_CLOSE,     ")",  TOKENTYPE_COMMON) \
	\
	/* Block Expression */ \
	X(BLOCK_OPEN,  "{", TOKENTYPE_SRC) \
	X(BLOCK_CLOSE, "}", TOKENTYPE_SRC) \
	\
	/* Stack/Register Manipulation */ \
	X(LOAD,   "load",   TOKENTYPE_IR) \
	X(POP,    "pop",    TOKENTYPE_IR) \
	X(PUSH,   "push",   TOKENTYPE_IR) \
	X(COPY,   "copy",   TOKENTYPE_COMMON) \
	X(MOVE,   "move",   TOKENTYPE_SRC) \
	X(REMOVE, "remove", TOKENTYPE_SRC)

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

	constexpr Op(Symbols kind_, size_t x_, size_t y_ = 0u, size_t z_ = 0u):
		kind(kind_), x(x_), y(y_), z(z_) {}

	constexpr Op(Symbols kind_, View sv_):
		kind(kind_), sv(sv_) {}

	constexpr Op(Symbols kind_):
		kind(kind_) {}

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
	Symbols linkage = Symbols::LOCAL;

	constexpr Decl() {}
	constexpr Decl(Effect effect_, Symbols linkage_):
		effect(effect_), linkage(linkage_) {}
};

using Declarations = std::unordered_map<View, Decl>;


template <TokenTypes MODE>
struct Context: Lexer<MODE> {
	size_t stack = 0u;
	size_t block_id = 0u;
	size_t register_id = 0u;

	IR instructions;
	Declarations decls;

	inline Context(klx::View src): Lexer<MODE>::Lexer(src) {
		instructions.reserve(instructions.capacity() + src.size());
	}

	size_t reg_at(size_t i) const {
		return (register_id - 1) - i;
	}

	size_t push(size_t n = 1) {
		stack += n;
		return stack;
	}

	size_t pop(size_t n = 1) {
		stack -= n;
		return stack;
	}

	size_t block() { return block_id++; }
	size_t reg() { return register_id++; }

	void reset() {
		stack = 0u;
		block_id = 0u;
		register_id = 0u;
	}

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
	Symbols::DEF,
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
	Symbols::DEF,
	Symbols::RET,
	Symbols::BLOCK,
	Symbols::END,
	Symbols::EXTERN,
	Symbols::LOCAL,
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
	Symbols::DEF,
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

constexpr auto common_is_type_annotation = equal(Symbols::EFFECT_OPEN);
constexpr auto src_is_block = equal(Symbols::BLOCK_OPEN);

// Is a basic instruction (i.e. not block/def/end/ret)
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
inline void   src_parse_literal    (SourceContext&);
inline void   src_parse_call       (SourceContext&);
inline void   src_parse_while      (SourceContext&);
inline void   src_parse_if         (SourceContext&);
inline void   src_parse_block      (SourceContext&);
inline void   src_parse_expression (SourceContext&);
inline Effect src_parse_annotation (SourceContext&);
inline void   src_parse_decl       (SourceContext&);
inline void   src_parse_def        (SourceContext&);
inline void   src_parse_statement  (SourceContext&);
inline void   src_parse            (SourceContext&);


// Expressions
inline void src_parse_literal(SourceContext& ctx) {
	size_t num = to_int(ctx.next().view);
	ctx.instruction(Symbols::LOAD, ctx.reg(), num);
	ctx.push();
}

inline void src_parse_call(SourceContext& ctx) {
	Token tok = ctx.next();
	View name = tok.view;

	// Argument intrinsics.
	if (eq_any(tok.kind,
		Symbols::COPY,
		Symbols::MOVE,
		Symbols::REMOVE
	)) {
		ctx.expect_token(equal(Symbols::INTEGER), ctx.peek().view, STR_ARG, name);

		size_t arg = to_int(ctx.next().view);
		ctx.expect_effect(more_equal(arg + 1u), name, STR_EFFECT, arg + 1u, ctx.stack);

		switch (tok.kind) {
			case Symbols::COPY: {
				ctx.push();
				ctx.instruction(tok.kind, ctx.reg(), ctx.reg_at(arg));
			} break;

			case Symbols::REMOVE: {
				ctx.pop();
			} break;

			case Symbols::MOVE: {

			} break;

			default: break;
		}

		// ctx.instruction(tok.kind, arg);
		return;
	}

	// Not an intrinsic.
	auto it = ctx.decls.find(name);

	if (it == ctx.decls.end())
		ctx.error(Phases::PHASE_SEMANTIC, name, STR_UNDECLARED, name);

	auto [effect, linkage] = it->second;

	// Push arguments to call.
	for (size_t i = 0; i != effect.in; ++i)
		ctx.instruction(Symbols::PUSH, ctx.reg_at(i));

	ctx.expect_effect(more_equal(effect.in), name, STR_EFFECT, effect.in, ctx.stack);
	ctx.instruction(Symbols::CALL, name);

	// Pop return values to registers.
	for (size_t i = 0; i != effect.out; ++i)
		ctx.instruction(Symbols::POP, ctx.reg());

	ctx.pop(effect.in);
	ctx.push(effect.out);
}

inline void src_parse_while(SourceContext& ctx) {
	View pos = ctx.next().view;  // skip `while`

	size_t header_block = ctx.block();
	size_t body_block = ctx.block();
	size_t end_block = ctx.block();

	ctx.instruction(Symbols::JUMP, header_block);
	ctx.instruction(Symbols::END);
	ctx.instruction(Symbols::BLOCK, header_block);

	// Expression.
	ctx.expect_token(src_is_expr, ctx.peek().view, STR_EXPR);
	src_parse_expression(ctx);

	// Consume boolean.
	ctx.expect_effect(more_equal(1u), ctx.peek().view, STR_EFFECT, 1u, ctx.stack);
	ctx.pop();

	ctx.instruction(Symbols::BRANCH, body_block, end_block);
	ctx.instruction(Symbols::END);
	size_t stack_before = ctx.stack;

	// Body.
	ctx.instruction(Symbols::BLOCK, body_block);
	ctx.expect_token(src_is_expr, ctx.peek().view, STR_EXPR);
	src_parse_expression(ctx);

	// Check is stack size has been altered.
	ctx.expect_effect(equal(stack_before), pos, STR_EFFECT_ALTERED, stack_before, ctx.stack);

	ctx.instruction(Symbols::JUMP, header_block);
	ctx.instruction(Symbols::END);
	ctx.instruction(Symbols::BLOCK, end_block);
}

inline void src_parse_if(SourceContext& ctx) {
	View pos = ctx.next().view;  // skip `if`

	size_t start_block = ctx.block();
	size_t else_block = ctx.block();
	size_t end_block = ctx.block();

	// Expression.
	ctx.expect_token(src_is_expr, ctx.peek().view, STR_EXPR);
	src_parse_expression(ctx);

	// Consume boolean.
	ctx.expect_effect(more_equal(1u), ctx.peek().view, STR_EFFECT, 1u, ctx.stack);
	ctx.pop();

	// Emit a branch instruction to jump to either the main body,
	// the else branch or the end of the branch entirely.
	ctx.instruction(Symbols::BRANCH, start_block, else_block);
	ctx.instruction(Symbols::END);
	ctx.instruction(Symbols::BLOCK, start_block);

	// Store the state of the stack before the body of the branch.
	size_t stack_before = ctx.stack;

	// First block.
	pos = ctx.peek().view;
	ctx.expect_token(src_is_expr, pos, STR_EXPR);
	src_parse_expression(ctx);

	// Second block.
	if (ctx.peek().kind == Symbols::ELSE) {
		// Jump to the end if the true branch is taken so that
		// we don't fall through to the false branch.
		ctx.instruction(Symbols::JUMP, end_block);

		size_t stack_body = ctx.stack;  // Store effect of body.
		ctx.stack = stack_before;   // Restore previous state of stack.

		// Create else block.
		ctx.instruction(Symbols::END);
		ctx.instruction(Symbols::BLOCK, else_block);
		pos = ctx.next().view;  // skip `else`

		ctx.expect_token(src_is_expr, pos, STR_EXPR);
		src_parse_expression(ctx);

		// Check is stack size has been altered.
		ctx.expect_effect(equal(stack_body), pos, STR_EFFECT_BRANCH, stack_body, ctx.stack);

		// Create end block.
		ctx.instruction(Symbols::JUMP, end_block);
		ctx.instruction(Symbols::END);
		ctx.instruction(Symbols::BLOCK, end_block);
	}

	else {
		// Create end of branch block.
		ctx.instruction(Symbols::JUMP, else_block);
		ctx.instruction(Symbols::END);
		ctx.instruction(Symbols::BLOCK, else_block);

		// Expect that the stack size has not been altered.
		ctx.expect_effect(equal(stack_before), pos, STR_EFFECT_ALTERED, stack_before, ctx.stack);
	}
}

inline void src_parse_block(SourceContext& ctx) {
	ctx.next();  // skip `{`
	ctx.expect_token(src_is_expr, ctx.peek().view, STR_EXPR);

	while (eq_none(ctx.peek().kind, Symbols::BLOCK_CLOSE, Symbols::TERMINATOR))
		src_parse_expression(ctx);

	ctx.expect_token(equal(Symbols::BLOCK_CLOSE), ctx.peek().view, STR_EXPECT, symbol_to_string(Symbols::BLOCK_CLOSE));
	ctx.next();  // skip `}`
}

inline void src_parse_expression(SourceContext& ctx) {
	switch (ctx.peek().kind) {
		case Symbols::INTEGER: return src_parse_literal(ctx);

		case Symbols::IDENTIFIER:
		case Symbols::COPY:
		case Symbols::MOVE:
		case Symbols::REMOVE:
			return src_parse_call(ctx);

		case Symbols::WHILE:      return src_parse_while(ctx);
		case Symbols::IF:         return src_parse_if(ctx);
		case Symbols::BLOCK_OPEN: return src_parse_block(ctx);

		default:
			ctx.error(Phases::PHASE_SYNTACTIC, ctx.peek().view, STR_EXPR);
	}
}


// Statements
inline Effect src_parse_annotation(SourceContext& ctx) {
	ctx.next();  // skip `(`

	size_t in = 0u;
	size_t out = 0u;

	while (ctx.peek().kind != Symbols::EFFECT_SEPERATOR) {
		ctx.expect_token(equal(Symbols::IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
		ctx.next();  // skip identifier
		in++;
	}

	ctx.expect_token(equal(Symbols::EFFECT_SEPERATOR), ctx.peek().view, STR_EXPECT, symbol_to_string(Symbols::EFFECT_SEPERATOR));
	ctx.next();  // skip `)`

	while (ctx.peek().kind != Symbols::EFFECT_CLOSE) {
		ctx.expect_token(equal(Symbols::IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
		ctx.next();  // skip identifier
		out++;
	}

	ctx.expect_token(equal(Symbols::EFFECT_CLOSE), ctx.peek().view, STR_EXPECT, symbol_to_string(Symbols::EFFECT_CLOSE));
	ctx.next();  // skip `)`

	return { in, out };
}

inline void src_parse_decl(SourceContext& ctx) {
	Symbols kind = ctx.next().kind;  // skip `internal`/`external`

	ctx.expect_token(equal(Symbols::IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
	View name = ctx.next().view;

	ctx.expect_token(common_is_type_annotation, ctx.peek().view, STR_ANNOTATION);
	Effect se = src_parse_annotation(ctx);

	if (kind == Symbols::DECL)
		kind = Symbols::LOCAL;

	else if (kind == Symbols::EXTERN)
		kind = Symbols::GLOBAL;

	// Store type signature.
	if (auto [it, succ] = ctx.decl(name, se, kind); not succ)
		ctx.error(Phases::PHASE_SEMANTIC, name, STR_MULTIPLE_DECLARED, name);
}

inline void src_parse_def(SourceContext& ctx) {
	ctx.reset();
	ctx.next();  // skip `def`

	ctx.expect_token(equal(Symbols::IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
	View name = ctx.next().view;

	Decl decl;

	// Inline declaration.
	if (common_is_type_annotation(ctx.peek().kind)) {
		Effect se = src_parse_annotation(ctx);

		if (auto [it, succ] = ctx.decl(name, se, Symbols::LOCAL); not succ)
			ctx.error(Phases::PHASE_SEMANTIC, name, STR_MULTIPLE_DECLARED, name);

		else
			decl = it->second;
	}

	// Split declaration.
	else {
		auto it = ctx.decls.find(name);

		if (it == ctx.decls.end())
			ctx.error(Phases::PHASE_SEMANTIC, name, STR_UNDECLARED, name);

		decl = it->second;
	}

	auto [effect, linkage] = decl;

	ctx.instruction(Symbols::DEF, name);
	ctx.instruction(Symbols::BLOCK, ctx.block());

	// Pop arguments to function into registers.
	for (size_t i = 0; i != effect.in; ++i)
		ctx.instruction(Symbols::POP, ctx.reg());

	ctx.stack = effect.in;

	ctx.expect_token(src_is_expr, ctx.peek().view, STR_EXPR);
	src_parse_expression(ctx);

	ctx.expect_effect(equal(effect.out), name, STR_EFFECT_RETURN, effect.out, ctx.stack);

	// Push return values to stack.
	for (size_t i = 0; i != effect.out; ++i)
		ctx.instruction(Symbols::PUSH, ctx.reg_at(i));

	ctx.instruction(Symbols::END);
	ctx.instruction(Symbols::RET);
}

inline void src_parse_statement(SourceContext& ctx) {
	switch (ctx.peek().kind) {
		case Symbols::DECL:
		case Symbols::EXTERN:
			return src_parse_decl(ctx);

		case Symbols::DEF:
			return src_parse_def(ctx);

		default:
			ctx.error(Phases::PHASE_SYNTACTIC, ctx.peek().view, STR_STMT);
	}
}

inline void src_parse(SourceContext& ctx) {
	ctx.expect_token(src_is_stmt, ctx.peek().view, STR_STMT);

	while (ctx.peek().kind != Symbols::TERMINATOR)
		src_parse_statement(ctx);
}

}


// IR serialisation
namespace klx {

template <TokenTypes MODE>
inline void serialise_ir(std::ostream& os, const Context<MODE>& ctx) {
	// Declarations.
	for (auto [name, value]: ctx.decls) {
		auto [effect, linkage] = value;
		outlnfmt(os, "{} {} ( {} -> {} )", symbol_to_string(linkage), name, effect.in, effect.out);
	}

	// Code.
	for (auto [kind, sv, x, y, z]: ctx.instructions) {
		if (not ir_is_valid(kind))
			ctx.error(Phases::PHASE_INTERMEDIATE, ""_sv, STR_UNKNOWN_IR, symbol_to_string(kind));

		if (kind == Symbols::NONE)
			continue;

		// Indentation.
		switch (kind) {
			case Symbols::DEF:
			case Symbols::RET: break;

			case Symbols::BLOCK:
			case Symbols::END: {
				out(os, " ");
			} break;

			default: {
				out(os, "  ");
			} break;
		}

		// Instructions.
		out(os, symbol_to_string(kind));

		switch (kind) {
			case Symbols::PUSH:
			case Symbols::POP:
			case Symbols::JUMP:
			case Symbols::BLOCK: {
				out(os, " ", x);
			} break;

			case Symbols::CALL:
			case Symbols::DEF: {
				out(os, " ", sv);
			} break;

			case Symbols::LOAD:
			case Symbols::COPY:
			case Symbols::BRANCH: {
				out(os, " ", x, " ", y);
			} break;

			case Symbols::END:
			case Symbols::RET:
			default: break;
		}

		out(os, '\n');
	}
}

}

#endif
