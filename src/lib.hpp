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


#define SYMBOL_TYPES \
	X(NONE,       none) \
	X(TERMINATOR, eof) \
	\
	X(BLOCK, block) \
	X(DECL,  decl) \
	X(DEF,   def) \
	X(WHILE, while) \
	X(IF,    if) \
	X(ELSE,  else) \
	X(ARROW, arrow) \
	\
	X(IDENTIFIER, identifier) \
	X(INTEGER,    integer) \
	\
	X(LPAREN, lparen) \
	X(RPAREN, rparen) \
	X(LBRACE, lbrace) \
	X(RBRACE, rbrace) \
	\
	X(COPY,   copy) \
	X(MOVE,   move) \
	X(REMOVE, remove) \
	X(PUSH,   push) \
	X(CALL,   call) \
	X(JUMP,   jump) \
	X(BRANCH, branch) \
	X(RET,    ret) \
	X(END,    end)

	#define X(name, str) name,
		enum class Symbols { SYMBOL_TYPES };
	#undef X

	#define X(name, str) #str##_sv,
		constexpr klx::View SYMBOL_TO_STRING[] = { SYMBOL_TYPES };
	#undef X
#undef SYMBOL_TYPES

inline std::ostream& operator<<(std::ostream& os, Symbols k) {
	return (os << SYMBOL_TO_STRING[(int)k]);
}


struct Token {
	klx::View view = ""_sv;
	klx::Symbols kind = Symbols::NONE;
};

inline std::ostream& operator<<(std::ostream& os, Token t) {
	return (os << '{' << t.kind << ", '" << t.view << "'}");
}


enum class LexerModes {
	SOURCE,
	INTERMEDIATE,
};

template <LexerModes MODE>
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

			if constexpr(MODE == LexerModes::SOURCE) {
				if      (view == "while"_sv) kind = Symbols::WHILE;
				else if (view == "if"_sv)    kind = Symbols::IF;
				else if (view == "def"_sv)   kind = Symbols::DEF;
				else if (view == "else"_sv)  kind = Symbols::ELSE;
				else if (view == "decl"_sv)  kind = Symbols::DECL;
				else if (view == "{"_sv)     kind = Symbols::LBRACE;
				else if (view == "}"_sv)     kind = Symbols::RBRACE;
				else if (view == "("_sv)     kind = Symbols::LPAREN;
				else if (view == ")"_sv)     kind = Symbols::RPAREN;
				else if (view == "->"_sv)    kind = Symbols::ARROW;
			}

			else if (MODE == LexerModes::INTERMEDIATE) {
				if      (view == "def"_sv)    kind = Symbols::DEF;
				else if (view == "block"_sv)  kind = Symbols::BLOCK;
				else if (view == "copy"_sv)   kind = Symbols::COPY;
				else if (view == "remove"_sv) kind = Symbols::REMOVE;
				else if (view == "move"_sv)   kind = Symbols::MOVE;
				else if (view == "push"_sv)   kind = Symbols::PUSH;
				else if (view == "call"_sv)   kind = Symbols::CALL;
				else if (view == "jump"_sv)   kind = Symbols::JUMP;
				else if (view == "branch"_sv) kind = Symbols::BRANCH;
				else if (view == "ret"_sv)    kind = Symbols::RET;
				else if (view == "end"_sv)    kind = Symbols::END;
				else if (view == "("_sv)      kind = Symbols::LPAREN;
				else if (view == ")"_sv)      kind = Symbols::RPAREN;
				else if (view == "->"_sv)     kind = Symbols::ARROW;
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

using IntermediateLexer = Lexer<LexerModes::INTERMEDIATE>;
using SourceLexer = Lexer<LexerModes::SOURCE>;


struct StackEffect {
	size_t in = 0;
	size_t out = 0;
};

inline std::ostream& operator<<(std::ostream& os, StackEffect se) {
	return (os << "( " << se.in << " -> " << se.out << " )");
}


struct Op {
	Symbols kind = Symbols::NONE;

	View sv;

	size_t x = 0;
	size_t y = 0;
	size_t z = 0;


	constexpr Op(Symbols kind_, size_t x_, size_t y_, size_t z_):
		kind(kind_), x(x_), y(y_), z(z_) {}

	constexpr Op(Symbols kind_, size_t x_, size_t y_):
		kind(kind_), x(x_), y(y_) {}

	constexpr Op(Symbols kind_, size_t x_):
		kind(kind_), x(x_) {}


	constexpr Op(Symbols kind_, View sv_):
		kind(kind_), sv(sv_) {}

	constexpr Op(Symbols kind_, View sv_, size_t x_, size_t y_, size_t z_):
		kind(kind_), sv(sv_), x(x_), y(y_), z(z_) {}

	constexpr Op(Symbols kind_, View sv_, size_t x_, size_t y_):
		kind(kind_), sv(sv_), x(x_), y(y_) {}

	constexpr Op(Symbols kind_, View sv_, size_t x_):
		kind(kind_), sv(sv_), x(x_) {}


	constexpr Op(Symbols kind_):
		kind(kind_) {}

	constexpr Op():
		kind(Symbols::NONE) {}
};


using IR = std::vector<Op>;
using Effects = std::unordered_map<View, StackEffect>;


struct Context: SourceLexer {
	size_t recent_block = 0;
	size_t block_id = 0;
	size_t stack = 0;

	IR instructions;
	Effects effects;


	inline Context(klx::View src):
		Lexer::Lexer(src)
	{
		effects = {
			{ "rm"_sv,   { 1, 0 } },
			{ "copy"_sv, { 1, 1 } },
			{ "move"_sv, { 1, 0 } },
		};
	}

	template <typename... Ts>
	decltype(auto) instruction(Ts&&... args) {
		return instructions.emplace_back(std::forward<Ts>(args)...);
	}

	decltype(auto) instruction_block(size_t id) {
		recent_block = instructions.size();
		return instruction(Symbols::BLOCK, id, stack);
	}

	decltype(auto) instruction_end() {
		instructions[recent_block].z = stack;
		return instruction(Symbols::END);
	}

	template <typename... Ts>
	decltype(auto) effect(Ts&&... args) {
		return effects.try_emplace(std::forward<Ts>(args)...);
	}

	size_t block() {
		return block_id++;
	}

	template <typename F, typename... Ts>
	void expect_token(const F& fn, View sv, Ts&&... args) {
		if (not fn(peek().kind))
			halt(Phases::PHASE_SYNTACTIC, original, sv, std::forward<Ts>(args)...);
	}

	template <typename F, typename... Ts>
	void expect_effect(const F& fn, View sv, Ts&&... args) {
		if (not fn(stack))
			halt(Phases::PHASE_SEMANTIC, original, sv, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	void error(Phases phase, View sv, Ts&&... args) {
		halt(phase, original, sv, std::forward<Ts>(args)...);
	}
};


struct StackContext: IntermediateLexer {
	IR instructions;

	inline StackContext(klx::View src):
		Lexer::Lexer(src) {}

	template <typename... Ts>
	decltype(auto) instruction(Ts&&... args) {
		return instructions.emplace_back(std::forward<Ts>(args)...);
	}

	template <typename F, typename... Ts>
	void expect_token(const F& fn, View sv, Ts&&... args) {
		if (not fn(peek().kind))
			halt(Phases::PHASE_SYNTACTIC, original, sv, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	void error(Phases phase, View sv, Ts&&... args) {
		halt(phase, original, sv, std::forward<Ts>(args)...);
	}
};


// Predicates.
constexpr auto is_stmt = partial_eq_any(Symbols::DEF, Symbols::DECL);
constexpr auto is_keyw = partial_eq_any(
	Symbols::WHILE,
	Symbols::IF,
	Symbols::DEF
);

constexpr auto is_expr = partial_eq_any(
	Symbols::INTEGER,
	Symbols::IDENTIFIER,
	Symbols::WHILE,
	Symbols::IF,
	Symbols::LBRACE
);

constexpr auto is_type_annotation = equal(Symbols::LPAREN);
constexpr auto is_block = equal(Symbols::LBRACE);
constexpr auto is_instruction = partial_eq_any(
	Symbols::COPY,
	Symbols::REMOVE,
	Symbols::MOVE,
	Symbols::PUSH,
	Symbols::CALL,
	Symbols::JUMP,
	Symbols::BRANCH,
	Symbols::RET,
	Symbols::END
);

// Stack IR parsing
inline StackEffect ir_parse_annotation (StackContext&);
inline void        ir_parse_def        (StackContext&);
inline void        ir_parse_block      (StackContext&);
inline void        ir_parse_identifier (StackContext&);
inline void        ir_parse            (StackContext&);


inline StackEffect ir_parse_annotation(StackContext& ctx) {
	ctx.next();  // skip `(`

	ctx.expect_token(equal(Symbols::INTEGER), ctx.peek().view, STR_INT);
	size_t in = to_int(ctx.next().view);

	ctx.expect_token(equal(Symbols::ARROW), ctx.peek().view, STR_EXPECT, "->"_sv);
	ctx.next();

	ctx.expect_token(equal(Symbols::INTEGER), ctx.peek().view, STR_INT);
	size_t out = to_int(ctx.next().view);

	ctx.expect_token(equal(Symbols::RPAREN), ctx.peek().view, STR_EXPECT, ")"_sv);
	ctx.next();  // skip `)`

	return { in, out };
}

inline void ir_parse_def(StackContext& ctx) {
	ctx.next();  // skip `def`

	ctx.expect_token(equal(Symbols::IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
	View name = ctx.next().view;

	ctx.expect_token(is_type_annotation, ctx.peek().view, STR_ANNOTATION);
	auto [in, out] = ir_parse_annotation(ctx);

	ctx.instruction(Symbols::DEF, name, in, out);

	while (eq_none(ctx.peek().kind, Symbols::RET, Symbols::TERMINATOR)) {
		ctx.expect_token(equal(Symbols::BLOCK), ctx.peek().view, STR_BLOCK);
		ir_parse_block(ctx);
	}

	ctx.expect_token(equal(Symbols::RET), ctx.peek().view, STR_EXPECT, "ret"_sv);
	ctx.next();  // skip `ret`

	ctx.instruction(Symbols::RET);
}

inline void ir_parse_block(StackContext& ctx) {
	ctx.next();  // skip `block`

	ctx.expect_token(equal(Symbols::INTEGER), ctx.peek().view, STR_INT);
	size_t name = to_int(ctx.next().view);

	ctx.expect_token(is_type_annotation, ctx.peek().view, STR_ANNOTATION);
	auto [in, out] = ir_parse_annotation(ctx);

	ctx.instruction(Symbols::BLOCK, name, in, out);

	while (eq_none(ctx.peek().kind, Symbols::END, Symbols::TERMINATOR)) {
		ctx.expect_token(is_instruction, ctx.peek().view, STR_INSTRUCTION);
		ir_parse_identifier(ctx);
	}

	ctx.expect_token(equal(Symbols::END), ctx.peek().view, STR_EXPECT, "end"_sv);
	ctx.next();  // skip `end`

	ctx.instruction(Symbols::END);
}

inline void ir_parse_identifier(StackContext& ctx) {
	Token tok = ctx.next();  // skip identifier

	switch (tok.kind) {
		case Symbols::COPY:
		case Symbols::REMOVE:
		case Symbols::MOVE:
		case Symbols::PUSH:
		case Symbols::JUMP: {
			ctx.expect_token(equal(Symbols::INTEGER), ctx.peek().view, STR_INT);
			size_t val = to_int(ctx.next().view);
			ctx.instruction(tok.kind, val);
		} break;

		case Symbols::BRANCH: {
			ctx.expect_token(equal(Symbols::INTEGER), ctx.peek().view, STR_INT);
			size_t a = to_int(ctx.next().view);

			ctx.expect_token(equal(Symbols::INTEGER), ctx.peek().view, STR_INT);
			size_t b = to_int(ctx.next().view);

			ctx.instruction(Symbols::BRANCH, a, b);
		} break;

		case Symbols::CALL: {
			ctx.expect_token(equal(Symbols::IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
			View name = ctx.next().view;

			ctx.expect_token(is_type_annotation, ctx.peek().view, STR_ANNOTATION);
			auto [in, out] = ir_parse_annotation(ctx);

			ctx.instruction(Symbols::CALL, name, in, out);
		} break;

		default:
			break;
	}
}

inline void ir_parse(StackContext& ctx) {
	ctx.expect_token(equal(Symbols::DEF), ctx.peek().view, STR_STMT);

	while (ctx.peek().kind != Symbols::TERMINATOR) {
		ctx.expect_token(equal(Symbols::DEF), ctx.peek().view, STR_DEF);
		ir_parse_def(ctx);
	}
}



// Parsing
inline void        src_parse_literal    (Context&);
inline void        src_parse_call       (Context&);
inline void        src_parse_while      (Context&);
inline void        src_parse_if         (Context&);
inline void        src_parse_block      (Context&);
inline void        src_parse_expression (Context&);

inline StackEffect src_parse_annotation (Context&);

inline void        src_parse_decl       (Context&);
inline void        src_parse_def        (Context&);
inline void        src_parse_statement  (Context&);
inline void        src_parse        (Context&);


// Expressions
inline void src_parse_literal(Context& ctx) {
	size_t num = to_int(ctx.next().view);
	ctx.instruction(Symbols::PUSH, num);
	ctx.stack++;
}

inline void src_parse_call(Context& ctx) {
	View name = ctx.next().view;

	// Argument intrinsics.
	if (eq_any(name, "cp"_sv, "mv"_sv, "rm"_sv)) {
		ctx.expect_token(equal(Symbols::INTEGER), ctx.peek().view, STR_ARG, name);

		size_t arg = to_int(ctx.next().view);
		Symbols kind = Symbols::NONE;

		ctx.expect_effect(more_equal(arg + 1), name, STR_EFFECT, arg + 1, ctx.stack);

		if (name == "mv"_sv)
			kind = Symbols::MOVE;

		else if (name == "cp"_sv) {
			kind = Symbols::COPY;
			ctx.stack++;
		}

		else if (name == "rm"_sv) {
			kind = Symbols::REMOVE;
			ctx.stack--;
		}

		ctx.instruction(kind, arg);

		return;
	}

	// Not an intrinsic.
	auto it = ctx.effects.find(name);

	if (it == ctx.effects.end())
		ctx.error(Phases::PHASE_SEMANTIC, name, STR_UNDECLARED, name);

	auto [in, out] = it->second;

	ctx.expect_effect(more_equal(in), name, STR_EFFECT, in, ctx.stack);
	ctx.instruction(Symbols::CALL, name, in, out);

	ctx.stack -= in;
	ctx.stack += out;
}

inline void src_parse_while(Context& ctx) {
	View pos = ctx.next().view;  // skip `while`

	size_t header_block = ctx.block();
	size_t body_block = ctx.block();
	size_t end_block = ctx.block();

	ctx.instruction(Symbols::JUMP, header_block);
	ctx.instruction_end();
	ctx.instruction_block(header_block);

	// Expression.
	ctx.expect_token(is_expr, ctx.peek().view, STR_EXPR);
	src_parse_expression(ctx);

	// Consume boolean.
	ctx.expect_effect(more_equal(1u), ctx.peek().view, STR_EFFECT, 1u, ctx.stack);
	ctx.stack--;

	ctx.instruction(Symbols::BRANCH, body_block, end_block);
	ctx.instruction_end();
	size_t stack_before = ctx.stack;

	// Body.
	ctx.instruction_block(body_block);
	ctx.expect_token(is_expr, ctx.peek().view, STR_EXPR);
	src_parse_expression(ctx);

	// Check is stack size has been altered.
	ctx.expect_effect(equal(stack_before), pos, STR_EFFECT_ALTERED, stack_before, ctx.stack);

	ctx.instruction(Symbols::JUMP, header_block);
	ctx.instruction_end();
	ctx.instruction_block(end_block);
}

inline void src_parse_if(Context& ctx) {
	View pos = ctx.next().view;  // skip `if`

	size_t start_block = ctx.block();
	size_t else_block = ctx.block();
	size_t end_block = ctx.block();

	// Expression.
	ctx.expect_token(is_expr, ctx.peek().view, STR_EXPR);
	src_parse_expression(ctx);

	// Consume boolean.
	ctx.expect_effect(more_equal(1u), ctx.peek().view, STR_EFFECT, 1u, ctx.stack);
	ctx.stack--;

	// Emit a branch instruction to jump to either the main body,
	// the else branch or the end of the branch entirely.
	ctx.instruction(Symbols::BRANCH, start_block, else_block);
	ctx.instruction_end();
	ctx.instruction_block(start_block);

	// Store the state of the stack before the body of the branch.
	size_t stack_before = ctx.stack;

	// First block.
	pos = ctx.peek().view;
	ctx.expect_token(is_expr, pos, STR_EXPR);
	src_parse_expression(ctx);

	// Second block.
	if (ctx.peek().kind == Symbols::ELSE) {
		// Jump to the end if the true branch is taken so that
		// we don't fall through to the false branch.
		ctx.instruction(Symbols::JUMP, end_block);

		size_t stack_body = ctx.stack;  // Store effect of body.
		ctx.stack = stack_before;   // Restore previous state of stack.

		// Create else block.
		ctx.instruction_end();
		ctx.instruction_block(else_block);
		pos = ctx.next().view;  // skip `else`

		ctx.expect_token(is_expr, pos, STR_EXPR);
		src_parse_expression(ctx);

		// Check is stack size has been altered.
		ctx.expect_effect(equal(stack_body), pos, STR_EFFECT_BRANCH, stack_body, ctx.stack);

		// Create end block.
		ctx.instruction(Symbols::JUMP, end_block);
		ctx.instruction_end();
		ctx.instruction_block(end_block);
	}

	else {
		// Create end of branch block.
		ctx.instruction(Symbols::JUMP, else_block);
		ctx.instruction_end();
		ctx.instruction_block(else_block);

		// Expect that the stack size has not been altered.
		ctx.expect_effect(equal(stack_before), pos, STR_EFFECT_ALTERED, stack_before, ctx.stack);
	}
}

inline void src_parse_block(Context& ctx) {
	ctx.next();  // skip `{`
	ctx.expect_token(is_expr, ctx.peek().view, STR_EXPR);

	while (eq_none(ctx.peek().kind, Symbols::RBRACE, Symbols::TERMINATOR))
		src_parse_expression(ctx);

	ctx.expect_token(equal(Symbols::RBRACE), ctx.peek().view, STR_EXPECT, "}"_sv);
	ctx.next();  // skip `}`
}

inline void src_parse_expression(Context& ctx) {
	switch (ctx.peek().kind) {
		case Symbols::INTEGER:    return src_parse_literal(ctx);
		case Symbols::IDENTIFIER: return src_parse_call(ctx);
		case Symbols::WHILE:      return src_parse_while(ctx);
		case Symbols::IF:         return src_parse_if(ctx);
		case Symbols::LBRACE:     return src_parse_block(ctx);

		default:
			ctx.error(Phases::PHASE_SYNTACTIC, ctx.peek().view, STR_EXPR);
	}
}


// Statements
inline StackEffect src_parse_annotation(Context& ctx) {
	ctx.next();  // skip `(`

	size_t in = 0;
	size_t out = 0;

	auto* ptr = &in;

	while (eq_none(ctx.peek().kind, Symbols::RPAREN, Symbols::TERMINATOR)) {
		if (ctx.peek().kind == Symbols::ARROW) {
			ctx.next();  // skip `->`
			ptr = &out;

			if (ctx.peek().kind == Symbols::RPAREN)
				break;
		}

		ctx.expect_token(equal(Symbols::IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
		ctx.next();  // skip identifier

		(*ptr)++;
	}

	ctx.expect_token(equal(Symbols::RPAREN), ctx.peek().view, STR_EXPECT, ")"_sv);
	ctx.next();  // skip `)`

	return { in, out };
}

inline void src_parse_decl(Context& ctx) {
	ctx.next();  // skip `decl`

	ctx.expect_token(equal(Symbols::IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
	View name = ctx.next().view;

	ctx.expect_token(is_type_annotation, ctx.peek().view, STR_ANNOTATION);
	StackEffect se = src_parse_annotation(ctx);

	// Store type signature.
	if (auto [it, succ] = ctx.effect(name, se); not succ)
		ctx.error(Phases::PHASE_SEMANTIC, name, STR_MULTIPLE_DECLARED, name);
}

inline void src_parse_def(Context& ctx) {
	ctx.stack = 0;
	ctx.block_id = 0;

	ctx.next();  // skip `def`

	ctx.expect_token(equal(Symbols::IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
	View name = ctx.next().view;

	auto it = ctx.effects.find(name);

	if (it == ctx.effects.end())
		ctx.error(Phases::PHASE_SEMANTIC, name, STR_UNDECLARED, name);

	auto [in, out] = it->second;
	ctx.stack = in;

	ctx.instruction(Symbols::DEF, name, in, out);
	ctx.instruction_block(ctx.block());

	ctx.expect_token(is_expr, ctx.peek().view, STR_EXPR);
	src_parse_expression(ctx);

	ctx.expect_effect(equal(out), name, STR_EFFECT_RETURN, out, ctx.stack);

	ctx.instruction_end();
	ctx.instruction(Symbols::RET);
}

inline void src_parse_statement(Context& ctx) {
	switch (ctx.peek().kind) {
		case Symbols::DECL:  return src_parse_decl(ctx);
		case Symbols::DEF:   return src_parse_def(ctx);

		default:
			ctx.error(Phases::PHASE_SYNTACTIC, ctx.peek().view, STR_STMT);
	}
}

inline void src_parse(Context& ctx) {
	ctx.expect_token(is_stmt, ctx.peek().view, STR_STMT);

	while (ctx.peek().kind != Symbols::TERMINATOR)
		src_parse_statement(ctx);
}

}


// IR serialisation
namespace klx {

inline std::ostream& operator<<(std::ostream& os, klx::Op instr) {
	out(os, instr.kind);

	switch (instr.kind) {
		case Symbols::NONE:
		case Symbols::PUSH:
		case Symbols::COPY:
		case Symbols::MOVE:
		case Symbols::REMOVE:
		case Symbols::JUMP:
			out(os, " ", instr.x); break;

		case Symbols::BLOCK:
			outfmt(os, " {} ( {} -> {} )", instr.x, instr.y, instr.z); break;

		case Symbols::CALL:
		case Symbols::DEF:
			outfmt(os, " {} ( {} -> {} )", instr.sv, instr.x, instr.y); break;

		case Symbols::BRANCH:
			outfmt(os, " {} {}", instr.x, instr.y); break;

		case Symbols::RET:
		default: break;
	}

	return os;
}

inline void serialise(const IR& ir) {
	for (auto& op: ir) {
		if (op.kind == Symbols::NONE)
			continue;

		switch (op.kind) {
			case Symbols::RET:
			case Symbols::DEF: break;

			case Symbols::BLOCK:
			case Symbols::END:
				print("\t"); break;

			default:
				print("\t\t"); break;
		}

		println(op);
	}
}

}

#endif
