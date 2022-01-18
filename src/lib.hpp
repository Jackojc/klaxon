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


#define TOKEN_TYPES \
	X(TKN_NONE,       none) \
	X(TKN_EOF,        eof) \
	\
	X(TKN_DECL,       decl) \
	X(TKN_DEF,        def) \
	X(TKN_WHILE,      while) \
	X(TKN_IF,         if) \
	X(TKN_ELSE,       else) \
	\
	X(TKN_IDENTIFIER, identifier) \
	X(TKN_NUM,        num) \
	\
	X(TKN_LPAREN,     lparen) \
	X(TKN_RPAREN,     rparen) \
	X(TKN_LBRACE,     lbrace) \
	X(TKN_RBRACE,     rbrace) \
	X(TKN_ARROW,      arrow) \
	\
	X(STACK_TKN_DEF,        def) \
	X(STACK_TKN_BLOCK,      block) \
	X(STACK_TKN_COPY,       copy) \
	X(STACK_TKN_MOVE,       move) \
	X(STACK_TKN_REMOVE,     remove) \
	X(STACK_TKN_PUSH,       push) \
	X(STACK_TKN_CALL,       call) \
	X(STACK_TKN_JUMP,       jump) \
	X(STACK_TKN_BRANCH,     branch) \
	X(STACK_TKN_RET,        ret) \
	X(STACK_TKN_END,        end) \
	X(STACK_TKN_IDENTIFIER, identifier) \
	X(STACK_TKN_INTEGER,    integer)

	#define X(name, str) name,
		enum class Tokens { TOKEN_TYPES };
	#undef X

	#define X(name, str) #str##_sv,
		constexpr klx::View TOKEN_TO_STRING[] = { TOKEN_TYPES };
	#undef X
#undef TOKEN_TYPES

inline std::ostream& operator<<(std::ostream& os, Tokens k) {
	return (os << TOKEN_TO_STRING[(int)k]);
}


struct Token {
	klx::View view = ""_sv;
	klx::Tokens kind = Tokens::TKN_NONE;
};

inline std::ostream& operator<<(std::ostream& os, Token t) {
	return (os << '{' << t.kind << ", '" << t.view << "'}");
}


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
			kind = Tokens::TKN_EOF;
			view = View {};  // set to eof
		}

		// Skip comments.
		else if (c == '#') {
			klx::consume_char(src, c, not_equal((uint32_t)'\n'));  // Skip until \n and then return next token.
			return next();
		}

		// Numbers.
		else if (klx::is_number(c)) {
			kind = Tokens::TKN_NUM;
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
			kind = Tokens::TKN_IDENTIFIER;
			view = klx::consume_char(src, c, klx::is_visible);

			if      (view == "while"_sv) kind = Tokens::TKN_WHILE;
			else if (view == "if"_sv)    kind = Tokens::TKN_IF;
			else if (view == "def"_sv)   kind = Tokens::TKN_DEF;
			else if (view == "else"_sv)  kind = Tokens::TKN_ELSE;
			else if (view == "decl"_sv)  kind = Tokens::TKN_DECL;

			else if (view == "{"_sv)  kind = Tokens::TKN_LBRACE;
			else if (view == "}"_sv)  kind = Tokens::TKN_RBRACE;
			else if (view == "("_sv)  kind = Tokens::TKN_LPAREN;
			else if (view == ")"_sv)  kind = Tokens::TKN_RPAREN;
			else if (view == "->"_sv) kind = Tokens::TKN_ARROW;
		}

		else {
			halt(Phases::PHASE_LEXICAL, original, view, STR_UNKNOWN_CHAR, view);
		}

		Token out = peek_;
		peek_ = tok;

		return out;
	}
};


struct StackLexer {
	klx::View original {};
	klx::View src {};

	klx::Token peek_ {};

	constexpr StackLexer(klx::View src_): original(src_), src(src_) {
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
			kind = Tokens::TKN_EOF;
			view = View {};  // set to eof
		}

		// Skip comments.
		else if (c == '#') {
			klx::consume_char(src, c, not_equal((uint32_t)'\n'));  // Skip until \n and then return next token.
			return next();
		}

		// Numbers.
		else if (klx::is_number(c)) {
			kind = Tokens::STACK_TKN_INTEGER;
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
			kind = Tokens::STACK_TKN_IDENTIFIER;
			view = klx::consume_char(src, c, klx::is_visible);

			if      (view == "def"_sv)    kind = Tokens::STACK_TKN_DEF;
			else if (view == "block"_sv)  kind = Tokens::STACK_TKN_BLOCK;
			else if (view == "copy"_sv)   kind = Tokens::STACK_TKN_COPY;
			else if (view == "remove"_sv) kind = Tokens::STACK_TKN_REMOVE;
			else if (view == "move"_sv)   kind = Tokens::STACK_TKN_MOVE;
			else if (view == "push"_sv)   kind = Tokens::STACK_TKN_PUSH;
			else if (view == "call"_sv)   kind = Tokens::STACK_TKN_CALL;
			else if (view == "jump"_sv)   kind = Tokens::STACK_TKN_JUMP;
			else if (view == "branch"_sv) kind = Tokens::STACK_TKN_BRANCH;
			else if (view == "ret"_sv)    kind = Tokens::STACK_TKN_RET;
			else if (view == "end"_sv)    kind = Tokens::STACK_TKN_END;
		}

		else {
			halt(Phases::PHASE_LEXICAL, original, view, STR_UNKNOWN_CHAR, view);
		}

		Token out = peek_;
		peek_ = tok;

		return out;
	}
};


struct StackEffect {
	size_t in = 0;
	size_t out = 0;
};

inline std::ostream& operator<<(std::ostream& os, StackEffect se) {
	return (os << "( " << se.in << " -> " << se.out << " )");
}


#define OPS \
	X(OP_NONE, none) \
	\
	X(OP_PUSH,   push) \
	X(OP_COPY,   copy) \
	X(OP_MOVE,   move) \
	X(OP_REMOVE, remove) \
	X(OP_CALL,   call) \
	X(OP_DEF,    def) \
	X(OP_RET,    ret) \
	X(OP_BLOCK,  block) \
	X(OP_END,    end) \
	X(OP_BRANCH, branch) \
	X(OP_JUMP,   jump)

	#define X(name, str) name,
		enum class Ops { OPS };
	#undef X

	#define X(name, str) #str##_sv,
		constexpr klx::View OP_TO_STRING[] = { OPS };
	#undef X

	inline std::ostream& operator<<(std::ostream& os, Ops o) {
		return (os << OP_TO_STRING[(int)o]);
	}
#undef OPS


struct Op {
	Ops kind = Ops::OP_NONE;

	View sv;

	size_t x = 0;
	size_t y = 0;
	size_t z = 0;


	constexpr Op(Ops kind_, size_t x_, size_t y_, size_t z_):
		kind(kind_), x(x_), y(y_), z(z_) {}

	constexpr Op(Ops kind_, size_t x_, size_t y_):
		kind(kind_), x(x_), y(y_) {}

	constexpr Op(Ops kind_, size_t x_):
		kind(kind_), x(x_) {}

	constexpr Op(Ops kind_, View sv_):
		kind(kind_), sv(sv_) {}

	constexpr Op(Ops kind_):
		kind(kind_) {}

	constexpr Op():
		kind(Ops::OP_NONE) {}
};


using IR = std::vector<Op>;
using Effects = std::unordered_map<View, StackEffect>;


struct Context: Lexer {
	size_t block_id = 0;
	size_t out_id = 0;
	size_t stack = 0;

	IR instructions;
	Effects effects;


	inline Context(klx::View src):
		Lexer::Lexer(src)
	{
		effects = {
			// { "add"_sv,  { 2, 1 } },
			// { "sub"_sv,  { 2, 1 } },
			// { "mul"_sv,  { 2, 1 } },
			// { "div"_sv,  { 2, 1 } },
			// { "mod"_sv,  { 2, 1 } },
			// { "lsh"_sv,  { 2, 1 } },
			// { "rsh"_sv,  { 2, 1 } },
			// { "lt"_sv,   { 2, 1 } },
			// { "mt"_sv,   { 2, 1 } },
			// { "eq"_sv,   { 2, 1 } },
			// { "and"_sv,  { 2, 1 } },
			// { "or"_sv,   { 2, 1 } },
			// { "not"_sv,  { 2, 1 } },
			// { "xor"_sv,  { 2, 1 } },
			// { "band"_sv, { 2, 1 } },
			// { "bor"_sv,  { 2, 1 } },
			// { "bnot"_sv, { 2, 1 } },
			// { "bxor"_sv, { 2, 1 } },
			// { "word"_sv, { 0, 1 } },
			{ "rm"_sv,   { 1, 0 } },
			{ "copy"_sv, { 1, 1 } },
			{ "move"_sv, { 1, 0 } },
		};
	}

	template <typename... Ts>
	decltype(auto) instruction(Ts&&... args) {
		return instructions.emplace_back(std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	decltype(auto) effect(Ts&&... args) {
		return effects.try_emplace(std::forward<Ts>(args)...);
	}

	size_t block() {
		return block_id++;
	}

	size_t out() {
		return out_id++;
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


struct StackContext: StackLexer {
	IR instructions;

	inline StackContext(klx::View src):
		StackLexer::StackLexer(src) {}

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
constexpr auto is_stmt = partial_eq_any(Tokens::TKN_DEF, Tokens::TKN_DECL);
constexpr auto is_keyw = partial_eq_any(Tokens::TKN_WHILE, Tokens::TKN_IF, Tokens::TKN_DEF);
constexpr auto is_expr = partial_eq_any(Tokens::TKN_NUM, Tokens::TKN_IDENTIFIER, Tokens::TKN_WHILE, Tokens::TKN_IF, Tokens::TKN_LBRACE);

constexpr auto is_type_annotation = equal(Tokens::TKN_LPAREN);
constexpr auto is_block = equal(Tokens::TKN_LBRACE);

constexpr auto stack_is_instruction = partial_eq_any(
	Tokens::STACK_TKN_COPY,
	Tokens::STACK_TKN_REMOVE,
	Tokens::STACK_TKN_MOVE,
	Tokens::STACK_TKN_PUSH,
	Tokens::STACK_TKN_CALL,
	Tokens::STACK_TKN_JUMP,
	Tokens::STACK_TKN_BRANCH,
	Tokens::STACK_TKN_RET,
	Tokens::STACK_TKN_END
);

// Stack IR parsing
void stack_parse_def(StackContext&);
void stack_parse_block(StackContext&);
void stack_parse_identifier(StackContext&);
void stack_deserialise(StackContext&);


inline void stack_parse_def(StackContext& ctx) {
	ctx.next();  // skip `def`

	ctx.expect_token(equal(Tokens::STACK_TKN_IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
	View name = ctx.next().view;

	ctx.instruction(Ops::OP_DEF, name);

	while (eq_none(ctx.peek().kind, Tokens::STACK_TKN_RET, Tokens::TKN_EOF)) {
		ctx.expect_token(equal(Tokens::STACK_TKN_BLOCK), ctx.peek().view, STR_BLOCK);
		stack_parse_block(ctx);
	}

	ctx.expect_token(equal(Tokens::STACK_TKN_RET), ctx.peek().view, STR_EXPECT, "ret"_sv);
	ctx.next();  // skip `ret`

	ctx.instruction(Ops::OP_RET);
}

inline void stack_parse_block(StackContext& ctx) {
	ctx.next();  // skip `block`

	ctx.expect_token(equal(Tokens::STACK_TKN_INTEGER), ctx.peek().view, STR_INT);
	size_t name = to_int(ctx.next().view);

	ctx.instruction(Ops::OP_BLOCK, name);

	while (eq_none(ctx.peek().kind, Tokens::STACK_TKN_END, Tokens::TKN_EOF)) {
		ctx.expect_token(stack_is_instruction, ctx.peek().view, STR_INSTRUCTION);
		stack_parse_identifier(ctx);
	}

	ctx.expect_token(equal(Tokens::STACK_TKN_END), ctx.peek().view, STR_EXPECT, "end"_sv);
	ctx.next();  // skip `end`

	ctx.instruction(Ops::OP_END);
}

inline void stack_parse_identifier(StackContext& ctx) {
	Token tok = ctx.next();  // skip identifier

	switch (tok.kind) {
		case Tokens::STACK_TKN_COPY:
		case Tokens::STACK_TKN_REMOVE:
		case Tokens::STACK_TKN_MOVE:
		case Tokens::STACK_TKN_PUSH:
		case Tokens::STACK_TKN_JUMP: {
			ctx.expect_token(equal(Tokens::STACK_TKN_INTEGER), ctx.peek().view, STR_INT);
			size_t val = to_int(ctx.next().view);

			Ops op;

			switch (tok.kind) {
				case Tokens::STACK_TKN_COPY:   op = Ops::OP_COPY;   break;
				case Tokens::STACK_TKN_REMOVE: op = Ops::OP_REMOVE; break;
				case Tokens::STACK_TKN_MOVE:   op = Ops::OP_MOVE;   break;
				case Tokens::STACK_TKN_PUSH:   op = Ops::OP_PUSH;   break;
				case Tokens::STACK_TKN_JUMP:   op = Ops::OP_JUMP;   break;

				default:
					op = Ops::OP_NONE;
			}

			ctx.instruction(op, val);
		} break;

		case Tokens::STACK_TKN_BRANCH: {
			ctx.expect_token(equal(Tokens::STACK_TKN_INTEGER), ctx.peek().view, STR_INT);
			size_t a = to_int(ctx.next().view);

			ctx.expect_token(equal(Tokens::STACK_TKN_INTEGER), ctx.peek().view, STR_INT);
			size_t b = to_int(ctx.next().view);

			ctx.instruction(Ops::OP_BRANCH, a, b);
		} break;

		case Tokens::STACK_TKN_CALL: {
			ctx.expect_token(equal(Tokens::STACK_TKN_IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
			View name = ctx.next().view;

			ctx.instruction(Ops::OP_CALL, name);
		} break;

		default:
			break;
	}
}

inline void stack_deserialise(StackContext& ctx) {
	ctx.expect_token(equal(Tokens::STACK_TKN_DEF), ctx.peek().view, STR_STMT);

	while (ctx.peek().kind != Tokens::TKN_EOF) {
		ctx.expect_token(equal(Tokens::STACK_TKN_DEF), ctx.peek().view, STR_DEF);
		stack_parse_def(ctx);
	}
}



// Parsing
void parse_literal(Context&);
void parse_call(Context&);
void parse_while(Context&);
void parse_if(Context&);
void parse_block(Context&);
void parse_expression(Context&);

StackEffect parse_annotation(Context&);

void parse_decl(Context&);
void parse_def(Context&);
void parse_statement(Context&);
void parse_program(Context&);


// Expressions
inline void parse_literal(Context& ctx) {
	size_t num = to_int(ctx.next().view);
	ctx.instruction(Ops::OP_PUSH, num);
	ctx.stack++;
}

inline void parse_call(Context& ctx) {
	View name = ctx.next().view;

	// Argument intrinsics.
	if (eq_any(name, "cp"_sv, "mv"_sv, "rm"_sv)) {
		ctx.expect_token(equal(Tokens::TKN_NUM), ctx.peek().view, STR_ARG, name);

		size_t arg = to_int(ctx.next().view);
		Ops kind = Ops::OP_NONE;

		ctx.expect_effect(more_equal(arg + 1), name, STR_EFFECT, arg + 1, ctx.stack);

		if (name == "cp"_sv) {
			kind = Ops::OP_COPY;
			ctx.stack++;
		}

		else if (name == "mv"_sv) {
			kind = Ops::OP_MOVE;
		}

		else if (name == "rm"_sv) {
			kind = Ops::OP_REMOVE;
			ctx.stack--;
		}

		ctx.instruction(kind, arg);

		return;
	}

	// Not an intrinsic.
	ctx.instruction(Ops::OP_CALL, name);

	auto it = ctx.effects.find(name);

	if (it == ctx.effects.end())
		ctx.error(Phases::PHASE_SEMANTIC, name, STR_UNDECLARED, name);

	auto [in, out] = it->second;

	ctx.expect_effect(more_equal(in), name, STR_EFFECT, in, ctx.stack);

	ctx.stack -= in;
	ctx.stack += out;
}

inline void parse_while(Context& ctx) {
	View pos = ctx.next().view;  // skip `while`

	size_t header_id = ctx.block();
	size_t body_id = ctx.block();

	ctx.instruction(Ops::OP_JUMP, header_id);
	ctx.instruction(Ops::OP_END);
	ctx.instruction(Ops::OP_BLOCK, header_id);

	// Expression.
	ctx.expect_token(is_expr, ctx.peek().view, STR_EXPR);
	parse_expression(ctx);

	// Consume boolean.
	ctx.expect_effect(more_equal(1u), ctx.peek().view, STR_EFFECT, 1u, ctx.stack);
	ctx.stack--;

	ctx.instruction(Ops::OP_BRANCH, body_id, ctx.block_id);
	ctx.instruction(Ops::OP_END);
	size_t stack_before = ctx.stack;

	// Body.
	ctx.instruction(Ops::OP_BLOCK, body_id);
	ctx.expect_token(is_expr, ctx.peek().view, STR_EXPR);
	parse_expression(ctx);

	// Check is stack size has been altered.
	ctx.expect_effect(equal(stack_before), pos, STR_EFFECT_ALTERED, stack_before, ctx.stack);

	ctx.instruction(Ops::OP_JUMP, header_id);
	ctx.instruction(Ops::OP_END);
	ctx.instruction(Ops::OP_BLOCK, ctx.block());
}

inline void parse_if(Context& ctx) {
	View pos = ctx.next().view;  // skip `if`

	size_t start_block = ctx.block();
	size_t else_block = ctx.block();
	size_t end_block = ctx.block();

	// Expression.
	ctx.expect_token(is_expr, ctx.peek().view, STR_EXPR);
	parse_expression(ctx);

	// Consume boolean.
	ctx.expect_effect(more_equal(1u), ctx.peek().view, STR_EFFECT, 1u, ctx.stack);
	ctx.stack--;

	// Emit a branch instruction to jump to either the main body,
	// the else branch or the end of the branch entirely.
	ctx.instruction(Ops::OP_BRANCH, start_block, else_block);
	ctx.instruction(Ops::OP_END);
	ctx.instruction(Ops::OP_BLOCK, start_block);

	// Store the state of the stack before the body of the branch.
	size_t stack_before = ctx.stack;

	// First block.
	pos = ctx.peek().view;
	ctx.expect_token(is_expr, pos, STR_EXPR);
	parse_expression(ctx);

	// Second block.
	if (ctx.peek().kind == Tokens::TKN_ELSE) {
		// Jump to the end if the true branch is taken so that
		// we don't fall through to the false branch.
		ctx.instruction(Ops::OP_JUMP, end_block);

		size_t stack_body = ctx.stack;  // Store effect of body.
		ctx.stack = stack_before;   // Restore previous state of stack.

		// Create else block.
		ctx.instruction(Ops::OP_END);
		ctx.instruction(Ops::OP_BLOCK, else_block);
		pos = ctx.next().view;  // skip `else`

		ctx.expect_token(is_expr, pos, STR_EXPR);
		parse_expression(ctx);

		// Check is stack size has been altered.
		ctx.expect_effect(equal(stack_body), pos, STR_EFFECT_BRANCH, stack_body, ctx.stack);

		// Create end block.
		ctx.instruction(Ops::OP_JUMP, end_block);
		ctx.instruction(Ops::OP_END);
		ctx.instruction(Ops::OP_BLOCK, end_block);
	}

	else {
		// Create end of branch block.
		ctx.instruction(Ops::OP_JUMP, else_block);
		ctx.instruction(Ops::OP_END);
		ctx.instruction(Ops::OP_BLOCK, else_block);

		// Expect that the stack size has not been altered.
		ctx.expect_effect(equal(stack_before), pos, STR_EFFECT_ALTERED, stack_before, ctx.stack);
	}
}

inline void parse_block(Context& ctx) {
	ctx.next();  // skip `{`
	ctx.expect_token(is_expr, ctx.peek().view, STR_EXPR);

	while (eq_none(ctx.peek().kind, Tokens::TKN_RBRACE, Tokens::TKN_EOF))
		parse_expression(ctx);

	ctx.expect_token(equal(Tokens::TKN_RBRACE), ctx.peek().view, STR_EXPECT, "}"_sv);
	ctx.next();  // skip `}`
}

inline void parse_expression(Context& ctx) {
	switch (ctx.peek().kind) {
		case Tokens::TKN_NUM:        return parse_literal(ctx);
		case Tokens::TKN_IDENTIFIER: return parse_call(ctx);
		case Tokens::TKN_WHILE:      return parse_while(ctx);
		case Tokens::TKN_IF:         return parse_if(ctx);
		case Tokens::TKN_LBRACE:     return parse_block(ctx);

		default:
			ctx.error(Phases::PHASE_SYNTACTIC, ctx.peek().view, STR_EXPR);
	}
}


// Statements
inline StackEffect parse_annotation(Context& ctx) {
	ctx.next();  // skip `(`

	size_t in = 0;
	size_t out = 0;

	auto* ptr = &in;

	while (eq_none(ctx.peek().kind, Tokens::TKN_RPAREN, Tokens::TKN_EOF)) {
		if (ctx.peek().kind == Tokens::TKN_ARROW) {
			ctx.next();  // skip `->`
			ptr = &out;

			if (ctx.peek().kind == Tokens::TKN_RPAREN)
				break;
		}

		ctx.expect_token(equal(Tokens::TKN_IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
		ctx.next();  // skip identifier

		(*ptr)++;
	}

	ctx.expect_token(equal(Tokens::TKN_RPAREN), ctx.peek().view, STR_EXPECT, ")"_sv);
	ctx.next();  // skip `)`

	return { in, out };
}

inline void parse_decl(Context& ctx) {
	ctx.next();  // skip `decl`

	ctx.expect_token(equal(Tokens::TKN_IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
	View name = ctx.next().view;

	ctx.expect_token(is_type_annotation, ctx.peek().view, STR_ANNOTATION);
	StackEffect se = parse_annotation(ctx);

	// Store type signature.
	if (auto [it, succ] = ctx.effect(name, se); not succ)
		ctx.error(Phases::PHASE_SEMANTIC, name, STR_MULTIPLE_DECLARED, name);
}

inline void parse_def(Context& ctx) {
	ctx.out_id = 0; // Reset function local return IDs.
	ctx.stack = 0;

	ctx.next();  // skip `def`

	ctx.expect_token(equal(Tokens::TKN_IDENTIFIER), ctx.peek().view, STR_IDENTIFIER);
	View name = ctx.next().view;

	auto it = ctx.effects.find(name);

	if (it == ctx.effects.end())
		ctx.error(Phases::PHASE_SEMANTIC, name, STR_UNDECLARED, name);

	auto [in, out] = it->second;
	ctx.stack = in;

	ctx.instruction(Ops::OP_DEF, name);
	ctx.instruction(Ops::OP_BLOCK, ctx.block());

	ctx.expect_token(is_expr, ctx.peek().view, STR_EXPR);
	parse_expression(ctx);

	ctx.expect_effect(equal(out), name, STR_EFFECT_RETURN, out, ctx.stack);

	ctx.instruction(Ops::OP_END);
	ctx.instruction(Ops::OP_RET);
}

inline void parse_statement(Context& ctx) {
	switch (ctx.peek().kind) {
		case Tokens::TKN_DECL:  return parse_decl(ctx);
		case Tokens::TKN_DEF:   return parse_def(ctx);

		default:
			ctx.error(Phases::PHASE_SYNTACTIC, ctx.peek().view, STR_STMT);
	}
}

inline void parse_program(Context& ctx) {
	ctx.expect_token(is_stmt, ctx.peek().view, STR_STMT);

	while (ctx.peek().kind != Tokens::TKN_EOF)
		parse_statement(ctx);
}

}


// IR passes
namespace klx {

inline std::ostream& operator<<(std::ostream& os, klx::Op instr) {
	out(os, instr.kind);

	switch (instr.kind) {
		case Ops::OP_NONE:
		case Ops::OP_PUSH:
		case Ops::OP_COPY:
		case Ops::OP_MOVE:
		case Ops::OP_REMOVE:
		case Ops::OP_JUMP:
		case Ops::OP_BLOCK:
			out(os, " ", instr.x); break;

		case Ops::OP_CALL:
		case Ops::OP_DEF:
			outfmt(os, " {}", instr.sv); break;

		case Ops::OP_BRANCH:
			outfmt(os, " {} {}", instr.x, instr.y); break;

		case Ops::OP_RET:
		default: break;
	}

	return os;
}

inline void stack_serialise(const IR& ir) {
	for (auto& op: ir) {
		if (op.kind == Ops::OP_NONE)
			continue;

		switch (op.kind) {
			case Ops::OP_RET:
			case Ops::OP_DEF: break;

			case Ops::OP_BLOCK:
			case Ops::OP_END:
				print("  "); break;

			default:
				print("    "); break;
		}

		println(op);
	}
}

}

#endif
