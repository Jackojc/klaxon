#ifndef KLX_PARSER_HPP
#define KLX_PARSER_HPP

#include <lib/def.hpp>
#include <lib/str.hpp>
#include <lib/svec.hpp>

#include <def.hpp>
#include <lexer.hpp>
#include <ir.hpp>
#include <codegen.hpp>


// Structures
struct macro_t {
	br::str_view name;
	br::str_view src;
};

struct function_t {
	br::str_view name;
};

struct parser_t {
	lexer_t lexer;

	br::svec<macro_t, MAX_MACROS> macros;

	br::svec<function_t, MAX_FUNCTIONS> functions;
	br::svec<function_t, MAX_FUNCTIONS> external_functions;
};


// Forward declarations
template <typename T> constexpr decltype(auto) make_default_macros(T);
inline parser_t make_parser(br::str_view);

constexpr bool is_stmt(token_t);
constexpr bool is_keyw(token_t);
constexpr bool is_intrinsic(token_t);
constexpr bool is_expr(token_t);


inline token_t parse_number     (parser_t&, ir_t&, token_t);
inline token_t parse_identifier (parser_t&, ir_t&, token_t);
inline token_t parse_intrinsic  (parser_t&, ir_t&, token_t);
inline token_t parse_while      (parser_t&, ir_t&, token_t);
inline token_t parse_if         (parser_t&, ir_t&, token_t);
inline token_t parse_block      (parser_t&, ir_t&, token_t);

inline token_t parse_extern     (parser_t&, ir_t&);
inline token_t parse_def        (parser_t&, ir_t&);
inline token_t parse_macro      (parser_t&, ir_t&);

inline token_t parse_expr       (parser_t&, ir_t&);
inline token_t parse_stmt       (parser_t&, ir_t&);
inline token_t parse_program    (parser_t&, ir_t&);


// Functions
template <typename T> constexpr decltype(auto) make_default_macros(T v) {
	#define X(name, code) \
		v = br::emplace(v, #name##_sv, #code##_sv);

		DEFAULT_MACROS

	#undef X

	return v;
}

inline parser_t make_parser(br::str_view src) {
	parser_t parser = { make_lexer(src), {}, {}, {} };
	parser.macros = make_default_macros(parser.macros);
	return parser;
}


// Helper predicates
constexpr bool is_stmt(token_t token) {
	return br::eq_any(token.kind, TKN_DEF, TKN_MACRO, TKN_EXTERN);
}

constexpr bool is_keyw(token_t token) {
	return br::eq_any(token.kind, TKN_WHILE, TKN_IF, TKN_MACRO, TKN_DEF, TKN_EXTERN);
}

constexpr bool is_intrinsic(token_t token) {
	return br::eq_any(token.kind,
		TKN_ADD, TKN_SUB, TKN_MUL, TKN_DIV, TKN_MOD, TKN_RSH, TKN_LSH,
		TKN_DUP, TKN_SWAP, TKN_DROP, TKN_OVER,
		TKN_LESS, TKN_MORE, TKN_LESSEQ, TKN_MOREEQ, TKN_EQ, TKN_NOTEQ,
		TKN_AND, TKN_OR, TKN_NOT,
		TKN_BAND, TKN_BOR, TKN_BNOT,
		TKN_SYSCALL, TKN_WORD, TKN_LOAD, TKN_STORE, TKN_AT
	);
}

constexpr bool is_expr(token_t token) {
	return br::eq_any(token.kind, TKN_NUM, TKN_IDENT, TKN_WHILE, TKN_IF, TKN_LBLOCK, TKN_ATTR) or is_intrinsic(token);
}


// Parsing functions
inline token_t parse_number(parser_t& parser, ir_t& repr, token_t attr) {
	auto& [lexer, macros, functions, external_functions] = parser;
	const auto& [original, src, peek] = lexer;

	token_t last_token = next_token(lexer);
	br::str_view number = last_token.view; // consume number.

	ir<NODE_NUMBER>(repr, attr, number);

	return last_token;
}

inline token_t parse_identifier(parser_t& parser, ir_t& repr, token_t attr) {
	auto& [lexer, macros, functions, external_functions] = parser;
	const auto& [original, src, peek] = lexer;

	token_t last_token = next_token(lexer);
	br::str_view identifier = last_token.view; // consume identifier.

	// Check if it's a macro.
	for (br::index_t i = 0; i != length(macros); ++i) {
		auto [macro_ident, body] = at(macros, i);

		// Found a match.
		if (eq(macro_ident, identifier)) {
			parser_t mparser = parser;
			mparser.lexer = make_lexer(body);

			// Parse expressions in macro body.
			while (mparser.lexer.peek.kind != TKN_EOF)
				parse_expr(mparser, repr);

			ir<NODE_MACRO_CALL>(repr, attr, identifier);
			return last_token;
		}
	}

	// Check if it's a function.
	for (br::index_t i = 0; i != length(functions); ++i) {
		auto [fn_ident] = at(functions, i);

		// Found a match.
		if (eq(fn_ident, identifier)) {
			ir<NODE_DEF_CALL>(repr, attr, identifier);
			return last_token;
		}
	}

	// Check if it's an external function call.
	for (br::index_t i = 0; i != length(external_functions); ++i) {
		auto [fn_ident] = at(external_functions, i);

		// Found a match.
		if (eq(fn_ident, identifier)) {
			ir<NODE_EXTERN_CALL>(repr, attr, identifier);
			return last_token;
		}
	}

	br::halt("undefined function '{}'", identifier);
}

inline token_t parse_while(parser_t& parser, ir_t& repr, token_t attr) {
	auto& [lexer, macros, functions, external_functions] = parser;
	const auto& [original, src, peek] = lexer;

	next_token(lexer); // skip `while` keyword.

	token_t last_token;

	ir<NODE_WHILE>(repr, attr,
		// Test expression.
		[&] {
			if (not is_expr(parser.lexer.peek))
				br::halt("expected test condition for while");

			parse_expr(parser, repr);
		},

		// Body expression.
		[&] {
			if (not is_expr(parser.lexer.peek))
				br::halt("expected expression for while body");

			last_token = parse_expr(parser, repr);
		}
	);

	return last_token;
}

inline token_t parse_if(parser_t& parser, ir_t& repr, token_t attr) {
	auto& [lexer, macros, functions, external_functions] = parser;
	const auto& [original, src, peek] = lexer;

	next_token(lexer); // skip `if` keyword.

	token_t last_token;

	ir<NODE_IF>(repr, attr,
		// Test expression.
		[&] {
			if (not is_expr(parser.lexer.peek))
				br::halt("expected test condition for if");

			parse_expr(parser, repr);
		},

		// Body expression.
		[&] {
			if (not is_expr(parser.lexer.peek))
				br::halt("expected expression for if body");

			last_token = parse_expr(parser, repr);
		}
	);

	return last_token;
}

inline token_t parse_extern(parser_t& parser, ir_t& repr) {
	auto& [lexer, macros, functions, external_functions] = parser;
	const auto& [original, src, peek] = lexer;

	next_token(lexer); // skip `extern` keyword.

	if (peek.kind != TKN_IDENT)
		br::halt("expected identifier for extern");

	token_t last_token = next_token(lexer); // consume identifier
	br::str_view identifier = last_token.view;

	functions = br::emplace(external_functions, identifier);

	ir<NODE_EXTERN>(repr, identifier);

	return last_token;
}

inline token_t parse_def(parser_t& parser, ir_t& repr) {
	auto& [lexer, macros, functions, external_functions] = parser;
	const auto& [original, src, peek] = lexer;

	next_token(lexer); // skip `def` keyword.

	if (peek.kind != TKN_IDENT)
		br::halt("expected identifier for def");

	br::str_view name = next_token(lexer).view; // consume macro name.

	functions = emplace(functions, name);

	token_t last_token;

	ir<NODE_DEF>(repr, name, [&] {
		if (not is_expr(parser.lexer.peek))
			br::halt("expected expression for def body");

		last_token = parse_expr(parser, repr);
	});

	return last_token;
}

inline token_t parse_macro(parser_t& parser, ir_t& repr) {
	auto& [lexer, macros, functions, external_functions] = parser;
	const auto& [original, src, peek] = lexer;

	next_token(lexer); // skip `macro` keyword.

	if (peek.kind != TKN_IDENT)
		br::halt("expected identifier for macro");

	br::str_view name = next_token(lexer).view; // consume macro name.

	token_t last_token;

	ir<NODE_MACRO>(repr, name, [&] {
		if (not is_expr(parser.lexer.peek))
			br::halt("expected expression for macro body");

		const char* const begin = parser.lexer.peek.view.begin; // store beginning token of macro.
			last_token = parse_expr(parser, repr);
		const char* const end = last_token.view.end;

		// the range [begin, end] is our macro body.
		parser.macros = emplace(parser.macros, name, br::make_sv(begin, end));
	});

	return last_token;
}

inline token_t parse_block(parser_t& parser, ir_t& repr) {
	auto& [lexer, macros, functions, external_functions] = parser;
	const auto& [original, src, peek] = lexer;

	next_token(lexer); // skip `{` token.

	ir<NODE_BLOCK>(repr, [&] {
		if (not is_expr(parser.lexer.peek))
			br::halt("expected expression for block body");

		while (parser.lexer.peek.kind != TKN_RBLOCK)
			parse_expr(parser, repr);
	});

	if (peek.kind != TKN_RBLOCK)
		br::halt("expected '}'");

	return next_token(lexer); // skip `}` token.
}

inline token_t parse_intrinsic(parser_t& parser, ir_t& repr, token_t attr) {
	auto& [lexer, macros, functions, external_functions] = parser;
	const auto& [original, src, peek] = lexer;

	token_t last_token = next_token(lexer);
	br::str_view name = last_token.view;

	ir<NODE_INTRINSIC>(repr, attr, name, last_token.kind);

	return last_token;
}

inline token_t parse_stmt(parser_t& parser, ir_t& repr) {
	auto& [lexer, macros, functions, external_functions] = parser;
	const auto& [original, src, peek] = lexer;

	token_t last_token;

	ir<NODE_STMT>(repr, [&] {
		switch (parser.lexer.peek.kind) {
			case TKN_MACRO:  last_token = parse_macro(parser, repr);  break;
			case TKN_EXTERN: last_token = parse_extern(parser, repr); break;
			case TKN_DEF:    last_token = parse_def(parser, repr);    break;

			default:
				br::halt("expecting statement, got '{}'", token_str[parser.lexer.peek.kind]);
		}
	});

	return last_token;
}

inline token_t parse_expr(parser_t& parser, ir_t& repr) {
	auto& [lexer, macros, functions, external_functions] = parser;
	const auto& [original, src, peek] = lexer;

	token_t last_token;

	ir<NODE_EXPR>(repr, [&] {
		token_t attr {};

		if (parser.lexer.peek.kind == TKN_ATTR)
			attr = next_token(lexer);

		switch (parser.lexer.peek.kind) {
			case TKN_NUM:    last_token = parse_number(parser, repr, attr);     break;
			case TKN_IDENT:  last_token = parse_identifier(parser, repr, attr); break;
			case TKN_WHILE:  last_token = parse_while(parser, repr, attr);      break;
			case TKN_IF:     last_token = parse_if(parser, repr, attr);         break;
			case TKN_LBLOCK: last_token = parse_block(parser, repr);            break;

			default: {
				if (is_intrinsic(parser.lexer.peek)) {
					last_token = parse_intrinsic(parser, repr, attr);
					break;
				}

				br::halt("expecting expression, got '{}'", token_str[parser.lexer.peek.kind]);
			}
		}
	});

	return last_token;
}

inline token_t parse_program(parser_t& parser, ir_t& repr) {
	auto& [lexer, macros, functions, external_functions] = parser;
	const auto& [original, src, peek] = lexer;

	token_t last_token;

	ir<NODE_PROGRAM>(repr, [&] {
		while (parser.lexer.peek.kind != TKN_EOF)
			last_token = parse_stmt(parser, repr);
	});

	return last_token;
}

#endif
