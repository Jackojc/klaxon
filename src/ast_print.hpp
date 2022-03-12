#ifndef KLX_AST_PRINT_HPP
#define KLX_AST_PRINT_HPP

#include <lib.hpp>

namespace klx {

template <TokenTypes MODE>
inline IR::iterator ast_print(
	Context<MODE>& ctx,
	IR::iterator it,
	IR::iterator end,
	size_t spaces = 0
) {
	const auto indent = [&] {
		for (size_t i = 0; i != spaces; ++i)
			print("    ");
	};

	auto current = it;

	switch (it->kind) {
		// Statements.
		case Symbols::EXTERN:
		case Symbols::DECL: {
			indent(); printlnfmt("{} ( {} -> {} )", current->kind, current->x, current->y);
			it++;
		} break;

		case Symbols::FN: {
			indent(); println("fn ", current->sv);
			it = ast_print(ctx, it + 1, end, spaces + 1);
		} break;

		case Symbols::PROGRAM: {
			it++;

			while (it->kind != Symbols::END)
				it = ast_print(ctx, it, end, spaces);

			it++;
		} break;

		// Expressions.
		case Symbols::EXPR: {
			it++;

			while (it->kind != Symbols::END)
				it = ast_print(ctx, it, end, spaces);

			it++;
		} break;

		case Symbols::INTEGER: {
			indent(); println(current->x);
			it++;
		} break;

		case Symbols::IDENTIFIER: {
			indent(); println(current->sv);
			it++;
		} break;

		case Symbols::COPY:
		case Symbols::REMOVE:
		case Symbols::MOVE: {
			indent(); println(current->kind, " ", current->x);
			it++;
		} break;

		case Symbols::WHILE: {
			indent(); println("while");

			indent(); println(".test:");
			it = ast_print(ctx, it + 1, end, spaces + 1);  // Test

			indent(); println(".body:");
			it = ast_print(ctx, it, end, spaces + 1);  // Body
		} break;

		case Symbols::IF: {
			indent(); println("if");

			indent(); println(".test:");
			it = ast_print(ctx, it + 1, end, spaces + 1);  // Test

			indent(); println(".true:");
			it = ast_print(ctx, it, end, spaces + 1);  // True

			indent(); println(".false:");
			it = ast_print(ctx, it, end, spaces + 1);  // False
		} break;

		default: {
			ctx.error(Phases::PHASE_SEMANTIC, current->sv, STR_INVALID_SYMBOL, it->kind);
		} break;
	}

	return it;
}

template <TokenTypes MODE>
inline void ast_print(Context<MODE>& ctx) {
	auto it = ctx.instructions.begin();
	auto end = ctx.instructions.end();

	while (it != end)
		it = ast_print(ctx, it, end, 0);
}

}

#endif
