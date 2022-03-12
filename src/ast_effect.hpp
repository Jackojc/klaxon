#ifndef KLX_AST_EFFECT_HPP
#define KLX_AST_EFFECT_HPP

#include <lib.hpp>

namespace klx {

template <TokenTypes MODE>
inline IR::iterator ast_effect(Context<MODE>& ctx, IR::iterator it, IR::iterator end) {
	auto current = it;

	switch (current->kind) {
		// Statements.
		case Symbols::EXTERN:
		case Symbols::DECL: {
			it++;
		} break;

		case Symbols::FN: {
			auto decl_it = ctx.decls.find(current->sv);

			if (decl_it == ctx.decls.end())
				ctx.error(Phases::PHASE_EFFECT_CHECK, current->sv, STR_UNDECLARED, current->sv);

			auto [effect, linkage] = decl_it->second;
			ctx.stack = effect.in;

			it = ast_effect(ctx, it + 1, end);

			ctx.expect_effect(equal(effect.out), current->sv, STR_EFFECT_RETURN, effect.out, ctx.stack);
		} break;

		case Symbols::PROGRAM: {
			it++;

			while (it->kind != Symbols::END)
				it = ast_effect(ctx, it, end);

			it++;
		} break;

		// Expressions.
		case Symbols::COPY: {
			ctx.expect_effect(more_equal(current->x + 1), current->sv, STR_EFFECT, current->x + 1, ctx.stack);
			ctx.stack++;
			it++;
		} break;

		case Symbols::REMOVE: {
			ctx.expect_effect(more_equal(current->x + 1), current->sv, STR_EFFECT, current->x + 1, ctx.stack);
			ctx.stack--;
			it++;
		} break;

		case Symbols::MOVE: {
			ctx.expect_effect(more_equal(current->x + 1), current->sv, STR_EFFECT, current->x + 1, ctx.stack);
			it++;
		} break;

		case Symbols::INTEGER: {
			ctx.stack++;
			it++;
		} break;

		case Symbols::IDENTIFIER: {
			auto decl_it = ctx.decls.find(current->sv);

			if (decl_it == ctx.decls.end())
				ctx.error(Phases::PHASE_EFFECT_CHECK, current->sv, STR_UNDECLARED, current->sv);

			auto [effect, linkage] = decl_it->second;

			ctx.expect_effect(more_equal(effect.in), current->sv, STR_EFFECT, effect.in, ctx.stack);

			ctx.stack -= effect.in;
			ctx.stack += effect.out;

			it++;
		} break;

		case Symbols::EXPR: {
			it++;

			while (it->kind != Symbols::END)
				it = ast_effect(ctx, it, end);

			it++;
		} break;

		case Symbols::WHILE: {
			it = ast_effect(ctx, it + 1, end);  // Test

			ctx.expect_effect(more_equal(1u), it->sv, STR_EFFECT, 1u, ctx.stack);
			ctx.stack--;

			size_t initial = ctx.stack;

			it = ast_effect(ctx, it, end);  // Body
			ctx.expect_effect(equal(initial), current->sv, STR_EFFECT_ALTERED, initial, ctx.stack);
		} break;

		case Symbols::IF: {
			it = ast_effect(ctx, it + 1, end);  // Test

			ctx.expect_effect(more_equal(1u), current->sv, STR_EFFECT, 1u, ctx.stack);
			ctx.stack--;

			size_t initial = ctx.stack;    // Store size before branches.

			it = ast_effect(ctx, it, end);  // True
			size_t first = ctx.stack;      // Store effect of true branch.

			ctx.stack = initial;           // Restore stack to initial state.
			it = ast_effect(ctx, it, end);  // False
			size_t second = ctx.stack;

			// Check if branches have the same effect.
			ctx.expect_effect(equal(first), current->sv, STR_EFFECT_BRANCH, first, second);
		} break;

		default: {
			ctx.error(Phases::PHASE_EFFECT_CHECK, current->sv, STR_INVALID_SYMBOL, current->kind);
		} break;
	}

	return it;
}

template <TokenTypes MODE>
inline void ast_effect(Context<MODE>& ctx) {
	auto it = ctx.instructions.begin();
	auto end = ctx.instructions.end();

	it = ast_effect(ctx, it, end);
}

}

#endif
