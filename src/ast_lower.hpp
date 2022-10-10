#ifndef KLX_EMIT_HPP
#define KLX_EMIT_HPP

#include <lib.hpp>

namespace klx {

template <TokenTypes MODE>
inline IR::iterator emit(Context<MODE>& ctx, IR& instructions, IR::iterator it, const IR::iterator end) {
	auto current = it;
	auto [kind, sv, x, y, z] = *current;

	switch (current->kind) {
		// Statements.
		case Symbols::EXTERN:
		case Symbols::DECL: {
			instructions.emplace_back(kind, sv, x, y, z);
			it++;
		} break;

		case Symbols::FN: {
			ctx.register_id = 0;
			ctx.block_id = 0;

			instructions.emplace_back(kind, sv, x, y, z); // Def instruction
			instructions.emplace_back(Symbols::BLOCK, sv, ctx.block_id++); // Introduce a block to enclose body

			// Lookup function declaration.
			auto decl = ctx.decls.find(current->sv);

			if (decl == ctx.decls.end())
				ctx.error(Phases::PHASE_INTERMEDIATE, current->sv, STR_UNDECLARED, current->sv);

			auto [effect, linkage] = decl->second;

			// Lookup function stack frame size
			auto frame = ctx.reserves.find(current->sv);

			if (frame == ctx.reserves.end())
				ctx.error(Phases::PHASE_INTERMEDIATE, current->sv, STR_RESERVE, current->sv);

			instructions.emplace_back(Symbols::RESERVE, current->sv, frame->second);

			// for (size_t i = 0; i != effect.in; ++i)
			// 	instructions.emplace_back(Symbols::POP, sv, ctx.register_id++);

			it = emit(ctx, instructions, it + 1, end); // Function body

			// for (size_t i = 0; i != effect.out; ++i)
			// 	instructions.emplace_back(Symbols::PUSH, sv, ctx.register_id--);

			instructions.emplace_back(Symbols::END, sv);
			instructions.emplace_back(Symbols::RET, sv);
		} break;

		case Symbols::PROGRAM: {
			it++;

			while (it->kind != Symbols::END)
				it = emit(ctx, instructions, it, end);

			it++;
		} break;

		// Expressions.
		case Symbols::COPY: {
			size_t reg = ctx.register_id - 1;
			instructions.emplace_back(Symbols::COPY, sv, ctx.register_id++, reg);
			it++;
		} break;

		case Symbols::REMOVE: {
			it++;
		} break;

		case Symbols::MOVE: {
			it++;
		} break;

		case Symbols::INTEGER: {
			instructions.emplace_back(Symbols::LET, sv, ctx.register_id++, x);
			it++;
		} break;

		case Symbols::IDENTIFIER: {
			auto decl = ctx.decls.find(current->sv);

			if (decl == ctx.decls.end())
				ctx.error(Phases::PHASE_INTERMEDIATE, current->sv, STR_UNDECLARED, current->sv);

			auto [effect, linkage] = decl->second;

			// for (size_t i = effect.in; i != 0; --i)
			// 	instructions.emplace_back(Symbols::PUSH, sv, ctx.register_id - i);

			instructions.emplace_back(Symbols::CALL, sv);

			// for (size_t i = 0; i != effect.out; ++i)
			// 	instructions.emplace_back(Symbols::POP, sv, ctx.register_id++);

			it++;
		} break;

		case Symbols::EXPR: {
			it++;

			while (it->kind != Symbols::END)
				it = emit(ctx, instructions, it, end);

			it++;
		} break;

		case Symbols::WHILE: {
			size_t header_block = ctx.block_id++;
			size_t body_block = ctx.block_id++;
			size_t end_block = ctx.block_id++;

			instructions.emplace_back(Symbols::JUMP, sv, header_block);
			instructions.emplace_back(Symbols::END, sv);
			instructions.emplace_back(Symbols::BLOCK, sv, header_block);

			it = emit(ctx, instructions, it + 1, end);  // Test

			instructions.emplace_back(Symbols::BRANCH, sv, body_block, end_block);
			instructions.emplace_back(Symbols::END, sv);
			instructions.emplace_back(Symbols::BLOCK, sv, body_block);

			it = emit(ctx, instructions, it, end);  // Body

			instructions.emplace_back(Symbols::JUMP, sv, header_block);
			instructions.emplace_back(Symbols::END, sv);
			instructions.emplace_back(Symbols::BLOCK, sv, end_block);
		} break;

		case Symbols::IF: {
			size_t true_block = ctx.block_id++;
			size_t false_block = ctx.block_id++;
			size_t end_block = ctx.block_id++;

			it = emit(ctx, instructions, it + 1, end);  // Test

			instructions.emplace_back(Symbols::BRANCH, sv, true_block, false_block);
			instructions.emplace_back(Symbols::END, sv);
			instructions.emplace_back(Symbols::BLOCK, sv, true_block);

			it = emit(ctx, instructions, it, end);  // True

			instructions.emplace_back(Symbols::JUMP, sv, end_block);
			instructions.emplace_back(Symbols::END, sv);
			instructions.emplace_back(Symbols::BLOCK, sv, false_block);

			it = emit(ctx, instructions, it, end);  // False

			instructions.emplace_back(Symbols::JUMP, sv, end_block);
			instructions.emplace_back(Symbols::END, sv);
			instructions.emplace_back(Symbols::BLOCK, sv, end_block);
		} break;

		default: {
			ctx.error(Phases::PHASE_INTERMEDIATE, current->sv, STR_INVALID_SYMBOL, current->kind);
		} break;
	}

	return it;
}

template <TokenTypes MODE>
inline void emit(Context<MODE>& ctx) {
	IR instructions;

	auto it = ctx.instructions.begin();
	auto end = ctx.instructions.end();

	it = emit(ctx, instructions, it, end);

	ctx.instructions = instructions;
}

}

#endif
