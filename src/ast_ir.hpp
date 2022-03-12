#ifndef KLX_AST_IR_HPP
#define KLX_AST_IR_HPP

#include <lib.hpp>

namespace klx {

template <TokenTypes MODE>
inline void instruction_print(Context<MODE>& ctx, std::ostream& os, Op op) {
	auto [kind, sv, x, y, z] = op;

	// Indentation.
	switch (kind) {
		case Symbols::EXTERN:
		case Symbols::DECL:
		case Symbols::FN:
		case Symbols::RET:
			break;

		case Symbols::BLOCK:
		case Symbols::END: {
			print("  ");
		} break;

		default: {
			print("    ");
		} break;
	}

	print(kind, " ");

	// Instruction.
	switch (kind) {
		case Symbols::EXTERN:
		case Symbols::DECL: {
			printlnfmt("{} ( {} -> {} )", sv, x, y);
		} break;

		case Symbols::FN:
		case Symbols::CALL: {
			println(sv);
		} break;

		case Symbols::POP:
		case Symbols::PUSH:
		case Symbols::JUMP:
		case Symbols::BLOCK: {
			println(x);
		} break;

		case Symbols::COPY:
		case Symbols::LOAD:
		case Symbols::BRANCH: {
			println(x, " ", y);
		} break;

		case Symbols::RET:
		case Symbols::END:
			break;

		default: {
			ctx.error(Phases::PHASE_INTERMEDIATE, sv, STR_INVALID_SYMBOL, kind);
		} break;
	}
}

template <TokenTypes MODE>
inline IR::iterator ast_ir(Context<MODE>& ctx, IR::iterator it, const IR::iterator end) {
	auto current = it;

	switch (current->kind) {
		// Statements.
		case Symbols::EXTERN:
		case Symbols::DECL: {
			instruction_print(ctx, std::cout, *current);
			it++;
		} break;

		case Symbols::FN: {
			ctx.register_id = 0;
			ctx.block_id = 0;

			instruction_print(ctx, std::cout, *current);
			instruction_print(ctx, std::cout, Op { Symbols::BLOCK, current->sv, ctx.block_id++ });

			auto decl = ctx.decls.find(current->sv);

			if (decl == ctx.decls.end())
				ctx.error(Phases::PHASE_INTERMEDIATE, current->sv, STR_UNDECLARED, current->sv);

			auto [effect, linkage] = decl->second;

			for (size_t i = 0; i != effect.in; ++i)
				println("    pop ", ctx.register_id++);

			it = ast_ir(ctx, it + 1, end);

			for (size_t i = 0; i != effect.out; ++i)
				println("    push ", ctx.register_id--);

			println("  end");
			println("ret");
		} break;

		case Symbols::PROGRAM: {
			it++;

			while (it->kind != Symbols::END)
				it = ast_ir(ctx, it, end);

			it++;
		} break;

		// Expressions.
		case Symbols::COPY: {
			size_t reg = ctx.register_id - 1;
			println("    copy ", ctx.register_id++, " ", reg);
			it++;
		} break;

		case Symbols::REMOVE: {
			it++;
		} break;

		case Symbols::MOVE: {
			it++;
		} break;

		case Symbols::INTEGER: {
			println("    load ", ctx.register_id++, " ", current->x);
			it++;
		} break;

		case Symbols::IDENTIFIER: {
			auto decl = ctx.decls.find(current->sv);

			if (decl == ctx.decls.end())
				ctx.error(Phases::PHASE_INTERMEDIATE, current->sv, STR_UNDECLARED, current->sv);

			auto [effect, linkage] = decl->second;

			for (size_t i = effect.in; i != 0; --i)
				println("    push ", ctx.register_id - i);

			// ctx.register_id -= effect.in;

			println("    call ", current->sv);

			for (size_t i = 0; i != effect.out; ++i)
				println("    pop ", ctx.register_id++);

			it++;
		} break;

		case Symbols::EXPR: {
			it++;

			while (it->kind != Symbols::END)
				it = ast_ir(ctx, it, end);

			it++;
		} break;

		case Symbols::WHILE: {
			size_t header_block = ctx.block_id++;
			size_t body_block = ctx.block_id++;
			size_t end_block = ctx.block_id++;

			println("    jump ", header_block);
			println("  end");
			println("  block ", header_block);

			it = ast_ir(ctx, it + 1, end);  // Test

			println("    branch ", body_block, " ", end_block);
			println("  end");
			println("  block ", body_block);

			it = ast_ir(ctx, it, end);  // Body

			println("    jump ", header_block);
			println("  end");
			println("  block ", end_block);
		} break;

		case Symbols::IF: {
			size_t true_block = ctx.block_id++;
			size_t false_block = ctx.block_id++;
			size_t end_block = ctx.block_id++;

			it = ast_ir(ctx, it + 1, end);  // Test

			println("    branch ", true_block, " ", false_block);
			println("  end");
			println("  block ", true_block);

			it = ast_ir(ctx, it, end);  // True

			println("    jump ", end_block);
			println("  end");
			println("  block ", false_block);

			it = ast_ir(ctx, it, end);  // False

			println("   jump ", end_block);
			println("  end");
			println("  block ", end_block);
		} break;

		default: {
			ctx.error(Phases::PHASE_INTERMEDIATE, current->sv, STR_INVALID_SYMBOL, current->kind);
		} break;
	}

	return it;
}

template <TokenTypes MODE>
inline void ast_ir(Context<MODE>& ctx) {
	auto it = ctx.instructions.begin();
	auto end = ctx.instructions.end();

	it = ast_ir(ctx, it, end);
}

}

#endif
