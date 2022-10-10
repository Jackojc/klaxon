#ifndef KLX_SEMANTIC_HPP
#define KLX_SEMANTIC_HPP

#include <lib.hpp>

namespace klx {

template <TokenTypes M>
struct Semantic {
	Lexer<M>& lexer;
	Context& ast;
	Context& ir;

	size_t stack = 0u;
	size_t block = 0u;
	size_t reg = 0u;

	std::vector<size_t> registers;

	IR::iterator it;
	IR::iterator end;

	Semantic(Lexer<M>& lexer_, Context& ast_, Context& ir_):
		lexer(lexer_), ast(ast_), ir(ir_),
		it(ast_.begin()), end(ast_.end())
	{}

	template <typename F, typename... Ts>
	inline void effect(const F& fn, View sv, Ts&&... args) {
		if (not fn(stack))
			lexer.error(Phases::PHASE_SEMANTIC, sv, std::forward<Ts>(args)...);
	}

	inline size_t reg_alloc() {
		registers.emplace_back(reg);
		return reg++;
	}

	inline size_t reg_dealloc(size_t i = 0) {
		size_t r = registers.back();
		registers.pop_back();
		return r;
	}

	inline size_t reg_at(size_t i) {
		return *(registers.rbegin() + i);
	}

	inline size_t reg_top() {
		return reg_at(0);
	}

	inline void semantic() {
		auto current = it++;
		auto [kind, sv, x, y, z] = *current;

		switch (kind) {
			case Symbols::PROGRAM:
			case Symbols::EXPR: {
				while (it->kind != Symbols::END)
					semantic();

				it++;  // skip `end`
			} break;


			// Statements.
			case Symbols::EXTERN:
			case Symbols::DECL: {
				ir.instruction(kind, sv, x, y, z);
			} break;

			case Symbols::FN: {
				reg = 0u;

				ir.instruction(kind, sv, x, y, z);  // Def instruction
				ir.instruction(Symbols::BLOCK, sv, block++);  // Introduce a block to enclose body

				auto decl = ast.decls.find(sv);

				if (decl == ast.decls.end())
					lexer.error(Phases::PHASE_EFFECT_CHECK, sv, STR_UNDECLARED, sv);

				auto [in, out] = decl->second;
				stack = in;

				for (size_t i = 0; i != in; ++i)
					ir.instruction(Symbols::POP, sv, reg_alloc());

				semantic();
				effect(equal(out), sv, STR_EFFECT_RETURN, out, stack);

				for (size_t i = 0; i != out; ++i)
					ir.instruction(Symbols::PUSH, sv, reg_dealloc());

				ir.instruction(Symbols::END, sv);
				ir.instruction(Symbols::RET, sv);
			} break;


			// Expressions.
			case Symbols::COPY: {
				effect(more_equal(x + 1), sv, STR_EFFECT, x + 1, stack);
				ir.instruction(Symbols::COPY, sv, reg_alloc(), reg_top());
				stack++;
			} break;

			case Symbols::REMOVE: {
				effect(more_equal(x + 1), sv, STR_EFFECT, x + 1, stack);
				registers.erase((registers.rbegin() + x + 1).base());
				stack--;
			} break;

			case Symbols::MOVE: {
				effect(more_equal(x + 1), sv, STR_EFFECT, x + 1, stack);
				std::rotate(registers.rbegin(), registers.rbegin() + x, registers.rend());
			} break;

			case Symbols::INTEGER: {
				ir.instruction(Symbols::LET, sv, reg_alloc(), x);
				stack++;
			} break;

			case Symbols::IDENTIFIER: {
				auto decl = ast.decls.find(sv);

				if (decl == ast.decls.end())
					lexer.error(Phases::PHASE_EFFECT_CHECK, sv, STR_UNDECLARED, sv);

				auto [in, out] = decl->second;

				// Push values we want to pass to the function.
				for (size_t i = 0; i != in; ++i)
					ir.instruction(Symbols::PUSH, sv, reg_dealloc());

				// Make sure sure the stack has at least `in` elements.
				effect(more_equal(in), sv, STR_EFFECT, in, stack);
				stack += out - in;

				ir.instruction(Symbols::CALL, sv);

				// Pop returned values.
				for (size_t i = 0; i != out; ++i)
					ir.instruction(Symbols::POP, sv, reg_alloc());
			} break;


			// Control Flow
			case Symbols::WHILE: {
				semantic();  // Test

				effect(more_equal(1u), it->sv, STR_EFFECT, 1u, stack);
				stack--;

				size_t initial = stack;

				semantic();  // Body
				effect(equal(initial), sv, STR_EFFECT_ALTERED, initial, stack);
			} break;

			case Symbols::IF: {
				semantic();  // Test

				effect(more_equal(1u), sv, STR_EFFECT, 1u, stack);
				stack--;

				size_t initial = stack; // Store size before branches.

				semantic();              // True
				size_t first = stack;   // Store effect of true branch.

				stack = initial;        // Restore stack to initial state.

				semantic();              // False
				size_t second = stack;

				// Check if branches have the same effect.
				effect(equal(first), sv, STR_EFFECT_BRANCH, first, second);
			} break;


			default: {
				lexer.error(Phases::PHASE_SEMANTIC, sv, STR_INVALID_SYMBOL, it->kind);
			} break;
		}
	}
};

template <TokenTypes M>
inline Context semantic(Lexer<M>& lexer, Context& ast) {
	Context ir;
	auto s = Semantic(lexer, ast, ir);
	s.semantic();
	return ir;
}

}

#endif
