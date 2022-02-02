#include <iostream>
#include <type_traits>
#include <string>
#include <lib.hpp>

namespace klx {

constexpr size_t INLINE_LIMIT = 20;


// Inlining.
struct Def {
	IR::const_iterator begin;
	IR::const_iterator end;
	size_t max_block;

	Def(IR::const_iterator begin_, IR::const_iterator end_, size_t max_block_):
		begin(begin_), end(end_), max_block(max_block_) {}
};

using Defs = std::unordered_map<View, Def>;

inline void perform_inline(IR& new_ir, Defs::iterator it) {
	auto [begin, end, max_block] = it->second;

	begin++; // Skip function header.
	begin++;
	end--;   // Skip function footer.

	auto inlined_begin = new_ir.insert(new_ir.end(), begin, end);
	auto inlined_end = inlined_begin + std::distance(begin, end);

	size_t new_max_block = 0;

	// Re-number any inlined blocks so we don't get conflicts.
	for (; inlined_begin != inlined_end; ++inlined_begin) {
		if (inlined_begin->kind == Symbols::BLOCK) {
			inlined_begin->x += max_block;
			new_max_block = inlined_begin->x;
		}

		else if (inlined_begin->kind == Symbols::JUMP) {
			inlined_begin->x += max_block;
		}

		else if (inlined_begin->kind == Symbols::BRANCH) {
			inlined_begin->x += max_block;
			inlined_begin->y += max_block;
		}
	}

	it->second.max_block = new_max_block;
}

inline size_t opt_function_inlining(const IR& ir, IR& new_ir) {
	Defs defs;
	size_t inline_count = 0;  // Count the number of calls we inline.

	for (auto it = ir.begin(); it != ir.end(); ++it) {
		if (it->kind != Symbols::DEF)
			continue;

		// Start of definition.
		auto def_it = it;

		size_t def_length = 0;
		size_t max_block = 0;

		// Iterate blocks in definition.
		for (; it->kind != Symbols::RET; ++it) {
			// Don't count blocks as part of the definition size.
			if (eq_none(it->kind, Symbols::BLOCK, Symbols::END))
				def_length++;

			// Check if we can inline this call.
			if (it->kind == Symbols::CALL) {
				auto element = defs.find(it->sv);

				if (element != defs.end()) {
					perform_inline(new_ir, element);
					inline_count++;
					continue; // Do not append the call to `new_ir`.
				}
			}

			// Count maximum block number.
			else if (it->kind == Symbols::BLOCK)
				max_block = max(max_block, it->x);

			// Emplace current instruction to `new_ir`.
			new_ir.emplace_back(*it);
		}

		new_ir.emplace_back(Symbols::RET);

		// If function definition is below threshold, consider it a candidate
		// for inlining.
		if (def_length <= INLINE_LIMIT)
			auto [element, succ] = defs.try_emplace(def_it->sv, def_it, it, max_block);
	}

	if (ir.size() == new_ir.size())
		return 0;

	return inline_count;
}


// Dead function elimination.
using SeenCalls = std::unordered_set<View>;

inline void opt_dead_function_elimination(const IR& ir, IR& new_ir, View ancestor, SeenCalls& seen) {
	for (auto it = ir.begin(); it != ir.end(); ++it) {
		if (it->kind != Symbols::DEF or it->sv != ancestor)
			continue;

		auto def_it = it;

		for (; it->kind != Symbols::RET; ++it) {
			if (it->kind == Symbols::CALL) {
				auto [element, succ] = seen.emplace(it->sv);

				if (succ)
					opt_dead_function_elimination(ir, new_ir, it->sv, seen);
			}
		}

		new_ir.insert(new_ir.end(), def_it, it + 1);
	}
}



inline void opt_indirect_branch_elimination(const IR& ir, IR& new_ir) {
	new_ir = ir;
}



inline void opt_const_fold(const IR& ir, IR& new_ir) {
	new_ir = ir;
}

}

int main(int argc, const char* argv[]) {
	std::istreambuf_iterator<char> begin(std::cin), end;
	std::string input(begin, end);

	klx::View src { &*input.begin(), &*input.end() };
	klx::StackContext ctx { src };

	try {
		if (not klx::utf_validate(src))
			ctx.error(klx::Phases::PHASE_ENCODING, src, klx::STR_ENCODING);

		klx::ir_parse(ctx);

		klx::IR ir_a = std::move(ctx.instructions);
		klx::IR ir_b;

		ir_b.reserve(ir_a.capacity());

		klx::IR* ir_current = &ir_a;
		klx::IR* ir_new = &ir_b;

		const auto run_pass = [&] (auto&& x, auto&&... xs) {
			if constexpr(not std::is_same_v<decltype(x(*ir_current, *ir_new, xs...)), void>) {
				auto ret = x(*ir_current, *ir_new, xs...);
				std::swap(ir_current, ir_new);
				ir_new->clear();
				return ret;
			}

			else {
				x(*ir_current, *ir_new, xs...);
				std::swap(ir_current, ir_new);
				ir_new->clear();
			}
		};


		// Inline until we have no more candidate functions.
		while (run_pass(klx::opt_function_inlining) > 0);

		// Eliminate functions which are never called.
		klx::SeenCalls seen;
		run_pass(klx::opt_dead_function_elimination, "main"_sv, seen);

		// run_pass(klx::opt_indirect_branch_elimination);
		// run_pass(klx::opt_const_fold);

		klx::serialise(*ir_current);
	}

	catch (klx::Error) {
		return 1;
	}

	return 0;
}
