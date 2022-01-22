#include <iostream>
#include <string>
#include <lib.hpp>

namespace klx {
	// Constants
	constexpr size_t INLINE_LIMIT = 15;
}


namespace klx {

struct Def {
	size_t begin = 0;
	size_t end = 0;

	constexpr Def(size_t begin_ = 0, size_t end_ = 0):
		begin(begin_), end(end_) {}
};

// Store information about the source.
// struct StaticAnalysis {
// 	size_t block_id = 0;
// 	std::array<std::vector<size_t>, (size_t)Ops::OP_TOTAL> instructions;
// 	std::unordered_map<View, std::vector<View>> call_graph;
// };

// inline StaticAnalysis analyse(IR& ir) {
// 	StaticAnalysis sa;

// 	for (auto it = ir.begin(); it != ir.end(); ++it) {
// 		sa[(size_t)it->kind].emplace_back(std::distance(ir.begin(), it));
// 	}

// 	return sa;
// }
// Call graph

// Generate a callgraph to find which functions are called and by who.
using CallGraph = std::unordered_map<View, std::vector<View>>;

inline CallGraph generate_call_graph(IR& ir) {
	CallGraph cg;

	for (auto it = ir.begin(); it != ir.end(); ++it) {
		if (it->kind != Ops::OP_DEF)
			continue;

		View def_sv = it->sv;
		auto [def_it, succ] = cg.try_emplace(def_sv, std::vector<View>{});

		// Function body.
		for (; it->kind != Ops::OP_RET; ++it) {
			if (it->kind != Ops::OP_CALL)
				continue;

			// Function call.
			View call_sv = it->sv;
			def_it->second.emplace_back(call_sv);
		}
	}

	return cg;
}

// Prune any calls which don't have `main` as an ancestor.
namespace detail {
	inline void prune_call_graph(IR& ir, CallGraph& old_cg, CallGraph& cg, View old_sv, View sv) {
		if (auto it = old_cg.find(sv); it != old_cg.end()) {
			for (View call: it->second) {
				if (old_sv == call)
					break;

				detail::prune_call_graph(ir, old_cg, cg, sv, call);
			}

			cg.insert(*it);
		}
	}
}

inline CallGraph prune_call_graph(IR& ir, CallGraph& old_cg, View sv) {
	CallGraph cg;
	detail::prune_call_graph(ir, old_cg, cg, ""_sv, sv);
	return cg;
}


// Function inlining.
inline decltype(auto) gather_definitions(IR& ir) {
	std::unordered_map<View, Def> defs = {
		{ "main"_sv, { 0, 0 } },
	};

	// Find all definitions.
	for (auto it = ir.begin(); it != ir.end(); ++it) {
		if (it->kind != Ops::OP_DEF)
			continue;

		auto def = it;

		while (it->kind != Ops::OP_RET)
			++it;

		size_t begin = std::distance(ir.begin(), def);
		size_t end = std::distance(ir.begin(), it);

		defs.try_emplace(def->sv, begin, end);
	}

	return defs;
}

inline decltype(auto) gather_calls(IR& ir) {
	// Find all call sites.
	std::vector<size_t> calls;

	// Count calls and record offsets into the IR.
	for (auto it = ir.begin(); it != ir.end(); ++it) {
		if (it->kind == Ops::OP_CALL)
			calls.emplace_back(std::distance(ir.begin(), it));
	}

	return calls;
}

void opt_function_inlining(IR& ir) {
	while (true) {
		auto defs = gather_definitions(ir);
		auto calls = gather_calls(ir);

		// Remove any candidate calls that aren't common to both `defs` and `calls`.
		calls.erase(std::remove_if(calls.begin(), calls.end(), [&] (auto&& x) {
			auto def_it = defs.find(ir[x].sv);

			// Check if definition is known.
			if (def_it == defs.end())
				return true; // Definition must be external, remove this candidate.

			// Check if function called once.
			size_t n_calls = std::count(calls.begin(), calls.end(), x);

			if (n_calls == 1)
				return false; // Function is called once so we lose nothing by inlining.

			// Check if function is above inline limit.
			auto [begin, end] = def_it->second;
			size_t def_length = end - begin;

			if (def_length > INLINE_LIMIT)
				return true; // Remove function due to being too large.

			return false;
		}), calls.end());

		if (calls.empty() or defs.empty())
			break;

		// Move the noops just after every call.
		size_t adjust = 0;

		for (size_t callsite: calls) {
			auto it = ir.begin() + callsite + adjust;

			auto [begin, end] = defs.at(it->sv);

			auto begin_it = ir.begin() + begin;
			auto end_it = ir.begin() + end;

			if (end_it->kind != Ops::OP_RET)
				continue;

			it->kind = Ops::OP_NONE;
			++it;

			++begin_it;  // Skip `def`
			++begin_it;  // Skip `block`
			--end_it;    // Skip `end`

			size_t length = std::distance(begin_it, end_it);

			it = ir.begin() + callsite + adjust;
			ir.insert(it, begin_it, end_it);

			adjust += length;
		}

		// Remove noops so they don't interfere with calculating function sizes
		// in the next pass.
		ir.erase(std::remove_if(ir.begin(), ir.end(), [] (auto&& x) {
			return x.kind == Ops::OP_NONE;
		}), ir.end());
	}
}

// Dead code elimination.
inline void opt_dead_code_elimination(IR& ir, CallGraph& cg) {
	for (auto it = ir.begin(); it != ir.end(); ++it) {
		if (it->kind != Ops::OP_DEF)
			continue;

		if (cg.find(it->sv) != cg.end())
			continue;

		for (; it->kind != Ops::OP_RET; ++it)
			it->kind = Ops::OP_NONE;

		it->kind = Ops::OP_NONE;
	}
}

// Constant folding.
inline void opt_const_fold(IR& ir) {
	for (auto it = ir.begin(); it != ir.end(); ++it) {
		if (it->kind != Ops::OP_BLOCK)
			continue;

		std::vector<IR::iterator> pushes;

		while (eq_none(it->kind, Ops::OP_END, Ops::OP_RET)) {
			if (it->kind == Ops::OP_PUSH)
				pushes.emplace_back(it);

			// Do not allow folding beyond a call boundary.
			// This is because a function may produce a
			// number of values at runtime and we cannot
			// know this at compile time.
			else if (it->kind == Ops::OP_CALL)
				pushes.clear();

			else if (it->kind == Ops::OP_COPY) {
				// Set current instruction to the instruction
				// pointed to by `cp`.
				if (it->x + 1 <= pushes.size()) {
					*it = **(pushes.rbegin() + it->x);
					continue;  // Skip ++it
				}
			}

			else if (it->kind == Ops::OP_MOVE) {
				// Swap instructions at TOS and instruction
				// pointed to by `rot`.
				if (it->x + 1 <= pushes.size()) {
					it->kind = Ops::OP_NONE;  // Remove `mv`
					std::rotate(pushes.rbegin(), pushes.rbegin() + it->x, pushes.rbegin() + it->x + 1);
				}
			}

			else if (it->kind == Ops::OP_REMOVE) {
				if (it->x + 1 <= pushes.size()) {
					auto push_it = pushes.rbegin() + it->x;
					auto instr_it = *push_it;

					// Set instruction pointed to by `rm` to noop.
					instr_it->kind = Ops::OP_NONE;
					it->kind = Ops::OP_NONE;  // Remove `rm`

					// Erase corresponding entry in `pushes`.
					pushes.erase(std::next(push_it).base());
				}
			}

			++it;
		}
	}
}

// Eliminate indirect jumps due to if/else chains.
inline void opt_indirect_branch_elimination(IR& ir) {
	for (auto it = ir.begin(); it != ir.end(); ++it) {
		if (it->kind != Ops::OP_DEF)
			continue;

		auto def = it++;
		std::vector<IR::iterator> branches;

		for (; it->kind != Ops::OP_RET; ++it) {
			if (eq_any(it->kind, Ops::OP_BRANCH, Ops::OP_JUMP))
				branches.emplace_back(it);

			else if (it->kind != Ops::OP_BLOCK)
				continue;

			// Start of a basic block.
			auto block = it++;

			if (it->kind != Ops::OP_JUMP)
				continue;

			auto jump = it++;

			if (it->kind != Ops::OP_END)
				continue;

			auto end = it;

			// Candidate for removal.
			size_t block_id = block->x;
			size_t jump_id = jump->x;

			// Loop backwards through branches.
			for (auto branch_it: branches) {
				if (branch_it->kind == Ops::OP_BRANCH) {
					if (branch_it->x == block_id) {
						branch_it->x = jump_id;
					}

					else if (branch_it->y == block_id) {
						branch_it->y = jump_id;
					}
				}

				else if (branch_it->kind == Ops::OP_JUMP and branch_it->x == block_id) {
					branch_it->x = jump_id;
				}
			}

			block->kind = Ops::OP_NONE;
			jump->kind = Ops::OP_NONE;
			end->kind = Ops::OP_NONE;
		}
	}
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

		klx::stack_deserialise(ctx);


		klx::IR ir_a = std::move(ctx.instructions);
		klx::IR ir_b;

		ir_b.reserve(ir_a.capacity());

		klx::opt_function_inlining(ir_a, ir_b);

		// auto cg = klx::generate_call_graph(ir);
		// cg = klx::prune_call_graph(ir, cg, "main"_sv);
		// klx::opt_dead_code_elimination(ir, cg);

		// klx::opt_const_fold(ir);
		// klx::opt_indirect_branch_elimination(ir);

		klx::stack_serialise(ir);
	}

	catch (klx::Error) {
		return 1;
	}

	return 0;
}
