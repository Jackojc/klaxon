#include <iostream>
#include <string>
#include <lib.hpp>

namespace klx {

// Constants
constexpr size_t INLINE_LIMIT = 15;


// Function inlining.
struct Def {
	size_t begin = 0;
	size_t end = 0;

	constexpr Def(size_t begin_ = 0, size_t end_ = 0):
		begin(begin_), end(end_) {}
};

inline decltype(auto) gather_candidates(IR& ir) {
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
		size_t length = std::distance(def, it);

		if (length > INLINE_LIMIT)
			continue;

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
		auto candidates = gather_candidates(ir);
		auto calls = gather_calls(ir);

		// Remove any calls that aren't common to both `candidates` and `calls`.
		calls.erase(std::remove_if(calls.begin(), calls.end(), [&] (auto&& x) {
			return candidates.find(ir[x].sv) == candidates.end();
		}), calls.end());

		if (calls.empty() or candidates.empty())
			break;

		// Move the noops just after every call.
		size_t adjust = 0;

		for (size_t callsite: calls) {
			auto it = ir.begin() + callsite + adjust;

			auto [begin, end] = candidates.at(it->sv);

			auto begin_it = ir.begin() + begin;
			auto end_it = ir.begin() + end;

			if (end_it->kind != Ops::OP_RET)
				continue;

			do {
				it->kind = Ops::OP_NONE;
				++it;
			} while (it->kind == Ops::OP_OUT);

			begin_it++;  // Skip `def`

			++begin_it;  // Skip `block`
			--end_it;    // Skip `end`

			while (begin_it->kind == Ops::OP_ARG)
				++begin_it;

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
inline void opt_dead_code_elimination(IR& ir) {
	struct Def {
		IR::iterator it {};
		size_t calls = 0;

		constexpr Def(IR::iterator it_, size_t calls_):
			it(it_), calls(calls_) {}
	};

	std::unordered_map<View, Def> defs = {
		{ "main"_sv, Def { ir.end(), 1 } }
	};

	// Record definition positions and number of calls.
	for (auto it = ir.begin(); it != ir.end(); ++it) {
		if (it->kind == Ops::OP_DEF) {
			if (auto [element, succ] = defs.try_emplace(it->sv, it, 0); not succ)
				element->second.it = it;
		}

		else if (it->kind == Ops::OP_CALL) {
			if (auto [element, succ] = defs.try_emplace(it->sv, it, 1); not succ)
				element->second.calls++;
		}
	}

	// Overwrite unused definitions with noops.
	for (auto& [sv, def]: defs) {
		auto [it, calls] = def;

		if (calls > 0)
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
			if (eq_any(it->kind, Ops::OP_PUSH, Ops::OP_ARG, Ops::OP_OUT))
				pushes.emplace_back(it);

			else if (it->kind == Ops::OP_AT) {
				// Set current instruction to the instruction
				// pointed to by `at`.
				*it = **(pushes.rbegin() + it->x);
				continue;  // Skip ++it
			}

			else if (it->kind == Ops::OP_ROT) {
				// Swap instructions at TOS and instruction
				// pointed to by `rot`.
				std::swap(**pushes.rbegin(), **(pushes.rbegin() + it->x));
				it->kind = Ops::OP_NONE;  // Remove `rot`
			}

			else if (it->kind == Ops::OP_DROP) {
				auto push_it = pushes.rbegin() + it->x;
				auto instr_it = *push_it;

				// Set instruction pointed to by `drop` to noop.
				instr_it->kind = Ops::OP_NONE;
				it->kind = Ops::OP_NONE;  // Remove `drop`

				// Erase corresponding entry in `pushes`.
				pushes.erase(std::next(push_it).base());
			}

			++it;
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

		klx::IR& ir = ctx.instructions;

		klx::opt_function_inlining(ir);
		klx::opt_dead_code_elimination(ir);
		klx::opt_const_fold(ir);

		klx::stack_serialise(ir);
	}

	catch (klx::Error) {
		return 1;
	}

	return 0;
}
