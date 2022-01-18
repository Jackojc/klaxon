#include <iostream>
#include <sstream>
#include <string>
#include <lib.hpp>

namespace klx {

	inline void control_flow_graph(IR& ir) {
		std::ostringstream ss;

		outln(ss, "digraph G {");
		outln(ss, "  node [shape=record style=filled fontsize=10 fontname=\"Consolas\" fillcolor=\"#bfbfbf\"]");
		outln(ss, "  graph [style=filled fontsize=10 fontname=\"Consolas\" fillcolor=\"#efefef\" packmode=\"graph\"]");
		outln(ss, "  edge [fontsize=10 fontname=\"Consolas\" arrowsize=0.5]");

		for (auto it = ir.begin(); it != ir.end(); ++it) {
			if (it->kind != Ops::OP_DEF)
				continue;

			outlnfmt(ss, "  subgraph cluster_{} {", std::distance(ir.begin(), it));
			outlnfmt(ss, "    label=\"{}\"", it->sv);

			// Iterate body of function.
			for (++it; it->kind != Ops::OP_RET; ++it) {
				if (it->kind != Ops::OP_BLOCK)
					continue;

				std::ostringstream ss_block;
				auto block = it++;

				for (; it->kind != Ops::OP_END; ++it) {
					if (it->kind == Ops::OP_JUMP) {
						outlnfmt(ss, "    n{}:s -> n{}:n", block->x, it->x);
					}

					else if (it->kind == Ops::OP_BRANCH) {
						outlnfmt(ss, "    n{}:w -> n{}:n [label=\"true\" color=\"#0d00ff\"]", block->x, it->x);
						outlnfmt(ss, "    n{}:e -> n{}:n [label=\"false\" color=\"#ff8400\"]", block->x, it->y);
					}

					else if (it->kind == Ops::OP_CALL) {
						auto [begin, end] = it->sv;

						std::string str;

						for (; begin != end; ++begin) {
							switch (*begin) {
								case '<':
								case '>':
								case '|':
								case '{':
								case '}':
									str += "\\";

								default: break;
							}

							str += *begin;
						}

						out(ss_block, "call ", str, "\\l");
						continue;
					}

					out(ss_block, *it, "\\l");
				}

				std::string code = ss_block.str();

				if (code.empty()) outlnfmt(ss, "    n{} [label=\"{{block {}}}\"]", block->x, block->x);
				else              outlnfmt(ss, "    n{} [label=\"{{block {}|{}}}\"]", block->x, block->x, code);
			}

			outln(ss, "  }");
		}

		outln(ss, "}");
		print(ss.str());
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

		klx::control_flow_graph(ir);
	}

	catch (klx::Error) {
		return 1;
	}

	return 0;
}
