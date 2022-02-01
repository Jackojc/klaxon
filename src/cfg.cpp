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
			if (it->kind != Symbols::DEF)
				continue;

			outlnfmt(ss, "  subgraph cluster_{} {", std::distance(ir.begin(), it));
			outlnfmt(ss, "    label=\"{} ({}->{})\"", it->sv, it->x, it->y);

			auto def = it++;

			// Iterate body of function.
			for (; it->kind != Symbols::RET; ++it) {
				if (it->kind != Symbols::BLOCK)
					continue;

				std::ostringstream ss_block;

				auto block = it++;

				for (; it->kind != Symbols::END; ++it) {
					if (it->kind == Symbols::JUMP) {
						outlnfmt(ss, "    \"n_{}_{}\":s -> \"n_{}_{}\":n", def->sv, block->x, def->sv, it->x);
					}

					else if (it->kind == Symbols::BRANCH) {
						outlnfmt(ss, "    \"n_{}_{}\":sw -> \"n_{}_{}\":n [label=\"true\" color=\"#0d00ff\"] [weight=10000]", def->sv, block->x, def->sv, it->x);
						outlnfmt(ss, "    \"n_{}_{}\":se -> \"n_{}_{}\":n [label=\"false\" color=\"#ff8400\"] [weight=10000]", def->sv, block->x, def->sv, it->y);
					}

					else if (it->kind == Symbols::CALL) {
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

						outfmt(ss_block, "call {} ({}-\\>{})\\l", str, it->x, it->y);
						continue;
					}

					out(ss_block, *it, "\\l");
				}

				std::string code = ss_block.str();

				if (code.empty()) outlnfmt(ss, "    \"n_{}_{}\" [label=\"{{block {} ({}-\\>{})}}\"] [weight=10]", def->sv, block->x, block->x, block->y, block->z);
				else              outlnfmt(ss, "    \"n_{}_{}\" [label=\"{{block {} ({}-\\>{})|{}}}\"] [weight=10]", def->sv, block->x, block->x, block->y, block->z, code);
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

		klx::ir_parse(ctx);
		klx::IR& ir = ctx.instructions;

		klx::control_flow_graph(ir);
	}

	catch (klx::Error) {
		return 1;
	}

	return 0;
}
