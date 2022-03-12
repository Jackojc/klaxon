#include <iostream>
#include <string>
#include <lib.hpp>

#include <ast_print.hpp>
#include <ast_effect.hpp>
#include <ast_ir.hpp>

int main(int argc, const char* argv[]) {
	std::istreambuf_iterator<char> begin(std::cin), end;
	std::string input(begin, end);

	klx::View src { &*input.begin(), &*input.end() };
	klx::SourceContext ctx { src };

	try {
		if (not klx::utf_validate(src))
			ctx.error(klx::Phases::PHASE_ENCODING, src, klx::STR_ENCODING);

		klx::src_parse(ctx);

		klx::ast_effect(ctx);

		klx::println("--- AST ---");
		klx::ast_print(ctx);

		klx::println("--- IR ---");
		klx::ast_ir(ctx);
	}

	catch (klx::Error) {
		return 1;
	}

	return 0;
}

