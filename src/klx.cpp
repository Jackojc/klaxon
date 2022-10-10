#include <iostream>
#include <string>
#include <lib.hpp>

#include <semantic.hpp>
#include <emit.hpp>

int main(int argc, const char* argv[]) {
	std::istreambuf_iterator<char> begin(std::cin), end;
	std::string input(begin, end);

	try {
		klx::View src { &*input.begin(), &*input.end() };
		klx::SourceLexer lexer { src };

		if (not klx::utf_validate(src))
			lexer.error(klx::Phases::PHASE_ENCODING, src, klx::STR_ENCODING);

		auto ast = klx::parse(lexer);
		auto ir = klx::semantic(lexer, ast);
		auto ss = klx::emit(lexer, ir);

		std::cout << ss.str();

		// klx::println("--- AST ---");
		// klx::ast_print(ctx);
	}

	catch (klx::Error) {
		return 1;
	}

	return 0;
}

