#include <iostream>
#include <string>
#include <lib.hpp>

int main(int argc, const char* argv[]) {
	std::istreambuf_iterator<char> begin(std::cin), end;
	std::string input(begin, end);

	klx::View src { &*input.begin(), &*input.end() };
	klx::Context ctx { src };

	try {
		if (not klx::utf_validate(src))
			ctx.error(klx::Phases::PHASE_ENCODING, src, klx::STR_ENCODING);

		klx::src_parse(ctx);
		klx::serialise(ctx.instructions);
	}

	catch (klx::Error) {
		return 1;
	}

	return 0;
}

