#include <lib/str.hpp>
#include <lib/arg.hpp>
#include <lib/unicode.hpp>
#include <lib/file.hpp>
#include <lib/log.hpp>
#include <lib/svec.hpp>
#include <lib/vec.hpp>

#include <def.hpp>
#include <lexer.hpp>
#include <ir.hpp>
#include <codegen.hpp>
#include <parser.hpp>


int main(int argc, const char* argv[]) {
	br::str_view path;
	br::str_view platform = "linux-x86_64"_sv;

	br::argparse(argc, argv, br::ignore_positional(),
		br::opt_arg(path, "-f"_sv, "--file"_sv, "the file to compile"_sv),
		br::opt_arg(path, "-p"_sv, "--platform"_sv, "target platform"_sv)
	);

	if (is_null(path))
		br::halt("no path specified");

	const auto src = br::map_file(path);

	if (not br::utf_validate(src))
		br::halt("invalid encoding");

	parser_t parser = make_parser(src);
	ir_t repr = make_ir();

	parse_program(parser, repr);
	compile(repr, platform_lookup(platform));

	br::unmap_file(src);
	return 0;
}
