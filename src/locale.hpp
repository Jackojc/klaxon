#ifndef KLX_LOCALE_HPP
#define KLX_LOCALE_HPP

#include <view.hpp>

namespace klx {

	constexpr View STR_UNDECLARED        = "undeclared function '{}'"_sv;
	constexpr View STR_MULTIPLE_DECLARED = "multiple declarations of '{}'"_sv;
	constexpr View STR_MULTIPLE_DEFINED  = "multiple definitions of '{}'"_sv;

	constexpr View STR_UNKNOWN_CHAR      = "unknown character '{}'"_sv;
	constexpr View STR_NOT_NUMBER        = "not a valid digit '{}'"_sv;

	constexpr View STR_EFFECT            = "expected {} value(s) but got {}"_sv;
	constexpr View STR_EFFECT_RETURN     = "expected {} return value(s) but got {}"_sv;
	constexpr View STR_EFFECT_ALTERED    = "stack size modified from {} to {} value(s)"_sv;
	constexpr View STR_EFFECT_BRANCH     = "expected {} value(s) in else branch but got {}"_sv;

	constexpr View STR_ARG               = "expecting an argument for '{}'"_sv;
	constexpr View STR_EXPECT            = "expecting '{}'"_sv;
	constexpr View STR_EXPR              = "expecting an expression"_sv;
	constexpr View STR_STMT              = "expecting a statement"_sv;
	constexpr View STR_DEF               = "expecting a definition"_sv;
	constexpr View STR_BLOCK             = "expecting a block"_sv;
	constexpr View STR_INT               = "expecting an integer"_sv;
	constexpr View STR_IDENTIFIER        = "expecting an identifier"_sv;
	constexpr View STR_INSTRUCTION       = "expecting an instruction"_sv;

	constexpr View STR_ENCODING          = "invalid source encoding"_sv;

}

#endif
