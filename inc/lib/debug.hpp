#ifndef BR_DEBUG_H
#define BR_DEBUG_H

#include <cstdio>

#include <lib/def.hpp>
#include <lib/assert.hpp>
#include <lib/print.hpp>

namespace br {

	#define BR_TODO(msg) \
		do { \
			br::errlnfmt("[{}:{}] todo: {}!", __FILE__, BR_STR(__LINE__), (msg)); \
		} while (0)

	#ifndef BR_DISABLE_ASSERT
		#define BR_DEBUG_RUN(expr) \
			do { \
				( (expr) ); \
			} while (0)

		// We call this function in order to evaluate `expr` only once in case
		// it has side effects. If we used a macro for this, it would be evaluated
		// twice. Once for printing the result and once for returning it.
		namespace detail {
			template <typename T>
			inline decltype(auto) print_debug_impl(const char* file, const char* line, const char* expr_s, T&& expr) {
				br::errlnfmt("[{}:{}] {} = {}", file, line, expr_s, expr);
				return expr;
			}
		}

		#define BR_DEBUG(expr) \
			( br::detail::print_debug_impl(__FILE__, BR_STR(__LINE__), BR_STR(expr), (expr)) )

	#else
		#define BR_DEBUG_RUN(expr) do {} while (0)
		#define BR_DEBUG(expr) ( (expr) )
	#endif

}

#endif

