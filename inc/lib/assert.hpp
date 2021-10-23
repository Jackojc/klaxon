#ifndef BR_ASSERT_H
#define BR_ASSERT_H

#include <cstdio>

#include <lib/def.hpp>
#include <lib/exit.hpp>

namespace br {

	// BR_ENABLE_ASSERT  => Enable assertions even when NDEBUG is set.
	// BR_DISABLE_ASSERT => Disable assertions explicitly.

	#if defined(NDEBUG) && !defined(BR_ENABLE_ASSERT)
		#define BR_DISABLE_ASSERT
	#endif

	#define BR_UNIMPLEMENTED() \
		do { \
			std::fputs(BR_TRACE "unimplemented!", stderr); \
			br::exit(br::EXIT_FAILURE); \
		} while (0)

	#ifndef BR_DISABLE_ASSERT
		#define BR_UNREACHABLE() \
			do { \
				std::fputs(BR_TRACE "unreachable!", stderr); \
				br::exit(br::EXIT_FAILURE); \
			} while (0)

		#define BR_ASSERT(cond) \
			do { \
				if (not (cond)) { \
					std::fputs(BR_TRACE "assertion failed: '" BR_STR(cond) "'!", stderr); \
					br::exit(br::EXIT_FAILURE); \
				} \
			} while (0)

		#define BR_STATIC_ASSERT(cond) \
			do { \
				static_assert(cond, BR_TRACE "assertion failed: '" #cond "'!"); \
			} while (0)

	#else
		#define BR_UNREACHABLE() do {} while (0)
		#define BR_ASSERT(cond) do {} while (0)
		#define BR_STATIC_ASSERT(cond) do {} while (0)
	#endif

}

#endif
