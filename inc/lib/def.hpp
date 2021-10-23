#ifndef BR_DEF_H
#define BR_DEF_H

#include <cstdint>
#include <climits>

namespace br {

	// Aliases for sized types.
	using u8_t = uint_least8_t;
	using i8_t = int_least8_t;

	using u16_t = uint_least16_t;
	using i16_t = int_least16_t;

	using u32_t = uint_least32_t;
	using i32_t = int_least32_t;

	using u64_t = uint_least64_t;
	using i64_t = int_least64_t;


	// Named aliases.
	using byte_t   = br::u8_t;
	using char_t   = br::u32_t;
	using index_t  = br::u64_t;
	using size_t   = br::u64_t;
	using length_t = br::u64_t;
	using offset_t = br::i64_t;
	using ptr_t    = uintptr_t;
	using fd_t     = br::i32_t;

	using nullptr_t = decltype(nullptr);


	// Numeric limits.
	template <typename T> constexpr T limit_max();
	template <typename T> constexpr T limit_min();

	// 8
	template <> constexpr u8_t limit_max<u8_t>() { return UCHAR_MAX; }
	template <> constexpr i8_t limit_max<i8_t>() { return SCHAR_MIN; }

	template <> constexpr u8_t limit_min<u8_t>() { return 0; }
	template <> constexpr i8_t limit_min<i8_t>() { return SCHAR_MIN; }

	// 16
	template <> constexpr u16_t limit_max<u16_t>() { return USHRT_MAX; }
	template <> constexpr i16_t limit_max<i16_t>() { return SHRT_MAX; }

	template <> constexpr u16_t limit_min<u16_t>() { return 0; }
	template <> constexpr i16_t limit_min<i16_t>() { return SHRT_MIN; }

	// 32
	template <> constexpr u32_t limit_max<u32_t>() { return UINT_MAX; }
	template <> constexpr i32_t limit_max<i32_t>() { return INT_MAX; }

	template <> constexpr u32_t limit_min<u32_t>() { return 0; }
	template <> constexpr i32_t limit_min<i32_t>() { return INT_MIN; }

	// 64
	template <> constexpr u64_t limit_max<u64_t>() { return ULLONG_MAX; }
	template <> constexpr i64_t limit_max<i64_t>() { return LLONG_MAX; }

	template <> constexpr u64_t limit_min<u64_t>() { return 0; }
	template <> constexpr i64_t limit_min<i64_t>() { return LLONG_MIN; }


	// Detect platform.
	#define BR_PLATFORM_UNKNOWN

	// Windows.
	#ifdef _WIN64
		#define BR_PLATFORM_WINDOWS
		#undef BR_PLATFORM_UNKNOWN
	#endif

	#ifdef _WIN32
		#define BR_PLATFORM_WINDOWS
		#undef BR_PLATFORM_UNKNOWN
	#endif

	#ifdef __WINDOWS__
		#define BR_PLATFORM_WINDOWS
		#undef BR_PLATFORM_UNKNOWN
	#endif


	// Linux.
	#ifdef __linux
		#define BR_PLATFORM_LINUX
		#undef BR_PLATFORM_UNKNOWN
	#endif

	#ifdef linux
		#define BR_PLATFORM_LINUX
		#undef BR_PLATFORM_UNKNOWN
	#endif


	// BSDs.
	#ifdef __DragonFly__
		#define BR_PLATFORM_BSD
		#undef BR_PLATFORM_UNKNOWN
	#endif

	#ifdef __FreeBSD__
		#define BR_PLATFORM_BSD
		#undef BR_PLATFORM_UNKNOWN
	#endif

	#ifdef __NETBSD__
		#define BR_PLATFORM_BSD
		#undef BR_PLATFORM_UNKNOWN
	#endif

	#ifdef __OpenBSD__
		#define BR_PLATFORM_BSD
		#undef BR_PLATFORM_UNKNOWN
	#endif


	// MacOS.
	#ifdef __APPLE__
		#define BR_PLATFORM_OSX
		#undef BR_PLATFORM_UNKNOWN
	#endif

	#ifdef macintosh
		#define BR_PLATFORM_OSX
		#undef BR_PLATFORM_UNKNOWN
	#endif

	#ifdef __MACH__
		#define BR_PLATFORM_OSX
		#undef BR_PLATFORM_UNKNOWN
	#endif


	// Other UNIX.
	#ifdef __unix
		#define BR_PLATFORM_UNIX
		#undef BR_PLATFORM_UNKNOWN
	#endif

	#ifdef unix
		#define BR_PLATFORM_UNIX
		#undef BR_PLATFORM_UNKNOWN
	#endif


	// Detect compiler.
	#define BR_COMPILER_UNKNOWN

	#ifdef __GNUC__
		#define BR_COMPILER_GCC
		#undef BR_COMPILER_UNKNOWN
	#endif

	#ifdef __INTEL_COMPILER
		#define BR_COMPILER_INTEL
		#undef BR_COMPILER_UNKNOWN
	#endif

	#ifdef __clang__
		#define BR_COMPILER_CLANG
		#undef BR_COMPILER_UNKNOWN
	#endif

	#ifdef _MSC_VER
		#define BR_COMPILER_MSVC
		#undef BR_COMPILER_UNKNOWN
	#endif


	// Utility macros.
	// Stringify a macro def.
	// i.e. BR_STR(__LINE__) => "42" as opposed to "__LINE__"
	#define BR_STR_IMPL_(x) #x
	#define BR_STR(x) BR_STR_IMPL_(x)

	// Concatenate macro defs.
	// i.e. BR_CAT(__FILE__, __LINE__) => "foo.c10" as opposed to "__FILE____LINE__"
	#define BR_CAT_IMPL_(x, y) x##y
	#define BR_CAT(x, y) BR_CAT_IMPL_(x, y)

	// Create a uniquely named variable for use in a macro.
	#define BR_VAR(x) BR_CAT(var_, BR_CAT(x, BR_CAT(__LINE__, _)))

	// Evaluate expressions at beginning and ending of a scope.
	#define BR_SCOPE(open, close) \
		for ( \
			br::index_t BR_VAR(i) = ((open), 0); \
			!BR_VAR(i); \
			(BR_VAR(i)++), (close) \
		)

	// Evaluate expression at end of scope.
	#define BR_DEFER(close) \
		for ( \
			br::index_t BR_VAR(i) = 0; \
			!BR_VAR(i); \
			(BR_VAR(i)++), (close) \
		)


	// Print filename and line number `[foo.cpp:12]`
	#define BR_TRACE "[" __FILE__ ":" BR_STR(__LINE__) "] "


	// Constants
	constexpr size_t VEC_MINIMUM_LENGTH = 10;
	constexpr size_t PATH_MAXIMUM_LENGTH = 4096;
	constexpr size_t HELP_STR_LENGTH = 4096;
	constexpr size_t USAGE_STR_LENGTH = 2048;

}

#endif

