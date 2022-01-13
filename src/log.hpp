#ifndef KLX_COLOUR_HPP
#define KLX_COLOUR_HPP

#include <utility>
#include <util.hpp>
#include <print.hpp>

namespace klx {

	#define KLX_ANSI_RESET  "\x1b[0m"
	#define KLX_ANSI_BOLD   "\x1b[1m"

	#define KLX_ANSI_FG_BLACK    "\x1b[30m"
	#define KLX_ANSI_FG_RED      "\x1b[31m"
	#define KLX_ANSI_FG_GREEN    "\x1b[32m"
	#define KLX_ANSI_FG_YELLOW   "\x1b[33m"
	#define KLX_ANSI_FG_BLUE     "\x1b[34m"
	#define KLX_ANSI_FG_MAGENTA  "\x1b[35m"
	#define KLX_ANSI_FG_CYAN     "\x1b[36m"
	#define KLX_ANSI_FG_WHITE    "\x1b[37m"

	#define KLX_ANSI_FG_BRIGHT_BLACK    "\x1b[30;1m"
	#define KLX_ANSI_FG_BRIGHT_RED      "\x1b[31;1m"
	#define KLX_ANSI_FG_BRIGHT_GREEN    "\x1b[32;1m"
	#define KLX_ANSI_FG_BRIGHT_YELLOW   "\x1b[33;1m"
	#define KLX_ANSI_FG_BRIGHT_BLUE     "\x1b[34;1m"
	#define KLX_ANSI_FG_BRIGHT_MAGENTA  "\x1b[35;1m"
	#define KLX_ANSI_FG_BRIGHT_CYAN     "\x1b[36;1m"
	#define KLX_ANSI_FG_BRIGHT_WHITE    "\x1b[37;1m"

	#define KLX_ANSI_BG_BLACK    "\x1b[40m"
	#define KLX_ANSI_BG_RED      "\x1b[41m"
	#define KLX_ANSI_BG_GREEN    "\x1b[42m"
	#define KLX_ANSI_BG_YELLOW   "\x1b[43m"
	#define KLX_ANSI_BG_BLUE     "\x1b[44m"
	#define KLX_ANSI_BG_MAGENTA  "\x1b[45m"
	#define KLX_ANSI_BG_CYAN     "\x1b[46m"
	#define KLX_ANSI_BG_WHITE    "\x1b[47m"

	#define KLX_ANSI_BG_BRIGHT_BLACK    "\x1b[40;1m"
	#define KLX_ANSI_BG_BRIGHT_RED      "\x1b[41;1m"
	#define KLX_ANSI_BG_BRIGHT_GREEN    "\x1b[42;1m"
	#define KLX_ANSI_BG_BRIGHT_YELLOW   "\x1b[43;1m"
	#define KLX_ANSI_BG_BRIGHT_BLUE     "\x1b[44;1m"
	#define KLX_ANSI_BG_BRIGHT_MAGENTA  "\x1b[45;1m"
	#define KLX_ANSI_BG_BRIGHT_CYAN     "\x1b[46;1m"
	#define KLX_ANSI_BG_BRIGHT_WHITE    "\x1b[47;1m"

	enum {
		ANSI_RESET,
		ANSI_BOLD,

		ANSI_FG_BLACK,
		ANSI_FG_RED,
		ANSI_FG_GREEN,
		ANSI_FG_YELLOW,
		ANSI_FG_BLUE,
		ANSI_FG_MAGENTA,
		ANSI_FG_CYAN,
		ANSI_FG_WHITE,

		ANSI_FG_BRIGHT_BLACK,
		ANSI_FG_BRIGHT_RED,
		ANSI_FG_BRIGHT_GREEN,
		ANSI_FG_BRIGHT_YELLOW,
		ANSI_FG_BRIGHT_BLUE,
		ANSI_FG_BRIGHT_MAGENTA,
		ANSI_FG_BRIGHT_CYAN,
		ANSI_FG_BRIGHT_WHITE,

		ANSI_BG_BLACK,
		ANSI_BG_RED,
		ANSI_BG_GREEN,
		ANSI_BG_YELLOW,
		ANSI_BG_BLUE,
		ANSI_BG_MAGENTA,
		ANSI_BG_CYAN,
		ANSI_BG_WHITE,

		ANSI_BG_BRIGHT_BLACK,
		ANSI_BG_BRIGHT_RED,
		ANSI_BG_BRIGHT_GREEN,
		ANSI_BG_BRIGHT_YELLOW,
		ANSI_BG_BRIGHT_BLUE,
		ANSI_BG_BRIGHT_MAGENTA,
		ANSI_BG_BRIGHT_CYAN,
		ANSI_BG_BRIGHT_WHITE,

		ANSI_TOTAL,
	};

	namespace detail {
		constexpr const char* INTERNAL_COLOUR_TABLE__[] = {
			KLX_ANSI_RESET, "",
			KLX_ANSI_BOLD, "",

			KLX_ANSI_FG_BLACK, "",
			KLX_ANSI_FG_RED, "",
			KLX_ANSI_FG_GREEN, "",
			KLX_ANSI_FG_YELLOW, "",
			KLX_ANSI_FG_BLUE, "",
			KLX_ANSI_FG_MAGENTA, "",
			KLX_ANSI_FG_CYAN, "",
			KLX_ANSI_FG_WHITE, "",

			KLX_ANSI_FG_BRIGHT_BLACK, "",
			KLX_ANSI_FG_BRIGHT_RED, "",
			KLX_ANSI_FG_BRIGHT_GREEN, "",
			KLX_ANSI_FG_BRIGHT_YELLOW, "",
			KLX_ANSI_FG_BRIGHT_BLUE, "",
			KLX_ANSI_FG_BRIGHT_MAGENTA, "",
			KLX_ANSI_FG_BRIGHT_CYAN, "",
			KLX_ANSI_FG_BRIGHT_WHITE, "",

			KLX_ANSI_BG_BLACK, "",
			KLX_ANSI_BG_RED, "",
			KLX_ANSI_BG_GREEN, "",
			KLX_ANSI_BG_YELLOW, "",
			KLX_ANSI_BG_BLUE, "",
			KLX_ANSI_BG_MAGENTA, "",
			KLX_ANSI_BG_CYAN, "",
			KLX_ANSI_BG_WHITE, "",

			KLX_ANSI_BG_BRIGHT_BLACK, "",
			KLX_ANSI_BG_BRIGHT_RED, "",
			KLX_ANSI_BG_BRIGHT_GREEN, "",
			KLX_ANSI_BG_BRIGHT_YELLOW, "",
			KLX_ANSI_BG_BRIGHT_BLUE, "",
			KLX_ANSI_BG_BRIGHT_MAGENTA, "",
			KLX_ANSI_BG_BRIGHT_CYAN, "",
			KLX_ANSI_BG_BRIGHT_WHITE, "",
		};
	}

	// Get a colour given its name and whether or not colours are enabled.
	inline auto colour(size_t colour, bool enabled = true) {
		return detail::INTERNAL_COLOUR_TABLE__[2u * colour + !enabled];
	}


	#define KLX_LOG_1_STYLE  KLX_ANSI_RESET     "[-]"
	#define KLX_LOG_2_STYLE  KLX_ANSI_FG_BLUE   "[*]"
	#define KLX_LOG_3_STYLE  KLX_ANSI_FG_RED    "[!]"
	#define KLX_LOG_4_STYLE  KLX_ANSI_FG_GREEN  "[^]"


	enum {
		LOG_LEVEL_1,
		LOG_LEVEL_2,
		LOG_LEVEL_3,
		LOG_LEVEL_4,
	};


	namespace detail {
		constexpr auto lvl_to_style(size_t lvl) {
			switch (lvl) {
				case LOG_LEVEL_1: return KLX_LOG_1_STYLE " ";
				case LOG_LEVEL_2: return KLX_LOG_2_STYLE " ";
				case LOG_LEVEL_3: return KLX_LOG_3_STYLE " ";
				case LOG_LEVEL_4: return KLX_LOG_4_STYLE " ";
			}

			return "";
		}
	}


	#ifndef KLX_DISABLE_ASSERT
		#define KLX_DEBUG_RUN(expr) \
			do { \
				( (expr) ); \
			} while (0)

		// We call this function in order to evaluate `expr` only once in case
		// it has side effects. If we used a macro for this, it would be evaluated
		// twice. Once for printing the result and once for returning it.
		namespace detail {
			template <typename T>
			inline decltype(auto) print_debug_impl(const char* file, const char* line, const char* expr_s, T&& expr) {
				klx::errlnfmt("[{}:{}] {} = {}", file, line, expr_s, std::forward<T>(expr));
				return std::forward<T>(expr);
			}
		}

		#define KLX_DEBUG(expr) \
			( klx::detail::print_debug_impl(__FILE__, KLX_STR(__LINE__), KLX_STR(expr), (expr)) )

	#else
		#define KLX_DEBUG_RUN(expr) do {} while (0)
		#define KLX_DEBUG(expr) ( (expr) )
	#endif


	#define KLX_LOG(...) \
		do { [KLX_VAR(fn_name) = __func__] (size_t KLX_VAR(lvl), auto&&... KLX_VAR(args)) { \
			KLX_DEBUG_RUN(( klx::err(KLX_TRACE, klx::detail::lvl_to_style(KLX_VAR(lvl))) )); \
			KLX_DEBUG_RUN(( klx::err("`", KLX_VAR(fn_name), "`") )); \
			\
			if constexpr(sizeof...(KLX_VAR(args)) > 0) \
				KLX_DEBUG_RUN( (klx::err(" => ")) ); \
				KLX_DEBUG_RUN( (klx::errfmt(std::forward<decltype(KLX_VAR(args))>(KLX_VAR(args))...)) ); \
			\
			KLX_DEBUG_RUN(( klx::errln( KLX_ANSI_RESET ) )); \
		} ( __VA_ARGS__ ); } while (0)

}

#endif
