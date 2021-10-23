#ifndef BR_LOG_H
#define BR_LOG_H

#include <lib/def.hpp>
#include <lib/print.hpp>
#include <lib/assert.hpp>
#include <lib/debug.hpp>

namespace br {

	#define BR_ANSI_RESET  "\x1b[0m"
	#define BR_ANSI_BOLD   "\x1b[1m"

	#define BR_ANSI_FG_BLACK    "\x1b[30m"
	#define BR_ANSI_FG_RED      "\x1b[31m"
	#define BR_ANSI_FG_GREEN    "\x1b[32m"
	#define BR_ANSI_FG_YELLOW   "\x1b[33m"
	#define BR_ANSI_FG_BLUE     "\x1b[34m"
	#define BR_ANSI_FG_MAGENTA  "\x1b[35m"
	#define BR_ANSI_FG_CYAN     "\x1b[36m"
	#define BR_ANSI_FG_WHITE    "\x1b[37m"

	#define BR_ANSI_FG_BRIGHT_BLACK    "\x1b[30;1m"
	#define BR_ANSI_FG_BRIGHT_RED      "\x1b[31;1m"
	#define BR_ANSI_FG_BRIGHT_GREEN    "\x1b[32;1m"
	#define BR_ANSI_FG_BRIGHT_YELLOW   "\x1b[33;1m"
	#define BR_ANSI_FG_BRIGHT_BLUE     "\x1b[34;1m"
	#define BR_ANSI_FG_BRIGHT_MAGENTA  "\x1b[35;1m"
	#define BR_ANSI_FG_BRIGHT_CYAN     "\x1b[36;1m"
	#define BR_ANSI_FG_BRIGHT_WHITE    "\x1b[37;1m"

	#define BR_ANSI_BG_BLACK    "\x1b[40m"
	#define BR_ANSI_BG_RED      "\x1b[41m"
	#define BR_ANSI_BG_GREEN    "\x1b[42m"
	#define BR_ANSI_BG_YELLOW   "\x1b[43m"
	#define BR_ANSI_BG_BLUE     "\x1b[44m"
	#define BR_ANSI_BG_MAGENTA  "\x1b[45m"
	#define BR_ANSI_BG_CYAN     "\x1b[46m"
	#define BR_ANSI_BG_WHITE    "\x1b[47m"

	#define BR_ANSI_BG_BRIGHT_BLACK    "\x1b[40;1m"
	#define BR_ANSI_BG_BRIGHT_RED      "\x1b[41;1m"
	#define BR_ANSI_BG_BRIGHT_GREEN    "\x1b[42;1m"
	#define BR_ANSI_BG_BRIGHT_YELLOW   "\x1b[43;1m"
	#define BR_ANSI_BG_BRIGHT_BLUE     "\x1b[44;1m"
	#define BR_ANSI_BG_BRIGHT_MAGENTA  "\x1b[45;1m"
	#define BR_ANSI_BG_BRIGHT_CYAN     "\x1b[46;1m"
	#define BR_ANSI_BG_BRIGHT_WHITE    "\x1b[47;1m"

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
			BR_ANSI_RESET, "",
			BR_ANSI_BOLD, "",

			BR_ANSI_FG_BLACK, "",
			BR_ANSI_FG_RED, "",
			BR_ANSI_FG_GREEN, "",
			BR_ANSI_FG_YELLOW, "",
			BR_ANSI_FG_BLUE, "",
			BR_ANSI_FG_MAGENTA, "",
			BR_ANSI_FG_CYAN, "",
			BR_ANSI_FG_WHITE, "",

			BR_ANSI_FG_BRIGHT_BLACK, "",
			BR_ANSI_FG_BRIGHT_RED, "",
			BR_ANSI_FG_BRIGHT_GREEN, "",
			BR_ANSI_FG_BRIGHT_YELLOW, "",
			BR_ANSI_FG_BRIGHT_BLUE, "",
			BR_ANSI_FG_BRIGHT_MAGENTA, "",
			BR_ANSI_FG_BRIGHT_CYAN, "",
			BR_ANSI_FG_BRIGHT_WHITE, "",

			BR_ANSI_BG_BLACK, "",
			BR_ANSI_BG_RED, "",
			BR_ANSI_BG_GREEN, "",
			BR_ANSI_BG_YELLOW, "",
			BR_ANSI_BG_BLUE, "",
			BR_ANSI_BG_MAGENTA, "",
			BR_ANSI_BG_CYAN, "",
			BR_ANSI_BG_WHITE, "",

			BR_ANSI_BG_BRIGHT_BLACK, "",
			BR_ANSI_BG_BRIGHT_RED, "",
			BR_ANSI_BG_BRIGHT_GREEN, "",
			BR_ANSI_BG_BRIGHT_YELLOW, "",
			BR_ANSI_BG_BRIGHT_BLUE, "",
			BR_ANSI_BG_BRIGHT_MAGENTA, "",
			BR_ANSI_BG_BRIGHT_CYAN, "",
			BR_ANSI_BG_BRIGHT_WHITE, "",
		};
	}

	// Get a colour given its name and whether or not colours are enabled.
	inline auto colour(index_t colour, bool enabled) {
		return detail::INTERNAL_COLOUR_TABLE__[colour * 2 + !enabled];
	}


	#define BR_LOG_INFO_STYLE     BR_ANSI_RESET     "[-]"
	#define BR_LOG_WARN_STYLE     BR_ANSI_FG_BLUE   "[*]"
	#define BR_LOG_ERROR_STYLE    BR_ANSI_FG_RED    "[!]"
	#define BR_LOG_SUCCESS_STYLE  BR_ANSI_FG_GREEN  "[^]"


	enum: u8_t {
		LOG_LEVEL_INFO,
		LOG_LEVEL_WARN,
		LOG_LEVEL_ERROR,
		LOG_LEVEL_SUCCESS,
	};


	namespace detail {
		inline auto lvl_to_style(u8_t lvl) {
			switch (lvl) {
				case LOG_LEVEL_INFO:    return BR_LOG_INFO_STYLE " ";
				case LOG_LEVEL_WARN:    return BR_LOG_WARN_STYLE " ";
				case LOG_LEVEL_ERROR:   return BR_LOG_ERROR_STYLE " ";
				case LOG_LEVEL_SUCCESS: return BR_LOG_SUCCESS_STYLE " ";
			}

			return "";
		}
	}


	#define BR_LOG(...) \
		do { [BR_VAR(fn_name) = __func__] (br::u8_t BR_VAR(lvl), auto... BR_VAR(args)) { \
			BR_DEBUG_RUN(( br::err(BR_TRACE, br::detail::lvl_to_style(BR_VAR(lvl))) )); \
			BR_DEBUG_RUN(( br::err("`", BR_VAR(fn_name), "`") )); \
			\
			if constexpr(sizeof...(BR_VAR(args)) > 0) \
				BR_DEBUG_RUN( (br::err(" => ")) ); \
				BR_DEBUG_RUN( (br::errfmt(BR_VAR(args)...)) ); \
			\
			BR_DEBUG_RUN(( br::errln( BR_ANSI_RESET ) )); \
		} ( __VA_ARGS__ ); } while (0)

}

#endif

