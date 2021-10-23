#ifndef BR_ARG_H
#define BR_ARG_H

#include <lib/def.hpp>
#include <lib/str.hpp>
#include <lib/exit.hpp>
#include <lib/trait.hpp>
#include <lib/assert.hpp>
#include <lib/svec.hpp>
#include <lib/print.hpp>

namespace br {

	namespace detail {
		using arg_err_t = u8_t;
		enum: arg_err_t {
			ARG_SUCCESS         = 0b00000000,
			ARG_ERR_UNKNOWN_ARG = 0b00000001,
			ARG_ERR_TAKES_ARG   = 0b00000010,
			ARG_POSITIONAL      = 0b00000100,
		};

		using arg_meta_t = u8_t;
		enum: arg_meta_t {
			META_NONE       = 0b00000000,
			META_TAKES_ARG  = 0b00000001,
			META_POSITIONAL = 0b00000010,
		};

		enum {
			MESSAGE_TAKES_ARG,
			MESSAGE_UNKNOWN_ARG,
		};

		constexpr str_view messages[] = {
			"option '{}' takes an argument"_sv,
			"unknown argument: '{}'"_sv,
		};
	}


	// Return the next argument.
	str_view argshift(int& argc, const char**& argv) {
		if (argc <= 0)
			return {};

		str_view result = make_sv(*argv, length(*argv));
		argv++, argc--;

		return result;
	}

	// Put back an argument.
	void argunshift(int& argc, const char**& argv) {
		argv--, argc++;
	}


	template <typename T>
	struct opt_t {
		T fn;
		detail::arg_meta_t meta;
		str_view long_opt;
		str_view short_opt;
		str_view help;
	};

	template <typename T, typename... Ts>
	opt_t(T, Ts...) -> opt_t<T>;


	template <typename T>
	struct positional_t {
		T fn;
		detail::arg_meta_t meta;
	};

	template <typename T, typename... Ts>
	positional_t(T, Ts...) -> positional_t<T>;


	using usage_buf_t = svec<str_view, USAGE_STR_LENGTH>;
	using help_buf_t  = svec<str_view, HELP_STR_LENGTH>;


	namespace detail {
		template <typename T>
		inline void print_buf(const T& buf) {
			for (index_t i = 0; i != length(buf); ++i)
				err(at(buf, i));
		}

		template <typename T, typename... Ts>
		constexpr T cat(T buf, Ts... args) {
			return ([&] (auto x) {
				buf = emplace(buf, x);
				return buf;
			} (args), ...);
		}
	}


	template <typename... Ts>
	constexpr usage_buf_t generate_usage(str_view exe, detail::arg_meta_t meta, Ts... parsers) {
		usage_buf_t buf;

		buf = detail::cat(buf, "usage: "_sv, exe);

		([&] (auto parser) {
			auto [fn, meta, lng, shrt, help] = parser;
			buf = detail::cat(buf, " [ "_sv, lng);

			if (meta & detail::META_TAKES_ARG)
				buf = emplace(buf, " <x>"_sv);

			buf = emplace(buf, " ]"_sv);
		} (parsers), ...);

		if (meta & detail::META_POSITIONAL)
			buf = emplace(buf, " [ ... ]"_sv);

		buf = emplace(buf, "\n"_sv);
		return buf;
	}

	template <typename... Ts>
	constexpr help_buf_t generate_help(str_view exe, detail::arg_meta_t meta, Ts... parsers) {
		help_buf_t buf;

		([&] (auto parser) {
			auto [fn, meta, lng, shrt, help] = parser;
			buf = detail::cat(buf, "  "_sv, shrt, ", "_sv, lng);

			if (meta & detail::META_TAKES_ARG)
				buf = emplace(buf, " <x>"_sv);

			buf = detail::cat(buf, "\t"_sv, help, "\n"_sv);
		} (parsers), ...);

		return buf;
	}


	// Attempt to parse an option by comparing string in opt_t to arg.
	template <typename T>
	inline bool optparse(int& argc, const char**& argv, str_view arg, detail::arg_err_t& flags, opt_t<T> parser) {
		auto [fn, meta, lng, shrt, help] = parser;

		// If argument doesn't match long or short option,
		// we return false (to break the `or` chain) and
		// we set the unknown arg flag.
		if (as_byte(arg) != '-') {
			flags |= detail::ARG_POSITIONAL;
			return true;
		}

		if (none(eq(arg, shrt), eq(arg, lng))) {
			flags |= detail::ARG_ERR_UNKNOWN_ARG;
			return false;
		}

		// Call user provided handler and pass in argument.
		return fn(argc, argv, arg, flags);
	}



	template <typename T, typename... Ts>
	inline void argparse(int& argc, const char**& argv, positional_t<T> positional, Ts... parsers) {
		BR_STATIC_ASSERT((conjunction_v<is_specialisation<Ts, opt_t>...>));
		str_view exe = argshift(argc, argv); // argv[0]

		usage_buf_t usage = generate_usage(exe, positional.meta, parsers...);
		help_buf_t help = generate_help(exe, positional.meta, parsers...);

		const auto help_handler = [&help] {
			detail::print_buf(help);
			exit(EXIT_SUCCESS);
		};

		opt_t help_opt = opt_action(help_handler, "--help"_sv, "-h"_sv, "view this help message"_sv);

		if (argc == 0) {
			detail::print_buf(usage);
			exit(EXIT_FAILURE);
		}

		// Loop over all arguments.
		str_view arg = argshift(argc, argv);

		while (not is_null(arg)) {
			detail::arg_err_t flags = detail::ARG_SUCCESS;

			// Run all option parsers on the current argument.
			// Because we use `or` here, we get lazy evaluation.
			bool found = (
				optparse(argc, argv, arg, flags, parsers) or ... or // User args
				optparse(argc, argv, arg, flags, help_opt) // Help
			);

			// If we found no matches, we can assume an error occurred.
			if (not found) {
				if (flags & detail::ARG_ERR_TAKES_ARG) {
					errlnfmt(detail::messages[detail::MESSAGE_TAKES_ARG], arg);
				}

				else if (flags & detail::ARG_ERR_UNKNOWN_ARG) {
					errlnfmt(detail::messages[detail::MESSAGE_UNKNOWN_ARG], arg);
				}

				// Print usage and exit.
				detail::print_buf(usage);
				exit(EXIT_FAILURE);
			}

			// At this point the argument has been matched.
			// We check if the argument is positional and if so,
			// call the user specified handler for positional args.
			if (flags & detail::ARG_POSITIONAL) {
				positional.fn(argc, argv, arg, flags);
			}

			// Shift to the next argument.
			arg = argshift(argc, argv);
		}
	}



	// Wrap user handler for positional arg where the user only cares about the string.
	template <typename F>
	constexpr decltype(auto) positional(F func) {
		return positional_t {
			[func] (int&, const char**&, br::str_view sv, br::detail::arg_err_t&) {
				func(sv);
				return true;
			},
			detail::META_POSITIONAL
		};
	}


	// Do nothing with positional args.
	constexpr decltype(auto) ignore_positional() {
		return positional_t { [] (auto...) {}, detail::META_NONE };
	}


	// Consumes an extra argument for an option that requires an argument.
	constexpr decltype(auto) opt_arg(str_view& ref, str_view lng, str_view shrt, str_view help) {
		return opt_t {
			[&ref] (int& argc, const char**& argv, str_view arg, detail::arg_err_t& flags) {
				arg = argshift(argc, argv);

				if (is_null(arg)) {
					flags |= detail::ARG_ERR_TAKES_ARG;
					return false;
				}

				ref = arg;
				return true;
			},
			detail::META_TAKES_ARG, lng, shrt, help
		};
	}


	constexpr decltype(auto) opt_sv(str_view& ref, str_view lng, str_view shrt, str_view help) {
		return opt_t {
			[&ref] (int&, const char**&, str_view arg, detail::arg_err_t&) {
				ref = arg;
				return true;
			},
			detail::META_NONE, lng, shrt, help
		};
	}

	template <typename F>
	constexpr decltype(auto) opt_action(F func, str_view lng, str_view shrt, str_view help) {
		return opt_t {
			[func] (int&, const char**&, str_view arg, detail::arg_err_t&) {
				func();
				return true;
			},
			detail::META_NONE, lng, shrt, help
		};
	}


	constexpr decltype(auto) opt_toggle(bool& ref, str_view lng, str_view shrt, str_view help) {
		return opt_t {
			[&ref] (int&, const char**&, str_view arg, detail::arg_err_t&) {
				ref = not ref;
				return true;
			},
			detail::META_NONE, lng, shrt, help
		};
	}

	constexpr decltype(auto) opt_set(bool& ref, str_view lng, str_view shrt, str_view help) {
		return opt_t{
			[&ref] (int&, const char**&, str_view arg, detail::arg_err_t&) {
				ref = true;
				return true;
			},
			detail::META_NONE, lng, shrt, help
		};
	}

	constexpr decltype(auto) opt_unset(bool& ref, str_view lng, str_view shrt, str_view help) {
		return opt_t{
			[&ref] (int&, const char**&, str_view arg, detail::arg_err_t&) {
				ref = false;
				return true;
			},
			detail::META_NONE, lng, shrt, help
		};
	}



}

#endif
