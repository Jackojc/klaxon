#ifndef KLX_PRINT_HPP
#define KLX_PRINT_HPP

#include <iostream>
#include <utility>
#include <cstring>

#include <view.hpp>

namespace klx {

	// generic stream
	template <typename... Ts> inline std::ostream& out(std::ostream& os, Ts&&... args) {
		return ((os << std::forward<Ts>(args)), ...);
	}

	template <typename... Ts> inline std::ostream& outln(std::ostream& os, Ts&&... args) {
		return ((os << std::forward<Ts>(args)), ..., (os << '\n'));
	}


	// stdout
	template <typename... Ts> inline std::ostream& print(Ts&&... args) {
		return (out(std::cout, std::forward<Ts>(args)), ...);
	}

	template <typename... Ts> inline std::ostream& println(Ts&&... args) {
		return (out(std::cout, std::forward<Ts>(args)), ..., out(std::cout, '\n'));
	}


	// stderr
	template <typename... Ts> inline std::ostream& err(Ts&&... args) {
		return (out(std::cerr, std::forward<Ts>(args)), ...);
	}

	template <typename... Ts> inline std::ostream& errln(Ts&&... args) {
		return (out(std::cerr, std::forward<Ts>(args)), ..., out(std::cerr, '\n'));
	}


	// Formatted output.
	// {} placeholders, escape with {{ or }}.
	namespace detail {
		template <typename T>
		inline std::ostream& outfmt(std::ostream& os, View& fmt, T&& arg) {
			View view {};

			// auto& [sbegin, send] = fmt;
			// auto& [begin, end] = view;

			bool have_left  = false;
			bool have_right = false;

			View chr = as_view(fmt);

			while (true) {
				if (fmt.is_eof())
					break;

				else if (eq_any(chr, "{", "}")) {
					const View cmp = chr;
					view = consume_view(fmt, chr, equal(cmp));

					if      (view == "{{") out(os, "{");
					else if (view == "}}") out(os, "}");

					else if (view == "{") have_left  = true;
					else if (view == "}") have_right = true;
				}

				else {
					view = consume_view(fmt, chr, partial_eq_none("{", "}"));
					out(os, view);
				}

				if (have_left and have_right) {
					out(os, std::forward<T>(arg));
					break;
				}
			}

			return os;
		}
	}

	inline std::ostream& outfmt(std::ostream& os, View fmt) {
		return out(os, fmt);
	}

	template <typename T, typename... Ts>
	inline std::ostream& outfmt(std::ostream& os, View fmt, T&& first, Ts&&... args) {
		detail::outfmt(os, fmt, std::forward<T>(first));
		return outfmt(os, fmt, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	inline std::ostream& outlnfmt(std::ostream& os, View fmt, Ts&&... args) {
		return (outfmt(os, fmt, std::forward<Ts>(args)...) << '\n');
	}


	// Formatted print variants.
	template <typename... Ts> inline std::ostream& printfmt(View fmt, Ts&&... args) {
		return outfmt(std::cout, fmt, std::forward<Ts>(args)...);
	}

	template <typename... Ts> inline std::ostream& printlnfmt(View fmt, Ts&&... args) {
		return outlnfmt(std::cout, fmt, std::forward<Ts>(args)...);
	}


	template <typename... Ts> inline std::ostream& errfmt(View fmt, Ts&&... args) {
		return outfmt(std::cerr, fmt, std::forward<Ts>(args)...);
	}

	template <typename... Ts> inline std::ostream& errlnfmt(View fmt, Ts&&... args) {
		return outlnfmt(std::cerr, fmt, std::forward<Ts>(args)...);
	}

}

#endif

