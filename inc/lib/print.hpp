#ifndef BR_PRINT_H
#define BR_PRINT_H

#include <iostream>

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <lib/def.hpp>
#include <lib/assert.hpp>
#include <lib/trait.hpp>
#include <lib/misc.hpp>
#include <lib/exit.hpp>
#include <lib/str.hpp>
#include <lib/vec.hpp>

namespace br {

	using stream = std::FILE*;

	namespace detail {
		// Functions to print pointer in hex format.
		template <typename T>
		inline void print_ptr(stream ss, T ptr) {
			ptr_t x = reinterpret_cast<ptr_t>(ptr);

			constexpr auto buffer_size = sizeof(ptr_t) * 2;
			constexpr char digits[] = "0123456789abcdef";

			char buf[buffer_size];

			index_t s = 0; // Number of skipped zeros.
			index_t l = 1; // Flag which determines if we are leading or not.

			for (index_t i = 0; i < buffer_size; i++) {
				u8_t v = (x >> ((buffer_size - i - 1) * 4)) & 0xF;
				buf[i] = digits[v];

				l = (v == 0) * l; // If we are not at the start, set leading to zero.
				s += l; // Increment skipped zeros if we are leading and `v` is zero.
			}

			// If `x` is zero, shift `s` backwards by one so that
			// we print 0. Otherwise, we would print nothing.
			s -= (x == 0);

			std::fwrite("0x", sizeof(byte_t), sizeof("0x"), ss);
			std::fwrite(buf + s, sizeof(byte_t), buffer_size - s, ss);
		}

		// We want to print `const char*` as a string of characters,
		// not a hex literal.
		inline void print_const_char_ptr(stream ss, const char* const str) {
			std::fwrite(str, sizeof(byte_t), std::strlen(str), ss);
		}

		// Print integral types.
		template <typename T>
		inline void print_signed(stream ss, T orig) {
			// Count max digits in i32.
			constexpr auto md = count_digits(limit_max<T>());
			char buf[md];

			// Fill buffer with `-` character so we don't need
			// to conditionall handle negative numbers.
			// If the number is negative, we just print `buf - 1`.
			for (index_t i = 1; i < md; i++)
				buf[i] = '-';

			// Get absolute value of `orig`.
			i32_t x = orig * ((orig > 0) - (orig < 0));

			// Setup pointer and counter that we can use
			// later to pass to fwrite.
			char* ptr = buf + md;
			size_t count = 0;

			// Run at least once to handle `0`.
			do {
				// Move pointer backwards from end of buffer.
				// Increment count.
				ptr--, count++;

				// Pull off least significant digit and add
				// the `0` character literal to bring it into
				// ASCII range.
				*ptr = (x % 10) + '0';
				x /= 10;
			} while (x);

			// Write `count` bytes starting from `ptr`.
			// We subtract one from `ptr` if the number is
			// negative in order to include the sign.
			// We add one to the count if the number is negative
			// also so that we don't drop the last digit after
			// moving the pointer backwards.
			std::fwrite(ptr - (orig < 0), sizeof(byte_t), count + (orig < 0), ss);
		}

		template <typename T>
		inline void print_unsigned(stream ss, T x) {
			// See above for explanation.
			constexpr auto md = count_digits(limit_max<T>());
			char buf[md] = { 0 };

			char* ptr = buf + md;
			size_t count = 0;

			// Run at least once to handle `0`.
			do {
				ptr--, count++;
				*ptr = (x % 10) + '0';
				x /= 10;
			} while (x);

			std::fwrite(ptr, sizeof(byte_t), count, ss);
		}

		inline void print_bool(stream ss, bool b) {
			std::fwrite(b ? "true" : "false", sizeof(byte_t), std::strlen(b ? "true" : "false"), ss);
		}

		inline void print_str_view(stream ss, str_view str) {
			std::fwrite(str.begin, sizeof(byte_t), length(str), ss);
		}

		inline void print_char(stream ss, char c) {
			std::fwrite(&c, sizeof(char), 1, ss);
		}

		inline void print_byte(stream ss, byte_t c) {
			std::fwrite(&c, sizeof(byte_t), 1, ss);
		}


		// Dispatch to various print functions.
		template <typename T>
		inline void print(stream ss, T t) {
			if constexpr(is_same_v<T, const char*>) {
				detail::print_const_char_ptr(ss, t);
			}

			else if constexpr(is_same_v<T, bool>) {
				detail::print_bool(ss, t);
			}

			else if constexpr(is_same_v<T, char>) {
				detail::print_char(ss, t);
			}

			else if constexpr(is_same_v<T, byte_t>) {
				detail::print_byte(ss, t);
			}

			else if constexpr(is_same_v<T, str_view>) {
				detail::print_str_view(ss, t);
			}

			// Integer types.
			else if constexpr(is_integral_v<T> and is_signed_v<T>) {
				detail::print_signed(ss, t);
			}

			else if constexpr(is_integral_v<T> and is_unsigned_v<T>) {
				detail::print_unsigned(ss, t);
			}

			// Check if `T` is a pointer type. If it is, we use
			// print_ptr instead to print the `t` in hex format.
			else if constexpr(is_pointer_v<T> or is_null_pointer_v<T>) {
				detail::print_ptr(ss, t);
			}

			else {
				BR_UNIMPLEMENTED();
			}
		}


		template <typename T>
		inline void print_container(stream ss, T v) {
			print(ss, "[");

			print(ss, front(v));

			for (br::index_t i = 1; i != br::length(v); i++) {
				print(ss, ", ");
				print(ss, br::at(v, i));
			}

			print(ss, "]");
		}


		// Print containers.
		template <typename T>
		inline void print(stream ss, vec<T> v) {
			print_container(ss, v);
		}


		template <typename T, size_t N>
		inline void print(stream ss, svec<T, N> v) {
			print_container(ss, v);
		}


		// Formatted print implementation.
		template <typename T>
		inline str_view printfmt_impl(stream ss, str_view fmt, T first) {
			str_view sv;

			while (not eof(fmt)) {
				fmt = iter_next_view(fmt, sv);

				if (eq(sv, "{"_sv)) {
					fmt = iter_next_view(fmt, sv);

					// Ensure if `}` immediately follows `{`.
					BR_ASSERT(eq(sv, "}"_sv));

					// We have found `{}`...
					detail::print(ss, first);

					// Break here because we want to pass off to printfmt again
					// to handle remaining placeholders.
					// Otherwise, we would print the same value for every
					// placeholder.
					break;
				}

				// Just print character.
				else {
					detail::print(ss, sv);
				}
			}

			return fmt;
		}

		// Base case.
		inline str_view printfmt(stream ss, str_view fmt) {
			detail::print(ss, fmt);
			return fmt;
		}

		// Call printfmt recursively.
		template <typename T, typename... Ts>
		inline str_view printfmt(stream ss, str_view fmt, T first, Ts... args) {
			fmt = detail::printfmt_impl(ss, fmt, first);
			return printfmt(ss, fmt, args...);
		}

	}


	// Print to stderr.
	// Unformatted printing functions.
	template <typename T, typename... Ts>
	inline void err(T first, Ts... rest) {
		detail::print(stderr, first);
		(err(rest), ...);
	}

	template <typename... Ts>
	inline void errln(Ts... args) {
		(err(args), ...);
		err('\n');
	}


	// Formatted print.
	template <typename... Ts>
	inline void errfmt(str_view fmt, Ts... args) {
		detail::printfmt(stderr, fmt, args...);
	}

	template <typename... Ts>
	inline void errlnfmt(str_view fmt, Ts... args) {
		detail::printfmt(stderr, fmt, args...);
		detail::print(stderr, '\n');
	}


	// Overloads for `const char*` strings.
	template <typename... Ts>
	inline void errfmt(const char* fmt, Ts... args) {
		errfmt(make_sv(fmt, length(fmt)), args...);
	}

	template <typename... Ts>
	inline void errlnfmt(const char* fmt, Ts... args) {
		errlnfmt(make_sv(fmt, length(fmt)), args...);
	}


	// Print to stdout.
	// Unformatted printing functions.
	template <typename T, typename... Ts>
	inline void print(T first, Ts... rest) {
		detail::print(stdout, first);
		(print(rest), ...);
	}

	template <typename... Ts>
	inline void println(Ts... args) {
		(print(args), ...);
		print('\n');
	}


	// Formatted print.
	template <typename... Ts>
	inline void printfmt(str_view fmt, Ts... args) {
		detail::printfmt(stdout, fmt, args...);
	}

	template <typename... Ts>
	inline void printlnfmt(str_view fmt, Ts... args) {
		detail::printfmt(stdout, fmt, args...);
		detail::print(stdout, '\n');
	}


	// Overloads for `const char*` strings.
	template <typename... Ts>
	inline void printfmt(const char* fmt, Ts... args) {
		printfmt(make_sv(fmt, length(fmt)), args...);
	}

	template <typename... Ts>
	inline void printlnfmt(const char* fmt, Ts... args) {
		printlnfmt(make_sv(fmt, length(fmt)), args...);
	}


	// Halt.
	template <typename... Ts>
	[[noreturn]] inline void halt(Ts... args) {
		br::errlnfmt(args...);
		exit(1);
	}

}

#endif

