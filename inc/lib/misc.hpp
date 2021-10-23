#ifndef BR_MISC_H
#define BR_MISC_H

#include <lib/def.hpp>

namespace br {

	// Absolute difference between 2 pointers.
	template <typename T>
	constexpr size_t ptrdiff(const T a, const T b) {
		return
			((b - a) * (b > a)) +  // b > a => b - a
			((a - b) * (a > b));   // a > b => a - b
	}


	// Branchless ternary.
	template <typename T> constexpr auto condition(bool cond, T a, T b) {
		return (cond * a) + (!cond * b);
	}


	// Branchless absolute function.
	template <typename T> constexpr auto abs(T v) {
		return v * ( (v > 0) - (v < 0) );
	}


	// Branchless min, max and clamp.
	template <typename T> constexpr auto min(T a, T b) {
		return condition(a < b, a, b);
	}

	template <typename T> constexpr auto max(T a, T b) {
		return condition(a > b, a, b);
	}

	template <typename T> constexpr T clamp(T x, T mn, T mx) {
		return max(mn, min(x, mx));
	}


	// Swap two values by ref.
	template <typename T> constexpr void swap(T& a, T& b) {
		T tmp = a;
		a = b;
		b = tmp;
	}


	// Count the number of digits in an integer (radix 10).
	template <typename T>
	constexpr size_t count_digits(T x) {
		size_t count = 0;

		// Loop at least once to handle `0`.
		do {
			x = x / 10;
			count++;
		} while (x != 0);

		return count;
	}


	// Get length of static array.
	template <typename T, size_t N>
	constexpr size_t length(T(&)[N]) {
		return N;
	}


	// Check if any arguments are true.
	template <typename T, typename... Ts>
	constexpr bool any(T&& first, Ts&&... rest) {
		return ((first or rest) or ...);
	}

	// Check if all arguments are true.
	template <typename T, typename... Ts>
	constexpr bool all(T&& first, Ts&&... rest) {
		return ((first and rest) and ...);
	}

	// Check if all arguments are false.
	template <typename T, typename... Ts>
	constexpr bool none(T&& first, Ts&&... rest) {
		return ((not first and not rest) and ...);
	}


	// Check if all arguments are equal to first.
	template <typename T, typename... Ts>
	constexpr bool eq_all(T&& first, Ts&&... rest) {
		return ((first == rest) and ...);
	}

	// Check if any arguments are equal to first.
	template <typename T, typename... Ts>
	constexpr bool eq_any(T&& first, Ts&&... rest) {
		return ((first == rest) or ...);
	}

	// Check if none of the arguments are equal to first.
	template <typename T, typename... Ts>
	constexpr bool eq_none(T&& first, Ts&&... rest) {
		return ((first != rest) and ...);
	}


	// Read/write data at specified index.
	template <typename T> constexpr auto at(T v, index_t i) {
		return *data(v, i);
	}

	template <typename T> constexpr auto set(T v, index_t i, T x) {
		*data(v, i) = x;
		return v;
	}

	template <typename T> [[nodiscard]] constexpr auto empty(T v) {
		return length(v) == 0;
	}



	// Access the front and back of a container with random access.
	template <typename T, size_t N> constexpr auto front(T(&v)[N]) {
		return v[0];
	}

	template <typename T, size_t N> constexpr auto back(T(&v)[N]) {
		return v[N - 1];
	}

	template <typename T> constexpr auto front(T v) {
		return at(v, 0);
	}

	template <typename T> constexpr auto back(T v) {
		return at(v, length(v) - 1);
	}


	// Count leading zeros.
	constexpr u32_t countl_zero(u32_t x) {
		#if defined(BR_COMPILER_CLANG) || defined(BR_COMPILER_GCC)
			return __builtin_clz(x);
		#else
			x = ~x;
			uint32_t count = 0;

			while (x & (1u << 31u)) {
				count++;
				x <<= 1;
			}

			return count;
		#endif
	}

	constexpr u32_t countl_one(u32_t x) {
		return countl_zero(~x);
	}


	// Wrap two types.
	template <typename T1, typename T2>
	struct pair_t {
		T1 first;
		T2 second;
	};

	template <typename T1, typename T2>
	pair_t(T1, T2) -> pair_t<T1, T2>;

}

#endif
