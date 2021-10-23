#ifndef BR_STATIC_VEC_H
#define BR_STATIC_VEC_H

#include <lib/def.hpp>
#include <lib/trait.hpp>
#include <lib/assert.hpp>
#include <lib/misc.hpp>
#include <lib/array.hpp>

namespace br {

	template <typename T, size_t N>
	struct svec {
		using type = T;

		array<T, N> data;
		size_t capacity = N;
		size_t used = 0;
	};


	// Size and capacity.
	template <typename T, size_t N>
	constexpr size_t length(svec<T, N> v) {
		return v.used;
	}

	template <typename T, size_t N>
	constexpr size_t capacity(svec<T, N> v) {
		return N;
	}


	template <typename T, size_t N>
	constexpr auto data(svec<T, N>& v, index_t i) {
		return data(v.data, i);
	}


	// In-place construct an element.
	template <typename T, size_t N, typename... Ts>
	[[nodiscard]] constexpr auto emplace(svec<T, N> v, Ts... args) {
		BR_ASSERT(v.used != v.capacity);

		*data(v.data, v.used) = T { args... };
		v.used++;

		return v;
	}


	// Push back an element.
	template <typename T, size_t N>
	[[nodiscard]] constexpr auto push(svec<T, N> v, T x) {
		return emplace(v, x);
	}

	template <typename T, size_t N>
	[[nodiscard]] constexpr auto pop(svec<T, N> v, size_t n = 1) {
		BR_ASSERT(n > 0);
		v.used -= n;
		return v;
	}


	// Make a vector with no elements.
	template <typename T, size_t N>
	[[nodiscard]] constexpr auto make_svec() {
		return svec<T, N>{};
	}


	// Make a vector with a number of elements.
	template <typename... Ts>
	[[nodiscard]] constexpr auto make_svec(Ts... args) {
		using T = first_t<Ts...>;
		BR_STATIC_ASSERT((equivalence_v<T, Ts...>));

		// Allocate initial buffer.
		auto v = svec<T, sizeof...(Ts)>{};

		// Push back elements.
		index_t i = 0;
		([&] (auto x) { *data(v.data, i++) = x; } (args), ...);

		return v;
	}

}

#endif

