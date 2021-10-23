#ifndef BR_ARRAY_H
#define BR_ARRAY_H

#include <lib/def.hpp>
#include <lib/trait.hpp>
#include <lib/assert.hpp>
#include <lib/misc.hpp>

namespace br {

	template <typename T, size_t N>
	struct array {
		using type = T;
		T data[N];
	};


	// Size and capacity.
	template <typename T, size_t N>
	constexpr size_t length(array<T, N> v) {
		return N;
	}

	template <typename T, size_t N>
	constexpr size_t capacity(array<T, N> v) {
		return N;
	}


	template <typename T, size_t N>
	constexpr auto data(array<T, N>& v, index_t i) {
		return v.data + i;
	}



	// Make a vector with no elements.
	template <typename T, size_t N>
	[[nodiscard]] constexpr auto make_array() {
		return array<T, N>{};
	}


	// Make a vector with a number of elements.
	template <typename... Ts>
	[[nodiscard]] constexpr auto make_array(Ts... args) {
		using T = first_t<Ts...>;
		BR_STATIC_ASSERT((equivalence_v<T, Ts...>));

		// Allocate initial buffer.
		auto v = array<T, sizeof...(Ts)>{};

		// Push back elements.
		index_t i = 0;
		([&] (auto x) { *data(v, i++) = x; } (args), ...);

		return v;
	}

}

#endif


