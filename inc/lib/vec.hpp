#ifndef BR_VEC_H
#define BR_VEC_H

#include <lib/def.hpp>
#include <lib/trait.hpp>
#include <lib/mem.hpp>
#include <lib/misc.hpp>
#include <lib/assert.hpp>

namespace br {

	template <typename T>
	struct vec {
		using type = T;

		T* data = nullptr;
		size_t capacity = 0;
		size_t used = 0;
	};


	// Size and capacity.
	template <typename T>
	constexpr size_t length(vec<T> v) {
		return v.used;
	}

	template <typename T>
	constexpr size_t capacity(vec<T> v) {
		return v.capacity;
	}


	// Access a specific index.
	template <typename T>
	constexpr auto data(vec<T>& v, index_t i) {
		return v.data + i;
	}


	namespace detail {
		constexpr size_t grow_cap(size_t cap) {
			return cap + cap / 2; // Grow by 1.5x.
		}

		// Grow a vector by a growth factor of 1.5.
		template <typename T>
		[[nodiscard]] inline vec<T> grow(vec<T> v) {
			const size_t new_cap = grow_cap(v.capacity); // Increase by 1/2 of capacity
			v.data = br::alloc<T>(new_cap, v.data);  // Realloc moves memory for us.
			v.capacity = new_cap; // Update capacity.
			return v;
		}

		// Allocate initial storage space for vector.
		template <typename T>
		[[nodiscard]] inline vec<T> alloc(vec<T> v, size_t count) {
			const size_t new_cap = max(VEC_MINIMUM_LENGTH, grow_cap(count));
			v.data = br::alloc<T>(new_cap);
			v.capacity = new_cap;
			return v;
		}
	}


	// In-place construct an element.
	template <typename T, typename... Ts>
	[[nodiscard]] constexpr vec<T> emplace(vec<T> v, Ts... args) {
		if (v.used == v.capacity)
			v = detail::grow(v);

		v.data[v.used] = T { args... };
		v.used++;

		return v;
	}


	// Push back an element.
	template <typename T>
	[[nodiscard]] constexpr vec<T> push(vec<T> v, T x) {
		return emplace(v, x);
	}

	template <typename T>
	[[nodiscard]] constexpr vec<T> pop(vec<T> v, size_t n = 1) {
		BR_ASSERT(n > 0);
		v.used -= n;
		return v;
	}


	// Increase capacity by N.
	template <typename T>
	[[nodiscard]] constexpr vec<T> reserve(vec<T> v, size_t n) {
		v.capacity += n;
		v.data = br::alloc<T>(v.capacity, v.data);
		return v;
	}

	// Emplace N new elements.
	template <typename T>
	[[nodiscard]] constexpr vec<T> resize(vec<T> v, size_t n) {
		while (n--)
			emplace(v);
	}


	// Make a vector with no elements.
	template <typename T>
	[[nodiscard]] constexpr auto make_vec() {
		return detail::alloc(vec<T>{ nullptr, 0, 0 }, 0); // Minimum length value will be used here.
	}


	// Make a vector with a number of elements.
	template <typename... Ts>
	[[nodiscard]] constexpr auto make_vec(Ts... args) {
		using T = first_t<Ts...>;
		BR_STATIC_ASSERT((equivalence_v<T, Ts...>));

		// Allocate initial buffer.
		auto v = detail::alloc(vec<T>{ nullptr, 0, sizeof...(Ts) }, sizeof...(Ts));

		// Push back elements.
		index_t i = 0;
		([&] (auto x) { *data(v, i++) = x; } (args), ...);

		return v;
	}


	// Destroy a vector and release its memory.
	template <typename T>
	[[nodiscard]] constexpr vec<T> destroy_vec(vec<T> v) {
		free(v.data);
		v.data = nullptr;
		return v;
	}

}

#endif
