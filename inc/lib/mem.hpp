#ifndef BR_MEM_H
#define BR_MEM_H

#include <cstdlib>
#include <lib/def.hpp>
#include <lib/assert.hpp>

namespace br {

	// Copy a region of memory to another (regions must not overlap)
	template <typename T1, typename T2>
	inline void memcpy(T1* src, T2* dest, size_t count) {
		for (index_t i = 0; i < count; i++) {
			*((char*)dest + i) = *((char*)src + i);
		}
	}

	template <typename T1, typename T2>
	inline void memmove(T1* src, T2* dest, size_t count) {
		index_t i;
		index_t end;
		i32_t direction;

		if (dest > src) {
			i = count;
			end = 0;
			direction = -1;
		}

		else {
			i = 0;
			end = count;
			direction = 1;
		}

		for (; i != end; i += direction)
			*((char*)dest + i) = *((char*)src + i);
	}


	// Fill buffer with value.
	template <typename T, typename V>
	inline void fill(T& arr, size_t count, V val) {
		for (index_t i = 0; i < count; i++) {
			arr[i] = val;
		}
	}


	// Allocate or expand a chunk of memory.
	// Realloc acts like malloc when provided with nullptr.
	template <typename T>
	inline T* alloc(size_t count = 1, T* old_ptr = nullptr) {
		BR_ASSERT(count != 0); // UB if count is zero.
		T* ptr = static_cast<T*>(std::realloc(old_ptr, sizeof(T) * count));
		BR_ASSERT(ptr != nullptr);
		return ptr;
	}

	template <typename T>
	inline void free(T* ptr) {
		std::free(ptr);
	}

}

#endif
