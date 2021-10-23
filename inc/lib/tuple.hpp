#ifndef BR_TUPLE_H
#define BR_TUPLE_H

#include <lib/def.hpp>
#include <lib/trait.hpp>
#include <lib/assert.hpp>
#include <lib/misc.hpp>

namespace br {

	namespace detail {
		template <size_t I, typename T>
		struct tuple_impl {
			T val;

			constexpr tuple_impl(const T& val_): val(val_) {}
		};

		template <size_t I, typename... Ts>
		struct tuple_rec_base {};

		template <size_t I, typename L, typename... Ts>
		struct tuple_rec_base<I, L, Ts...>:
			tuple_impl<I, L>,
			tuple_rec_base<I + 1, Ts...>
		{
			template <typename X, typename... Xs>
			constexpr tuple_rec_base(X arg, Xs... args):
				tuple_impl<I, X>(arg),
				tuple_rec_base<I + 1, Ts...>(args...)
			{}
		};
	}

	template <typename L, typename... Ts>
	struct tuple: detail::tuple_rec_base<0, L, Ts...> {
	 	template <typename... Xs>
	 	constexpr tuple(Xs... args):
	    	detail::tuple_rec_base<0, L, Ts...>(args...)
	 	{}
	};

	template <typename... Ts>
	tuple(Ts... args) -> tuple<Ts...>;

	template <size_t I, typename L, typename... Ts>
	struct extract_type_at {
		using type = typename extract_type_at<I - 1, Ts...>::type;
	};

	template <typename L, typename... Ts>
	struct extract_type_at<0, L, Ts...> {
		using type = L;
	};

	template <size_t I, typename... Ts>
	constexpr decltype(auto) get(tuple<Ts...>& t)  {
		return (static_cast<
			detail::tuple_impl<I, typename extract_type_at<I, Ts...>::type>&
		>(t)).val;
	}

}

#endif



