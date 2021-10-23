#ifndef BR_UNICODE_HPP
#define BR_UNICODE_HPP

#include <lib/def.hpp>

namespace br {
	#include <lib/unicode_internal.hpp>
}

namespace br {

	inline bool is_letter(char_t c) {
		return is_lu(c) or is_ll(c) or is_lt(c) or is_lm(c) or is_lo(c);
	}

	// things like accents.
	inline bool is_mark(char_t c) {
		return is_mn(c) or is_mc(c) or is_me(c);
	}

	inline bool is_number(char_t c) {
		return is_nd(c) or is_nl(c) or is_no(c);
	}

	inline bool is_punctuation(char_t c) {
		return is_pc(c) or is_pd(c) or is_ps(c) or is_pe(c) or is_pi(c) or is_pf(c) or is_po(c);
	}

	inline bool is_symbol(char_t c) {
		return is_sm(c) or is_sc(c) or is_sk(c) or is_so(c);
	}

	inline bool is_seperator(char_t c) {
		return is_zs(c) or is_zl(c) or is_zp(c);
	}

	inline bool is_control(char_t c) {
		return is_cc(c) or is_cf(c);
	}

	inline bool is_other(char_t c) {
		return is_cc(c) or is_cf(c) or is_cs(c) or is_co(c);
	}

	inline bool is_alphanumeric(char_t c) {
		return is_letter(c) or is_number(c);
	}

	inline bool is_visible(char_t c) {
		return is_alphanumeric(c) or is_symbol(c) or is_punctuation(c);
	}

	inline bool is_whitespace(char_t c) {
		return is_zs(c) or is_control(c);
	}

}

#endif
