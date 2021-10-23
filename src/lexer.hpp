#ifndef KLX_LEXER_HPP
#define KLX_LEXER_HPP

#include <lib/def.hpp>
#include <lib/str.hpp>
#include <lib/unicode.hpp>
#include <lib/print.hpp>

#include <def.hpp>


// Structures
struct token_t {
	br::str_view view = ""_sv;
	token_type_t kind = TKN_NONE;
};

struct lexer_t {
	br::str_view original = ""_sv;
	br::str_view src = ""_sv;
	token_t peek = token_t {};
};


// Forward declaration
inline token_t next_token(lexer_t& lexer);
inline token_t make_token(token_type_t, br::str_view);
inline lexer_t make_lexer(br::str_view);


// Functions
inline token_t make_token(token_type_t kind, br::str_view view) {
	return { view, kind };
}

inline lexer_t make_lexer(br::str_view src) {
	lexer_t lexer = { src, src, {} };
	next_token(lexer);
	return lexer;
}

inline token_t next_token(lexer_t& lexer) {
	token_t tok {};

	auto& [original, src, peek] = lexer;
	auto& [sbegin, send] = src;

	auto& [view, kind] = tok;
	auto& [begin, end] = view;

	br::char_t chr = br::as_char(src);

	// Consume characters from `src` while predicate holds.
	constexpr auto consume = [] (br::str_view src, br::char_t& chr, bool(*fn)(br::char_t)) {
		br::str_view sv = src;
		do {
			src.begin = sv.begin;
			sv = br::iter_next_char(sv, chr);
		} while (not br::eof(src) and fn(chr));
		return src;
	};

	while (true) {
		begin = sbegin; // Reset begin position to current char.
		end = sbegin;

		if (br::eof(src)) {
			kind = TKN_EOF;
			end = sbegin;
		}

		else if (chr == '#') {
			kind = TKN_COMMENT;
			src = consume(src, chr, [] (br::char_t c) {
				return c != '\n';
			});
			continue;
		}

		else if (br::is_number(chr)) {
			kind = TKN_NUM;
			src = consume(src, chr, br::is_number);
			end = sbegin;
		}

		else if (chr == '@') {
			kind = TKN_ATTR;
			src = br::iter_next_char(src, chr); // skip '@'
			begin = sbegin;
			src = consume(src, chr, br::is_visible);
			end = sbegin;
		}

		else if (chr == '{') {
			kind = TKN_LBLOCK;
			src = br::iter_next_char(src, chr);
			end = sbegin;
		}

		else if (chr == '}') {
			kind = TKN_RBLOCK;
			src = br::iter_next_char(src, chr);
			end = sbegin;
		}

		else if (br::is_visible(chr)) {
			kind = TKN_IDENT;
			src = consume(src, chr, [] (br::char_t c) {
				return br::is_visible(c) and c != '}';
			});
			end = sbegin;

			// Keywords.
			if      (eq(view, "while"_sv))  kind = TKN_WHILE;
			else if (eq(view, "if"_sv))     kind = TKN_IF;
			else if (eq(view, "else"_sv))   kind = TKN_ELSE;
			else if (eq(view, "macro"_sv))  kind = TKN_MACRO;
			else if (eq(view, "def"_sv))    kind = TKN_DEF;
			else if (eq(view, "extern"_sv)) kind = TKN_EXTERN;

			else if (eq(view, "+"_sv)) kind = TKN_ADD;
			else if (eq(view, "-"_sv)) kind = TKN_SUB;
			else if (eq(view, "*"_sv)) kind = TKN_MUL;
			else if (eq(view, "/"_sv)) kind = TKN_DIV;
			else if (eq(view, "%"_sv)) kind = TKN_MOD;

			else if (eq(view, "<"_sv))  kind = TKN_LESS;
			else if (eq(view, ">"_sv))  kind = TKN_MORE;
			else if (eq(view, "="_sv))  kind = TKN_EQ;
			else if (eq(view, "<="_sv)) kind = TKN_LESSEQ;
			else if (eq(view, ">="_sv)) kind = TKN_MOREEQ;
			else if (eq(view, "!="_sv)) kind = TKN_NOTEQ;
			else if (eq(view, "<<"_sv)) kind = TKN_LSH;
			else if (eq(view, ">>"_sv)) kind = TKN_RSH;

			else if (eq(view, "dup"_sv))  kind = TKN_DUP;
			else if (eq(view, "swap"_sv)) kind = TKN_SWAP;
			else if (eq(view, "drop"_sv)) kind = TKN_DROP;
			else if (eq(view, "over"_sv)) kind = TKN_OVER;
			else if (eq(view, "at"_sv))   kind = TKN_AT;

			else if (eq(view, "syscall"_sv)) kind = TKN_SYSCALL;
			else if (eq(view, "word"_sv))    kind = TKN_WORD;
			else if (eq(view, "load"_sv))    kind = TKN_LOAD;
			else if (eq(view, "store"_sv))   kind = TKN_STORE;

			else if (eq(view, "and"_sv)) kind = TKN_AND;
			else if (eq(view, "or"_sv))  kind = TKN_OR;
			else if (eq(view, "not"_sv)) kind = TKN_NOT;

			else if (eq(view, "&"_sv)) kind = TKN_BAND;
			else if (eq(view, "|"_sv)) kind = TKN_BOR;
			else if (eq(view, "~"_sv)) kind = TKN_BNOT;

			else if (eq(view, "false"_sv)) { kind = TKN_NUM; view = "0"_sv; }
			else if (eq(view, "true"_sv))  { kind = TKN_NUM; view = "1"_sv; }
		}

		else if (br::is_whitespace(chr)) {
			kind = TKN_WSPACE;
			src = consume(src, chr, br::is_whitespace);
			continue;
		}

		else {
			br::halt("unknown character '{}'", br::as_view(src));
		}

		break;
	}

	token_t ret = peek;
	peek = tok;

	return ret;
}

#endif
