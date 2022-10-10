#ifndef KLX_EMIT_HPP
#define KLX_EMIT_HPP

#include <sstream>
#include <lib.hpp>

namespace klx {

template <TokenTypes M>
struct Emit {
	Lexer<M>& lexer;
	Context& ir;

	std::ostringstream& ss;

	IR::iterator it;
	IR::iterator end;

	Emit(Lexer<M>& lexer_, Context& ir_, std::ostringstream& ss_):
		lexer(lexer_), ir(ir_), ss(ss_),
		it(ir_.begin()), end(ir_.end())
	{}

	inline void emit() {
		for (; it != end; ++it) {
			auto [kind, sv, x, y, z] = *it;

			// Indentation.
			switch (kind) {
				case Symbols::EXTERN:
				case Symbols::DECL:
				case Symbols::FN:
				case Symbols::RET:
					break;

				case Symbols::BLOCK:
				case Symbols::END: {
					out(ss, "  ");
				} break;

				default: {
					out(ss, "    ");
				} break;
			}

			out(ss, kind, " ");

			// Instruction.
			switch (kind) {
				case Symbols::EXTERN:
				case Symbols::DECL: {
					outfmt(ss, "{} ( {} -> {} )", sv, x, y);
				} break;

				case Symbols::FN:
				case Symbols::CALL: {
					out(ss, sv);
				} break;

				case Symbols::POP:
				case Symbols::PUSH:
				case Symbols::JUMP:
				case Symbols::BLOCK: {
					out(ss, x);
				} break;

				case Symbols::PHI:
				case Symbols::COPY:
				case Symbols::LET:
				case Symbols::BRANCH: {
					out(ss, x, " ", y);
				} break;

				case Symbols::RET:
				case Symbols::END:
					break;

				default: {
					lexer.error(Phases::PHASE_INTERMEDIATE, sv, STR_INVALID_SYMBOL, kind);
				} break;
			}

			outln(ss);
		}
	}
};

template <TokenTypes M>
inline std::ostringstream emit(Lexer<M>& lexer, Context& ir) {
	std::ostringstream ss;
	Emit(lexer, ir, ss).emit();
	return ss;
}

}

#endif
