#include <sstream>
#include <iomanip>
#include <iostream>
#include <vector>
#include <string>
#include <lib.hpp>

namespace klx {

#define REGISTERS \
	X(NONE, "none") \
	\
	X(RAX, "rax") \
	X(RBX, "rbx") \
	X(RCX, "rcx") \
	X(RDX, "rdx") \
	X(RSP, "rsp") \
	X(RBP, "rbp") \
	X(RSI, "rsi") \
	X(RDI, "rdi") \
	\
	X(EAX, "eax") \
	X(EBX, "ebx") \
	X(ECX, "ecx") \
	X(EDX, "edx") \
	X(ESP, "esp") \
	X(EBP, "ebp") \
	X(ESI, "esi") \
	X(EDI, "edi") \
	\
	X(AX, "ax") \
	X(BX, "bx") \
	X(CX, "cx") \
	X(DX, "dx") \
	X(SP, "sp") \
	X(BP, "bp") \
	X(SI, "si") \
	X(DI, "di") \
	\
	X(AH, "ah") \
	X(AL, "al") \
	\
	X(BH, "bh") \
	X(BL, "bl") \
	\
	X(CH, "ch") \
	X(CL, "cl") \
	\
	X(DH, "dh") \
	X(DL, "dl") \
	\
	X(SPL, "spl") \
	X(BPL, "bpl") \
	X(SIL, "sil") \
	X(DIL, "dil") \
	\
	X(R8, "r8") \
	X(R9, "r9") \
	X(R10, "r10") \
	X(R11, "r11") \
	X(R12, "r12") \
	X(R13, "r13") \
	X(R14, "r14") \
	X(R15, "r15") \
	\
	X(R8D, "r8d") \
	X(R9D, "r9d") \
	X(R10D, "r10d") \
	X(R11D, "r11d") \
	X(R12D, "r12d") \
	X(R13D, "r13d") \
	X(R14D, "r14d") \
	X(R15D, "r15d") \
	\
	X(R8W, "r8w") \
	X(R9W, "r9w") \
	X(R10W, "r10w") \
	X(R11W, "r11w") \
	X(R12W, "r12w") \
	X(R13W, "r13w") \
	X(R14W, "r14w") \
	X(R15W, "r15w") \
	\
	X(R8B, "r8b") \
	X(R9B, "r9b") \
	X(R10B, "r10b") \
	X(R11B, "r11b") \
	X(R12B, "r12b") \
	X(R13B, "r13b") \
	X(R14B, "r14b") \
	X(R15B, "r15b")

	#define X(name, str) name,
		enum class Registers { REGISTERS };
	#undef X

	#define X(name, str) str##_sv,
		constexpr View REGISTER_TO_STRING[] = { REGISTERS };
	#undef X

	constexpr View register_to_string(Registers r) {
		return REGISTER_TO_STRING[(int)r];
	}

#undef REGISTERS

inline std::ostream& operator<<(std::ostream& os, Registers r) {
	return (os << register_to_string(r));
}

#define MACHINE_OPS \
	X(NONE,  "nop") \
	X(LABEL, "label") \
	\
	X(CALL, "call") \
	X(RET,  "ret") \
	X(MOV,  "mov") \
	X(JMP,  "jmp") \
	X(JZ,   "jz") \
	X(JNZ,  "jnz") \
	X(TEST, "test") \
	\
	X(PUSH_REG, "push") \
	X(PUSH_LIT, "push") \
	X(POP,  "pop") \
	\
	X(SETE,  "sete") \
	X(SETNE, "setne") \
	X(SETL,  "setl") \
	X(SETLE, "setle") \
	X(SETG,  "setg") \
	X(SETGE, "setge") \
	\
	X(ADD, "add") \
	X(SUB, "sub") \
	X(MUL, "mul") \
	X(DIV, "div") \
	X(MOD, "mod") \
	\
	X(SAL, "sal") \
	X(SAR, "sar") \
	\
	X(AND, "and") \
	X(OR,  "or") \
	X(XOR, "xor") \
	X(NOT, "not")

	#define X(name, str) name,
		enum class AsmOps { MACHINE_OPS };
	#undef X

	#define X(name, str) str##_sv,
		constexpr View MACHINEOP_TO_STRING[] = { MACHINE_OPS };
	#undef X

	constexpr View asm_to_string(AsmOps mo) {
		return MACHINEOP_TO_STRING[(int)mo];
	}

#undef MACHINE_OPS

inline std::ostream& operator<<(std::ostream& os, AsmOps a) {
	return (os << asm_to_string(a));
}

struct AsmOp {
	AsmOps kind = AsmOps::NONE;

	size_t i = 0u;

	View x;
	View y;
	View z;


	constexpr AsmOp(AsmOps kind_, View x_, View y_, View z_):
		kind(kind_), x(x_), y(y_), z(z_) {}

	constexpr AsmOp(AsmOps kind_, View x_, View y_):
		kind(kind_), x(x_), y(y_) {}

	constexpr AsmOp(AsmOps kind_, View x_):
		kind(kind_), x(x_) {}


	constexpr AsmOp(AsmOps kind_, size_t i_):
		kind(kind_), i(i_) {}

	constexpr AsmOp(AsmOps kind_, size_t i_, View x_, View y_, View z_):
		kind(kind_), i(i_), x(x_), y(y_), z(z_) {}

	constexpr AsmOp(AsmOps kind_, size_t i_, View x_, View y_):
		kind(kind_), i(i_), x(x_), y(y_) {}

	constexpr AsmOp(AsmOps kind_, size_t i_, View x_):
		kind(kind_), i(i_), x(x_) {}


	constexpr AsmOp(AsmOps kind_):
		kind(kind_) {}

	constexpr AsmOp():
		kind(AsmOps::NONE) {}
};

using Assembly = std::vector<AsmOp>;

inline Assembly code_generation(const IR& ir) {
	Assembly assembly;

	for (auto it = ir.begin(); it != ir.end(); ++it) {
		if (it->kind != Symbols::DEF)
			continue;

		auto def_it = it++;
		assembly.emplace_back(AsmOps::LABEL, 0, def_it->sv);

		for (; it->kind != Symbols::RET; ++it) {
			if (it->kind != Symbols::BLOCK)
				continue;

			auto block_it = it++;
			assembly.emplace_back(AsmOps::LABEL, block_it->x, def_it->sv);

			for (; it->kind != Symbols::END; ++it) {
				switch (it->kind) {
					case Symbols::BRANCH: {
						assembly.emplace_back(AsmOps::JZ, it->x, def_it->sv);
					} break;

					case Symbols::CALL: {
						assembly.emplace_back(AsmOps::CALL, it->sv);
					} break;

					case Symbols::PUSH: {
						assembly.emplace_back(AsmOps::PUSH_LIT, it->x);
					} break;

					default: break;
				}
			}
		}

		assembly.emplace_back(AsmOps::RET);
	}

	return assembly;
}

template <typename... Ts>
inline std::string label_mangle(Ts&&... args) {
	std::ostringstream ss;
	((ss << std::forward<Ts>(args)), ...);

	std::string s = ss.str();
	std::ostringstream hx_ss;

	hx_ss << std::hex << std::setfill('0') << "KLX_";

	for (char c: s) {
		// ASCII
		if (
			(c >= 'a' and c <= 'z') or
			(c >= 'A' and c <= 'Z') or
			(c >= '0' and c <= '9') or
			(c == '_')
		)
			hx_ss << c;

		// UTF-8
		else
			hx_ss << std::setw(2) << (uint32_t)c;
	}

	return hx_ss.str();
}

inline std::ostream& operator<<(std::ostream& os, klx::AsmOp instr) {
	switch (instr.kind) {
		case AsmOps::LABEL: break;

		default:
			out(os, instr.kind); break;
	}

	switch (instr.kind) {
		case AsmOps::LABEL:
			outfmt(os, "{}_{}:  ;; {}_{}", label_mangle(instr.x), instr.i, instr.x, instr.i); break;

		case AsmOps::CALL:
		case AsmOps::JMP:
		case AsmOps::JZ:
			outfmt(os, " {}_{}  ;; {}_{}", label_mangle(instr.x), instr.i, instr.x, instr.i); break;

		case AsmOps::MOV:
		case AsmOps::TEST:

		case AsmOps::ADD:
		case AsmOps::SUB:
		case AsmOps::MUL:
		case AsmOps::DIV:
		case AsmOps::MOD:

		case AsmOps::AND:
		case AsmOps::OR:
		case AsmOps::XOR:
		case AsmOps::NOT:

		case AsmOps::SAL:
		case AsmOps::SAR:
			outfmt(os, " {} {}", instr.x, instr.y); break;

		case AsmOps::PUSH_LIT:
			out(os, " qword ", instr.i); break;

		case AsmOps::PUSH_REG:
		case AsmOps::POP:

		case AsmOps::SETE:
		case AsmOps::SETNE:
		case AsmOps::SETL:
		case AsmOps::SETLE:
		case AsmOps::SETG:
		case AsmOps::SETGE:
			out(os, " ", instr.x); break;

		case AsmOps::RET:
		case AsmOps::NONE:
		default: break;
	}

	return os;
}

inline void serialise_asm(const Assembly& assembly) {
	for (auto& op: assembly) {
		if (op.kind == AsmOps::NONE)
			continue;

		switch (op.kind) {
			case AsmOps::LABEL: break;
			default:
				print("\t"); break;
		}

		println(op);
	}
}

}

int main(int argc, const char* argv[]) {
	std::istreambuf_iterator<char> begin(std::cin), end;
	std::string input(begin, end);

	klx::View src { &*input.begin(), &*input.end() };
	klx::StackContext ctx { src };

	try {
		if (not klx::utf_validate(src))
			ctx.error(klx::Phases::PHASE_ENCODING, src, klx::STR_ENCODING);

		klx::ir_parse(ctx);
		klx::IR& ir = ctx.instructions;

		klx::Assembly assembly = klx::code_generation(ir);
		klx::serialise_asm(assembly);
	}

	catch (klx::Error) {
		return 1;
	}

	return 0;
}

