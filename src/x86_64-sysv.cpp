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

#define MACHINE_OPS \
	X(NONE, "nop") \
	X(LABEL, "label") \
	\
	X(CALL, "call") \
	X(RET,  "ret") \
	X(MOV, "mov") \
	X(JMP, "jmp") \
	X(JZ, "jz") \
	X(TEST, "test") \
	\
	X(PUSH, "push") \
	X(POP, "pop") \
	\
	X(SETE, "sete") \
	X(SETNE, "setne") \
	X(SETL, "setl") \
	X(SETG, "setg") \
	X(SETGE, "setge") \
	X(SETLE, "setle") \
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
		enum class MachineOps { MACHINE_OPS };
	#undef X

	#define X(name, str) str##_sv,
		constexpr View MACHINEOP_TO_STRING[] = { MACHINE_OPS };
	#undef X

	constexpr View symbol_to_string(MachineOps mo) {
		return MACHINEOP_TO_STRING[(int)mo];
	}

#undef MACHINE_OPS

struct MachineOp {
	MachineOps kind = MachineOps::NONE;

	View sv;

	size_t x = 0;
	size_t y = 0;
	size_t z = 0;


	constexpr MachineOp(MachineOps kind_, size_t x_, size_t y_, size_t z_):
		kind(kind_), x(x_), y(y_), z(z_) {}

	constexpr MachineOp(MachineOps kind_, size_t x_, size_t y_):
		kind(kind_), x(x_), y(y_) {}

	constexpr MachineOp(MachineOps kind_, size_t x_):
		kind(kind_), x(x_) {}


	constexpr MachineOp(MachineOps kind_, View sv_):
		kind(kind_), sv(sv_) {}

	constexpr MachineOp(MachineOps kind_, View sv_, size_t x_, size_t y_, size_t z_):
		kind(kind_), sv(sv_), x(x_), y(y_), z(z_) {}

	constexpr MachineOp(MachineOps kind_, View sv_, size_t x_, size_t y_):
		kind(kind_), sv(sv_), x(x_), y(y_) {}

	constexpr MachineOp(MachineOps kind_, View sv_, size_t x_):
		kind(kind_), sv(sv_), x(x_) {}


	constexpr MachineOp(MachineOps kind_):
		kind(kind_) {}

	constexpr MachineOp():
		kind(MachineOps::NONE) {}
};

using Assembly = std::vector<MachineOp>;

inline Assembly code_generation(const IR& ir) {
	Assembly assembly;

	for (auto it = ir.begin(); it != ir.end(); ++it) {
		if (it->kind != Symbols::DEF)
			continue;

		auto def_it = it++;

		for (; it->kind != Symbols::RET; ++it) {
			if (it->kind != Symbols::BLOCK)
				continue;

			auto block_it = it++;

			for (; it->kind != Symbols::END; ++it) {
				switch (it->kind) {
					case Symbols::DEF: {

					} break;

					case Symbols::BLOCK: {

					} break;

					case Symbols:: : {

					} break;
				}
			}
		}
	}

	return assembly;
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

		klx::code_generation(ir);
	}

	catch (klx::Error) {
		return 1;
	}

	return 0;
}

