#ifndef KLX_CODEGEN_HPP
#define KLX_CODEGEN_HPP

#include <queue>

#include <lib/def.hpp>
#include <lib/str.hpp>
#include <lib/svec.hpp>

#include <def.hpp>


// Functions
platform_type_t platform_lookup(br::str_view platform) {
	if (br::eq(platform, "linux-x86_64"_sv))  return PLATFORM_LINUX_X86_64;
	if (br::eq(platform, "linux-arm64"_sv))   return PLATFORM_LINUX_ARM64;
	if (br::eq(platform, "linux-riscv64"_sv)) return PLATFORM_LINUX_RISCV64;

	br::halt("unknown platform '{}'", platform);
}


void compile_linux_x86_64(ir_t& repr) {
	using reg_t = br::str_view;

	constexpr reg_t default_registers[] = {
		"r15"_sv,
		"r14"_sv,
		"r13"_sv,
		"r12"_sv,
		"r11"_sv,  // reserved for rflags when using syscall
		"r10"_sv,
		"r9"_sv,
		"r8"_sv,
		"rbp"_sv,  // base pointer (unused by functions in klaxon)
		"rsi"_sv,
		"rdi"_sv,
		// "rdx"_sv,  // used for remainder of div
		// "rcx"_sv,  // used as counter by shl/shr and also clobbered by syscalls
		"rbx"_sv,
		// "rax"_sv,  // used by mul/div/mod implicitly
	};

	constexpr reg_t syscall_registers[] = {
		"rax"_sv, "rdi"_sv, "rsi"_sv, "rdx"_sv, "r10"_sv, "r8"_sv, "r9"_sv,
	};

	constexpr br::size_t WORD_SIZE = 8;
	constexpr br::size_t n_registers = (sizeof(default_registers) / sizeof(br::str_view));
	constexpr br::size_t n_syscall_registers = (sizeof(syscall_registers) / sizeof(br::str_view));

	reg_t registers[n_registers];
	br::size_t used_registers = 0;


	// Reset register order and used_registers
	const auto reg_reset = [&] {
		used_registers = 0;

		for (br::index_t i = 0; i < n_registers; i++)
			registers[i] = default_registers[i];
	};


	// if used registers >= available registers, we
	// must spill the oldest register to the stack.
	const auto reg_need_spill = [&] {
		return used_registers >= n_registers;
	};


	//     <--
	// [ a b c d ]
	// [ b c d a ]
	const auto reg_rotate_l = [&] (br::index_t = 1) {
		auto first = br::front(registers);

		for (br::index_t i = 0; i < n_registers - 1; i++)
			registers[i] = registers[i + 1];

		registers[n_registers - 1] = first;
	};

	//     -->
	// [ a b c d ]
	// [ d a b c ]
	const auto reg_rotate_r = [&] (br::index_t = 1) {
		auto last = br::back(registers);

		for (br::index_t i = n_registers - 1; i > 0; i--)
			registers[i] = registers[i - 1];

		registers[0] = last;
	};


	// Access nth element of the stack relative to top or bottom.
	const auto reg_bottom = [&] (br::index_t i = 0) {
		return registers[n_registers - 1 - i];
	};

	const auto reg_top = [&] (br::index_t i = 0) {
		return registers[i];
	};


	// Allocate a new register.
	const auto reg_push = [&] {
		reg_t reg = reg_bottom();
		used_registers += used_registers == (n_registers - 1) ? 0 : 1;
		reg_rotate_r();
		return reg;
	};

	// Free a register.
	const auto reg_pop = [&] {
		reg_t reg = reg_top();
		used_registers -= used_registers == 0 ? 0 : 1;
		reg_rotate_l();
		return reg;
	};


	const auto reg_spill_all = [&] {
		for (br::index_t i = 0; i < used_registers; i++) {
			reg_t reg = reg_pop();
			br::println("\tpush ", reg);
		}
	};


	reg_reset();

	for (br::index_t i = 0; i < br::length(repr.ops); i++) {
		const auto op = br::at(repr.ops, i);
		const auto [kind, flags, sv1, sv2, n1, n2] = op;

		switch (kind) {
			// Metadata ops
			case OP_HEAD: {
				br::println("BITS 64");
				br::println("global _start");
				br::println("global main");
				br::println("section .text");

			} break;

			case OP_FOOT: {
				// entry
				br::println("\n_start:  ; entry");
				br::println("\tcall main");

				// exit syscall
				br::println("\tmov rax, 60  ; exit");
				br::println("\txor rdi, rdi");
				br::println("\tsyscall");
			} break;

			case OP_LABEL: {
				br::printlnfmt("\t.LL_{}:", n1);
			} break;

			case OP_DEF: {
				br::printlnfmt("\n{}:", sv1);
				reg_reset();
			} break;

			case OP_EXTERN: br::println("extern ", sv1); break;

			case OP_SYSCALL: {
				// get number of args from optional attribute.
				br::index_t n_args = flags & ARG_SV1 ?
					br::to_int<br::index_t>(sv1) :
					n_syscall_registers;

				if (n_args > n_syscall_registers)
					br::halt("syscall takes {} arguments at most", n_syscall_registers);

				n_args++;
				used_registers -= n_args;

				br::println("\t;  syscall");

				// spill `n_args` number of registers to stack.
				for (br::index_t i = 0; i < n_args; i++)
					br::println("\tpush ", reg_pop());

				// move the the top `n_args` stack elements into the appropriate syscall registers.
				for (br::index_t i = 0; i < n_args; i++)
					br::printlnfmt("\tmov {}, [rsp+{}]", syscall_registers[i], (n_args - i - 1) * 8);

				// perform syscall and then pop the top `n_args` elements from stack.
				br::println("\tsyscall");
				br::println("\tadd rsp, ", n_args * 8);

				br::printlnfmt("\tmov {}, rax", reg_push()); // return code
			} break;

			case OP_WORD: {
				reg_t arg = reg_push();
				br::printlnfmt("\tmov {}, {}  ; word", arg, WORD_SIZE);
			} break;

			case OP_LOAD: {
				reg_t addr = reg_top(); // this is sexy asf
				br::printlnfmt("\tmov {}, [{}]  ; load", addr, addr);
			} break;

			case OP_STORE: {
				reg_t rhs = reg_pop();  // value
				reg_t lhs = reg_pop(); // addr
				br::printlnfmt("\tmov qword [{}], {}  ; store", lhs, rhs);
			} break;

			case OP_AT: {
				if ((flags & ARG_SV1) != ARG_SV1)
					br::halt("[] takes a compile time argument");

				br::printlnfmt("\tmov {}, {}  ; [{}]", reg_push(), reg_top(br::to_int<br::index_t>(sv1)), sv1);
			} break;


			// Control flow ops
			case OP_CALL: {
				br::println("\tcall ", sv1);
			} break;

			case OP_RET: {
				br::println("\tret");
			} break;

			case OP_JMP: {
				br::println("\tjmp .LL_", n1);
			} break;

			case OP_JZ: {
				reg_t arg = reg_pop();
				br::printlnfmt("\ttest {}, {} ;  jz", arg, arg);
				br::println("\tjz .LL_", n1);
				reg_spill_all();
				reg_reset();
			}; break;


			// Arithmetic ops
			case OP_ADD: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				br::printlnfmt("\tadd {}, {}  ; +", lhs, rhs);
			} break;

			case OP_SUB: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				br::printlnfmt("\tsub {}, {}  ; -", lhs, rhs);
			} break;

			case OP_MUL: {
				// `mul` implicitly uses `rax` for it's destination.
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				// br::println("\tpush rax");
				br::printlnfmt("\tmov rax, {}  ; *", lhs);
				br::println("\timul ", rhs);
				br::printlnfmt("\tmov {}, rax", lhs);
				// br::println("\tpop rax");
			} break;

			case OP_DIV: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				// br::println("\txor rdx, rdx  ; /"); // zero rdx or else div will cat rdx and rax
				// br::println("\tpush rax");
				br::printlnfmt("\tmov rax, {}  ; /", lhs);
				br::println("\tcqo");
				br::println("\tidiv ", rhs);
				br::printlnfmt("\tmov {}, rax", lhs); // quotient in rax
				// br::println("\tpop rax");
			} break;

			case OP_MOD: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				// br::println("\txor rdx, rdx  ; %"); // zero rdx or else div will cat rdx and rax
				// br::println("\tpush rax");
				br::printlnfmt("\tmov rax, {}  ; %", lhs);
				br::println("\tcqo");
				br::println("\tidiv ", rhs);
				br::printlnfmt("\tmov {}, rdx", lhs); // remainder in rdx
				// br::println("\tpop rax");
			} break;

			case OP_LSH: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				// br::println("\tpush rcx  ; <<"); // save rcx
				br::printlnfmt("\tmov rcx, {}", rhs);
				br::printlnfmt("\tshl {}, cl", lhs);
				// br::println("\tpop rcx");
			} break;

			case OP_RSH: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				// br::println("\tpush rcx  ; >>"); // save rcx
				br::printlnfmt("\tmov rcx, {}", rhs);
				br::printlnfmt("\tshr {}, cl", lhs);
				// br::println("\tpop rcx");
			} break;


			// Stack manipulation ops
			case OP_PUSH: {
				auto spill = reg_need_spill();

				reg_t reg = reg_push();

				if (spill)
					br::printlnfmt("\tpush {}  ; spill", reg);

				br::printlnfmt("\tmov {}, {}  ; push", reg, sv1);
			} break;

			case OP_DUP: {
				reg_t rhs = reg_top(0);
				reg_t lhs = reg_push();
				br::printlnfmt("\tmov {}, {}  ; dup", lhs, rhs);
			} break;

			case OP_SWAP: {
				br::printlnfmt("\tmov rax, {}  ; swap", reg_top(0));
				br::printlnfmt("\tmov {}, {}", reg_top(0), reg_top(1));
				br::printlnfmt("\tmov {}, rax", reg_top(1));
			} break;

			case OP_DROP: {
				reg_pop();
			} break;

			case OP_OVER: {
				reg_t over = reg_top(1);
				reg_t top = reg_push();
				br::printlnfmt("\tmov {}, {}  ; over", top, over);
			} break;


			// Comparison ops
			case OP_EQ: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				br::printlnfmt("\tcmp {}, {}  ; ==", lhs, rhs);
				br::println("\tsete al");
				br::printlnfmt("\tmovzx {}, al", lhs);
			} break;

			case OP_NOT_EQ: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				br::printlnfmt("\tcmp {}, {}  ; !=", lhs, rhs);
				br::println("\tsetne al");
				br::printlnfmt("\tmovzx {}, al", lhs);
			} break;

			case OP_LESS: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				br::printlnfmt("\tcmp {}, {}  ; <", lhs, rhs);
				br::println("\tsetl al");
				br::printlnfmt("\tmovzx {}, al", lhs);
			} break;

			case OP_MORE: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				br::printlnfmt("\tcmp {}, {}  ; >", lhs, rhs);
				br::println("\tsetg al");
				br::printlnfmt("\tmovzx {}, al", lhs);
			} break;

			case OP_LESS_EQ: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				br::printlnfmt("\tcmp {}, {}  ; <=", lhs, rhs);
				br::println("\tsetle al");
				br::printlnfmt("\tmovzx {}, al", lhs);
			} break;

			case OP_MORE_EQ: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				br::printlnfmt("\tcmp {}, {}  ; >=", lhs, rhs);
				br::println("\tsetge al");
				br::printlnfmt("\tmovzx {}, al", lhs);
			} break;


			// Logical ops
			case OP_AND: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				// br::println("\tpush rcx  ; and"); // save rcx and pop it later since we need cl as a temporary
				br::printlnfmt("\ttest {}, {}", lhs, lhs);
				br::println("\tsetne cl");
				br::printlnfmt("\ttest {}, {}", rhs, rhs);
				br::println("\tsetne al");
				br::println("\tand al, cl");
				br::printlnfmt("\tmovzx {}, al", lhs);
				// br::println("\tpop rcx");
			} break;

			case OP_OR: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				br::printlnfmt("\tor {}, {}  ; or", lhs, rhs);
				br::println("\tsetne al");
				br::printlnfmt("\tmovzx {}, al", lhs);
			} break;

			case OP_NOT: {
				reg_t arg = reg_top();
				br::printlnfmt("\ttest {}, {}  ; not", arg, arg);
				br::println("\tsete al");
				br::printlnfmt("\tmovzx {}, al", arg);
			} break;


			// Bitwise ops
			case OP_BAND: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				br::printlnfmt("\tand {}, {}  ; &", lhs, rhs);
			} break;

			case OP_BOR: {
				reg_t lhs = reg_top(1);
				reg_t rhs = reg_pop();
				br::printlnfmt("\tor {}, {}  ; |", lhs, rhs);
			} break;

			case OP_BNOT: {
				reg_t arg = reg_top(0);
				br::printlnfmt("\tnot {}  ; ~", arg);
			} break;

			default:
				br::halt("unknown operation '{}'", kind);
		};
	}
}

void compile_linux_arm64(ir_t& repr) { BR_UNREACHABLE(); }
void compile_linux_riscv64(ir_t& repr) { BR_UNREACHABLE(); }


void compile(ir_t& repr, platform_type_t platform) {
	void(*fn)(ir_t&) = nullptr;

	// Runtime dispatch
	switch (platform) {
		case PLATFORM_LINUX_X86_64:  fn = compile_linux_x86_64; break;
		case PLATFORM_LINUX_ARM64:   fn = compile_linux_arm64; break;
		case PLATFORM_LINUX_RISCV64: fn = compile_linux_riscv64; break;

		default:
			br::halt("unknown platform '{}'", platform);
	};

	fn(repr);
}

#endif
