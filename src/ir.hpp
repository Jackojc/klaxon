#ifndef KLX_IR_HPP
#define KLX_IR_HPP

#include <lib/def.hpp>
#include <lib/str.hpp>
#include <lib/vec.hpp>
#include <lib/trait.hpp>

#include <def.hpp>


// Structures
struct op_t {
	op_type_t kind = OP_NONE;
	op_flags_t flags = ARG_NONE;
	br::str_view sv1, sv2;
	br::size_t n1, n2;
};

constexpr op_t make_op(op_type_t kind) {
	return { kind, ARG_NONE, ""_sv, ""_sv, 0, 0 };
}

constexpr op_t make_op(op_type_t kind, br::str_view a) {
	return { kind, ARG_SV1, a, ""_sv, 0, 0 };
}

constexpr op_t make_op(op_type_t kind, br::str_view a, br::str_view b) {
	return { kind, ARG_SV1 | ARG_SV2, a, b, 0, 0 };
}

constexpr op_t make_op(op_type_t kind, br::size_t a) {
	return { kind, ARG_N1, ""_sv, ""_sv, a, 0 };
}

constexpr op_t make_op(op_type_t kind, br::size_t a, br::size_t b) {
	return { kind, ARG_N1 | ARG_N2, ""_sv, ""_sv, a, b };
}


struct ir_t {
	ir_flags_t flags;
	br::size_t block_id;

	br::vec<op_t> ops;
};

constexpr ir_t make_ir() {
	return { CFLAG_NONE, 0, br::make_vec<op_t>() };
}

constexpr ir_t destroy_ir(ir_t repr) {
	repr.ops = br::destroy_vec(repr.ops);
	return repr;
}


// Functions
template <node_type_t K, typename... Ts>
void ir(ir_t& repr, Ts... args) {
	// If compilation is disabled, we still want to call the child nodes
	// in order to correctly parse the program but we _don't_ want to
	// print the surrounding assembly. This check will call every arg
	// that is invocable in the form `x()` while ignoring the other
	// arguments. This saves us having to do the check in every case
	// and handle it differently.
	if (repr.flags & CFLAG_DISABLE) {
		([&] (auto x) {
			if constexpr (br::is_noargs_invocable_v<br::decay_t<decltype(x)>>) {
				x();
			}
		} (args), ...);

		return; // Exit early to avoid hitting the cases below.
	}

	if constexpr (K == NODE_WHILE) { [&] (token_t attr, auto test, auto body) {
		const auto loop_id = repr.block_id++;
		const auto branch_id = repr.block_id++;

		repr.ops = br::push(repr.ops, make_op(OP_LABEL, loop_id));
		test();
		repr.ops = br::push(repr.ops, make_op(OP_JZ, branch_id));
		body();
		repr.ops = br::push(repr.ops, make_op(OP_JMP, loop_id));
		repr.ops = br::push(repr.ops, make_op(OP_LABEL, branch_id));
	} (args...); }

	else if constexpr (K == NODE_IF) { [&] (token_t attr, auto test, auto body) {
		const auto branch_id = repr.block_id++;

		test();
		repr.ops = br::push(repr.ops, make_op(OP_JZ, branch_id));
		body();
		repr.ops = br::push(repr.ops, make_op(OP_LABEL, branch_id));
	} (args...); }


	// Macro definition.
	else if constexpr (K == NODE_MACRO) { [&] (br::str_view name, auto body) {
		repr.flags |= CFLAG_DISABLE; // turn off compilation temporarily.
		body();
		repr.flags &= ~CFLAG_DISABLE;
	} (args...); }


	// Function definition.
	else if constexpr (K == NODE_DEF) { [&] (br::str_view name, auto body) {
		repr.ops = br::push(repr.ops, make_op(OP_DEF, name));
		body();
		repr.ops = br::push(repr.ops, make_op(OP_RET));
	} (args...); }


	// Extern.
	else if constexpr (K == NODE_EXTERN) { [&] (br::str_view name) {
		repr.ops = br::push(repr.ops, make_op(OP_EXTERN, name));
	} (args...); }


	// Function call.
	else if constexpr (K == NODE_DEF_CALL) { [&] (token_t attr, br::str_view name) {
		repr.ops = br::push(repr.ops, make_op(OP_CALL, name));
	} (args...); }


	// Integer literal.
	else if constexpr (K == NODE_NUMBER) { [&] (token_t attr, br::str_view num) {
		repr.ops = br::push(repr.ops, make_op(OP_PUSH, num));
	} (args...); }


	// Intrinsic functions.
	else if constexpr (K == NODE_INTRINSIC) { [&] (token_t attr, br::str_view name, token_type_t kind) {
		op_type_t op = OP_NONE;

		switch (kind) {
			case TKN_ADD:     op = OP_ADD; break;
			case TKN_SUB:     op = OP_SUB; break;
			case TKN_MUL:     op = OP_MUL; break;
			case TKN_DIV:     op = OP_DIV; break;
			case TKN_MOD:     op = OP_MOD; break;

			case TKN_DUP:     op = OP_DUP; break;
			case TKN_SWAP:    op = OP_SWAP; break;
			case TKN_DROP:    op = OP_DROP; break;
			case TKN_OVER:    op = OP_OVER; break;

			case TKN_EQ:      op = OP_EQ; break;
			case TKN_NOTEQ:   op = OP_NOT_EQ; break;

			case TKN_LESS:    op = OP_LESS; break;
			case TKN_MORE:    op = OP_MORE; break;
			case TKN_LESSEQ:  op = OP_LESS_EQ; break;
			case TKN_MOREEQ:  op = OP_MORE_EQ; break;

			case TKN_RSH:     op = OP_RSH; break;
			case TKN_LSH:     op = OP_LSH; break;

			case TKN_AND:     op = OP_AND; break;
			case TKN_OR:      op = OP_OR; break;
			case TKN_NOT:     op = OP_NOT; break;

			case TKN_BAND:    op = OP_BAND; break;
			case TKN_BOR:     op = OP_BOR; break;
			case TKN_BNOT:    op = OP_BNOT; break;

			case TKN_SYSCALL: op = OP_SYSCALL; break;
			case TKN_WORD:    op = OP_WORD; break;
			case TKN_LOAD:    op = OP_LOAD; break;
			case TKN_STORE:   op = OP_STORE; break;
			case TKN_AT:      op = OP_AT; break;

			default:
				br::halt("unknown intrinsic '{}'", kind);
		}

		if (attr.kind == TKN_NONE)
			repr.ops = br::push(repr.ops, make_op(op));

		else
			repr.ops = br::push(repr.ops, make_op(op, attr.view));
	} (args...); }


	// Program boilerplate.
	else if constexpr (K == NODE_PROGRAM) { [&] (auto body) {
		repr.ops = br::push(repr.ops, make_op(OP_HEAD));
		body();
		repr.ops = br::push(repr.ops, make_op(OP_FOOT));
	} (args...); }


	// Intermediate nodes.
	else if constexpr (br::eq_any(K,
		NODE_BLOCK,
		NODE_STMT,
		NODE_EXPR
	)) { [&] (auto body) {
		body();
	} (args...); }
}

#endif
