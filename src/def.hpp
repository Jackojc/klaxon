#ifndef KLX_DEF_HPP
#define KLX_DEF_HPP

#include <lib/def.hpp>
#include <lib/str.hpp>


// Constants
constexpr br::size_t MAX_MACROS = 256;
constexpr br::size_t MAX_FUNCTIONS = 256;
constexpr br::size_t MAX_REGISTERS = 32;
constexpr br::size_t MAX_OP_ARGS = 6;


// Aliases
using token_type_t = br::u8_t;
using node_type_t = br::u8_t;
using op_type_t = br::u8_t;
using platform_type_t = br::size_t;

using parser_flags_t = br::u8_t;
using ir_flags_t = br::u8_t;
using op_flags_t = br::u8_t;


enum: op_flags_t {
	ARG_NONE = 0b0000'0000,
	ARG_SV1  = 0b0000'0001,
	ARG_SV2  = 0b0000'0010,
	ARG_N1   = 0b0000'0100,
	ARG_N2   = 0b0000'1000,
};

enum: ir_flags_t {
	CFLAG_NONE    = 0b00000000,
	CFLAG_DISABLE = 0b00000001,
};


#define DEFAULT_MACROS \
	X(dup2, over over) \
	X(drop2, drop drop) \
	X(nip, swap drop) \
	X(tuck, swap over)


// Tokens
#define TOKEN_TYPES \
	X(TKN_NONE) \
	X(TKN_EOF) \
	X(TKN_WSPACE) \
	X(TKN_COMMENT) \
	X(TKN_NUM) \
	X(TKN_IDENT) \
	X(TKN_WHILE) \
	X(TKN_IF) \
	X(TKN_ELSE) \
	X(TKN_MACRO) \
	X(TKN_DEF) \
	X(TKN_EXTERN) \
	X(TKN_LBLOCK) \
	X(TKN_RBLOCK) \
	X(TKN_ATTR) \
	X(TKN_WORD) \
	X(TKN_LOAD) \
	X(TKN_STORE) \
	X(TKN_AT) \
	X(TKN_ADD) \
	X(TKN_SUB) \
	X(TKN_MUL) \
	X(TKN_DIV) \
	X(TKN_MOD) \
	X(TKN_DUP) \
	X(TKN_SWAP) \
	X(TKN_DROP) \
	X(TKN_SYSCALL) \
	X(TKN_RSH) \
	X(TKN_LSH) \
	X(TKN_OVER) \
	X(TKN_LESS) \
	X(TKN_MORE) \
	X(TKN_EQ) \
	X(TKN_LESSEQ) \
	X(TKN_MOREEQ) \
	X(TKN_NOTEQ) \
	X(TKN_AND) \
	X(TKN_OR) \
	X(TKN_NOT) \
	X(TKN_BAND) \
	X(TKN_BOR) \
	X(TKN_BNOT)


// Nodes
#define NODE_TYPES \
	X(NONE_NONE) \
	X(NODE_WHILE) \
	X(NODE_IF) \
	X(NODE_DEF) \
	X(NODE_MACRO) \
	X(NODE_DEF_CALL) \
	X(NODE_MACRO_CALL) \
	X(NODE_EXTERN_CALL) \
	X(NODE_EXTERN) \
	X(NODE_NUMBER) \
	X(NODE_INTRINSIC) \
	X(NODE_PROGRAM) \
	X(NODE_BLOCK) \
	X(NODE_STMT) \
	X(NODE_EXPR)


// Ops
#define OP_TYPES \
	X(OP_NONE) \
	X(OP_HEAD) \
	X(OP_FOOT) \
	X(OP_LABEL) \
	X(OP_DEF) \
	X(OP_RET) \
	X(OP_EXTERN) \
	X(OP_WORD) \
	X(OP_AT) \
	X(OP_SYSCALL) \
	X(OP_LOAD) \
	X(OP_STORE) \
	X(OP_CALL) \
	X(OP_JMP) \
	X(OP_JZ) \
	X(OP_PUSH) \
	X(OP_ADD) \
	X(OP_SUB) \
	X(OP_MUL) \
	X(OP_DIV) \
	X(OP_MOD) \
	X(OP_LSH) \
	X(OP_RSH) \
	X(OP_DUP) \
	X(OP_SWAP) \
	X(OP_DROP) \
	X(OP_OVER) \
	X(OP_EQ) \
	X(OP_NOT_EQ) \
	X(OP_LESS) \
	X(OP_MORE) \
	X(OP_LESS_EQ) \
	X(OP_MORE_EQ) \
	X(OP_AND) \
	X(OP_OR) \
	X(OP_NOT) \
	X(OP_BAND) \
	X(OP_BOR) \
	X(OP_BNOT)


// Platforms
#define PLATFORM_TYPES \
	X(PLATFORM_NONE) \
	X(PLATFORM_LINUX_X86_64) \
	X(PLATFORM_LINUX_RISCV64) \
	X(PLATFORM_LINUX_ARM64)


// Tokens
#define X(name) name,
	enum: token_type_t { TOKEN_TYPES };
#undef X

#define X(name) #name##_sv,
	constexpr br::str_view token_str[] = { TOKEN_TYPES };
#undef X


// Nodes
#define X(name) name,
	enum: node_type_t { NODE_TYPES };
#undef X

#define X(name) #name##_sv,
	constexpr br::str_view node_str[] = { NODE_TYPES };
#undef X


// Ops
#define X(name) name,
	enum: op_type_t { OP_TYPES };
#undef X

#define X(name) #name##_sv,
	constexpr br::str_view op_str[] = { OP_TYPES };
#undef X


// Platforms
#define X(name) name,
	enum: platform_type_t { PLATFORM_TYPES };
#undef X

#define X(name) #name##_sv,
	constexpr br::str_view platform_str[] = { PLATFORM_TYPES };
#undef X


// Undef everything
#undef TOKEN_TYPES
#undef NODE_TYPES
#undef OP_TYPES
#undef PLATFORM_TYPES

#endif
