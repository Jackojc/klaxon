; prelude for linux x64
; notes:
; .LB_ = label branch
; .LL_ = label loop

; == CORE ==
%macro k_ret 0
	ret
%endmacro

%macro k_def 1
	%1:
%endmacro

%macro k_extern 1
	extern %1
%endmacro

%macro k_call 1
	call %1
%endmacro


; == CONTROL FLOW ==
%macro k_loop_label 1
	.LL_%1:
%endmacro

%macro k_branch_label 1
	.LB_%1:
%endmacro


%macro k_loop 1
	jmp .LL_%1
%endmacro

%macro k_jz 2
	pop %1
	cmp %1, 0
	jz .LB_%2
%endmacro


; == SYSCALLS ==
; exit syscall
%macro k_exit 1
	mov rax, 60
	mov rdi, %1
	syscall
%endmacro


; == INTRINSIC ==
; push ( -- n )
%macro k_push 1
	push %1
%endmacro

; add ( a b -- c )
%macro k_add 2
	pop %1
	pop %2
	add %1, %2
	push %2
%endmacro

; sub ( a b -- c )
%macro k_sub 2
	pop %1
	pop %2
	sub %1, %2
	push %2
%endmacro

; mul ( a b -- c )
%macro k_mul 2
%endmacro

; div ( a b -- c )
%macro k_div 2
%endmacro

; mod ( a b -- c )
%macro k_mod 2
%endmacro

%macro k_lsh 2
%endmacro

%macro k_rsh 2
%endmacro

%macro k_dup 1
	pop %1
	push %1
	push %1
%endmacro

%macro k_swap 2
	pop %1
	pop %2
	push %1
	push %2
%endmacro

%macro k_drop 1
	pop %1
%endmacro

%macro k_over 2
%endmacro

%macro k_eq 2
%endmacro

%macro k_not_eq 2
%endmacro

%macro k_less 2
%endmacro

%macro k_more 2
%endmacro

%macro k_less_eq 2
%endmacro

%macro k_more_eq 2
%endmacro

%macro k_and 2
%endmacro

%macro k_or 2
%endmacro

%macro k_not 2
%endmacro

%macro k_band 2
%endmacro

%macro k_bor 2
%endmacro

%macro k_bnot 2
%endmacro


; == HEADER ==
BITS 64
section .text

global _start
_start:
	call main
	k_exit 0

