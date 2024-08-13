.intel_syntax noprefix

	.section .data
fdout: .string "%d"

.section .bss
.align 8
result: .skip 8

.section .text
.extern printf
.global _start

_start:
	mov rax, 5
	push rax
	
	# Peek Stack
	mov rax, [rsp]
	mov [result], rax
	
	# Align Stack
	mov rax, rsp
	and rax, 0x0F
	sub rsp, rax

	# Syscall libc printf
	mov rdi, fdout
	mov rsi, [result]
	call printf

	# Restore Stack
	add rsp, rax

	mov rax, 60
	xor rdi, rdi
	syscall
