section .data
    nl db 10

section .bss
    input resb 32
    number resq 1

section .text
    global _start

get:
    ; Read input
    mov rax, 0
    mov rdi, 0
    mov rsi, input
    mov rdx, 32
    syscall

    ; Convert string to integer
    mov rdi, input
    call atoi
    mov [number], rax
    ret

puts:
    ; Print the number
    mov rdi, rax
    call print_int
  
    call newline
    ret

newline:
    mov rax, 1
    mov rdi, 1
    mov rsi, nl
    mov rdx, 1
    syscall
    ret

; Function to convert ASCII string to integer
atoi:
    xor rax, rax
    xor rcx, rcx
.loop:
    movzx rdx, byte [rdi + rcx]
    cmp dl, '0'
    jb .done
    cmp dl, '9'
    ja .done
    sub dl, '0'
    imul rax, 10
    add rax, rdx
    inc rcx
    jmp .loop
.done:
    ret

; Function to print an integer
print_int:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov qword [rbp-8], 0  ; NULL terminator
    mov rax, rdi
    mov rcx, 10
    mov rsi, rbp
.loop:
    xor rdx, rdx
    div rcx
    add dl, '0'
    dec rsi
    mov [rsi], dl
    test rax, rax
    jnz .loop
    mov rax, 1
    mov rdi, 1
    mov rdx, rbp
    sub rdx, rsi
    syscall
    mov rsp, rbp
    pop rbp
    ret

_start:
