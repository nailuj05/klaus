section .data
    format db "%d", 10, 0  ; Format string for printf

section .bss
    result resq 1        ; Reserve space for the result

section .text
    global _start

puts_rax:
    ; Prepare for calling printf
    mov rdi, format      ; First argument: format string
    mov rsi, [result]    ; Second argument: the result
    xor rax, rax         ; Clear rax for calling printf
    call printf          ; Call printf function from C library
    ret                  ; Return from the function

extern printf           ; External declaration of printf (libc)

_start:
    ; Load the numbers into registers
    mov rax, 10
    push rax

    mov rbx, 16
    push rbx

    push rbx

    ; Add the numbers
    add rax, rbx

    ; Store the result
    mov [result], rax

    ; Print the result
    call puts_rax
    call puts_rax

    ; Exit the program
    mov rax, 60          ; sys_exit system call number
    xor rdi, rdi         ; Exit status 0
    syscall

