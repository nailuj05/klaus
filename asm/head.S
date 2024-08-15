section .data
    format db "%d", 10, 0  ; Format string for printf

section .bss
    result resq 1        ; Reserve space for the result

section .text
    global _start

puts:
    ; Prepare for calling printf
    mov [result], [rsp]
    mov rdi, format      ; First argument: format string
    mov rsi, [result]    ; Second argument: the result
    xor rax, rax         ; Clear rax for calling printf
    call printf          ; Call printf function from C library
    ret                  ; Return from the function

extern printf           ; External declaration of printf (libc)
