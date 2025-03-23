section .bss
  buffer resb 20  ; Buffer for number conversion

section .data
	_stack dq 0
  newline db 10  ; Newline character

section .text
  global print_int  ; Expose function for external use
  extern write      ; syscall wrapper for sys_write
  global _start 

print_int:
  ; Stack frame setup
  push rdi
  push rsi
  push rdx
  push rcx
  push r8
  push r9
	
  mov rdi, buffer + 19  ; Point to the end of buffer
  mov rcx, 0            ; Digit counter

.convert:
  mov rdx, 0            ; Clear RDX for division
  mov rbx, 10           ; Divisor
  div rbx               ; RAX = RAX / 10, RDX = RAX % 10
  add dl, '0'           ; Convert remainder to ASCII
  dec rdi               ; Move buffer pointer back
  mov [rdi], dl         ; Store ASCII character
  inc rcx               ; Increment digit counter
  test rax, rax         ; Check if quotient is zero
  jnz .convert          ; If not, continue conversion
	
  ; Print number
  mov rsi, rdi          ; Pointer to number
  mov rdx, rcx          ; Length of number
  mov rax, 1            ; syscall: sys_write
  mov rdi, 1            ; File descriptor: stdout
  syscall
		
    ; Print newline
  mov rsi, newline      ; Pointer to newline
  mov rdx, 1            ; Length 1
  syscall

    ; Restore registers
  pop r9
  pop r8
  pop rcx
  pop rdx
  pop rsi
  pop rdi
  ret


