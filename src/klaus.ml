type cmpType = | Equal | Less | More | Leq | Meq
type token = | Push of int | Pop | Puts | Read | Add | Sub | Mul | Div | Cmp of cmpType

(*ASM Header*)
let head = 
"section .data
  fdout db \"%d\", 10, 0

section .bss
  result resq 1

section .text
	extern printf
	global _start

_start:\n"

let tail = 
"
  mov rax, 60
  xor rdi, rdi
  syscall"

let program = 
"
Push 5
Push 10
Push 16
Puts
"

let tokenizer (program: string) : string list =  
  let lines = String.trim program |> String.split_on_char '\n' in 
  List.filter (fun s -> s != "\n") lines |> List.map (fun s -> String.split_on_char ' ' s) |> List.flatten 

let rec parser ins = function 
  | [] -> ins
  | t::ts -> match t with
    | "Push" -> (match ts with
        | t::ts -> parser (Push(int_of_string t)::ins) ts
        | _ -> failwith "value expected")
    | "Pop" -> parser (Pop::ins) ts
    | "Puts" -> parser (Puts::ins) ts
    | "Read" -> parser (Read::ins) ts
    | "Add" -> parser (Add::ins) ts
    | "Sub" -> parser (Sub::ins) ts
    | "Mul" -> parser (Mul::ins) ts
    | "Div" -> parser (Div::ins) ts
    | "Cmp" -> (match ts with
        | t::ts -> (match t with 
          | "=" -> parser (Cmp(Equal)::ins) ts 
          | "<=" -> parser (Cmp(Leq)::ins) ts 
          | ">=" -> parser (Cmp(Equal)::ins) ts 
          | "<" -> parser (Cmp(Less)::ins) ts 
          | ">" -> parser (Cmp(More)::ins) ts 
          | _ -> failwith "not a compare type")
        | _ -> failwith "compare type expected")
    | "\n" -> parser [] ts
    | _ -> failwith ("Not a valid token: " ^ t)

let gen_push sp asm n = asm ^ "
  mov rax, " ^ string_of_int n ^ "
  push rax"

let gen_pop sp asm = if sp <= 0 then failwith "stack is empty" else asm ^ "
  pop rax
  nxor rax, rax\n"

let gen_puts sp asm = if sp < 1 then failwith "stack has less than 2 elements" else asm ^ "
	; Peek Stack
	mov rax, [rsp]
	mov [result], rax
	
	; Align Stack
	mov rax, rsp
	and rax, 0x0F
	sub rsp, rax

	; Syscall libc printf
	mov rdi, fdout
	mov rsi, [result]
	call printf

	; Restore Stack
	add rsp, rax\n"

let rec codegen sp asm = function 
  | t::ts -> (match t with
    | Push n -> let asm' = gen_push sp asm n in codegen (sp+1) asm' ts 
    | Pop -> let asm' = gen_pop sp asm in codegen (sp-1) asm' ts
    | Puts -> let asm' = gen_puts sp asm in codegen sp asm' ts
    | _ -> failwith "not implemented yet")
  | [] -> asm

let assemble file asm = 
  let oc = open_out file in
  Printf.fprintf oc "%s\n" (asm ^ tail);
  close_out oc;

  ()


let () = tokenizer program |> parser [] |> List.rev 
  |> codegen 0 head |> assemble "out.s"
