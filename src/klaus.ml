type cmpType = Equal | Less | More | Leq | Meq
type token = Push of int | Pop | Puts | Read | Add | Sub | Mul | Div | Cmp of cmpType

(*ASM Header*)
let head =
  "section .data\n\
   format db \"%d\", 10, 0  ; Format string for printf\n\n\
   section .bss\n\
   result resq 1        ; Reserve space for the result\n\n\
   section .text\n\
   global _start\n\n\
   puts:\n\
   ; Prepare for calling printf\n\
   mov [result], rax\n\
   mov rdi, format      ; First argument: format string\n\
   mov rsi, [result]    ; Second argument: the result\n\
   xor rax, rax         ; Clear rax for calling printf\n\
   call printf          ; Call printf function from C library\n\
   ret                  ; Return from the function\n\n\
   extern printf           ; External declaration of printf (libc)\n\n\
   _start:\n"

let tail = "mov rax, 60\nxor rdi, rdi\nsyscall"
let program = "\nPush 5\nPush 10\nPush 16\nPuts\n"

let tokenizer (program : string) : string list =
  let lines = String.trim program |> String.split_on_char '\n' in
  List.filter (fun s -> s != "\n") lines |> List.map (fun s -> String.split_on_char ' ' s) |> List.flatten

let rec parser ins = function
  | [] -> ins
  | t :: ts -> (
      match t with
      | "Push" -> ( match ts with t :: ts -> parser (Push (int_of_string t) :: ins) ts | _ -> failwith "value expected")
      | "Pop" -> parser (Pop :: ins) ts
      | "Puts" -> parser (Puts :: ins) ts
      | "Read" -> parser (Read :: ins) ts
      | "Add" -> parser (Add :: ins) ts
      | "Sub" -> parser (Sub :: ins) ts
      | "Mul" -> parser (Mul :: ins) ts
      | "Div" -> parser (Div :: ins) ts
      | "Cmp" -> (
          match ts with
          | t :: ts -> (
              match t with
              | "=" -> parser (Cmp Equal :: ins) ts
              | "<=" -> parser (Cmp Leq :: ins) ts
              | ">=" -> parser (Cmp Equal :: ins) ts
              | "<" -> parser (Cmp Less :: ins) ts
              | ">" -> parser (Cmp More :: ins) ts
              | _ -> failwith "not a compare type")
          | _ -> failwith "compare type expected")
      | "\n" -> parser [] ts
      | _ -> failwith ("Not a valid token: " ^ t))

let gen_push sp asm n = asm ^ "\nmov rax, " ^ string_of_int n ^ "\npush rax\n"
let gen_pop sp asm = if sp <= 0 then failwith "stack is empty" else asm ^ "\npop rax\nnxor rax, rax\n"

let gen_puts sp asm =
  if sp < 1 then failwith "stack has less than 2 elements" else asm ^ "\npop rax\nmov [result], rax\npush rax\ncall puts\n\n"

let rec codegen sp asm = function
  | t :: ts -> (
      match t with
      | Push n ->
          let asm' = gen_push sp asm n in
          codegen (sp + 1) asm' ts
      | Pop ->
          let asm' = gen_pop sp asm in
          codegen (sp - 1) asm' ts
      | Puts ->
          let asm' = gen_puts sp asm in
          codegen sp asm' ts
      | _ -> failwith "not implemented yet")
  | [] -> asm

let assemble file asm =
  let oc = open_out file in
  Printf.fprintf oc "%s\n" (asm ^ tail);
  close_out oc;
  match Sys.command "nasm -f elf64 -o out.o out.s" with
  | 0 -> (
      match Sys.command "ld -o out out.o -lc -e _start -dynamic-linker /lib64/ld-linux-x86-64.so.2" with
      | 0 ->
          Sys.command "rm -rf out.o";
          ()
      | _ -> failwith "linking failed")
  | _ -> failwith "assembly failed"

let () = tokenizer program |> parser [] |> List.rev |> codegen 0 head |> assemble "out.s"
