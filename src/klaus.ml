type cmpType = Equal | Less | More | Leq | Meq
type token = Push of int | Pop | Puts | Read | Add | Sub | Mul | Div | Dup | Swap | Cmp of (cmpType * string) | Label of string | End

(*ASM Header*)
let head =
  "section .data\n\
   pformat db \"%ld\", 10, 0  ; Format string for printf\n\n\
   sformat db \"%d\", 0 ; Format string for scanf\n\
   section .bss\n\
   result resq 1\n\n\
   num resq 1\n\n\
   section .text\n\
   global _start\n\n\
   extern printf\n\n\
   extern scanf\n\n\
   _start:\n"

let tail = "mov rax, 60\nxor rdi, rdi\nsyscall"
let align = "\nmov rbp, rsp\nand rsp, -16 \n"
let restore = "\nmov rsp, rbp\n"

let tokenizer (program : string) : string list =
  let lines = String.trim program |> String.split_on_char '\n' in
  List.filter (fun s -> s != "\n" && not (String.starts_with ~prefix:"#" s)) lines
  |> List.map (fun s -> String.split_on_char ' ' s)
  |> List.flatten

let extract_label x = if String.starts_with ~prefix:":" x && String.length x > 1 then Some (String.sub x 1 (String.length x - 1)) else None

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
      | "Dup" -> parser (Dup :: ins) ts
      | "Swap" -> parser (Swap :: ins) ts
      | "Cmp" -> (
          match ts with
          | t :: ts -> (
              let label = match extract_label t with Some l -> l | None -> failwith "Not a label" in
              match t with
              | "=" -> parser (Cmp (Equal, label) :: ins) ts
              | "<=" -> parser (Cmp (Leq, label) :: ins) ts
              | ">=" -> parser (Cmp (Equal, label) :: ins) ts
              | "<" -> parser (Cmp (Less, label) :: ins) ts
              | ">" -> parser (Cmp (More, label) :: ins) ts
              | _ -> failwith "not a compare type")
          | _ -> failwith "compare type expected")
      | "End" -> parser (End :: ins) ts
      | "\n" -> parser [] ts
      | x -> ( match extract_label t with Some label -> parser (Label label :: ins) ts | None -> failwith "Not a valid token"))

let gen_push asm n = asm ^ "\nmov rax, " ^ string_of_int n ^ "\npush rax\n"
let gen_pop asm = asm ^ "\npop rax\nxor rax, rax\n"
let gen_puts asm = asm ^ "\npop rbx\nmov rsi, rbx\npush rbx\n" ^ align ^ "\nmov rdi, pformat\nxor rax, rax\ncall printf\n\n" ^ restore
let gen_read asm = asm ^ align ^ "\nmov rdi, sformat\nmov rsi, num\nxor rax, rax\ncall scanf\nmov rax, [num]\npush rax\n\n" ^ restore
let gen_add asm = asm ^ "\npop rax\npop rbx\nadd rax, rbx\npush rax\n\n"
let gen_sub asm = asm ^ "\npop rbx\npop rax\nsub rax, rbx\npush rax\n\n"
let gen_mul asm = asm ^ "\npop rax\npop rbx\nmul rbx\npush rax\n\n"
let gen_div asm = asm ^ "\npop rax\npop rbx\ndiv rbx\npush rax\n\n"
let gen_dup asm = asm ^ "\npop rax\npush rax\npush rax\n\n"
let gen_swap asm = asm ^ "\npop rax\npop rbx\npush rax\npush rbx\n\n"
let gen_end asm = asm ^ "\n" ^ tail ^ "\n"

let rec codegen asm = function
  | t :: ts -> (
      match t with
      | Push n ->
          let asm' = gen_push asm n in
          codegen asm' ts
      | Pop ->
          let asm' = gen_pop asm in
          codegen asm' ts
      | Puts ->
          let asm' = gen_puts asm in
          codegen asm' ts
      | Read ->
          let asm' = gen_read asm in
          codegen asm' ts
      | Add ->
          let asm' = gen_add asm in
          codegen asm' ts
      | Sub ->
          let asm' = gen_sub asm in
          codegen asm' ts
      | Mul ->
          let asm' = gen_mul asm in
          codegen asm' ts
      | Div ->
          let asm' = gen_div asm in
          codegen asm' ts
      | Dup ->
          let asm' = gen_dup asm in
          codegen asm' ts
      | Swap ->
          let asm' = gen_swap asm in
          codegen asm' ts
      | End ->
          let asm' = gen_end asm in
          codegen asm' ts
      | _ -> failwith "not implemented yet")
  | [] -> asm

let assemble file asm =
  let oc = open_out file in
  Printf.fprintf oc "%s\n" (asm ^ tail);
  close_out oc;
  match Sys.command "nasm -f elf64 -o out.o out.s" with
  | 0 -> (
      match Sys.command "ld -o out out.o -lc -e _start -dynamic-linker /lib64/ld-linux-x86-64.so.2" with
      | 0 -> ()
      | _ -> failwith "linking failed")
  | _ -> failwith "assembly failed"

let read_file filename =
  let channel = open_in filename in
  let contents = really_input_string channel (in_channel_length channel) in
  close_in channel;
  contents

let handle_args : string =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Usage: klaus <filename>\n";
    failwith "")
  else
    let filename = Sys.argv.(1) in
    try read_file filename with Sys_error msg -> failwith ("Error: " ^ msg ^ "\n")

let () =
  let program = handle_args in
  tokenizer program |> parser [] |> List.rev |> codegen head |> assemble "out.s"
