type cmpType = Equal | Less | More | Leq | Beq
type mode = Imm of int | Stack

type token =
  | Push of int
  | Pop
  | Puts
  | Read
  | Add
  | Sub
  | Mul
  | Div
  | Dup
  | Swap
  | Cmp of (cmpType * string)
  | Jmp of string
  | Get of mode
  | End
  | Label of string

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
   _start:\n\n"

let tail = "mov rax, 60\nxor rdi, rdi\nsyscall"
let align = "\nmov rbp, rsp\nand rsp, -16 \n"
let restore = "\nmov rsp, rbp\n"

let tokenizer (program : string) : string list =
  let lines = String.trim program |> String.split_on_char '\n' in
  List.filter (fun s -> s <> "" && not (String.starts_with ~prefix:"#" s)) lines
  |> List.map (fun s -> String.split_on_char ' ' s)
  |> List.flatten

let extract_label x =
  if
    String.starts_with ~prefix:":" x
    && String.length x > 1
    && String.fold_left (fun acc c -> if c == ':' then acc + 1 else acc) 0 x == 1
  then Some (String.sub x 1 (String.length x - 1))
  else None

let get_mode ts line : string list * mode =
  match ts with
  | t :: ts -> ( match int_of_string_opt t with Some n -> (ts, Imm n) | None -> (t :: ts, Stack))
  | _ -> failwith ("expected token in line:" ^ string_of_int line)

let rec parser ins line = function
  | [] -> ins
  | t :: ts -> (
      match t with
      | "Push" -> (
          match ts with
          | t :: ts -> parser (Push (int_of_string t) :: ins) (line + 1) ts
          | _ -> failwith ("value expected in line:" ^ string_of_int line))
      | "Pop" -> parser (Pop :: ins) (line + 1) ts
      | "Puts" -> parser (Puts :: ins) (line + 1) ts
      | "Read" -> parser (Read :: ins) (line + 1) ts
      | "Add" -> parser (Add :: ins) (line + 1) ts
      | "Sub" -> parser (Sub :: ins) (line + 1) ts
      | "Mul" -> parser (Mul :: ins) (line + 1) ts
      | "Div" -> parser (Div :: ins) (line + 1) ts
      | "Dup" -> parser (Dup :: ins) (line + 1) ts
      | "Swap" -> parser (Swap :: ins) (line + 1) ts
      | "Cmp" -> (
          match ts with
          | t :: t2 :: ts -> (
              let label =
                match extract_label t2 with Some l -> l | None -> failwith "Not a label in line:" ^ string_of_int line
              in
              match t with
              | "=" -> parser (Cmp (Equal, label) :: ins) (line + 1) ts
              | "<=" -> parser (Cmp (Leq, label) :: ins) (line + 1) ts
              | ">=" -> parser (Cmp (Beq, label) :: ins) (line + 1) ts
              | "<" -> parser (Cmp (Less, label) :: ins) (line + 1) ts
              | ">" -> parser (Cmp (More, label) :: ins) (line + 1) ts
              | _ -> failwith ("not a compare type in line:" ^ string_of_int line))
          | _ -> failwith ("compare type expected in line:" ^ string_of_int line))
      | "Jmp" -> (
          match ts with
          | t :: ts ->
              let label =
                match extract_label t with Some l -> l | None -> failwith "Not a label in line:" ^ string_of_int line
              in
              parser (Jmp label :: ins) (line + 1) ts
          | _ -> failwith "not a label")
      | "Get" ->
          let ts, mode = get_mode ts line in
          parser (Get mode :: ins) (line + 1) ts
      | "End" -> parser (End :: ins) (line + 1) ts
      | "\n" -> parser [] (line + 1) ts
      | x -> (
          match extract_label t with
          | Some label -> parser (Label label :: ins) (line + 1) ts
          | None -> failwith ("Not a valid token in line:" ^ string_of_int line ^ " : " ^ x)))

let get_cmp_ins cmp label =
  (match cmp with
  | Equal -> "je " ^ label
  | Leq -> "jle " ^ label
  | Beq -> "jge " ^ label
  | Less -> "jl " ^ label
  | More -> "jg " ^ label)
  ^ "\n"

let gen_push asm n = asm ^ "\nmov rax, " ^ string_of_int n ^ "\npush rax\n"
let gen_pop asm = asm ^ "\npop rax\nxor rax, rax\n"
let gen_puts asm = asm ^ "\nmov rsi, [rsp]\n" ^ align ^ "\nmov rdi, pformat\nxor rax, rax\ncall printf\n\n" ^ restore

let gen_read asm =
  asm ^ align ^ "\nmov rdi, sformat\nmov rsi, num\nxor rax, rax\ncall scanf\nmov rax, [num]\n" ^ restore
  ^ "push rax\n\n"

let gen_add asm = asm ^ "\nmov rax, [rsp]\nmov rbx, [rsp + 8]\nadd rax, rbx\npush rax\n\n"
let gen_sub asm = asm ^ "\nmov rbx, [rsp]\nmov rax, [rsp + 8]\nsub rax, rbx\npush rax\n\n"
let gen_mul asm = asm ^ "\nmov rax, [rsp]\nmov rbx, [rsp + 8]\nmul rbx\npush rax\n\n"
let gen_div asm = asm ^ "\nmov rax, [rsp]\nmov rbx, [rsp + 8]\ndiv rbx\npush rax\n\n"
let gen_dup asm = asm ^ "\nmov rax, [rsp]\npush rax\n\n"
let gen_swap asm = asm ^ "\npop rax\npop rbx\npush rax\npush rbx\n\n"
let gen_end asm = asm ^ "\n" ^ tail ^ "\n"
let gen_label asm label = asm ^ "\n" ^ label ^ ":\n"
let gen_cmp asm cmp label = asm ^ "\nmov rax, [rsp]\nmov rbx, [rsp + 8]\ncmp rax, rbx\n" ^ get_cmp_ins cmp label
let gen_jmp asm label = asm ^ "\njmp " ^ label ^ "\n"
let gen_get_imm asm index = asm ^ "\nmov rax, [rsp + " ^ string_of_int (index * 8) ^ "]\npush rax\n\n"
let gen_get asm = asm ^ "\nmov rax, [rsp]\nmov rax, [rsp + 8 * rax]\npush rax\n\n"

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
      | Cmp (cmp, label) ->
          let asm' = gen_cmp asm cmp label in
          codegen asm' ts
      | Jmp label ->
          let asm' = gen_jmp asm label in
          codegen asm' ts
      | Get mode ->
          let asm' = match mode with Stack -> gen_get asm | Imm i -> gen_get_imm asm i in
          codegen asm' ts
      | Label label ->
          let asm' = gen_label asm label in
          codegen asm' ts
          (* | _ -> failwith "not implemented yet") *))
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
  tokenizer program |> parser [] 0 |> List.rev |> codegen head |> assemble "out.s"
