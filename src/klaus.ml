type cmpType = Equal | Neq | Less | Bigger | Leq | Beq
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
  | If of string
  | Cmp of cmpType
  | Load of string
  | Store of string
  | Loop of string
  | EndLoop of string
  | End of string
  | Exit 

(*ASM Header*)
let head = "ASMHEADER"

let tail = "\nand rsp, qword [_stack]\nmov rax, 60\nxor rdi, rdi\nsyscall"
let align = "\nmov rbp, rsp\nand rsp, -16 \n"
let restore = "\nmov rsp, rbp\n"

let remove_comment s =
  try
    let index = String.index s '#' in
    String.sub s 0 index
  with Not_found -> s

let tokenizer (program : string) : string list =
  let lines = String.split_on_char '\n' program in
  let clean_line line =
    let line = remove_comment line in
    String.split_on_char ' ' line
    |> List.filter (fun s -> not (String.exists (fun c -> c = ' ' || c = '\t' || c = '\n') s))
  in
  List.concat (List.map clean_line lines)

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


type tvar =
  | Def of string
  | Use of string
  | Non

let rec allowed_name (first: bool) = function
  | [] -> true
  | c :: s -> if 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' ||
                   c == '_' || (not first && '0' <= c && c <= '9') then
                allowed_name false s
              else
                false

let charl_to_string chars = (String.of_seq (List.to_seq chars))
let string_to_charl string = (List.of_seq (String.to_seq string))

let is_var s = match s with
  | c::n ->
     if c == ':' && allowed_name true n then
       Def((charl_to_string n))
     else if allowed_name true s then
       Use(charl_to_string s)
     else
       Non
  | [] -> failwith "fuck"

type contextstack = Loop of int | LoopIf of int | If of int

let get_if_context = function
  | [] -> If(0)
  | c::cs -> match c with
             | Loop c -> LoopIf(c)
             | LoopIf c -> If(c+1)
             | If c -> If(c+1)

let rec parser ins (vl: string list) cs lex =
  match lex with
  | [] -> ins, vl, []
  | t :: ts -> (
    match t with
    | "" -> parser ins vl cs ts
    | "\n" -> parser ins vl cs ts
    | "\t" -> parser ins vl cs ts
    | "{" -> parser ins vl cs ts (* add scoping for this in the future *)
    | "}" -> parser ins vl cs ts
    | "." -> parser (Pop :: ins) vl cs ts
    | "puts" -> parser (Puts :: ins) vl cs ts
    | "read" -> parser (Read :: ins) vl cs ts
    | "+" -> parser (Add :: ins) vl cs ts
    | "-" -> parser (Sub :: ins) vl cs ts
    | "*" -> parser (Mul :: ins) vl cs ts
    | "/" -> parser (Div :: ins) vl cs ts
    | "dup" -> parser (Dup :: ins) vl cs ts
    | "swap" -> parser (Swap :: ins) vl cs ts
    | "if" -> let con(n) = get_if_context cs in
              let ins, vl, ts = parser (If ("end" ^ string_of_int c) :: ins) vl (con::cs) ts in
              match con with
              | LoopIf -> ins, vl, ts
              | _ -> parser (End ("end" ^ string_of_int n) :: ins) vl cs ts
    (* compares *)
    | "<"  -> parser (Cmp Less   :: ins) vl cs ts
    | "<=" -> parser (Cmp Leq    :: ins) vl cs ts
    | ">"  -> parser (Cmp Bigger :: ins) vl cs ts
    | ">=" -> parser (Cmp Beq    :: ins) vl cs ts
    | "==" -> parser (Cmp Equal  :: ins) vl cs ts
    | "!=" -> parser (Cmp Neq    :: ins) vl cs ts
    | "loop" -> let ins, vl, ts = parser (Loop ("loop" ^ string_of_int c) :: ins) vl (Loop(c)::cs) ts in
                parser (EndLoop ("loop" ^ string_of_int c) :: ins) vl cs ts
    | "end" -> ins, vl, ts
    | "exit" -> parser (Exit :: ins) vl cs ts
    (* Push and variables *)
    | str -> match int_of_string_opt str with
             (*case 1: push new var onto the stack*)
             | Some i -> parser (Push i :: ins) vl cs ts
             (*case 2: new var assignment or var usage*)
             | None -> match is_var (string_to_charl str) with
                       | Def n -> parser (Store n :: ins) (n::vl) cs ts
                       | Use n -> parser (Load n :: ins) vl cs ts
                       | Non -> print_endline str; failwith "illegal instruction"
  (*todo next: if/loop/break implementation and logic*)
  )

let get_set_ins cmp =
  (match cmp with
   | Neq    -> "setne al"
   | Equal  -> "sete al"
   | Leq    -> "setle al"
   | Beq    -> "setge al"
   | Less   -> "setl al"
   | Bigger -> "setg al")
  ^ "\nmovzx rax, al\n"

let gen_push asm n = asm ^ "\nmov rax, " ^ string_of_int n ^ "\npush rax\n"
let gen_pop asm = asm ^ "\npop rax\nxor rax, rax\n"
let gen_puts asm = asm ^ "\nmov rax, [rsp]\n" ^ align ^ "\ncall print_int\n\n" ^ restore
let gen_read asm = asm ^ align ^ "\ncall get\n" ^ restore ^ "push rax\n"
let gen_add asm = asm ^ "\npop rax\npop rbx\nadd rax, rbx\npush rax\n"
let gen_sub asm = asm ^ "\npop rbx\npop rax\nsub rax, rbx\npush rax\n"
let gen_mul asm = asm ^ "\npop rax\npop rbx\nmul rbx\npush rax\n"
let gen_div asm = asm ^ "\npop rax\npop rbx\ndiv rbx\npush rax\n"
let gen_dup asm = asm ^ "\nmov rax, [rsp]\npush rax\n"
let gen_swap asm = asm ^ "\npop rax\npop rbx\npush rax\npush rbx\n"
let gen_label asm label = asm ^ "\n" ^ label ^ ":\n"
let gen_cmp asm cmp = asm ^ "\nmov rbx, [rsp]\nmov rax, [rsp + 8]\ncmp rax, rbx\n" ^
                              (get_set_ins cmp) ^ "push rax\n"
let gen_jz asm label = asm ^ "\npop rax\ntest rax, rax\njz " ^ label ^ "\n"
let gen_jmp asm label = asm ^ "\njmp " ^ label ^ "\n"
let gen_load asm name = asm ^ "\nmov rax, qword [" ^ name ^ "]\npush rax\n"
let gen_store asm name = asm ^ "\nmov rax, [rsp]\nmov qword [" ^ name ^ "], rax\n"
let gen_exit asm = asm ^ "\n" ^ tail ^ "\n"

let vargen vl asm =
  let f = (fun acc k -> acc ^ k ^ " dq 0\n") in
  let sdata = List.sort_uniq compare vl |> List.fold_left f "" in
  "section .data\n" ^ sdata ^ "\n" ^ head ^ "\n_start:\n" (* "\nmov qword [_stack], rsp\n" *)
  
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
      | If label ->
          let asm' = gen_jz asm label in
          codegen asm' ts
      | Cmp cmp ->
          let asm' = gen_cmp asm cmp in
          codegen asm' ts
      | Load name ->
         let asm' = gen_load asm name in
         codegen asm' ts
      | Store name ->
         let asm' = gen_store asm name in
         codegen asm' ts
      | Loop label ->
         let asm' = gen_label asm label in
         codegen asm' ts
      | EndLoop label ->
         let asm' = gen_jmp asm label in
         codegen asm' ts
      | End label ->
         let asm' = gen_label asm label in
         codegen asm' ts
      | Exit ->
          let asm' = gen_exit asm in
          codegen asm' ts)
      (* | _ -> failwith "not implemented yet") *)
  | [] -> asm

let assemble file asm =
  let oc = open_out file in
  Printf.fprintf oc "%s\n" (asm ^ tail);
  close_out oc;
  match Sys.command "nasm -f elf64 -g -F dwarf -o out.o out.s" with
  | 0 -> ( match Sys.command "ld -o out out.o -e _start" with 0 -> () | _ -> failwith "linking failed")
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
    let tokens = tokenizer program in
    let parsed, variables, _ = parser [] [] 0 tokens in
    let reversed = List.rev parsed in
    let generated = codegen (vargen variables head) reversed in
    assemble "out.s" generated

