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
  | Mod
  | Dup
  | Swap
  | BeginScope
  | EndScope
  | BeginSpace
  | EndSpace
  | If of string
  | Cmp of cmpType
  | Load of string
  | Store of string
  | Loop of string
  | EndLoop of string
  | Break of string
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
    (* Split on spaces AND tabs correctly *)
    let words = String.split_on_char ' ' line in
    let words = List.concat_map (fun word -> String.split_on_char '\t' word) words in
    (* Filter out empty tokens *)
    List.filter (fun s -> String.length s > 0) words
  in
  List.concat_map clean_line lines


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

let is_var (s: char list) = match s with
  | c::n ->
     if c == ':' && allowed_name true n then
       Def((charl_to_string n))
     else if allowed_name true s then
       Use(charl_to_string s)
     else
       Non
  | [] -> failwith "fuck"

(* instructions, variable list, if stack, loop stack, lexer tokens *)
let rec parser ins (vl: string list) is ls lex =
  match lex with
  | [] -> ins, vl, [], 0, 0
  | t :: ts -> (
    match t with
    | "" -> parser ins vl is ls ts
    | "\n" -> parser ins vl is ls ts
    | "\t" -> parser ins vl is ls ts
    | "(" -> parser (BeginSpace::ins) vl is ls ts
    | ")" -> parser (EndSpace::ins) vl is ls ts
    | "{" -> parser (BeginScope::ins) vl is ls ts
    | "}" -> parser (EndScope::ins) vl is ls ts
    | "." -> parser (Pop::ins) vl is ls ts
    | "puts" -> parser (Puts::ins) vl is ls ts
    | "read" -> parser (Read::ins) vl is ls ts
    | "+" -> parser (Add::ins) vl is ls ts
    | "-" -> parser (Sub::ins) vl is ls ts
    | "*" -> parser (Mul::ins) vl is ls ts
    | "/" -> parser (Div::ins) vl is ls ts
    | "%" -> parser (Mod::ins) vl is ls ts
    | "dup" -> parser (Dup::ins) vl is ls ts
    | "swap" -> parser (Swap::ins) vl is ls ts
    | "if" -> let ins, vl, ts, is', ls' =
                parser (If ("e" ^ string_of_int is)::ins) vl (is+1) ls ts in
              parser (End ("e" ^ string_of_int is)::ins) vl is' ls' ts
    (* compares *)
    | "<"  -> parser (Cmp Less  ::ins) vl is ls ts
    | "<=" -> parser (Cmp Leq   ::ins) vl is ls ts
    | ">"  -> parser (Cmp Bigger::ins) vl is ls ts
    | ">=" -> parser (Cmp Beq   ::ins) vl is ls ts
    | "==" -> parser (Cmp Equal ::ins) vl is ls ts
    | "!=" -> parser (Cmp Neq   ::ins) vl is ls ts
    | "loop" -> let ins, vl, ts, is', ls' =
                  parser (Loop ("l" ^ string_of_int ls)::ins) vl is (ls+1) ts in
                parser (EndLoop ("l" ^ string_of_int ls)::ins) vl is' ls' ts
    | "end" -> ins, vl, ts, is, ls
    | "break" -> parser (Break ("el" ^ string_of_int (ls-1))::ins) vl is ls ts
    | "exit" -> parser (Exit::ins) vl is ls ts
    (* Push and variables *)
    | str -> match int_of_string_opt str with
             (*case 1: push new var onto the stack*)
             | Some i -> parser (Push i::ins) vl is ls ts
             (*case 2: new var assignment or var usage*)
             | None -> match is_var (string_to_charl str) with
                       | Def n -> parser (Store n::ins) (n::vl) is ls ts
                       | Use n -> parser (Load n::ins) vl is ls ts
                       | Non -> print_endline str; failwith "illegal instruction"
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
let gen_puts asm = asm ^ "\nmov rax, [rsp]\n" ^ align ^ "\ncall puts\n\n" ^ restore
let gen_read asm = asm ^ align ^ "\ncall get\n" ^ restore ^ "push rax\n"
let gen_add asm = asm ^ "\npop rax\npop rbx\nadd rax, rbx\npush rax\n"
let gen_sub asm = asm ^ "\npop rbx\npop rax\nsub rax, rbx\npush rax\n"
let gen_mul asm = asm ^ "\npop rax\npop rbx\nmul rbx\npush rax\n"
let gen_div asm = asm ^ "\npop rbx\npop rax\ncqo\nidiv rbx\npush rax\n"
let gen_mod asm = asm ^ "\npop rbx\npop rax\ncqo\nidiv rbx\npush rdx\n"
let gen_dup asm = asm ^ "\nmov rax, [rsp]\npush rax\n"
let gen_swap asm = asm ^ "\npop rax\npop rbx\npush rax\npush rbx\n"
let gen_bscope asm = asm ^ "\npush qword [_stack]\nmov qword [_stack], rsp\n"
let gen_escope asm = asm ^ "\nmov rsp, qword [_stack]\npop qword [_stack]\n"
let gen_bspace asm = asm ^ "\npush qword [_stack]\nmov qword [_stack], rsp\n"
let gen_espace asm = asm ^ "\nmov rax, [rsp]\nmov rsp, qword [_stack]\npop qword [_stack]\npush rax\n"
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
  "section .data\n" ^ sdata ^ "\n" ^ head ^ "\n_start:\n"
  
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
      | Mod ->
          let asm' = gen_mod asm in
          codegen asm' ts
      | Dup ->
          let asm' = gen_dup asm in
          codegen asm' ts
      | Swap ->
          let asm' = gen_swap asm in
          codegen asm' ts
      | BeginScope ->
          let asm' = gen_bscope asm in
          codegen asm' ts
      | EndScope ->
          let asm' = gen_escope asm in
          codegen asm' ts
      | BeginSpace ->
          let asm' = gen_bspace asm in
          codegen asm' ts
      | EndSpace ->
          let asm' = gen_espace asm in
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
         let asm'' = gen_label asm' ("e"^label) in
         codegen asm'' ts
      | Break label ->
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

let assemble file asm debug =
  let oc = open_out (file^".s") in
  Printf.fprintf oc "%s\n" (asm ^ tail);
  close_out oc;
  let d = if debug then "-g" else "" in
  match Sys.command ("nasm -f elf64 "^d^" -F dwarf -o "^file^".o "^file^".s") with
  | 0 -> ( match Sys.command ("ld -o "^file^" "^file^".o -e _start") with
           | 0 -> ()
           | _ -> failwith "linking failed"
         )
  | _ -> failwith "assembly failed"

let read_file filename =
  let channel = open_in filename in
  let contents = really_input_string channel (in_channel_length channel) in
  close_in channel;
  contents

let usage_msg = "usage: klaus [-d] <input> -o <output>\n"
let debug = ref false
let input_file = ref ""
let output_file = ref "out"
let anon_fun filename = input_file := filename

let arg_list = [
    ("-d", Arg.Set debug, "generate debug information");
    ("-o", Arg.Set_string output_file, "set output file name");
  ]

let () =
  Arg.parse arg_list anon_fun usage_msg;
  let input = try read_file !input_file with Sys_error msg -> failwith ("error: " ^ msg ^ "\n") in
  let tokens = tokenizer input in
  let parsed, variables, _, _, _ = parser [] [] 0 0 tokens in
  let reversed = List.rev parsed in
  let generated = codegen (vargen variables head) reversed in
  assemble !output_file generated !debug

