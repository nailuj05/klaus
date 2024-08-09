type cmpType = | Equal | Less | More | Leq | Meq
type token = | Push of int | Pop | Puts | Read | Add | Sub | Mul | Div | Cmp of cmpType

let program = 
"
Push 5
Push 10
Add
Push 16
Mul
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
    | "Puts" -> parser (Pop::ins) ts
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

let rec codegen asm = function 
  | t::ts -> (match t with
    | Push n -> failwith "Push"
    | Pop -> failwith "Pop"
    | Puts -> failwith "Puts"
    | _ -> failwith "not implemented yet")
  | [] -> asm

let () = tokenizer program |> parser [] |> codegen ""; ()

