
(** Absyn **)
type typ =
| Int
| Char
| Float
| Void
| Ptr of typ

and binary_op =
| PLUS
| MINUS
| TIMES
| DIVIDE

and expression =
| E_Null
| E_Int of int
| E_Float of float
| E_Char of string
| E_Var of string
| E_Binary of binary_op * expression * expression
| E_Call of string * (expression list)
| E_Addr of string
| E_Deref of string
| E_Index of string * expression

and statement =
| S_Declare of typ * string
| S_DeclareArray of typ * string * expression
| S_DeclareAssign of typ * string * expression
| S_Assign of string * expression
| S_ArrayAssign of string * expression * expression
| S_Block of statement list
| S_Call of string * (expression list)
| S_Return of expression
| S_BlindReturn
| S_If of expression * statement * statement
| S_While of expression * statement
| S_Break
| S_Continue

and toplevel =
| T_Function of typ * string * (typ * string) list * statement list
| T_Declare of typ * string
| T_DeclareAssign of typ * string * expression

(** Printer **)
let tab_string i = String.init i (fun _ -> '\t')

let rec print_typ t = match t with
| Int -> "int"
| Char -> "char"
| Float -> "float"
| Void -> "void"
| Ptr s -> (print_typ s)^"*"

and print_binary_op b = match b with
| PLUS -> " + "
| MINUS -> " - "
| TIMES -> " * "
| DIVIDE -> " / "

and print_expression e = match e with
| E_Null -> "NULL"
| E_Int i -> string_of_int i
| E_Char c -> Printf.sprintf "'%s'" c
| E_Float f -> string_of_float f
| E_Var v -> v
| E_Addr v -> "&"^v
| E_Binary(op,expr1,expr2) -> "(" ^ (print_expression expr1) ^ (print_binary_op op) ^ (print_expression expr2) ^ ")"
| E_Call(n,args) -> n^"("^(String.concat "," (List.map print_expression args))^")"
| E_Deref v -> "*"^v
| E_Index (v,idx) -> "("^v^"["^print_expression idx^"])"

and print_statement indent s = match s with
| S_Declare(t,ident) -> tab_string indent ^ print_typ t ^ " " ^ ident ^ ";\n"
| S_DeclareAssign(t, ident, expr) -> tab_string indent ^ print_typ t ^ " " ^ ident ^ " = " ^ print_expression expr ^ ";\n"
| S_DeclareArray(t, ident, expr) -> tab_string indent ^ print_typ t ^ " " ^ ident ^ "[" ^ print_expression expr ^ "];\n"
| S_Assign(ident, expr) -> tab_string indent ^ ident ^ " = " ^ print_expression expr ^ ";\n"
| S_ArrayAssign(ident,idx,expr) -> tab_string indent ^ ident ^ "[" ^print_expression idx^ "]" ^ " = " ^ print_expression expr ^ ";\n"
| S_Block(stmts) -> tab_string indent ^ "{\n" ^ (String.concat "" (List.map (print_statement (indent+1)) stmts)) ^ tab_string indent ^ "}\n"
| S_Call(ident,args) -> tab_string indent ^ ident ^ "("^(String.concat "," (List.map print_expression args))^");\n"
| S_Return(expr) -> tab_string indent ^ "return " ^ print_expression expr ^ ";\n"
| S_BlindReturn -> tab_string indent ^ "return;\n"
| S_If(expr,stmt1,stmt2) -> tab_string indent ^ "if (" ^print_expression expr^ ")\n" ^ (print_statement (indent+1) stmt1) ^ tab_string indent ^ "else\n" ^ (print_statement (indent+1) stmt2)
| S_While(cond, stmt) -> tab_string indent ^ "while(" ^ print_expression cond ^ ")\n"^ (print_statement (indent+1) stmt)
| S_Break -> tab_string indent ^ "break;\n"
| S_Continue -> tab_string indent ^ "continue;\n"



and print_toplevel t = match t with
| T_Function(t,ident,params,block) -> print_typ t ^ " " ^ ident ^ "(" ^ (String.concat "," (List.map (fun (typ,param)-> (print_typ typ) ^" "^ param) params)) ^ ") " ^ "{\n" ^ (String.concat "" (List.map (print_statement 1) block)) ^ "}\n\n"
| T_Declare(t,ident) -> print_typ t ^ " " ^ ident ^ ";\n"
| T_DeclareAssign(t,ident,expr) -> print_typ t ^ " " ^ ident ^ " = " ^ print_expression expr ^ ";\n"

(** Generator **)
type generators =
| Generators of 
  int * (* Counter *)
  (string*typ) list * (* Variable identifiers *)
  (string*typ*(typ list)) list * (* Function identifiers *)
  ((typ * bool * (generator_limits -> generators -> expression)) list) * (* Expression generators*)
  ((bool * (generator_limits -> generators -> (statement*generators))) list) * (* Statement generators*)
  ((generator_limits -> generators -> (toplevel*generators)) list) (* Topleved generators *)

and generator_limits =
| GenLimit of 
  int * (* Remaining expression depth *)
  int (* Remaining statement depth *)

let gs_expr_dec (GenLimit(expr_depth,stmt_depth)) =
  GenLimit(expr_depth-1,stmt_depth)

let gs_stmt_dec (GenLimit(expr_depth,stmt_depth)) =
  GenLimit(expr_depth,stmt_depth-1)

let generate_type () = match Random.int 4 with
| 0 -> Int
| 1 -> Char
| 2 -> Float
| 3 -> Void
| _ -> failwith "Unknown type"

let generate_nonvoid () = match Random.int (3*2) with
| 0 -> Int
| 1 -> Char
| 2 -> Float
| 3 -> Ptr Int
| 4 -> Ptr Char
| 5 -> Ptr Float
| _ -> failwith "Unknown type"

let rec generate_binary_op () = match Random.int 4 with
| 0 -> PLUS
| 1 -> MINUS
| 2 -> TIMES
| 3 -> DIVIDE
| _ -> failwith "Unknown binary operator"

and generate_expression typ (GenLimit(ed,_) as gl) (Generators(_,_,_,expr_gens,_,_) as gs) = 
  let expr_gens = if ed <= 0 then List.filter (fun (_,flat,_) -> flat) expr_gens else expr_gens in
  let expr_gens = List.filter (fun (t,_,_) -> if typ = t then true else false) expr_gens in
  let (_,_,f) = List.nth expr_gens (Random.int (List.length expr_gens)) in f (gs_expr_dec gl) gs

and generate_statement (GenLimit(_,sd) as gl) (Generators(_,_,_,_,stmt_gens,_) as gs) = 
  let stmt_gens = if sd <= 0 then List.filter (fun (flat,_) -> flat) stmt_gens else stmt_gens in
  let (_,f) = List.nth stmt_gens (Random.int (List.length stmt_gens)) in f (gs_stmt_dec gl) gs

and generate_toplevel gl (Generators(_,_,_,_,_,toplevel_gens) as gs) = 
  let f = List.nth toplevel_gens (Random.int (List.length toplevel_gens)) in f gl gs

and generate_compilation_unit min range gl gs =
  let rec aux i gs acc = match i with
  | 0 -> (List.rev acc, gs)
  | n -> let (top,gs) = generate_toplevel gl gs in aux (n-1) gs (top::acc)
  in
  aux ((Random.int range) + min) gs []