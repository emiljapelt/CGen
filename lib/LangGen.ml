
open Absyn

(** Printer **)
let tab_string i = String.init i (fun _ -> '\t')

let rec print_typ t = match t with
| Int -> "int"
| Short -> "short"
| Long -> "long"
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
| E_Null -> "(void*)0"
| E_Short i -> string_of_int i
| E_Int i -> string_of_int i
| E_Long l -> Int64.to_string l ^"l"
| E_Char c -> Printf.sprintf "'%s'" c
| E_Float f -> string_of_float f
| E_Var v -> v
| E_Addr v -> "&"^v
| E_Binary(op,expr1,expr2) -> "(" ^ (print_expression expr1) ^ (print_binary_op op) ^ (print_expression expr2) ^ ")"
| E_Call(n,args) -> n^"("^(String.concat "," (List.map print_expression args))^")"
| E_Deref v -> "*"^v
| E_Index (v,idx) -> "("^v^"["^print_expression idx^"])"
| E_Ternary (expr_c, expr1, expr2) -> "(" ^ (print_expression expr_c) ^ " ? " ^(print_expression expr1)^ " : " ^(print_expression expr2)^ ")"

and print_statement indent s = match s with
| S_Declare(t,ident) -> tab_string indent ^ print_typ t ^ " " ^ ident ^ ";\n"
| S_DeclareAssign(t, ident, expr) -> tab_string indent ^ print_typ t ^ " " ^ ident ^ " = " ^ print_expression expr ^ ";\n"
| S_DeclareArray(t, ident, expr) -> tab_string indent ^ print_typ t ^ " " ^ ident ^ "[" ^ print_expression expr ^ "];\n"
| S_Assign(ident, expr) -> tab_string indent ^ ident ^ " = " ^ print_expression expr ^ ";\n"
| S_ArrayAssign(ident,idx,expr) -> tab_string indent ^ ident ^ "[" ^print_expression idx^ "]" ^ " = " ^ print_expression expr ^ ";\n"
| S_Block([]) -> tab_string indent ^"{}\n"
| S_Block(stmts) -> tab_string indent ^ "{\n" ^ (String.concat "" (List.map (print_statement (indent+1)) stmts)) ^ tab_string indent ^ "}\n"
| S_Call(ident,args) -> tab_string indent ^ ident ^ "("^(String.concat "," (List.map print_expression args))^");\n"
| S_Return(expr) -> tab_string indent ^ "return " ^ print_expression expr ^ ";\n"
| S_BlindReturn -> tab_string indent ^ "return;\n"
| S_If(expr,stmt1,stmt2) -> tab_string indent ^ "if (" ^print_expression expr^ ")\n" ^ (print_statement indent stmt1) ^ tab_string indent ^ "else\n" ^ (print_statement indent stmt2)
| S_While(cond, stmt) -> tab_string indent ^ "while(" ^ print_expression cond ^ ")\n"^ (print_statement indent stmt)
| S_DoWhile(stmt,cond) -> tab_string indent ^ "do\n" ^ (print_statement indent stmt) ^ tab_string indent ^"while (" ^ (print_expression cond) ^ ");\n"
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
  ((typ * bool * (generator_limits -> generators -> expression)) list) * (* Expression generators*)
  ((typ * bool * (generator_limits -> generators -> expression)) list) * (* Call generators *)
  ((bool * (generator_limits -> generators -> (statement*generators))) list) * (* Statement generators*)
  ((bool * (generator_limits -> generators -> (statement*generators))) list) * (* Call generators*)
  ((generator_limits -> generators -> (toplevel*generators)) list) (* Topleved generators *)

and generator_limits =
| GenLimit of 
  int * (* Remaining expression depth *)
  int (* Remaining statement depth *)

let gs_expr_dec (GenLimit(expr_depth,stmt_depth)) =
  GenLimit(expr_depth-1,stmt_depth)

let gs_stmt_dec (GenLimit(expr_depth,stmt_depth)) =
  GenLimit(expr_depth,stmt_depth-1)

let generate_type () = match Random.int 6 with
| 0 -> Short
| 1 -> Int
| 2 -> Long
| 3 -> Char
| 4 -> Float
| 5 -> Void
| _ -> failwith "Unknown type"

let generate_nonvoid () = match Random.int (5*2) with
| 0 -> Short
| 1 -> Int
| 2 -> Long
| 3 -> Char
| 4 -> Float
| 5 -> Ptr Short
| 6 -> Ptr Int
| 7 -> Ptr Long
| 8 -> Ptr Char
| 9 -> Ptr Float
| _ -> failwith "Unknown type"

let rec generate_binary_op () = match Random.int 4 with
| 0 -> PLUS
| 1 -> MINUS
| 2 -> TIMES
| 3 -> DIVIDE
| _ -> failwith "Unknown binary operator"

and generate_expression typ (GenLimit(ed,_) as gl) (Generators(_,expr_gens,call_gens,_,_,_) as gs) = 
  let gens = match () |> Random.bool with
  | true -> call_gens
  | false -> expr_gens
  in
  let gens = if ed <= 0 then List.filter (fun (_,flat,_) -> flat) gens else gens in
  let gens = List.filter (fun (t,_,_) -> if typ = t then true else false) gens in
  let (_,_,f) = List.nth gens (Random.int (List.length gens)) in f (gs_expr_dec gl) gs
  

and generate_statement (GenLimit(_,sd) as gl) (Generators(_,_,_,stmt_gens,scall_gens,_) as gs) = 
  let gens = match () |> Random.bool with
  | true -> stmt_gens
  | false -> scall_gens
  in
  let gens = if sd <= 0 then List.filter (fun (flat,_) -> flat) gens else gens in
  let (_,f) = List.nth gens (Random.int (List.length gens)) in f (gs_stmt_dec gl) gs

and generate_toplevel gl (Generators(_,_,_,_,_,toplevel_gens) as gs) = 
  let f = List.nth toplevel_gens (Random.int (List.length toplevel_gens)) in f gl gs

and generate_compilation_unit toplevels gl gs =
  let rec aux i gs acc = match i with
  | 0 -> (List.rev acc, gs)
  | n -> let (top,gs) = generate_toplevel gl gs in aux (n-1) gs (top::acc)
  in
  aux toplevels gs []