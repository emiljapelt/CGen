type typ =
| Int
| Short
| Long
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
| E_Long of int64
| E_Int of int
| E_Short of int
| E_Float of float
| E_Char of string
| E_Var of string
| E_Binary of binary_op * expression * expression
| E_Call of string * (expression list)
| E_Addr of string
| E_Deref of string
| E_Index of string * expression
| E_Ternary of expression * expression * expression

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
| S_DoWhile of statement * expression
| S_Break
| S_Continue

and toplevel =
| T_Function of typ * string * (typ * string) list * statement list
| T_Declare of typ * string
| T_DeclareAssign of typ * string * expression


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

let print tops = List.iter (fun top -> Printf.printf "%s" (print_toplevel top)) tops