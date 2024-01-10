open Absyn

type 'a mutation_result = 
  | Continue of 'a
  | Stop of 'a

let mutate 
  (mutate_expr: (expression -> expression mutation_result))
  (mutate_stmt: (statement -> statement mutation_result)) 
  (mutate_top: (toplevel -> toplevel mutation_result))
  (prog : toplevel list) = 
  let rec expr_aux expr = match mutate_expr expr with
    | Stop new_expr -> new_expr
    | Continue new_expr -> match expr with
      | E_Null 
      | E_Long _ 
      | E_Int _ 
      | E_Short _ 
      | E_Float _ 
      | E_Char _ 
      | E_Addr _ 
      | E_Deref _ 
      | E_Var _ -> new_expr
      | E_Binary(op, expr1, expr2) -> E_Binary(op, expr_aux expr1, expr_aux expr2)
      | E_Call(str, exprs) -> E_Call(str, List.map expr_aux exprs)
      | E_Index(str,expr) -> E_Index(str,expr_aux expr)
      | E_Ternary(expr_cond, expr1, expr2) -> E_Ternary(expr_aux expr_cond, expr_aux expr1, expr_aux expr2)
  in
  let rec block_aux stmts acc = match stmts with
    | [] -> List.rev acc
    | stmt::t -> block_aux t ((stmt_aux stmt)::acc)
  and stmt_aux stmt = match mutate_stmt stmt with
    | Stop new_stmt -> new_stmt
    | Continue new_stmt -> match new_stmt with
      | S_Declare _ -> stmt
      | S_DeclareArray(t,n,expr) -> S_DeclareArray (t,n,expr_aux expr)
      | S_DeclareAssign(t,n,expr) -> S_DeclareAssign(t,n,expr_aux expr)
      | S_Assign(n,expr) -> S_Assign(n,expr_aux expr)
      | S_ArrayAssign(n,expr1,expr2) -> S_ArrayAssign(n,expr_aux expr1,expr_aux expr2)
      | S_Block(stmts) -> S_Block(block_aux stmts []) 
      | S_Call(n,exprs) -> S_Call(n,List.map expr_aux exprs)
      | S_Return(expr) -> S_Return(expr_aux expr)
      | S_BlindReturn -> stmt
      | S_If(expr,stmt1,stmt2) -> S_If(expr_aux expr,stmt_aux stmt1,stmt_aux stmt2)
      | S_While(expr,stmt) -> S_While(expr_aux expr,stmt_aux stmt)
      | S_DoWhile(stmt,expr) -> S_DoWhile(stmt_aux stmt,expr_aux expr)
      | S_Break -> stmt
      | S_Continue -> stmt
  in
  let rec aux prg acc = match prg with
    | [] -> List.rev acc
    | h::t -> ( match mutate_top h with
      | Stop new_top -> aux t (new_top::acc)
      | Continue new_top -> match new_top with
        | T_Function(ty,n,args,block) -> aux t (T_Function(ty,n,args,block_aux block [])::acc)
        | T_Declare _ -> aux t (new_top::acc)
        | T_DeclareAssign(ty,n,expr) -> aux t (T_DeclareAssign(ty,n,expr_aux expr)::acc) 
    )
  in 
  aux prog []