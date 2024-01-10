open Absyn
open Generators

let pos_int_gen n = Random.int n
let int_gen n = (Random.int n*2) - (n/2)
(*let large_pos_int_gen n = Int.max_int - (Random.int n)*)
let large_int_gen n = match Random.bool () with
  | true -> -(Int.max_int - (Random.int n))
  | false -> (Int.max_int - (Random.int n))

let long_gen n = Int64.sub (Random.int64 (Int64.mul n 2L))  (Int64.div n 2L)
let large_long_gen n = match Random.bool () with
  | true -> Int64.neg(Int64.sub Int64.max_int (Random.int64 n))
  | false -> (Int64.sub Int64.max_int (Random.int64 n))

(*let pos_float_gen n = Random.float n*)
let float_gen n = Float.sub (Random.float (Float.mul n 2.0)) (Float.div n 2.0)
let large_float_gen n = match Random.bool () with
  | true -> Float.neg(Float.sub Float.max_float (Random.float n))
  | false -> (Float.sub Float.max_float (Random.float n))

let expr_gens = [
  (Short, true, fun _ _ -> E_Short(int_gen 100));
  (Short, false, (fun gl gs -> E_Binary(generate_binary_op (), generate_expression Short (gs_expr_dec gl) gs, generate_expression Short (gs_expr_dec gl) gs)));
  (Short, false, (fun gl gs -> E_Ternary(generate_expression Int (gs_expr_dec gl) gs, generate_expression Short (gs_expr_dec gl) gs, generate_expression Short (gs_expr_dec gl) gs)));
  (Int, true, fun _ _ -> E_Int(int_gen 100));
  (Int, true, fun _ _ -> E_Int(large_int_gen 1000000));
  (Int, false, (fun gl gs -> E_Binary(generate_binary_op (), generate_expression Int (gs_expr_dec gl) gs, generate_expression Int (gs_expr_dec gl) gs)));
  (Int, false, (fun gl gs -> E_Ternary(generate_expression Int (gs_expr_dec gl) gs, generate_expression Int (gs_expr_dec gl) gs, generate_expression Int (gs_expr_dec gl) gs)));
  (Long, true, fun _ _ -> E_Long(long_gen 10000L)); 
  (Long, true, fun _ _ -> E_Long(large_long_gen 10000000L)); 
  (Long, false, (fun gl gs -> E_Binary(generate_binary_op (), generate_expression Long (gs_expr_dec gl) gs, generate_expression Long (gs_expr_dec gl) gs)));
  (Long, false, (fun gl gs -> E_Ternary(generate_expression Int (gs_expr_dec gl) gs, generate_expression Long (gs_expr_dec gl) gs, generate_expression Long (gs_expr_dec gl) gs)));
  (Char, true, (fun _ _ -> E_Char(Random.int 128 |> Char.chr |> Char.escaped)));
  (Char, false, (fun gl gs -> E_Ternary(generate_expression Int (gs_expr_dec gl) gs, generate_expression Char (gs_expr_dec gl) gs, generate_expression Char (gs_expr_dec gl) gs)));
  (Float, true, (fun _ _ -> E_Float(float_gen 100.0)));
  (Float, true, (fun _ _ -> E_Float(large_float_gen 10000.0)));
  (Float, false, (fun gl gs -> E_Binary(generate_binary_op (), generate_expression Float (gs_expr_dec gl) gs, generate_expression Float (gs_expr_dec gl) gs)));
  (Float, false, (fun gl gs -> E_Ternary(generate_expression Int (gs_expr_dec gl) gs, generate_expression Float (gs_expr_dec gl) gs, generate_expression Float (gs_expr_dec gl) gs)));
  (Ptr Short, true, (fun _ _ -> E_Null));
  (Ptr Short, false, (fun gl gs -> E_Ternary(generate_expression Int (gs_expr_dec gl) gs, generate_expression (Ptr Short) (gs_expr_dec gl) gs, generate_expression (Ptr Short) (gs_expr_dec gl) gs)));
  (Ptr Int, true, (fun _ _ -> E_Null));
  (Ptr Int, false, (fun gl gs -> E_Ternary(generate_expression Int (gs_expr_dec gl) gs, generate_expression (Ptr Int) (gs_expr_dec gl) gs, generate_expression (Ptr Int) (gs_expr_dec gl) gs)));
  (Ptr Long, true, (fun _ _ -> E_Null));
  (Ptr Long, false, (fun gl gs -> E_Ternary(generate_expression Int (gs_expr_dec gl) gs, generate_expression (Ptr Long) (gs_expr_dec gl) gs, generate_expression (Ptr Long) (gs_expr_dec gl) gs)));
  (Ptr Char, true, (fun _ _ -> E_Null));
  (Ptr Char, false, (fun gl gs -> E_Ternary(generate_expression Int (gs_expr_dec gl) gs, generate_expression (Ptr Char) (gs_expr_dec gl) gs, generate_expression (Ptr Char) (gs_expr_dec gl) gs)));
  (Ptr Float, true, (fun _ _ -> E_Null));
  (Ptr Float, false, (fun gl gs -> E_Ternary(generate_expression Int (gs_expr_dec gl) gs, generate_expression (Ptr Float) (gs_expr_dec gl) gs, generate_expression (Ptr Float) (gs_expr_dec gl) gs)));
]

let dec_expr_gens typ ident gens = match typ with
| Ptr s -> (Ptr typ,true,(fun _ _ -> E_Addr ident))::(typ,true,(fun _ _ -> E_Var ident))::(s,true,(fun _ _ -> E_Deref ident))::gens
| _  -> (Ptr typ,true,(fun _ _ -> E_Addr ident))::(typ,true,(fun _ _ -> E_Var ident))::gens

let dec_stmt_gens typ ident gens = match typ with
| Ptr s -> (true, fun gl gs -> (S_DerefAssign(ident,generate_expression s gl gs),gs))::(true, fun gl gs -> (S_Assign(ident,generate_expression typ gl gs),gs))::gens
| _ -> (true, fun gl gs -> (S_Assign(ident,generate_expression typ gl gs),gs))::gens

let dec_gen _ (Generators(counter,type_set,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens)) =
  let ident = "var_"^(string_of_int counter) in
  let typ = generate_nonvoid () in
  let new_type_set = type_set |> TypeSet.add typ |> TypeSet.add (Ptr typ) in
  (S_Declare(typ, ident), Generators(counter+1,new_type_set,(dec_expr_gens typ ident expr_gens),call_gens,dec_stmt_gens typ ident stmt_gens,scall_gens,toplevel_gens))

let dec_arr_gen _ (Generators(counter,type_set,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens)) =
  let ident = "var_"^(string_of_int counter) in
  let typ = generate_nonvoid () in
  let arr_typ = Ptr typ in
  let size = (Random.int 100) + 1 in
  let new_type_set = type_set |> TypeSet.add typ |> TypeSet.add arr_typ |> TypeSet.add (Ptr arr_typ) in
  (S_DeclareArray(typ,ident,E_Int size), (Generators(counter+1,new_type_set,(Ptr arr_typ,true,(fun _ _ -> E_Addr ident))::(arr_typ,true,(fun _ _ -> E_Var ident))::(typ,true,(fun _ _ -> E_Index(ident, E_Int(pos_int_gen size))))::expr_gens,call_gens, (true, fun gl gs -> (S_ArrayAssign(ident,E_Int(pos_int_gen size),generate_expression typ gl gs),gs))::stmt_gens,scall_gens,toplevel_gens)) )

let dec_ass_gen gl (Generators(counter,type_set,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens) as gs) =
  let ident = "var_"^(string_of_int counter) in
  let typ = generate_nonvoid () in
  let new_type_set = type_set |> TypeSet.add typ |> TypeSet.add (Ptr typ) in
  (S_DeclareAssign(typ, ident, generate_expression typ gl gs), Generators(counter+1,new_type_set,(dec_expr_gens typ ident expr_gens),call_gens, dec_stmt_gens typ ident stmt_gens,scall_gens,toplevel_gens))

let block_gen min range gl gs =
  let (stmts,_) = generate_stmt_list min range gl gs in
  (S_Block(stmts), gs)

let if_gen gl gs =
  let (stmt1,_) = block_gen 0 4 gl gs in
  let (stmt2,_) = block_gen 0 4 gl gs in
  (S_If(generate_expression Int gl gs, stmt1, stmt2), gs)

let while_gen gl (Generators(counter,type_set,expr_gens,call_gens,stmt_gens,scall_gens,top_gens) as gs) =
  let cond = generate_expression Int gl gs in
  let (stmt,_) = block_gen 1 4 gl (Generators(counter,type_set,expr_gens,call_gens,(true,fun _ gs -> (S_Break,gs))::(true,fun _ gs -> (S_Continue,gs))::stmt_gens,scall_gens,top_gens)) in
  match () |> Random.bool with
  | true -> (S_DoWhile(stmt,cond), gs)
  | false -> (S_While(cond,stmt), gs)

let stmt_gens = [
  (true, dec_gen);
  (true, dec_ass_gen);
  (true, dec_arr_gen);
  (false, block_gen 2 5);
  (false, if_gen);
  (false, while_gen)
]


let top_dec_gen _ (Generators(counter,type_set,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens)) =
  let ident = "var_"^(string_of_int counter) in
  let typ = generate_nonvoid () in
  let new_type_set = type_set |> TypeSet.add typ |> TypeSet.add (Ptr typ) in
  (T_Declare(typ, ident), Generators(counter+1,new_type_set,dec_expr_gens typ ident expr_gens,call_gens,dec_stmt_gens typ ident stmt_gens,scall_gens,toplevel_gens))

let top_dec_ass_gen gl (Generators(counter,type_set,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens) as gs) =
  let ident = "var_"^(string_of_int counter) in
  let typ = generate_nonvoid () in
  let expr = generate_expression typ gl gs in
  let new_type_set = type_set |> TypeSet.add typ |> TypeSet.add (Ptr typ) in
  (T_DeclareAssign(typ, ident, expr), Generators(counter+1,new_type_set,dec_expr_gens typ ident expr_gens,call_gens,dec_stmt_gens typ ident stmt_gens,scall_gens,toplevel_gens))

let top_dec_arr_gen _ (Generators(counter,type_set,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens)) =
  let ident = "var_"^(string_of_int counter) in
  let typ = generate_nonvoid () in
  let arr_typ = Ptr typ in
  let size = (Random.int 100) + 1 in
  let new_type_set = type_set |> TypeSet.add typ |> TypeSet.add arr_typ |> TypeSet.add (Ptr arr_typ) in
  (T_DeclareArray(typ,ident,E_Int size), (Generators(counter+1,new_type_set,(Ptr arr_typ,true,(fun _ _ -> E_Addr ident))::(arr_typ,true,(fun _ _ -> E_Var ident))::(typ,true,(fun _ _ -> E_Index(ident, E_Int(pos_int_gen size))))::expr_gens,call_gens, (true, fun gl gs -> (S_ArrayAssign(ident,E_Int(pos_int_gen size),generate_expression typ gl gs),gs))::stmt_gens,scall_gens,toplevel_gens)) )
  

let generate_params _ gs =
  let rec aux i (Generators(counter,type_set,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens) as gs) acc = match i with
    | 0 -> (acc,gs)
    | n -> (
      let typ = generate_nonvoid () in
      let ident = "param_"^(string_of_int counter) in
      aux (n-1) (Generators(counter+1,type_set,(dec_expr_gens typ ident expr_gens),call_gens,stmt_gens,scall_gens,toplevel_gens)) ((typ,ident)::acc)
    )
  in
  aux (Random.int 5) gs []

let function_gen gl (Generators(counter,_,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens) as gs) =
  let ident = "func_"^(string_of_int counter) in
  let typ = generate_type () in 
  let (params, Generators(counter,type_set,fun_expr_gens,fun_call_gens,_,_,_)) = generate_params gl gs in
  let param_types = List.map fst params |> TypeSet.of_list in
  let fun_type_set = TypeSet.union param_types type_set in
  let (fun_stmt_gens) = match typ with
  | Void -> (true,fun _ gs -> (S_BlindReturn,gs))::stmt_gens
  | Short -> (true,fun gl gs -> (S_Return(generate_expression Short gl gs),gs))::stmt_gens
  | Int -> (true,fun gl gs -> (S_Return(generate_expression Int gl gs),gs))::stmt_gens
  | Long -> (true,fun gl gs -> (S_Return(generate_expression Long gl gs),gs))::stmt_gens
  | Char -> (true,fun gl gs -> (S_Return(generate_expression Char gl gs),gs))::stmt_gens
  | Float -> (true,fun gl gs -> (S_Return(generate_expression Float gl gs),gs))::stmt_gens
  | _ -> failwith "Nope"
  in
  let (stmts,Generators(counter,_,_,_,_,_,_)) = generate_stmt_list 2 10 gl (Generators(counter+1,fun_type_set,fun_expr_gens,fun_call_gens,fun_stmt_gens,scall_gens,toplevel_gens)) in
  let new_call_gens = if typ = Void then call_gens else (typ,param_types,(fun gl gs -> E_Call(ident, List.map (fun (t,_) -> generate_expression t gl gs) params)))::call_gens in
  (T_Function(typ, ident, params, stmts), Generators(counter+1,type_set,expr_gens,new_call_gens,stmt_gens,(param_types,fun gl gs -> S_Call(ident, List.map (fun (t,_) -> generate_expression t gl gs) params),gs)::scall_gens, toplevel_gens))

let toplevel_gens = [
  top_dec_gen;
  top_dec_ass_gen;
  top_dec_arr_gen;
  function_gen;
]

