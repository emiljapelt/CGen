
open LangGen

let int_gen n = fun _ _ -> E_Int(Random.int n)

let expr_gens = [
  (Int, true, int_gen 100);
  (Int, false, (fun gl gs -> E_Binary(generate_binary_op (), generate_expression Int gl gs, generate_expression Int gl gs)));
  (Char, true, (fun _ _ -> E_Char(Random.int 128 |> Char.chr |> Char.escaped)));
  (Float, true, (fun _ _ -> E_Float(Random.float 100.0)));
  (Float, false, (fun gl gs -> E_Binary(generate_binary_op (), generate_expression Float gl gs, generate_expression Float gl gs)));
  (Ptr Int, true, (fun _ _ -> E_Null));
  (Ptr Char, true, (fun _ _ -> E_Null));
  (Ptr Float, true, (fun _ _ -> E_Null));
]

let dec_expr_gens typ ident gens = match typ with
| Ptr s -> (Ptr typ,true,(fun _ _ -> E_Addr ident))::(typ,true,(fun _ _ -> E_Var ident))::(s,true,(fun _ _ -> E_Deref ident))::gens
| _  -> (Ptr typ,true,(fun _ _ -> E_Addr ident))::(typ,true,(fun _ _ -> E_Var ident))::gens


let dec_gen _ (Generators(counter,expr_gens,stmt_gens,toplevel_gens)) =
  let ident = "var_"^(string_of_int counter) in
  let typ = generate_nonvoid () in
  (S_Declare(typ, ident), Generators(counter+1,(dec_expr_gens typ ident expr_gens),(true, fun gl gs -> (S_Assign(ident,generate_expression typ gl gs),gs))::stmt_gens,toplevel_gens))

let dec_arr_gen _ (Generators(counter,expr_gens,stmt_gens,toplevel_gens)) =
  let ident = "var_"^(string_of_int counter) in
  let typ = generate_nonvoid () in
  let arr_typ = Ptr typ in
  let size = (Random.int 100) + 1 in
  (S_DeclareArray(typ,ident,E_Int size), (Generators(counter+1,(Ptr arr_typ,true,(fun _ _ -> E_Addr ident))::(arr_typ,true,(fun _ _ -> E_Var ident))::(typ,true,(fun gl gs -> E_Index(ident, int_gen size gl gs)))::expr_gens, (true, fun gl gs -> (S_ArrayAssign(ident,int_gen size gl gs,generate_expression typ gl gs),gs))::stmt_gens,toplevel_gens)) )

let dec_ass_gen gl (Generators(counter,expr_gens,stmt_gens,toplevel_gens) as gs) =
  let ident = "var_"^(string_of_int counter) in
  let typ = generate_nonvoid () in
  (S_DeclareAssign(typ, ident, generate_expression typ gl gs), Generators(counter+1,(dec_expr_gens typ ident expr_gens),(true,fun gl gs -> (S_Assign(ident,generate_expression typ gl gs),gs))::stmt_gens,toplevel_gens))

let gen_stmt_list min range gl gs =
  let rec aux i gs acc = match i with
    | 0 -> List.rev acc
    | n -> let (stmt,gs) = generate_statement gl gs in aux (n-1) gs (stmt::acc)
  in
  (aux ((Random.int range)+min) gs [], gs)

let block_gen min range gl gs =
  let (stmts,_) = gen_stmt_list min range gl gs in
  (S_Block(stmts), gs)

let if_gen gl gs =
  let (stmt1,_) = block_gen 0 4 gl gs in
  let (stmt2,_) = block_gen 0 4 gl gs in
  (S_If(generate_expression Int gl gs, stmt1, stmt2), gs)

let while_gen gl (Generators(counter,expr_gens,stmt_gens,top_gens) as gs) =
  let cond = generate_expression Int gl gs in
  let (stmt,_) = block_gen 1 4 gl (Generators(counter+1,expr_gens,(true,fun _ _ -> (S_Break,gs))::(true,fun _ _ -> (S_Continue,gs))::stmt_gens,top_gens)) in
  (S_While(cond,stmt), gs)

let stmt_gens = [
  (true, dec_gen);
  (true, dec_ass_gen);
  (true, dec_arr_gen);
  (false, block_gen 2 5);
  (false, if_gen);
  (true, while_gen)
]


let top_dec_gen _ (Generators(counter,expr_gens,stmt_gens,toplevel_gens)) =
  let ident = "var_"^(string_of_int counter) in
  let typ = generate_nonvoid () in
  (T_Declare(typ, ident), Generators(counter+1,(typ,true,(fun _ _ -> E_Var ident))::expr_gens,(true,fun gl gs -> (S_Assign(ident,generate_expression typ gl gs),gs))::stmt_gens,toplevel_gens))

let generate_params _ gs =
  let rec aux i (Generators(counter,expr_gens,stmt_gens,toplevel_gens) as gs) acc = match i with
    | 0 -> (acc,gs)
    | n -> (
      let typ = generate_nonvoid () in
      let ident = "param_"^(string_of_int counter) in
      aux (n-1) (Generators(counter+1,(dec_expr_gens typ ident expr_gens),stmt_gens,toplevel_gens)) ((typ,ident)::acc)
    )
  in
  aux (Random.int 5) gs []

let function_gen gl (Generators(counter,expr_gens,stmt_gens,toplevel_gens) as gs) =
  let ident = "func_"^(string_of_int counter) in
  let typ = generate_type () in 
  let (params, Generators(counter,fun_expr_gens,_,_)) = generate_params gl gs in
  let (fun_stmt_gens) = match typ with
  | Void -> (true,fun _ gs -> (S_BlindReturn,gs))::stmt_gens
  | Int -> (true,fun gl gs -> (S_Return(generate_expression Int gl gs),gs))::stmt_gens
  | Char -> (true,fun gl gs -> (S_Return(generate_expression Char gl gs),gs))::stmt_gens
  | Float -> (true,fun gl gs -> (S_Return(generate_expression Float gl gs),gs))::stmt_gens
  | _ -> failwith "Nope"
  in
  let (stmts,Generators(counter,_,_,_)) = gen_stmt_list 2 5 gl (Generators(counter+1,fun_expr_gens,fun_stmt_gens,toplevel_gens)) in
  (T_Function(typ, ident, params, stmts), Generators(counter+1,(typ,false,(fun gl gs -> E_Call(ident, List.map (fun (t,_) -> generate_expression t gl gs) params)))::expr_gens, (true,fun gl gs -> S_Call(ident, List.map (fun (t,_) -> generate_expression t gl gs) params),gs)::stmt_gens, toplevel_gens))

let toplevel_gens = [
  top_dec_gen;
  function_gen;
]



let seed = int_of_string (Sys.argv.(1))
let () = if seed = 0 then Random.self_init () else Random.init seed
let gs = (Generators(0,expr_gens,stmt_gens,toplevel_gens))
let gl = (GenLimit(int_of_string (Sys.argv.(2)),int_of_string (Sys.argv.(3)))) 
let tops = int_of_string (Sys.argv.(4))

let (tops, gs) = generate_compilation_unit 0 tops gl gs
let (main_block, _) = gen_stmt_list 4 5 gl gs
let tops = tops @ [T_Function(Void, "main", [], main_block)]

let () = (*Printf.printf "#include <stddef.h>\n" ;*) List.iter (fun top -> Printf.printf "%s" (print_toplevel top)) tops