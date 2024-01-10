
open Absyn

module TypeSet = Set.Make (struct
  type t = typ

  let compare = type_equal
end)

(** Generator **)
type generators =
| Generators of 
  int * (* Counter *)
  TypeSet.t *
  ((typ * bool * (generator_limits -> generators -> expression)) list) * (* Expression generators*)
  ((typ * TypeSet.t * (generator_limits -> generators -> expression)) list) * (* Call generators *)
  ((bool * (generator_limits -> generators -> (statement*generators))) list) * (* Statement generators*)
  ((TypeSet.t * (generator_limits -> generators -> (statement*generators))) list) * (* Call generators*)
  ((generator_limits -> generators -> (toplevel*generators)) list) (* Toplevel generators *)

and generator_limits =
| GenLimit of 
  int * (* Remaining expression depth *)
  int (* Remaining statement depth *)

let generate_generators expr_gens stmt_gens top_gens =
  let type_set = List.fold_left (fun set (t,_,_) -> TypeSet.add t set) TypeSet.empty expr_gens in
  Generators(0,type_set,expr_gens,[],stmt_gens,[],top_gens)

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

and generate_expression typ (GenLimit(ed,_) as gl) (Generators(_,type_set,expr_gens,call_gens,_,_,_) as gs) = 
  let generate gens = 
    let (_,_,f) = List.nth gens (Random.int (List.length gens)) in f (gs_expr_dec gl) gs
  in
  let possible_exprs () = (
    let gens = if ed <= 0 then List.filter (fun (_,flat,_) -> flat) expr_gens else expr_gens in
    List.filter (fun (t,_,_) -> if typ = t then true else false) gens
  )
  in
  let possible_calls () =
    if ed <= 0 then [] else List.filter (fun (t,param_types,_) -> t = typ && TypeSet.subset param_types type_set) call_gens
  in
  match () |> Random.bool with
  | true -> generate (possible_exprs ())
  | false -> let call_gens = possible_calls () in if call_gens = [] then generate (possible_exprs ()) else generate call_gens

and generate_statement (GenLimit(_,sd) as gl) (Generators(_,type_set,_,_,stmt_gens,scall_gens,_) as gs) = 
  let generate gens = 
    let (_,f) = List.nth gens (Random.int (List.length gens)) in f (gs_stmt_dec gl) gs
  in
  let possible_stmts () = 
    if sd <= 0 then List.filter (fun (flat,_) -> flat) stmt_gens else stmt_gens
  in
  let possible_calls () =
    if sd <= 0 then [] else List.filter (fun (param_types,_) -> TypeSet.subset param_types type_set) scall_gens
  in
  match () |> Random.bool with
  | true -> generate (possible_stmts ())
  | false -> let call_gens = possible_calls () in if call_gens = [] then generate (possible_stmts ()) else generate call_gens

and generate_toplevel gl (Generators(_,_,_,_,_,_,toplevel_gens) as gs) = 
  let f = List.nth toplevel_gens (Random.int (List.length toplevel_gens)) in f gl gs

and generate_stmt_list min range gl gs =
  let rec aux i aux_gs acc = match i with
    | 0 -> List.rev acc
    | n -> let (stmt,ngs) = generate_statement gl aux_gs in aux (n-1) ngs (stmt::acc)
  in
  (aux ((Random.int range)+min) gs [], gs)

and generate_main gl (Generators(counter,type_set,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens)) = 
  match () |> Random.bool with
  | true -> (
    let (main_block, gs) = generate_stmt_list 4 5 gl (Generators(counter,type_set,expr_gens,call_gens,(true,fun gl gs -> (S_Return(generate_expression Int gl gs),gs))::stmt_gens,scall_gens,toplevel_gens)) in
    T_Function(Int, "main", [], main_block@[S_Return(generate_expression Int gl gs)])
  )
  | false -> (
    let (main_block, _) = generate_stmt_list 4 5 gl (Generators(counter,type_set,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens)) in
    T_Function(Void, "main", [], main_block)
  )

and generate_compilation_unit toplevels gl gs =
  let rec aux i gs acc = match i with
  | 0 -> (List.rev ((generate_main gl gs)::acc), gs)
  | n -> let (top,gs) = generate_toplevel gl gs in aux (n-1) gs (top::acc)
  in
  aux toplevels gs []
