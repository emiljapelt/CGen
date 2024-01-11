
open Absyn

module TypeSet = Set.Make (struct
  type t = typ

  let compare = fun t1 t2 -> compare (print_typ t1) (print_typ t2)
end)

(** Generator **)
type generators =
| Generators of 
  int * (* Counter *)
  TypeSet.t * (* Available types *)
  ((typ * bool * (generator_limits -> generators -> expression)) list) * (* Expression generators (expr_type, is_flat, generator) *)
  ((typ * TypeSet.t * (generator_limits -> generators -> expression)) list) * (* Call generators (expr_type, required_types, generator) *)
  ((bool * TypeSet.t * (generator_limits -> generators -> (statement*generators))) list) * (* Statement generators (is_flat, required_types, generator) *)
  ((TypeSet.t * (generator_limits -> generators -> (statement*generators))) list) * (* Call generators (required_types, generator) *)
  ((bool * (generator_limits -> generators -> (toplevel*generators))) list) (* Toplevel generators (base_gs, generator) *)

and generator_limits =
| GenLimit of 
  int * (* Remaining expression depth *)
  int * (* Remaining statement depth *)
  (int * int ) (* block min & range *)

let types lst = TypeSet.of_list lst

let generate_generators expr_gens stmt_gens top_gens =
  let type_set = List.fold_left (fun set (t,_,_) -> TypeSet.add t set) TypeSet.empty expr_gens in
  Generators(0,type_set,expr_gens,[],stmt_gens,[],top_gens)

let set_counter cnt (Generators(_,type_set,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens)) =
  (Generators(cnt,type_set,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens)) 

let combine 
  (Generators(counter,type_set,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens)) 
  (Generators(counter',type_set',expr_gens',call_gens',stmt_gens',scall_gens',toplevel_gens')) 
  =
  (Generators(max counter counter',TypeSet.union type_set type_set',expr_gens @ expr_gens',call_gens @ call_gens',stmt_gens @ stmt_gens',scall_gens @ scall_gens',toplevel_gens @ toplevel_gens'))

let gs_expr_dec (GenLimit(expr_depth,stmt_depth,block_lim)) =
  GenLimit(expr_depth-1,stmt_depth,block_lim)

let gs_stmt_dec (GenLimit(expr_depth,stmt_depth,block_lim)) =
  GenLimit(expr_depth,stmt_depth-1,block_lim)

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

and generate_expression typ (GenLimit(ed,_,_) as gl) (Generators(_,type_set,expr_gens,call_gens,_,_,_) as gs) = 
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

and generate_statement (GenLimit(_,sd,_) as gl) (Generators(_,type_set,_,_,stmt_gens,scall_gens,_) as gs) = 
  let generate gens = 
    let f = List.nth gens (Random.int (List.length gens)) in f (gs_stmt_dec gl) gs
  in
  let possible_stmts () = 
    let gens = if sd <= 0 then List.filter (fun (flat,_,_) -> flat) stmt_gens else stmt_gens in
    List.filter_map (fun (_,req_types,f) -> if TypeSet.subset req_types type_set then Some f else None) gens
  in
  let possible_calls () =
    if sd <= 0 then [] else List.filter_map (fun (param_types,f) -> if TypeSet.subset param_types type_set then Some f else None) scall_gens
  in
  (*Printf.printf "Available: %s\n" (TypeSet.to_list type_set |> List.map print_typ |> String.concat ", ");
  List.iter (fun (ps,_) -> Printf.printf "\t%s\n" (TypeSet.to_list ps |> List.map print_typ |> String.concat ", ")) scall_gens;*)
  match () |> Random.bool with
  | true -> generate (possible_stmts ())
  | false -> let call_gens = possible_calls () in if call_gens = [] then generate (possible_stmts ()) else generate call_gens

and generate_toplevel gl gs (Generators(_,_,_,_,_,_,toplevel_gens) as base_gs) = 
  let (base,f) = List.nth toplevel_gens (Random.int (List.length toplevel_gens)) in 
  if base then f gl base_gs else f gl gs

and generate_stmt_list min range gl gs =
  let rec aux i aux_gs acc = match i with
    | 0 -> List.rev acc
    | n -> let (stmt,ngs) = generate_statement gl aux_gs in aux (n-1) ngs (stmt::acc)
  in
  (aux ((Random.int range)+min) gs [], gs)

and generate_main (GenLimit(_,_,(b_min,b_range)) as gl) (Generators(counter,type_set,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens)) = 
  match () |> Random.bool with
  | true -> (
    let (main_block, gs) = generate_stmt_list b_min b_range gl (Generators(counter,type_set,expr_gens,call_gens,(true,types [Int],fun gl gs -> (S_Return(generate_expression Int gl gs),gs))::stmt_gens,scall_gens,toplevel_gens)) in
    T_Function(Int, "main", [], main_block@[S_Return(generate_expression Int gl gs)])
  )
  | false -> (
    let (main_block, _) = generate_stmt_list b_min b_range gl (Generators(counter,type_set,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens)) in
    T_Function(Void, "main", [], main_block)
  )

and generate_compilation_unit toplevels gl base_gs =
  let rec aux i gs cnt acc = match i with
  | 0 -> (List.rev ((generate_main gl gs)::acc), gs)
  | n -> let (top,ngs) = generate_toplevel gl (set_counter cnt gs) (set_counter cnt base_gs) in aux (n-1) (combine gs ngs) (cnt+1) (top::acc)
  in
  aux toplevels base_gs 0 []
