
open Absyn

(** Generator **)
type generators =
| Generators of 
  int * (* Counter *)
  ((typ * bool * (generator_limits -> generators -> expression)) list) * (* Expression generators*)
  ((typ * bool * (generator_limits -> generators -> expression)) list) * (* Call generators *)
  ((bool * (generator_limits -> generators -> (statement*generators))) list) * (* Statement generators*)
  ((bool * (generator_limits -> generators -> (statement*generators))) list) * (* Call generators*)
  ((generator_limits -> generators -> (toplevel*generators)) list) (* Toplevel generators *)

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

and generate_stmt_list min range gl gs =
  let rec aux i aux_gs acc = match i with
    | 0 -> List.rev acc
    | n -> let (stmt,ngs) = generate_statement gl aux_gs in aux (n-1) ngs (stmt::acc)
  in
  (aux ((Random.int range)+min) gs [], gs)

and generate_main gl (Generators(counter,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens)) = 
  match () |> Random.bool with
  | true -> (
    let (main_block, gs) = generate_stmt_list 4 5 gl (Generators(counter,expr_gens,call_gens,(true,fun gl gs -> (S_Return(generate_expression Int gl gs),gs))::stmt_gens,scall_gens,toplevel_gens)) in
    T_Function(Int, "main", [], main_block@[S_Return(generate_expression Int gl gs)])
  )
  | false -> (
    let (main_block, _) = generate_stmt_list 4 5 gl (Generators(counter,expr_gens,call_gens,stmt_gens,scall_gens,toplevel_gens)) in
    T_Function(Void, "main", [], main_block)
  )

and generate_compilation_unit toplevels gl gs =
  let rec aux i gs acc = match i with
  | 0 -> (List.rev ((generate_main gl gs)::acc), gs)
  | n -> let (top,gs) = generate_toplevel gl gs in aux (n-1) gs (top::acc)
  in
  aux toplevels gs []

