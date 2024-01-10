let () = Printexc.record_backtrace true

open LibCGen.Absyn
open LibCGen.LangGen
open LibCGen.Mutate
open LibCGen.Gens


let seed = int_of_string (Sys.argv.(1))
let () = if seed = 0 then Random.self_init () else Random.init seed
let gs = (Generators(0,expr_gens,expr_gens,stmt_gens,stmt_gens,toplevel_gens))
let gl = (GenLimit(int_of_string (Sys.argv.(2)),int_of_string (Sys.argv.(3)))) 
let tops = int_of_string (Sys.argv.(4))

let (tops, _) = generate_compilation_unit tops gl gs

let tops = tops |> mutate (function
  | E_Var(n) -> Stop(E_Var("destroyed_"^n))
  | E_Int i -> Stop(E_Int (-i))
  | e -> Continue e
) (function
  | S_Block _ -> Stop(S_Block [])
  | e -> Continue e
) (fun top -> Continue top)

let () = List.iter (fun top -> Printf.printf "%s" (print_toplevel top)) tops