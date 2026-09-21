(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { bytecode; }
 { native; }
*)
type ('a : immutable_data) shape = Var | Link of 'a
let[@def] (is_var @ total) (x : int shape @ immutable) =
  match x with Var -> true | Link _ -> false
let run () =
  let desc = Var in
  ghost_ (is_var_def desc);
  let u = () in let _proof : {u : unit | is_var desc} = u in
  ghost_ (
    let result = match desc with Var -> true | Link q -> q = 0 in
    let _result : {b : bool | b} = result in
    ());
  ()

let () = run ()
