(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { native; }
*)
type ('a : immutable_data) shape = Var | Link of 'a
let[@def] (is_var @ total) (x : int shape @ immutable) =
  match x with Var -> true | Link _ -> false
let run () =
  let desc = Var in
  ghost_ (is_var_def desc);
  let u = () in let proof : {u : unit | is_var desc} = refine_ u in
  let refine_ proof = proof in
  ghost_ (
    let result = match desc with Var -> true | Link q -> q = 0 in
    let result : {b : bool | b} = refine_ result in
    let refine_ result = result in ());
  ()

let () = run ()
