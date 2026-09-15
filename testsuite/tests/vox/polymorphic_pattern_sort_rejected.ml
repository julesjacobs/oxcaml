(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
 { expect.opt; }
*)
module Incorrect_branch = struct
type ('a : immutable_data) shape = Var | Link of 'a
let[@def] (is_var @ total) (x : int shape @ immutable) =
  match x with Var -> true | Link _ -> false
let bad () =
  let desc = Var in
  ghost_ (is_var_def desc);
  let u = () in let proof : {u : unit | is_var desc} = refine_ u in
  let refine_ proof = proof in
  ghost_ (
    let result = match desc with Var -> true | Link q -> q = 0 in
    let result : {b : bool | not b} = refine_ result in
    let refine_ result = result in ());
  ()

end;;
[%%expect{|
Line 12, characters 38-52:
12 |     let result : {b : bool | not b} = refine_ result in
                                           ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
