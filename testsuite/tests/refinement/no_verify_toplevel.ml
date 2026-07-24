(* TEST
 flags = "-vox-no-verify -vox-backend z3 -vox-smt-solver false";
 toplevel;
*)

let impossible = (1 : int{ _ = 0 });;
let ordinary = impossible + 1;;
ordinary;;
