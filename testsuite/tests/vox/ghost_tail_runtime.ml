(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { bytecode; }
 { native; }
*)

let rec loop n =
  if n > 0 then (loop (n - 1); ghost_ (); ())

let visits = ref 0
let rec effects n =
  if n > 0 then (effects (n - 1); ghost_ (); incr visits)

let () =
  loop 1_000_000;
  effects 10;
  Printf.printf "%d\n" !visits
