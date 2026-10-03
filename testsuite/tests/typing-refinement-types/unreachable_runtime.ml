(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { bytecode; }
 { native; }
 { flags += " -noassert"; bytecode; }
 { flags += " -noassert"; native; }
*)

type choice = Left | Right [@@inductive]

let (positive @ total) (x : {x : int | x > 0}) =
  if x > 0 then x else unreachable_ ()

let (left @ total) (c : {c : choice | c === Left}) =
  match c with Left -> 17 | Right -> unreachable_ ()

let (pair @ total) (x : {x : int | x > 0}) =
  if x > 0 then #(x, true) else unreachable_ ()

let () =
  if positive 3 <> 3 || left Left <> 17 then failwith "wrong value";
  let #(x, b) = pair 7 in
  if x <> 7 || not b then failwith "wrong product"
