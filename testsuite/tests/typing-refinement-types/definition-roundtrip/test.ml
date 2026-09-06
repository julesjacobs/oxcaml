(* TEST
 modules = "producer.ml";
 flags = "-extension refinement_types";
 has-z3;
 { bytecode; }
 { native; }
*)

module Alias = Producer
module Same = Alias
module Producer = struct let next _ = 0 end

let verified () : {n : int | n = 5} =
  let x = 3 in
  let result = Same.next x in
  let refine_ proof = Alias.next_def x in
  refine_ result

let multiple () : {n : int | n = 4} =
  let b = true in
  let x = 3 in
  let y = 9 in
  let result = Alias.choose b x y in
  let refine_ proof = Alias.choose_def b x y in
  refine_ result

let datatype x =
  let refine_ proof = Alias.box_def x in
  ()

let () = ignore (verified ()); ignore (multiple ()); ignore (datatype 0)

let dependent : (x : int) -> {v : int | v = x} -> unit =
  fun x y -> let refine_ proof = Alias.dependent_def x y in ()

let witnessed (x : int) : {y : int | y === x} =
  let result = Same.witnessed x in
  let refine_ proof = ghost_ (Same.witnessed_def x) in
  refine_ result

let static_identity (x : int) : Same.ghost_identity = refine_ x

let () =
  let seven = 7 in
  let refine_ result = witnessed seven in
  assert (result = 7);
  ignore (static_identity 9)
