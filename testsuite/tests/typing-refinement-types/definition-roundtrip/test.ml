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

let () = ignore (verified ()); ignore (multiple ())
