(* TEST
 flags = "-extension refinement_types";
 expect;
*)

let[@def] eq_body x =
  let _unused = 0 in ((=) : int -> int -> bool);;
[%%expect{|
val eq_body : 'a @ immutable -> int -> int -> bool = <fun>
val eq_body_def :
  (x : 'a) -> {u : unit | (eq_body x) === ((=) : int -> int -> bool)} = <fun>
|}]

let[@def] eq_outer x =
  (let _unused = 0 in (=) : int -> int -> bool);;
[%%expect{|
val eq_outer : 'a @ immutable -> int -> int -> bool = <fun>
val eq_outer_def :
  (x : 'a) -> {u : unit | (eq_outer x) === ((=) : int -> int -> bool)} =
  <fun>
|}]
