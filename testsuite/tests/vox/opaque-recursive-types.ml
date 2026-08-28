(* TEST
 flags = "-extension refinement_types -rectypes";
 has-z3;
 { expect; }
 { expect.opt; }
*)

let reflexive (_ : ('a list as 'a)) : {x : int | x === 0} =
  let x = 0 in
  refine_ x;;
[%%expect{|
val reflexive : ('a list as 'a) -> {x : int | x === 0} = <fun>
|}]
