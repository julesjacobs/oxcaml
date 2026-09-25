(* TEST
 flags += "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

let id x = x
[%%expect{|
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

let foo (x @ portable) = id x
[%%expect{|
val foo : 'a @ [< 'm & global portable] -> 'a @ [> 'm | dynamic] = <fun>
|}]

let id' = id
[%%expect{|
val id' : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

let apply_yielding (x @ yielding) = id x
[%%expect{|
val apply_yielding :
  'a @ [< 'm & global > yielding] -> 'a @ [> 'm | yielding dynamic] = <fun>
|}]
