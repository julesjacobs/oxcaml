(* TEST
 flags += "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

let fst x = fun y -> x
[%%expect{|
val fst : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm] = <fun>
|}]

let foo = fst 42
[%%expect{|
val foo : '_weak1 -> int @ [> aliased] = <fun>
|}]
