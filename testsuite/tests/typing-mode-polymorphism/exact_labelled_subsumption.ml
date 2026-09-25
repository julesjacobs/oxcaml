(* TEST
 flags = "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

let fst ~label1 ~label2 = label1
let foo = fst ~label2:()
[%%expect{|
val fst : label1:'a @ [< 'm & global] -> label2:'b @ 'n -> 'a @ [> 'm] =
  <fun>
val foo :
  label1:'a @ [< 'm & 'n & global] ->
  'a @ [> 'm | 'n mod many portable forkable unyielding stateless] = <fun>
|}]

let expected ~(label1 @ global) = label1
[%%expect{|
val expected : label1:'a @ [< 'm & global] -> 'a @ [> 'm] = <fun>
|}]

module Inferred = struct let foo = foo end
module Expected = struct let foo = expected end
module Forward : module type of Expected = Inferred
module Backward : module type of Inferred = Expected
[%%expect{|
module Inferred :
  sig
    val foo :
      label1:'a @ [< 'm & 'n & global] ->
      'a @ [> 'm | 'n mod many portable forkable unyielding stateless]
  end
module Expected :
  sig val foo : label1:'a @ [< 'm & global] -> 'a @ [> 'm] end
module Forward : sig val foo : label1:'a @ [< 'm & global] -> 'a @ [> 'm] end
module Backward :
  sig
    val foo :
      label1:'a @ [< 'm & 'n & global] ->
      'a @ [> 'm | 'n mod many portable forkable unyielding stateless]
  end
|}]
