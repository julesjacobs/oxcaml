(* TEST
 expect;
*)

let ( === ) x y = x = y;;
[%%expect{|
val ( === ) : 'a -> 'a -> bool = <fun>
|}]

let apply = 1 === 1
let alias = ( === );;
[%%expect{|
val apply : bool = true
val alias : 'a -> 'a -> bool = <fun>
|}]

module Equality = struct
  let ( === ) x y = x <> y
end;;
[%%expect{|
module Equality : sig val ( === ) : 'a -> 'a -> bool end
|}]

let qualified = Equality.( === ) 1 2
let opened = Equality.(1 === 2);;
[%%expect{|
val qualified : bool = true
val opened : bool = true
|}]
