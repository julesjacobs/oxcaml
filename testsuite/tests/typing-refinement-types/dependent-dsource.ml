(* TEST
 flags = "-extension refinement_types -dsource";
 expect;
*)

external equal : int -> int -> bool @@ total = "%equal"

type higher_order =
  (f : (int -> int)) -> {result : int | equal result (f 0)};;
[%%expect{|

external equal : int -> int -> bool @@ total = "%equal";;
external equal : int -> int -> bool = "%equal"

type higher_order = (f : (int -> int)) -> {result : int | equal result (f 0)};;
type higher_order = (f : (int -> int)) -> {result : int | equal result (f 0)}
|}]

type checked = { x : int | true }
let assume_value (x : int) : checked = assume_ x;;
[%%expect{|

type checked = {x : int | true};;
type checked = {x : int | true}

let assume_value (x : int)  : checked = assume_ x;;
val assume_value : int -> checked = <fun>
|}]

type immutable_argument =
  (x : int) @ immutable -> {y : int | equal y x}
;;
[%%expect{|

type immutable_argument = (x : int) @ immutable -> {y : int | equal y x};;
type immutable_argument = (x : int) @ immutable -> {y : int | equal y x}
|}]
