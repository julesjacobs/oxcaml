(* TEST
 flags = "-extension refinement_types -nopervasives";
 { expect; }
 { expect.opt; }
*)

external ( = ) : int -> int -> bool = "%equal"

let identity x : {n : int | n = x} =
  let n = x in
  refine_ n
;;
[%%expect{|
external ( = ) : int -> int -> bool = "%equal"
val identity : (x : int) -> {n : int | n = x} = <fun>
|}]
