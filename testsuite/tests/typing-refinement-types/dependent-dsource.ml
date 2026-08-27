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
