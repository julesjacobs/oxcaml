(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* A result annotation mentioning two parameters, with a recursive function
   or an expected dependent arrow. These used to fail with a type error whose
   two types printed the same. *)

let[@def] (p @ total) (x : int) (s : int) = x + s >= 0;;
[%%expect{|
val p : int -> int -> bool = <fun>
val p_def : (x : int) -> (s : int) -> {u : unit | (p x s) === ((x + s) >= 0)} =
  <fun>
|}]

let rec (f @ total) (x : int) (s : {s : int | s >= 0})
    : {u : unit | p x s || true} = ();;
[%%expect{|
val f : (x : int) -> (s : {s : int | s >= 0}) -> {u : unit | (p x s) || true} =
  <fun>
|}]

let (g @ total) : (x : int) -> (s : int) -> {u : unit | p x s || true} =
  fun x s : {u : unit | p x s || true} -> ();;
[%%expect{|
val g :
  (x : int) -> ((s : int) -> {u : unit | (p x s) || true}) @ total stateful =
  <fun>
|}]

let rec (count @ total) (x : int) (s : {s : int | s >= 0})
    : {u : unit | x = x && s >= 0} =
  if s = 0 then () else count x (s - 1)
  [@@decreases s];;
[%%expect{|
val count :
  (x : int) -> (s : {s : int | s >= 0}) -> {u : unit | (x = x) && (s >= 0)} =
  <fun>
|}]
