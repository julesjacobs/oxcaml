(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: the built-in iarray theory.  [Iarray.length a] and
   [Iarray.get a i] (sugar: [a.(i)]) reflect in expressions and appear
   in predicates as the reserved operations of an opaque array sort;
   the only axiom is length nonnegativity.  [get] is TOTAL in the
   logic, like division: the safe program get raises out of bounds, so
   no value flows there (partial correctness); bounds SAFETY is an
   opt-in contract.  Gated on [int iarray]: the MUTABLE array's
   identical primitives do not reflect. *)

let read (a : int iarray) (i : int) : int{ _ = a.(i) } = Iarray.get a i
[%%expect{|
Line 1, characters 57-71: vox VC:
  goal: (a.(i)) = (a.(i))
  hypotheses: <none>
val read : (a : int iarray) -> (i : int) -> int{ _ = (a.(i)) } = <fun>
|}]

(* Selfification names both operations; the qualified and sugar
   spellings are the same predicate. *)
let len2 (a : int iarray) : int{ _ = Iarray.length a + a.(0) } =
  let n = Iarray.length a in
  let h = Iarray.get a 0 in
  n + h
[%%expect{|
Line 4, characters 2-7: vox VC:
  goal: (n + h) = ((Iarray.length a) + (a.(0)))
  hypotheses:
  h = (a.(0))
  n = (Iarray.length a)
val len2 : (a : int iarray) -> int{ _ = ((Iarray.length a) + (a.(0))) } =
  <fun>
|}]

(* A mutable array's reads stay fresh unknowns (same primitives,
   different type). *)
let opaque_arr (m : int array) : int{ _ = 7 } =
  assume_unchecked_ (Array.length m)
[%%expect{|
Line 2, characters 20-36: vox VC (ASSUMED):
  goal: *unknown3* = 7
  hypotheses: <none>
val opaque_arr : int array -> int{ _ = 7 } = <fun>
|}]
