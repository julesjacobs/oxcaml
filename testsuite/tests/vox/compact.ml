(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: compact refinement syntax [ty{ pred }] (DESIGN.md wishlist).
   [_] denotes the bound value variable. *)
let a : int{ _ >= 0 } = refine_ 3
[%%expect{|
Line 1, characters 32-33: vox VC:
  goal: 3 >= 0
  hypotheses: <none>
val a : int{ _ >= 0 } = 3
|}]

(* The old spelling is the same type. *)
let a' : {v:int | v >= 0} = a
[%%expect{|
val a' : int{ _ >= 0 } = 3
|}]

(* Dependent arrow with a self-referencing domain refinement (the
   binder name denotes the refined value in its own type) and a
   codomain refinement mentioning both parameters. *)
let sub : (x : int{x > 3}) -> (y : int) -> int{ x + _ = y } =
  fun x y -> assume_ (y - (x :> int))
[%%expect{|
Line 2, characters 21-37: vox VC (ASSUMED):
  goal: (x + *vox-unknown*) = y
  hypotheses:
  x > 3
  a' >= 0
  a >= 0
val sub : (x : int{ _ > 3 }) -> (y : int) -> int{ (x + _) = y } = <fun>
|}]

(* Named result binder. *)
let above : (x : int) -> (y : int{ y > x }) = fun x -> assume_ (x + 1)
[%%expect{|
Line 1, characters 63-70: vox VC (ASSUMED):
  goal: *vox-unknown* > x
  hypotheses:
  a' >= 0
  a >= 0
val above : (x : int) -> int{ _ > x } = <fun>
|}]

(* Refinements are allowed at every skeleton type: [unit] carries a
   bare proposition... *)
let lt_witness : (x : int) -> (y : int) -> unit{ x < y } option =
  fun x y -> if x < y then Some (assume_ ()) else None
[%%expect{|
Line 2, characters 41-43: vox VC (ASSUMED):
  goal: x < y
  hypotheses:
  a' >= 0
  a >= 0
val lt_witness : (x : int) -> (y : int) -> unit{ x < y } option = <fun>
|}]

(* ...and other types get equality-only reasoning (a single
   uninterpreted solver sort). *)
let s : string{ _ = _ } = refine_ "hi"
[%%expect{|
Line 1, characters 34-38: vox VC:
  goal: *vox-unknown* = *vox-unknown*
  hypotheses:
  a' >= 0
  a >= 0
val s : string{ _ = _ } = "hi"
|}]

(* Self-reference in a parameter annotation. *)
let h (n : int{n > 0}) : int = (n :> int)
[%%expect{|
val h : int{ _ > 0 } -> int = <fun>
|}]

(* A named type must be a function parameter or refined. *)
let bad : (y : int) = 5
[%%expect{|
Line 1, characters 11-18:
1 | let bad : (y : int) = 5
               ^^^^^^^
Error: vox: (y : ...) names a value and is only meaningful as a function parameter or around a refined type (ty{ ... })
|}]
