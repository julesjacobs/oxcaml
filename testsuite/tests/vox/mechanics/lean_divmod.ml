(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: reflected division and remainder.  OCaml's [/] and [mod]
   truncate toward zero (T-division); the logic models them as Lean's
   [Int.tdiv]/[Int.tmod], so the semantics below are PROVED, not
   assumed -- and the runtime values printed by the toplevel agree. *)

let q : int{ _ = (-7) / 2 && _ = -3 } = refine_ ((-7) / 2)
[%%expect{|
val q : int{ (_ = (-7 / 2)) && (_ = -3) } = -3
|}]

let r : int{ _ = (-7) mod 2 && _ = -1 } = refine_ ((-7) mod 2)
[%%expect{|
val r : int{ (_ = (-7 mod 2)) && (_ = -1) } = -1
|}]

(* Tick alignment travels through arithmetic: the contract on [p] and
   the path fact prove the result's alignment. *)
let next_tick (p : int{ _ mod 25 = 0 }) : int{ _ mod 25 = 0 } =
  refine_ (p + 25)
[%%expect{|
val next_tick : int{ (_ mod 25) = 0 } -> int{ (_ mod 25) = 0 } = <fun>
|}]

let t50 : int =
  let refine_ t = next_tick 25 in
  t
[%%expect{|
val t50 : int = 50
|}]

(* Misalignment is CAUGHT (with the witness). *)
let bad : int =
  let refine_ t = next_tick 30 in
  t
[%%expect{|
Line 2, characters 28-30:
2 |   let refine_ t = next_tick 30 in
                                ^^
Error: vox: verification failed (lean).
       Goal: (30 mod 25) = 0
Hypotheses:
  (*unknown2* mod 25) = 0
  t = *unknown2*
  (t mod 25) = 0
  r = (-7 mod 2)
  (r = (-7 mod 2)) && (r = -1)
  q = (-7 / 2)
  (q = (-7 / 2)) && (q = -3)
Possible counterexample:
  *unknown2* = 0
  t = 0
  r = -1
  q = -3
(lean: error: `grind` failed)
|}]

(* [assume_] cannot compile a faithful runtime check of a predicate
   with division (the logic totalizes [x / 0] where the program
   raises); the checked form is rejected toward assume_unchecked_. *)
let unchecked (d : int) : int{ _ = 100 / d } = assume_ (100 / d)
[%%expect{|
Line 1, characters 55-64:
1 | let unchecked (d : int) : int{ _ = 100 / d } = assume_ (100 / d)
                                                           ^^^^^^^^^
Error: vox: assume_ compiles a runtime check of this refinement, but it involves a constructor, tuple, projection, spec function, quantifier, or division, which the compiled check cannot evaluate faithfully; use assume_unchecked_
|}]

(* Logarithmic recursion REFLECTS: the [@@vox.decreases n] metric's
   decrease goal runs through [Int.tdiv], which omega alone treats as
   an atom -- the emitted [decreasing_by] falls back to grind. *)
let rec total_ steps n = if n <= 1 then 0 else 1 + steps (n / 2)
[@@vox.decreases n]

let s4 : int{ _ = steps 4 && _ = 2 } = refine_ (steps 4)
[%%expect{|
val steps : int -> int = <fun>
val s4 : int{ (_ = (steps 4)) && (_ = 2) } = 2
|}]
