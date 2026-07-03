(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: VC generation display test.  Run with -vox-dry-run, so VCs are
   printed but not sent to the solver; the expected output lives in the
   inline [%%expect] blocks (update with [make promote-one
   TEST=vox/mechanics/vc_gen.ml]). *)

(* Intro on a literal: one VC with no hypotheses. *)
let x : {v:int | v > 0} = refine_ 3
[%%expect{|
Line 1, characters 34-35: vox VC:
  goal: 3 > 0
  hypotheses: <none>
val x : int{ _ > 0 } = 3
|}]

(* Toplevel binder facts accumulate for later items. *)
let b : {v:bool | v} = refine_ true
[%%expect{|
Line 1, characters 31-35: vox VC:
  goal: true
  hypotheses:
  x = 3
  x > 0
val b : bool{ _ } = true
|}]

(* assume_ skips the obligation but compiles a runtime check; flagged
   RUNTIME CHECKED. *)
let a : {v:int | v >= 0} = assume_ 5
[%%expect{|
Line 1, characters 35-36: vox VC (RUNTIME CHECKED):
  goal: 5 >= 0
  hypotheses:
  b = true
  b
  x = 3
  x > 0
val a : int{ _ >= 0 } = 5
|}]

(* Elimination: no VC. *)
let weaken (d : {v:int | not (v = 0)}) : int = (d :> int)
[%%expect{|
val weaken : int{ not (_ = 0) } -> int = <fun>
|}]

(* Coercions are transparent to naming: the goal is about [n], and the
   hypothesis from [n]'s own binder discharges it. *)
let reuse (n : {v:int | not (v = 0)}) : {v:int | not (v = 0)} =
  refine_ (n :> int)
[%%expect{|
Line 2, characters 11-12: vox VC:
  goal: not (n = 0)
  hypotheses:
  not (n = 0)
  a = 5
  a >= 0
  b = true
  b
  x = 3
  x > 0
val reuse : int{ not (_ = 0) } -> int{ not (_ = 0) } = <fun>
|}]

(* Unpacking: [w] gets [x]'s refinement as a fact. *)
let unpack : {v:int | v > 0} =
  let refine_ w = x in
  refine_ w
[%%expect{|
Line 3, characters 10-11: vox VC:
  goal: w > 0
  hypotheses:
  w = x
  w > 0
  a = 5
  a >= 0
  b = true
  b
  x = 3
  x > 0
val unpack : int{ _ > 0 } = 3
|}]

(* Path facts: each branch is checked under the condition / its
   negation. *)
let branch (c : bool) : {v:bool | v || not v} =
  if c then refine_ c else refine_ false
[%%expect{|
Line 2, characters 20-21: vox VC:
  goal: c || (not c)
  hypotheses:
  c
  unpack > 0
  a = 5
  a >= 0
  b = true
  b
  x = 3
  x > 0
Line 2, characters 35-40: vox VC:
  goal: false || (not false)
  hypotheses:
  not c
  unpack > 0
  a = 5
  a >= 0
  b = true
  b
  x = 3
  x > 0
val branch : bool -> bool{ _ || (not _) } = <fun>
|}]

(* assume_unchecked_ skips the obligation and the runtime check;
   flagged ASSUMED. *)
let au : {v:int | v >= 1} = assume_unchecked_ 7
[%%expect{|
Line 1, characters 46-47: vox VC (ASSUMED):
  goal: 7 >= 1
  hypotheses:
  unpack > 0
  a = 5
  a >= 0
  b = true
  b
  x = 3
  x > 0
val au : int{ _ >= 1 } = 7
|}]

(* The toplevel executes the compiled check: a failing assume_ raises
   Failure instead of binding a lie. *)
let lie : {v:int | v > 100} = assume_ 1
[%%expect{|
Line 1, characters 38-39: vox VC (RUNTIME CHECKED):
  goal: 1 > 100
  hypotheses:
  au = 7
  au >= 1
  unpack > 0
  a = 5
  a >= 0
  b = true
  b
  x = 3
  x > 0
Exception: Failure "vox: assume_ check failed at :1:38: _ > 100".
|}]

(* A checked CAST between refined types: [x] keeps its own refined
   type; the expected refinement becomes the obligation at [x]'s name,
   provable from [x]'s binder fact. *)
let cast : {v:int | v > -1} = refine_ x
[%%expect{|
Line 1, characters 38-39: vox VC:
  goal: x > -1
  hypotheses:
  lie = 1
  lie > 100
  au = 7
  au >= 1
  unpack > 0
  a = 5
  a >= 0
  b = true
  b
  x = 3
  x > 0
val cast : int{ _ > -1 } = 3
|}]
