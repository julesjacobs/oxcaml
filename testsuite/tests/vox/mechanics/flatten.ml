(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: a refinement over a refined ABBREVIATION flattens -- the
   layers conjoin on the underlying skeleton, so there is one normal
   form and [pos{ _ < 10 }] IS [int{ 0 < _ && _ < 10 }] (the same
   rigid type). *)

type pos = int{ 0 < _ }
[%%expect{|
type pos = int{ 0 < _ }
|}]

let d : pos{ _ < 10 } = 5
[%%expect{|
Line 1, characters 24-25: vox VC:
  goal: (0 < 5) && (5 < 10)
  hypotheses: <none>
val d : int{ (0 < _) && (_ < 10) } = 5
|}]

(* Rigid equality with the spelled form: the identity coercion. *)
let same (p : pos{ _ < 10 }) : int{ 0 < _ && _ < 10 } = p
[%%expect{|
Line 1, characters 56-57: vox VC:
  goal: (0 < p) && (p < 10)
  hypotheses:
  (0 < p) && (p < 10)
  d = 5
  (0 < d) && (d < 10)
val same : int{ (0 < _) && (_ < 10) } -> int{ (0 < _) && (_ < 10) } = <fun>
|}]

(* Layers accumulate through chains of abbreviations. *)
type digit = pos{ _ < 10 }
[%%expect{|
type digit = int{ (0 < _) && (_ < 10) }
|}]

let d7 : digit{ _ = 7 } = 7
[%%expect{|
Line 1, characters 26-27: vox VC:
  goal: ((0 < 7) && (7 < 10)) && (7 = 7)
  hypotheses:
  d = 5
  (0 < d) && (d < 10)
val d7 : int{ ((0 < _) && (_ < 10)) && (_ = 7) } = 7
|}]

(* Both layers are one obligation: the dumped goal is the
   conjunction. *)
let both : pos{ _ > -5 } = 12
[%%expect{|
Line 1, characters 27-29: vox VC:
  goal: (0 < 12) && (12 > -5)
  hypotheses:
  d7 = 7
  ((0 < d7) && (d7 < 10)) && (d7 = 7)
  d = 5
  (0 < d) && (d < 10)
val both : int{ (0 < _) && (_ > -5) } = 12
|}]
