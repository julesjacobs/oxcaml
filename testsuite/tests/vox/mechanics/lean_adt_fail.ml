(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: ADT and record obligations that must FAIL verification (lean
   backend).  The positive twins are demo/lean_adt.ml and
   demo/lean_records.ml; these probes check that the datatype encoding
   is not degenerate -- distinct constructors must stay distinct,
   injectivity must not conjure facts, and projections must not prove
   lies. *)

type t =
  | K of int
  | L
[%%expect{|
type t = K of int | L
|}]

(* A false constructor equality must fail (injectivity: K 4 <> K 3). *)
let bad : t{ _ = K 3 } = refine_ (K 4)
[%%expect{|
Line 1, characters 33-38:
1 | let bad : t{ _ = K 3 } = refine_ (K 4)
                                     ^^^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: K 4 = K 3
Hypotheses: <none>
The goal is false unconditionally.
|}]

(* Distinct constructors must never be equal. *)
let bad2 : t{ _ = K 0 } = refine_ L
[%%expect{|
Line 1, characters 34-35:
1 | let bad2 : t{ _ = K 0 } = refine_ L
                                      ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: L = K 0
Hypotheses: <none>
The goal is false unconditionally.
|}]

(* Injectivity gives y = 4 here, not y = 3: the goal must fail. *)
let inj (s : t{ _ = K 4 }) : {r:int | r = 3} =
  match s with
  | K y -> refine_ y
  | L -> refine_ 0
[%%expect{|
Line 3, characters 19-20:
3 |   | K y -> refine_ y
                       ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: y = 3
Hypotheses:
  s = K y
  s = K 4
Counterexample (validated -- every hypothesis holds and the goal fails here):
  y = 4
  s = K 4
|}]

type point =
  { px : int
  ; py : int
  }
[%%expect{|
type point = { px : int; py : int; }
|}]

(* A false projection fact must fail. *)
let badp : point{ _.px = 1 } = refine_ { px = 0; py = 0 }
[%%expect{|
Line 1, characters 39-57:
1 | let badp : point{ _.px = 1 } = refine_ { px = 0; py = 0 }
                                           ^^^^^^^^^^^^^^^^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: (mk (0, 0)).px = 1
Hypotheses: <none>
The goal is false unconditionally.
|}]

(* The functional-update frame gives _.py = p.py, never p.py + 1. *)
let badu : (p : point) -> point{ _.py = p.py + 1 } =
  fun p -> refine_ { p with px = 3 }
[%%expect{|
Line 2, characters 19-36:
2 |   fun p -> refine_ { p with px = 3 }
                       ^^^^^^^^^^^^^^^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: (mk (3, p.py)).py = p.py + 1
Hypotheses: <none>
Counterexample (validated -- every hypothesis holds and the goal fails here):
  p = mk (0, 0)
|}]

(* A failed phrase must not leak its datatype registration: every
   phrase above that used [t] FAILED and was backtracked, so
   redeclaring [t] (same stamp-free solver name .t) and proving
   something true must succeed -- no "two distinct types would share
   the solver-side name" collision with the rolled-back phrases. *)
type t =
  | A
  | B
[%%expect{|
type t = A | B
|}]

let ok : t{ _ = A } = refine_ A
[%%expect{|
val ok : t{ _ = A } = A
|}]
