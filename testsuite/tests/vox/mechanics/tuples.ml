(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: native tuples in predicates -- VC generation (no solver). *)

(* Construction names the product term over the components' names;
   the exact-synthesis rule applies to translatable tuples. *)
let p : (int * int){ _ = (1, 2) } = refine_ (1, 2)
[%%expect{|
Line 1, characters 44-50: vox VC:
  goal: (1, 2) = (1, 2)
  hypotheses: <none>
val p : (int * int){ _ = (1, 2) } = (1, 2)
|}]

(* Matching a variable against a tuple pattern contributes per-component
   projection facts, like simple records; wildcard components simply
   contribute nothing. *)
let f : (q : (int * int)) -> int{ _ = fst q } =
  fun q ->
    match q with
    | (x, _) -> refine_ x
[%%expect{|
Line 4, characters 24-25: vox VC:
  goal: x = (fst q)
  hypotheses:
  x = (fst q)
  p = (1, 2)
val f : (q : (int * int)) -> int{ _ = (fst q) } = <fun>
|}]

(* Triples: projections beyond pairs have no surface syntax; the match
   fact prints in the 1-based [.i] form. *)
let g : (t : (int * int * int)) -> (int * int * int){ _ = t } =
  fun t ->
    match t with
    | (a, b, c) -> refine_ (a, b, c)
[%%expect{|
Line 4, characters 27-36: vox VC:
  goal: (a, b, c) = t
  hypotheses:
  a = t.1
  b = t.2
  c = t.3
  p = (1, 2)
val g : (t : (int * int * int)) -> (int * int * int){ _ = t } = <fun>
|}]

(* Labeled tuples are not modelled: a labeled tuple expression has no
   logic translation (sound degrade), so exact synthesis rejects it. *)
let h = refine_ (~x:1, 2)
[%%expect{|
Line 1, characters 8-25:
1 | let h = refine_ (~x:1, 2)
            ^^^^^^^^^^^^^^^^^
Error: vox: refine_ cannot translate this expression into the logic (only variables, int/bool constants, + - * / mod ~-, comparisons at int or bool, and && || not are supported); add a refined type annotation
|}]

(* assume_ cannot runtime-check structural predicates: tuples point at
   assume_unchecked_ like constructors and field projections do. *)
let k (v : int * int) = (assume_ v : (int * int){ _ = (1, 2) })
[%%expect{|
Line 1, characters 33-34:
1 | let k (v : int * int) = (assume_ v : (int * int){ _ = (1, 2) })
                                     ^
Error: vox: assume_ compiles a runtime check of this refinement, but it involves a constructor, field projection, spec function, or division, which the compiled check cannot evaluate faithfully; use assume_unchecked_
|}]

(* fst/snd are reserved in predicates: a misapplied one is an error,
   never silently an uninterpreted spec function. *)
let m : int{ fst 1 2 = 0 } = assume_unchecked_ 0
[%%expect{|
Line 1, characters 13-20:
1 | let m : int{ fst 1 2 = 0 } = assume_unchecked_ 0
                 ^^^^^^^
Error: vox: fst expects exactly one argument in a refinement predicate
|}]
