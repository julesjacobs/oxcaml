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
let f (q : (int * int)) : int{ _ = fst q } =
  match q with
  | (x, _) -> refine_ x
[%%expect{|
Line 3, characters 22-23: vox VC:
  goal: x = (fst q)
  hypotheses:
  x = (fst q)
  p = (1, 2)
val f : (q : (int * int)) -> int{ _ = (fst q) } = <fun>
|}]

(* Triples: projections beyond pairs have no surface syntax; the match
   fact prints in the 1-based [.i] form. *)
let g (t : (int * int * int)) : (int * int * int){ _ = t } =
  match t with
  | (a, b, c) -> refine_ (a, b, c)
[%%expect{|
Line 3, characters 25-34: vox VC:
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
Error: vox: refine_ cannot translate this expression into the logic (only variables, int/bool constants, tuples, immutable field reads, fst/snd, calls to total_ functions, + - * / mod ~-, comparisons at int or bool, && || not, and constructors and records of simple types are supported); add a refined type annotation
|}]

(* assume_ cannot runtime-check structural predicates: tuples point at
   assume_unchecked_ like constructors and field projections do. *)
let k (v : int * int) = (assume_ v : (int * int){ _ = (1, 2) })
[%%expect{|
Line 1, characters 33-34:
1 | let k (v : int * int) = (assume_ v : (int * int){ _ = (1, 2) })
                                     ^
Error: vox: assume_ compiles a runtime check of this refinement, but v has a sort the check cannot evaluate faithfully (only ints, bools, and datatypes built from them can be checked); use assume_unchecked_
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

(* Tuples and pair projections are NAMEABLE, so they may be dependent
   arguments: the binder is substituted by the product term / the
   projection. *)
let first (p : (int * int)) : int{ _ = fst p } = refine_ (fst p)
let use (a : int) (b : int) : int{ _ = a } =
  let refine_ r = first (a, b) in
  refine_ r
[%%expect{|
Line 1, characters 57-64: vox VC:
  goal: (fst p) = (fst p)
  hypotheses:
  p#2 = (1, 2)
val first : (p : (int * int)) -> int{ _ = (fst p) } = <fun>
Line 4, characters 10-11: vox VC:
  goal: r = a
  hypotheses:
  *unknown4* = (fst (a, b))
  r = *unknown4*
  r = (fst (a, b))
  p = (1, 2)
val use : (a : int) -> int -> int{ _ = a } = <fun>
|}]

(* The projection primitives are generic block reads: a user external
   at a NON-pair type is not [fst] and stays unnameable. *)
type t2 = { x2 : int; y2 : int }
external cheat : t2 -> int = "%field0_immut"
let g (k : int) : int{ _ = k } = refine_ k
let bad (v : t2) : int = g (cheat v)
[%%expect{|
type t2 = { x2 : int; y2 : int; }
external cheat : t2 -> int = "%field0_immut"
Line 3, characters 41-42: vox VC:
  goal: k = k
  hypotheses:
  p = (1, 2)
val g : (k : int) -> int{ _ = k } = <fun>
Line 4, characters 27-36:
4 | let bad (v : t2) : int = g (cheat v)
                               ^^^^^^^^^
Error: vox: the argument for a dependent parameter must be a variable or a pure expression the logic can name (let-bind it first)
|}]

(* Equality translates at tuples of int/bool; ORDER comparisons do not
   (OCaml's tuple order is lexicographic; the logic has no order at
   product sorts), and neither does equality at other component types
   (float equality disagrees with the logic's at nan). *)
let lt_pair (p : (int * int)) (q : (int * int)) = refine_ (p < q)
[%%expect{|
Line 1, characters 50-65:
1 | let lt_pair (p : (int * int)) (q : (int * int)) = refine_ (p < q)
                                                      ^^^^^^^^^^^^^^^
Error: vox: refine_ cannot translate this expression into the logic (only variables, int/bool constants, tuples, immutable field reads, fst/snd, calls to total_ functions, + - * / mod ~-, comparisons at int or bool, && || not, and constructors and records of simple types are supported); add a refined type annotation
|}]
let eq_float (p : (float * float)) (q : (float * float)) = refine_ (p = q)
[%%expect{|
Line 1, characters 59-74:
1 | let eq_float (p : (float * float)) (q : (float * float)) = refine_ (p = q)
                                                               ^^^^^^^^^^^^^^^
Error: vox: refine_ cannot translate this expression into the logic (only variables, int/bool constants, tuples, immutable field reads, fst/snd, calls to total_ functions, + - * / mod ~-, comparisons at int or bool, && || not, and constructors and records of simple types are supported); add a refined type annotation
|}]

(* Destructuring a refined pair straight off a call: the components
   tie to a NAME for the result, and the result's refinement holds at
   that name -- recovered from the callee's instantiated result type,
   through the implicit erasure. *)
let mk (a : int) : (int * int){ fst _ = a + 1 } = (a + 1, 0)
let use_direct (a : int) : int{ _ = a + 1 } =
  let (r, _) = mk a in
  r
[%%expect{|
Line 1, characters 50-60: vox VC:
  goal: (fst (a + 1, 0)) = (a + 1)
  hypotheses:
  p = (1, 2)
val mk : (a : int) -> (int * int){ (fst _) = (a + 1) } = <fun>
Line 4, characters 2-3: vox VC:
  goal: r = (a + 1)
  hypotheses:
  (fst *unknown8*) = (a + 1)
  r = (fst *unknown8*)
  p = (1, 2)
val use_direct : (a : int) -> int{ _ = (a + 1) } = <fun>
|}]

(* Destructuring reaches NESTED components: each sub-pattern
   destructures its projection in turn, and an alias names it. *)
let mknest (a : int) : ((int * int) * int){ fst (fst _) = a } = ((a, 0), 1)
let use_nested (a : int) : int{ _ = a } =
  let ((x, _) as _p, _) = mknest a in
  x
[%%expect{|
Line 1, characters 64-75: vox VC:
  goal: (fst (fst ((a, 0), 1))) = a
  hypotheses:
  p = (1, 2)
val mknest : (a : int) -> ((int * int) * int){ (fst (fst _)) = a } = <fun>
Line 4, characters 2-3: vox VC:
  goal: x = a
  hypotheses:
  (fst (fst *unknown11*)) = a
  _p = (fst *unknown11*)
  x = (fst (fst *unknown11*))
  p = (1, 2)
val use_nested : (a : int) -> int{ _ = a } = <fun>
|}]

(* A match destructures a refined scrutinee like the equivalent let:
   ordinary patterns type at the SKELETON (an unpack arm is the one
   pattern that keeps the refined type), and every arm gets the
   scrutinee's refinement at the name. *)
let use_match (a : int) : int{ _ = a } =
  match mknest a with
  | ((x, _), _) when x > a -> x
  | ((x, _), _) -> x
[%%expect{|
Line 3, characters 30-31: vox VC:
  goal: x = a
  hypotheses:
  (fst (fst *unknown13*)) = a
  x = (fst (fst *unknown13*))
  p = (1, 2)
Line 4, characters 19-20: vox VC:
  goal: x = a
  hypotheses:
  (fst (fst *unknown13*)) = a
  x = (fst (fst *unknown13*))
  p = (1, 2)
val use_match : (a : int) -> int{ _ = a } = <fun>
|}]

(* An [if] scrutinee recovers its refinement when BOTH branches
   recover the same predicate; the facts of one branch alone do not
   hold on the other. *)
let use_if (a : int) (b : bool) : int{ _ = a + 1 } =
  let (r, _) = if b then mk a else mk a in
  r
[%%expect{|
Line 3, characters 2-3: vox VC:
  goal: r = (a + 1)
  hypotheses:
  (fst *unknown15*) = (a + 1)
  r = (fst *unknown15*)
  p = (1, 2)
val use_if : (a : int) -> bool -> int{ _ = (a + 1) } = <fun>
|}]

(* Constructor ARGUMENTS destructure too: each argument gets a name
   (its variable when the pattern is one, a fresh synthetic for a
   deeper pattern), the constructor equation speaks over the names,
   and the deeper pattern ties through its projections. *)
type box = Box of (int * int) * int
let use_box (v : box) (a : int) : int{ _ = a } =
  match v with
  | Box ((x, _) as p, k) -> ignore p; ignore k; x + a - x
[%%expect{|
type box = Box of (int * int) * int
Line 4, characters 48-57: vox VC:
  goal: ((x + a) - x) = a
  hypotheses:
  v = (Box (p, k))
  x = (fst p)
  p#2 = (1, 2)
val use_box : box -> (a : int) -> int{ _ = a } = <fun>
|}]
