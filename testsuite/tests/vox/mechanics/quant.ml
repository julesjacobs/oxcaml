(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: quantifiers in predicates.  [forall_ x. p] and [exists_ x. p]
   (keywords, like refine_/total_: bare forall/exists collide with
   existing identifiers); binders extend maximally right and may be
   listed ([forall_ x y. p]); implication [p -> q] desugars to
   [not p || q].  Binders are fresh Scoped idents, compared under the
   dependent-arrow binder pairing, so alpha-variants are the SAME
   rigid type.  A quantified predicate reaches the solver with the
   binder unannotated (predicates are untyped; Lean infers). *)

let lub : (x : int) -> int{ x <= _ && (forall_ z. x <= z -> _ <= z) } =
  fun x -> refine_ x
[%%expect{|
Line 2, characters 19-20: vox VC:
  goal: (x <= x) && (forall_ z. (not (x <= z)) || (x <= z))
  hypotheses: <none>
val lub :
  (x : int) -> int{ (x <= _) && (forall_ z. (not (x <= z)) || (_ <= z)) } =
  <fun>
|}]

(* Multiple binders nest; the printed form reparses. *)
let pair : unit{ forall_ i j. i + j = j + i } = assume_unchecked_ ()
[%%expect{|
Line 1, characters 66-68: vox VC (ASSUMED):
  goal: forall_ i. forall_ j. (i + j) = (j + i)
  hypotheses: <none>
val pair : unit{ forall_ i. forall_ j. (i + j) = (j + i) } = ()
|}]

(* Alpha-equivalence: differently named binders are the SAME type
   (the analogue of the dependent-arrow binder pairing), so this
   subsumption is free -- no VC. *)
let pair2 : unit{ forall_ a b. a + b = b + a } = pair
[%%expect{|
val pair2 : unit{ forall_ a. forall_ b. (a + b) = (b + a) } = ()
|}]

(* Different structure is a different type; subsumption turns the
   mismatch into an ordinary quantified obligation (here: commute the
   known fact -- a forall_ goal, within grind's reach). *)
let swapped : unit{ forall_ a b. b + a = a + b } = pair
[%%expect{|
Line 1, characters 51-55: vox VC:
  goal: forall_ a. forall_ b. (b + a) = (a + b)
  hypotheses:
  pair2 = pair
  forall_ a. forall_ b. (a + b) = (b + a)
  pair = ()
val swapped : unit{ forall_ a. forall_ b. (b + a) = (a + b) } = ()
|}]

(* A quantifier binder shadows program variables... *)
let shadow (k : int) : unit{ exists_ k. k = k } = assume_unchecked_ ()
[%%expect{|
Line 1, characters 68-70: vox VC (ASSUMED):
  goal: exists_ k. k = k
  hypotheses:
  swapped = pair
  forall_ a. forall_ b. (b + a) = (a + b)
  pair2 = pair
  forall_ a. forall_ b. (a + b) = (b + a)
  pair = ()
val shadow : int -> unit{ exists_ k. k = k } = <fun>
|}]

(* ... but may not shadow the refined value or an enclosing binder:
   the bound-value name would win the lookup. *)
let selfshadow (y : int{ forall_ y. y = y }) : int = 0
[%%expect{|
Line 1, characters 33-34:
1 | let selfshadow (y : int{ forall_ y. y = y }) : int = 0
                                     ^
Error: vox: this quantifier binder shadows the refined value or an enclosing binder; rename it
|}]

(* The long form uses the same predicate grammar: quantifiers,
   implication, and [_] work there too. *)
let lub2 : (x : int) -> {r:int | x <= r && (forall_ z. x <= z -> r <= z)} =
  fun x -> x
[%%expect{|
Line 2, characters 11-12: vox VC:
  goal: (x <= x) && (forall_ z. (not (x <= z)) || (x <= z))
  hypotheses:
  swapped = pair
  forall_ a. forall_ b. (b + a) = (a + b)
  pair2 = pair
  forall_ a. forall_ b. (a + b) = (b + a)
  pair = ()
val lub2 :
  (x : int) -> int{ (x <= _) && (forall_ z. (not (x <= z)) || (_ <= z)) } =
  <fun>
|}]

(* assume_ cannot compile a runtime check of a quantifier. *)
let checked : unit{ forall_ n. n = n } = assume_ ()
[%%expect{|
Line 1, characters 49-51:
1 | let checked : unit{ forall_ n. n = n } = assume_ ()
                                                     ^^
Error: vox: assume_ compiles a runtime check of this refinement, but it involves a constructor, tuple, projection, spec function, quantifier, or division, which the compiled check cannot evaluate faithfully; use assume_unchecked_
|}]
