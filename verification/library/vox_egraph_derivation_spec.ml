(* Derivations, the meaning of an [Equal] answer; part of the trusted
   specification. An [evidence] is a finite proof tree: reflexivity,
   symmetry, transitivity, a congruence step for each constructor with
   children, and [Rule (i, rule, subst)], one instance of rule [i] from
   [instantiate rule.lhs subst] to [instantiate rule.rhs subst]. A rule
   applies at the root of its step; congruence steps put it in a context.
   [endpoints] computes the two expressions a derivation relates, and [left]
   and [right] select them.

   [valid rules proof] checks every step: [Refl] of a well-sorted
   expression; [Trans] only when the right end of the first part is
   syntactically the left end of the second; congruence only when the left
   ends of the parts have the sorts the constructor needs (the right ends
   then have the same sorts, [Vox_egraph_derivation.sort_sound]); and
   [Rule] only when rule [i] of [rules] is exactly [rule], it is valid and
   [subst] gives each variable an expression of its sort. A [Rule] step
   carries its rule, and this check is what ties a derivation to [rules]
   (testsuite/tests/vox/egraph_rule_rejected.ml rejects a derivation that
   uses a rule not in the list). *)

module L = Vox_egraph_language_spec
module R = Vox_egraph_rule_spec

type evidence =
  | Refl of L.expr
  | Sym of evidence
  | Trans of evidence * evidence
  | Add_cong of evidence * evidence
  | Eq_cong of evidence * evidence
  | Int_if_cong of evidence * evidence * evidence
  | Bool_if_cong of evidence * evidence * evidence
  | Rule of int * R.rule * R.subst
[@@inductive]

let[@def] rec (endpoints @ total) (proof : evidence @ immutable) =
  match proof with
  | Refl expr -> (expr, expr)
  | Sym child -> let a, b = endpoints child in (b, a)
  | Trans (first, second) ->
    let a, _ = endpoints first in
    let _, b = endpoints second in
    (a, b)
  | Add_cong (a, b) ->
    let al, ar = endpoints a in
    let bl, br = endpoints b in
    (L.Add (al, bl), L.Add (ar, br))
  | Eq_cong (a, b) ->
    let al, ar = endpoints a in
    let bl, br = endpoints b in
    (L.Eq_int (al, bl), L.Eq_int (ar, br))
  | Int_if_cong (c, y, n) ->
    let cl, cr = endpoints c in
    let yl, yr = endpoints y in
    let nl, nr = endpoints n in
    (L.Int_if (cl, yl, nl), L.Int_if (cr, yr, nr))
  | Bool_if_cong (c, y, n) ->
    let cl, cr = endpoints c in
    let yl, yr = endpoints y in
    let nl, nr = endpoints n in
    (L.Bool_if (cl, yl, nl), L.Bool_if (cr, yr, nr))
  | Rule (_, rule, subst) ->
    (R.instantiate rule.lhs subst, R.instantiate rule.rhs subst)

let[@def] (left @ total) (proof : evidence @ immutable) =
  let a, _ = endpoints proof in a

let[@def] (right @ total) (proof : evidence @ immutable) =
  let _, b = endpoints proof in b

let[@def] rec (valid @ total) (rules : R.t @ immutable)
    (proof : evidence @ immutable) = ghost_ (
  match proof with
  | Refl expr -> (match L.sort expr with Some _ -> true | None -> false)
  | Sym child -> valid rules child
  | Trans (first, second) ->
    valid rules first && valid rules second && right first === left second
  | Add_cong (a, b) | Eq_cong (a, b) ->
    valid rules a && valid rules b && L.sort (left a) === Some L.Integer &&
    L.sort (left b) === Some L.Integer
  | Int_if_cong (c, y, n) ->
    valid rules c && valid rules y && valid rules n &&
    L.sort (left c) === Some L.Boolean &&
    L.sort (left y) === Some L.Integer &&
    L.sort (left n) === Some L.Integer
  | Bool_if_cong (c, y, n) ->
    valid rules c && valid rules y && valid rules n &&
    L.sort (left c) === Some L.Boolean &&
    L.sort (left y) === Some L.Boolean &&
    L.sort (left n) === Some L.Boolean
  | Rule (index, rule, subst) ->
    R.lookup_rule rules index === Some rule &&
    R.rule_valid rule && R.subst_valid rule.vars subst)
