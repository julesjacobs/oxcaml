(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Soundness of the functor + via boundary: a sealed functor whose
   result signature CLAIMS MORE than the implementation proves is
   rejected at the implementation's re-wrap VC.  The abstract-refines
   vs concrete-via reconciliation is fail-closed across a functor
   result ascription, exactly as for a flat sealed unit
   (mechanics/lean_via_boundary_fail.ml).  Here [add]'s result
   signature promises the element inserted TWICE; the honest insert
   inserts it once, and the boundary VC
   [elems (tins x t0) = ins x (ins x s)] is refuted. *)

type iset [@@vox.sort lean "ISet"]

module type ORD = sig
  type t [@@vox.sort int]
  val compare : (x : t) -> (y : t)
    -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) }
end

[%%vox.lean {lean|
def ISet := Int -> Prop
@[grind, expose] def mem_s (x : Int) (s : ISet) : Prop := s x
@[grind, expose] def ins (x : Int) (s : ISet) : ISet := fun y => y = x ∨ s y

@[grind] def tmem (x : Int) : Vox_tree -> Prop
  | .Leaf => False
  | .Node l v r => x = v ∨ tmem x l ∨ tmem x r

@[grind] def all_lt : Vox_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v < b ∧ all_lt l b ∧ all_lt r b

@[grind] def all_gt : Vox_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v > b ∧ all_gt l b ∧ all_gt r b

@[grind] def bst : Vox_tree -> Prop
  | .Leaf => True
  | .Node l v r => all_lt l v ∧ all_gt r v ∧ bst l ∧ bst r

@[grind, expose] def elems (t : Vox_tree) : ISet := fun x => tmem x t

@[grind] def tins (x : Int) : Vox_tree -> Vox_tree
  | .Leaf => .Node .Leaf x .Leaf
  | .Node l v r =>
      if x = v then .Node l v r
      else if x < v then .Node (tins x l) v r
      else .Node l v (tins x r)

theorem all_lt_tins (x b : Int) (t : Vox_tree)
    (h : all_lt t b) (hx : x < b) : all_lt (tins x t) b := by
  induction t <;> grind
grind_pattern all_lt_tins => all_lt (tins x t) b

theorem all_gt_tins (x b : Int) (t : Vox_tree)
    (h : all_gt t b) (hx : b < x) : all_gt (tins x t) b := by
  induction t <;> grind
grind_pattern all_gt_tins => all_gt (tins x t) b

theorem bst_tins (x : Int) (t : Vox_tree) (h : bst t) : bst (tins x t) := by
  induction t <;> grind
grind_pattern bst_tins => bst (tins x t)

theorem tmem_tins (x y : Int) (t : Vox_tree) :
    tmem y (tins x t) = (y = x ∨ tmem y t) := by
  induction t <;> grind
grind_pattern tmem_tins => tmem y (tins x t)

@[grind] theorem elems_tins (x : Int) (t : Vox_tree) :
    elems (tins x t) = ins x (elems t) := by
  funext y; exact tmem_tins x y t
|lean}]

module type SET = sig
  type elt
  type t : value refines (iset)
  val add : (x : elt) -> (s : t) -> t{ _ = ins x (ins x s) }
end

module Make (Ord : ORD) : SET with type elt = Ord.t = struct
  type elt = Ord.t
  type tree = Leaf | Node of tree * Ord.t * tree
  type t = tree{ bst _ } [@vox.via (elems : iset)]

  let rec go : (x : Ord.t) -> (u : tree{ bst _ })
      -> tree{ _ = tins x u && bst _ } =
    fun x u ->
      match u with
      | Leaf -> Node (Leaf, x, Leaf)
      | Node (l, v, r) ->
        let c = Ord.compare x v in
        if c = 0 then u
        else if c < 0 then let l2 = go x l in Node (l2, v, r)
        else let r2 = go x r in Node (l, v, r2)

  let add : (x : Ord.t) -> (s : t) -> t{ _ = ins x (ins x s) } =
    fun x s ->
      let refine_ t0 = s in
      let u2 : tree{ _ = tins x t0 && bst _ } = go x t0 in
      (u2 : t{ _ = ins x (ins x s) })
end

[%%expect{|
type iset
module type ORD =
  sig
    type t
    val compare :
      (x : t) ->
      (y : t) ->
      int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) }
  end
module type SET =
  sig
    type elt
    type t
    val add : (x : elt) -> (s : t) -> t{ _ = ins x (ins x s) }
  end
Line 89, characters 7-9:
89 |       (u2 : t{ _ = ins x (ins x s) })
            ^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: bst u2 && elems u2 = ins x (ins x s)
Hypotheses:
  u2 = tins x t0 && bst u2
  bst t0
  elems t0 = s
(lean: error: `grind` failed)
|}]
