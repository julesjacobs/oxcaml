(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A REAL binary search tree as a FUNCTOR over an ordered element type,
   with its external model a Lean SET carried by the [via] abstraction
   machinery -- Set.Make-style, properly ML-moduled.

   The pieces, and where each lives given vox's constraints:

   - [ORD] is the functor ARGUMENT signature.  Its element type carries
     a SORT ([@@vox.sort int]): the element's logical model is an int
     key, so the order is [Int]'s [<] (whose laws grind knows natively).
     [compare]'s refinement is the ORDERED CONTRACT -- it ties the sign
     of the result to that order, an obligation every instantiation must
     discharge.

   - The Lean MODEL lives at FILE TOP LEVEL, not inside the functor:
     [%%vox.lean] blocks nested in a module/functor body are dropped
     from the solver input (block collection is unit-level).  So the set
     model, the tree BST invariant, the abstraction [elems], and the
     search/insert lemmas are all declared here.  The datatype [tree]
     lives inside the functor but emits at the unit name
     [Vox_Lean_functor_bst_tree], which the top-level block references.

   - [Make]'s RESULT signature [SET] hides the tree: [t : refines(iset)]
     exposes only the set sort.  The .ml implements [t] as
     [tree{ bst _ } via (elems : iset)] and PROVES its specs honestly
     (zero [assume_unchecked_]) through a [refine_] unpack.

   - The set is modelled as a CHARACTERISTIC FUNCTION [Int -> Prop], so
     the abstraction bridge [elems (tins x t) = ins x (elems t)] is a
     one-line [funext] + the tree-level [tmem_tins].

   - [IntOrd] instantiates at [int]: its [compare] proves the ORDERED
     contract against [Int]'s [<].  A client then proves membership
     THROUGH the sealed abstraction, never seeing the tree. *)

type iset [@@vox.sort lean "ISet"]

module type ORD = sig
  type t [@@vox.sort int]
  val compare : (x : t) -> (y : t)
    -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) }
end

[%%vox.lean {lean|
-- the exported SET model: a set of int keys as a characteristic function
def ISet := Int -> Prop
@[grind, expose] def mem_s (x : Int) (s : ISet) : Prop := s x
@[grind, expose] def ins (x : Int) (s : ISet) : ISet := fun y => y = x ∨ s y

-- the tree model over int-keyed elements
@[grind] def tmem (x : Int) : Vox_Lean_functor_bst_tree -> Prop
  | .Leaf => False
  | .Node l v r => x = v ∨ tmem x l ∨ tmem x r

@[grind] def all_lt : Vox_Lean_functor_bst_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v < b ∧ all_lt l b ∧ all_lt r b

@[grind] def all_gt : Vox_Lean_functor_bst_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v > b ∧ all_gt l b ∧ all_gt r b

@[grind] def bst : Vox_Lean_functor_bst_tree -> Prop
  | .Leaf => True
  | .Node l v r => all_lt l v ∧ all_gt r v ∧ bst l ∧ bst r

-- the abstraction function: a tree's element set is its membership
@[grind, expose] def elems (t : Vox_Lean_functor_bst_tree) : ISet := fun x => tmem x t

-- the model insert, mirroring the code
@[grind] def tins (x : Int) : Vox_Lean_functor_bst_tree -> Vox_Lean_functor_bst_tree
  | .Leaf => .Node .Leaf x .Leaf
  | .Node l v r =>
      if x = v then .Node l v r
      else if x < v then .Node (tins x l) v r
      else .Node l v (tins x r)

-- one-path search completeness, from Int's order laws (native to grind)
theorem not_mem_lt (x b : Int) (t : Vox_Lean_functor_bst_tree)
    (h : all_lt t b) (hx : b <= x) : ¬ tmem x t := by
  induction t <;> grind
grind_pattern not_mem_lt => tmem x t, all_lt t b

theorem not_mem_gt (x b : Int) (t : Vox_Lean_functor_bst_tree)
    (h : all_gt t b) (hx : x <= b) : ¬ tmem x t := by
  induction t <;> grind
grind_pattern not_mem_gt => tmem x t, all_gt t b

-- insert preserves the ordering invariant
theorem all_lt_tins (x b : Int) (t : Vox_Lean_functor_bst_tree)
    (h : all_lt t b) (hx : x < b) : all_lt (tins x t) b := by
  induction t <;> grind
grind_pattern all_lt_tins => all_lt (tins x t) b

theorem all_gt_tins (x b : Int) (t : Vox_Lean_functor_bst_tree)
    (h : all_gt t b) (hx : b < x) : all_gt (tins x t) b := by
  induction t <;> grind
grind_pattern all_gt_tins => all_gt (tins x t) b

theorem bst_tins (x : Int) (t : Vox_Lean_functor_bst_tree)
    (h : bst t) : bst (tins x t) := by
  induction t <;> grind
grind_pattern bst_tins => bst (tins x t)

-- insert's effect on membership (tree level)
theorem tmem_tins (x y : Int) (t : Vox_Lean_functor_bst_tree) :
    tmem y (tins x t) = (y = x ∨ tmem y t) := by
  induction t <;> grind
grind_pattern tmem_tins => tmem y (tins x t)

-- bridges: the abstraction commutes with the set operations
@[grind] theorem mem_s_elems (x : Int) (t : Vox_Lean_functor_bst_tree) :
    mem_s x (elems t) = tmem x t := by simp only [mem_s, elems]

@[grind] theorem mem_s_ins (x : Int) (s : ISet) : mem_s x (ins x s) := by
  simp [mem_s, ins]

@[grind] theorem elems_tins (x : Int) (t : Vox_Lean_functor_bst_tree) :
    elems (tins x t) = ins x (elems t) := by
  funext y; exact tmem_tins x y t
|lean}]

module type SET = sig
  type elt
  type t : value refines (iset)
  val empty : t{ forall_ y. not (mem_s y _) }
  val add : (x : elt) -> (s : t) -> t{ _ = ins x s }
  val mem : (x : elt) -> (s : t) -> bool{ _ = mem_s x s }
end

module Make (Ord : ORD) : SET with type elt = Ord.t = struct
  type elt = Ord.t
  type tree = Leaf | Node of tree * Ord.t * tree
  type t = tree{ bst _ } [@vox.via (elems : iset)]

  let empty : t{ forall_ y. not (mem_s y _) } =
    (Leaf : t{ forall_ y. not (mem_s y _) })

  (* recursive ordered insert on the plain tree; result equals the model
     [tins] and preserves [bst].  Recursive results are bound (an
     anonymous call result's refinement is unreachable). *)
  let rec go : (x : Ord.t) -> (u : tree{ bst _ })
      -> tree{ _ = tins x u && bst _ } =
    fun x u ->
      match u with
      | Leaf -> Node (Leaf, x, Leaf)
      | Node (l, v, r) ->
        let c = Ord.compare x v in
        if c = 0 then u
        else if c < 0 then
          let l2 = go x l in
          Node (l2, v, r)
        else
          let r2 = go x r in
          Node (l, v, r2)

  (* [add] unpacks the image binder to its tree ([bst t0], link
     [elems t0 = s]), inserts, and re-wraps.  The inserted tree is bound
     at the SKELETON sort before injection into the via image: injecting
     a bare call result mis-sorts its skeleton self-fact. *)
  let add : (x : Ord.t) -> (s : t) -> t{ _ = ins x s } =
    fun x s ->
      let refine_ t0 = s in
      let u2 : tree{ _ = tins x t0 && bst _ } = go x t0 in
      (u2 : t{ _ = ins x s })

  let mem : (x : Ord.t) -> (s : t) -> bool{ _ = mem_s x s } =
    fun x s ->
      let refine_ t0 = s in
      let rec search : (u : tree{ bst _ }) -> bool{ _ = tmem x u } =
        fun u ->
          match u with
          | Leaf -> false
          | Node (l, v, r) ->
            let c = Ord.compare x v in
            if c = 0 then true
            else if c < 0 then search l
            else search r
      in
      (search t0 : bool{ _ = mem_s x s })
end

(* instantiation at int: [compare] proves the ORDERED contract against
   Int's [<] -- honestly, no assumption *)
module IntOrd = struct
  type t = int
  let compare : (x : int) -> (y : int)
      -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) } =
    fun x y -> if x < y then -1 else if x = y then 0 else 1
end

module IntSet = Make (IntOrd)

(* a client proves a membership fact THROUGH the instantiated
   abstraction, with no view of the tree: [add x s = ins x s], and
   [mem_s x (ins x s)] closes by the exported bridge *)
let member_after_add : (x : int) -> (s : IntSet.t) -> IntSet.t{ mem_s x _ } =
  fun x s -> IntSet.add x s
