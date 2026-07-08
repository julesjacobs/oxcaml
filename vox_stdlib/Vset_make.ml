(* Pays Vset_make.mli's SET contract over a CONCRETE ordered BST, keyed by
   the functor argument [O.t].  The tree lives inside [Make]; it emits at
   the unit name [Vox_Vset_make_tree], which the top-level block references
   (blocks are unit-level -- they cannot live inside the functor).  [t] is
   [tree{ bst _ } via (elems : iset)]; every spec is proved honestly
   through a [refine_] unpack (zero [assume_unchecked_]).  The set is a
   characteristic function, so the abstraction [elems t := fun x => tmem x t]
   commutes with [ins]/[empty_s] by [funext] over the tree lemmas. *)

type iset [@@vox.sort lean "ISet"]

module type ORD = sig
  type t [@@vox.sort int]
  val compare : (x : t) -> (y : t)
    -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) }
end

[%%vox.lean {lean|
-- client-facing set model (re-stated from the .mli; dual authoring)
abbrev ISet := Int -> Prop
@[grind, expose] def mem_s (x : Int) (s : ISet) : Prop := s x
@[grind, expose] def ins (x : Int) (s : ISet) : ISet := fun y => y = x ∨ s y
@[grind, expose] def empty_s : ISet := fun _ => False

-- the tree model over int-keyed elements (bst carries the ordering invariant)
@[grind] def tmem (x : Int) : Vox_Vset_make_tree -> Prop
  | .Leaf => False
  | .Node l v r => x = v ∨ tmem x l ∨ tmem x r

@[grind] def all_lt : Vox_Vset_make_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v < b ∧ all_lt l b ∧ all_lt r b

@[grind] def all_gt : Vox_Vset_make_tree -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v > b ∧ all_gt l b ∧ all_gt r b

@[grind] def bst : Vox_Vset_make_tree -> Prop
  | .Leaf => True
  | .Node l v r => all_lt l v ∧ all_gt r v ∧ bst l ∧ bst r

-- abstraction: a tree's element set is its membership char-function
@[grind, expose] def elems (t : Vox_Vset_make_tree) : ISet := fun x => tmem x t

@[grind] def tins (x : Int) : Vox_Vset_make_tree -> Vox_Vset_make_tree
  | .Leaf => .Node .Leaf x .Leaf
  | .Node l v r =>
      if x = v then .Node l v r
      else if x < v then .Node (tins x l) v r
      else .Node l v (tins x r)

-- one-path search completeness, from Int's order laws (grind-native)
theorem not_mem_lt (x b : Int) (t : Vox_Vset_make_tree)
    (h : all_lt t b) (hx : b <= x) : ¬ tmem x t := by
  induction t <;> grind
grind_pattern not_mem_lt => tmem x t, all_lt t b

theorem not_mem_gt (x b : Int) (t : Vox_Vset_make_tree)
    (h : all_gt t b) (hx : x <= b) : ¬ tmem x t := by
  induction t <;> grind
grind_pattern not_mem_gt => tmem x t, all_gt t b

theorem all_lt_tins (x b : Int) (t : Vox_Vset_make_tree)
    (h : all_lt t b) (hx : x < b) : all_lt (tins x t) b := by
  induction t <;> grind
grind_pattern all_lt_tins => all_lt (tins x t) b

theorem all_gt_tins (x b : Int) (t : Vox_Vset_make_tree)
    (h : all_gt t b) (hx : b < x) : all_gt (tins x t) b := by
  induction t <;> grind
grind_pattern all_gt_tins => all_gt (tins x t) b

theorem bst_tins (x : Int) (t : Vox_Vset_make_tree)
    (h : bst t) : bst (tins x t) := by
  induction t <;> grind
grind_pattern bst_tins => bst (tins x t)

theorem tmem_tins (x y : Int) (t : Vox_Vset_make_tree) :
    tmem y (tins x t) = (y = x ∨ tmem y t) := by
  induction t <;> grind
grind_pattern tmem_tins => tmem y (tins x t)

-- bridges: the abstraction commutes with the set operations
@[grind] theorem mem_s_elems (x : Int) (t : Vox_Vset_make_tree) :
    mem_s x (elems t) = tmem x t := by simp only [mem_s, elems]

@[grind] theorem elems_empty : elems .Leaf = empty_s := by
  funext y; simp only [elems, tmem, empty_s]

@[grind] theorem elems_tins (x : Int) (t : Vox_Vset_make_tree) :
    elems (tins x t) = ins x (elems t) := by
  funext y; simp only [elems, ins]; exact tmem_tins x y t

-- the client algebra laws, discharged over the concrete char-function defs
-- (beta-reduced by simp; grind alone will not reduce the point-update).
theorem mem_s_ins (x : Int) (s : ISet) : mem_s x (ins x s) := by
  simp only [mem_s, ins]; grind
grind_pattern mem_s_ins => mem_s x (ins x s)
theorem mem_s_ins_ne (x y : Int) (s : ISet) (h : x ≠ y) :
    mem_s y (ins x s) = mem_s y s := by
  simp only [mem_s, ins]; grind
grind_pattern mem_s_ins_ne => mem_s y (ins x s)
theorem mem_s_empty (x : Int) : ¬ mem_s x empty_s := by
  simp only [mem_s, empty_s]; grind
grind_pattern mem_s_empty => mem_s x empty_s
|lean}]

module type SET = sig
  type elt
  type t : value refines (iset)
  val empty : (u : unit) -> t{ _ = empty_s }
  val add : (x : elt) -> (s : t) -> t{ _ = ins x s }
  val mem : (x : elt) -> (s : t) -> bool{ _ = mem_s x s }
  val singleton : (x : elt) -> t{ _ = ins x empty_s }
end

module Make (O : ORD) : SET with type elt = O.t = struct
  type elt = O.t
  type tree = Leaf | Node of tree * O.t * tree
  type t = tree{ bst _ } [@vox.via (elems : iset)]

  let empty : (u : unit) -> t{ _ = empty_s } =
    fun _ -> (Leaf : t{ _ = empty_s })

  (* recursive ordered insert; result equals the model [tins] and preserves
     [bst].  Recursive results are bound (an anonymous call result's
     refinement is unreachable). *)
  let rec go : (x : O.t) -> (u : tree{ bst _ })
      -> tree{ _ = tins x u && bst _ } =
    fun x u ->
      match u with
      | Leaf -> Node (Leaf, x, Leaf)
      | Node (l, v, r) ->
        let c = O.compare x v in
        if c = 0 then u
        else if c < 0 then
          let l2 = go x l in
          Node (l2, v, r)
        else
          let r2 = go x r in
          Node (l, v, r2)

  let add : (x : O.t) -> (s : t) -> t{ _ = ins x s } =
    fun x s ->
      let refine_ t0 = s in
      let u2 : tree{ _ = tins x t0 && bst _ } = go x t0 in
      (u2 : t{ _ = ins x s })

  let mem : (x : O.t) -> (s : t) -> bool{ _ = mem_s x s } =
    fun x s ->
      let refine_ t0 = s in
      let rec search : (u : tree{ bst _ }) -> bool{ _ = tmem x u } =
        fun u ->
          match u with
          | Leaf -> false
          | Node (l, v, r) ->
            let c = O.compare x v in
            if c = 0 then true
            else if c < 0 then search l
            else search r
      in
      (search t0 : bool{ _ = mem_s x s })

  (* singleton = insert into empty; reuses [go]'s tins-spec and the empty
     bridge, so the postcondition [_ = ins x empty_s] follows without a new
     lemma. *)
  let singleton : (x : O.t) -> t{ _ = ins x empty_s } =
    fun x ->
      let u2 : tree{ _ = tins x Leaf && bst _ } = go x Leaf in
      (u2 : t{ _ = ins x empty_s })
end
