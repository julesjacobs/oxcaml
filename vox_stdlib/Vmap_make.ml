(* Pays Vmap_make.mli's MAP contract over a CONCRETE ordered BST keyed by
   the functor argument [O.t], with int values.  The tree lives inside
   [Make] and emits at the unit name [Vox_Vmap_make_tree], which the
   top-level block references.  [t] is [tree via (mmap : mmap)]; the map is
   a characteristic function, so the abstraction [mmap t := fun k => tfind k t]
   commutes with [m_add]/[m_empty] by [funext] over the tree lemma
   [tfind_tins].  No BST invariant is carried: the MODEL [tfind] navigates
   by the same ordered comparison the impl uses, so [tfind_tins] holds
   structurally (deterministic navigation) -- exactly the calibration that a
   whole-tree membership model (the set demo) needs [bst] for and an
   ordered-navigation model does not.  Every spec is proved honestly
   through a [refine_] unpack (zero [assume_unchecked_]). *)

type mmap [@@vox.sort lean "MMap"]
type mopt = MNone | MSome of int

module type ORD = sig
  type t [@@vox.sort int]
  val compare : (x : t) -> (y : t)
    -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) }
end

[%%vox.lean {lean|
-- client-facing map model (re-stated from the .mli; dual authoring)
abbrev MMap := Int -> Vox_Vmap_make_mopt
@[grind, expose] def m_find (k : Int) (m : MMap) : Vox_Vmap_make_mopt := m k
@[grind, expose] def m_empty : MMap := fun _ => .MNone
@[grind, expose] def m_add (k : Int) (v : Int) (m : MMap) : MMap :=
  fun k' => if k' = k then .MSome v else m k'
@[grind, expose] def m_haskey (k : Int) (m : MMap) : Prop :=
  m_find k m ≠ .MNone
-- the tree model over int-keyed entries (key, value both Int)
@[grind] def tfind (k : Int) : Vox_Vmap_make_tree -> Vox_Vmap_make_mopt
  | .Leaf => .MNone
  | .Node l k' v r =>
      if k = k' then .MSome v else if k < k' then tfind k l else tfind k r

@[grind, expose] def t_haskey (k : Int) (t : Vox_Vmap_make_tree) : Prop :=
  tfind k t ≠ .MNone

@[grind] def tins (k : Int) (v : Int) : Vox_Vmap_make_tree -> Vox_Vmap_make_tree
  | .Leaf => .Node .Leaf k v .Leaf
  | .Node l k' v' r =>
      if k = k' then .Node l k v r
      else if k < k' then .Node (tins k v l) k' v' r
      else .Node l k' v' (tins k v r)

-- abstraction: a tree's map is its keyed lookup
@[grind, expose] def mmap (t : Vox_Vmap_make_tree) : MMap := fun k => tfind k t

-- the key lemma: insert's effect on lookup (holds structurally, no bst --
-- both tfind and tins navigate by the same ordered comparison)
theorem tfind_tins (k k' v : Int) (t : Vox_Vmap_make_tree) :
    tfind k' (tins k v t) = (if k' = k then .MSome v else tfind k' t) := by
  induction t <;> grind
grind_pattern tfind_tins => tfind k' (tins k v t)

-- bridges: the abstraction commutes with the map operations
@[grind] theorem m_find_mmap (k : Int) (t : Vox_Vmap_make_tree) :
    m_find k (mmap t) = tfind k t := by simp only [m_find, mmap]

@[grind] theorem mmap_empty : mmap .Leaf = m_empty := by
  funext k; simp only [mmap, tfind, m_empty]

@[grind] theorem mmap_tins (k v : Int) (t : Vox_Vmap_make_tree) :
    mmap (tins k v t) = m_add k v (mmap t) := by
  funext k'; simp only [mmap, m_add]; exact tfind_tins k k' v t

-- the client algebra laws, discharged over the concrete char-function defs
-- (beta-reduced by simp; grind alone will not reduce the point-update).
theorem m_find_empty (k : Int) : m_find k m_empty = .MNone := by
  simp only [m_find, m_empty]
grind_pattern m_find_empty => m_find k m_empty
theorem m_find_add_eq (k v : Int) (m : MMap) :
    m_find k (m_add k v m) = .MSome v := by
  simp only [m_find, m_add]; grind
grind_pattern m_find_add_eq => m_find k (m_add k v m)
theorem m_find_add_ne (k k' v : Int) (m : MMap) (h : k ≠ k') :
    m_find k (m_add k' v m) = m_find k m := by
  simp only [m_find, m_add]; grind
grind_pattern m_find_add_ne => m_find k (m_add k' v m)
|lean}]

module type MAP = sig
  type key
  type t : value refines (mmap)
  val empty : (u : unit) -> t{ _ = m_empty }
  val find : (k : key) -> (m : t) -> mopt{ _ = m_find k m }
  val add : (k : key) -> (v : int) -> (m : t) -> t{ _ = m_add k v m }
  val mem : (k : key) -> (m : t) -> bool{ _ = m_haskey k m }
  val singleton : (k : key) -> (v : int) -> t{ _ = m_add k v m_empty }
end

module Make (O : ORD) : MAP with type key = O.t = struct
  type key = O.t
  type tree = Leaf | Node of tree * O.t * int * tree
  type t = tree{ 0 = 0 } [@vox.via (mmap : mmap)]

  let empty : (u : unit) -> t{ _ = m_empty } =
    fun _ -> (Leaf : t{ _ = m_empty })

  let find : (k : O.t) -> (m : t) -> mopt{ _ = m_find k m } =
    fun k m ->
      let refine_ t0 = m in
      let rec go : (u : tree) -> mopt{ _ = tfind k u } =
        fun u ->
          match u with
          | Leaf -> MNone
          | Node (l, k', v, r) ->
            let c = O.compare k k' in
            if c = 0 then MSome v
            else if c < 0 then go l
            else go r
      in
      (go t0 : mopt{ _ = m_find k m })

  (* recursive ordered insert/overwrite; result equals the model [tins].
     Recursive results are bound (an anonymous call result's refinement is
     unreachable). *)
  let rec go_ins : (k : O.t) -> (v : int) -> (u : tree) -> tree{ _ = tins k v u } =
    fun k v u ->
      match u with
      | Leaf -> Node (Leaf, k, v, Leaf)
      | Node (l, k', v', r) ->
        let c = O.compare k k' in
        if c = 0 then Node (l, k, v, r)
        else if c < 0 then
          let l2 = go_ins k v l in
          Node (l2, k', v', r)
        else
          let r2 = go_ins k v r in
          Node (l, k', v', r2)

  let add : (k : O.t) -> (v : int) -> (m : t) -> t{ _ = m_add k v m } =
    fun k v m ->
      let refine_ t0 = m in
      let u2 : tree{ _ = tins k v t0 } = go_ins k v t0 in
      (u2 : t{ _ = m_add k v m })

  let mem : (k : O.t) -> (m : t) -> bool{ _ = m_haskey k m } =
    fun k m ->
      let refine_ t0 = m in
      let rec go : (u : tree) -> bool{ _ = t_haskey k u } =
        fun u ->
          match u with
          | Leaf -> false
          | Node (l, k', _, r) ->
            let c = O.compare k k' in
            if c = 0 then true
            else if c < 0 then go l
            else go r
      in
      (go t0 : bool{ _ = m_haskey k m })

  let singleton : (k : O.t) -> (v : int) -> t{ _ = m_add k v m_empty } =
    fun k v ->
      let u2 : tree{ _ = tins k v Leaf } = go_ins k v Leaf in
      (u2 : t{ _ = m_add k v m_empty })
end
