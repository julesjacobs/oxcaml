(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A verified MAP as a FUNCTOR over an ordered, int-representable KEY type --
   Map.Make(ORD)-style, properly ML-moduled -- with its external model a Lean
   MAP (a characteristic function Int -> option) carried by [via].  The map
   companion to mechanics/lean_functor_bst.ml (which does the SET).

   - [ORD] is the functor argument: its key type carries [@@vox.sort int], so
     the order is [Int]'s [<] and [compare]'s refinement is the ORDERED
     CONTRACT every instantiation must discharge.
   - The Lean MODEL lives at FILE TOP LEVEL ([%%vox.lean] blocks are
     unit-level); the tree lives inside the functor and emits at
     [Vox_Lean_map_make_tree], referenced by the top-level block.
   - The map is modelled as a char-function [MMap := Int -> MOpt], so
     funext-equality IS map equality and [add] carries a STRUCTURAL spec
     [_ = m_add k v m] (OVERWRITE semantics -- idiomatic Map.add).
   - [IntOrd] instantiates at [int]; a client proves a lookup fact THROUGH the
     sealed abstraction, never seeing the tree. *)

type mmap [@@vox.sort lean "MMap"]
type mopt = MNone | MSome of int

module type ORD = sig
  type t [@@vox.sort int]
  val compare : (x : t) -> (y : t)
    -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) }
end

[%%vox.lean {lean|
abbrev MMap := Int -> Vox_Lean_map_make_mopt
@[grind, expose] def m_find (k : Int) (m : MMap) : Vox_Lean_map_make_mopt := m k
@[grind, expose] def m_empty : MMap := fun _ => .MNone
@[grind, expose] def m_add (k : Int) (v : Int) (m : MMap) : MMap :=
  fun k' => if k' = k then .MSome v else m k'

@[grind] def tfind (k : Int) : Vox_Lean_map_make_tree -> Vox_Lean_map_make_mopt
  | .Leaf => .MNone
  | .Node l k' v r =>
      if k = k' then .MSome v else if k < k' then tfind k l else tfind k r

@[grind] def tins (k : Int) (v : Int) :
    Vox_Lean_map_make_tree -> Vox_Lean_map_make_tree
  | .Leaf => .Node .Leaf k v .Leaf
  | .Node l k' v' r =>
      if k = k' then .Node l k v r
      else if k < k' then .Node (tins k v l) k' v' r
      else .Node l k' v' (tins k v r)

@[grind, expose] def mmap (t : Vox_Lean_map_make_tree) : MMap := fun k => tfind k t

theorem tfind_tins (k k' v : Int) (t : Vox_Lean_map_make_tree) :
    tfind k' (tins k v t) = (if k' = k then .MSome v else tfind k' t) := by
  induction t <;> grind
grind_pattern tfind_tins => tfind k' (tins k v t)

@[grind] theorem m_find_mmap (k : Int) (t : Vox_Lean_map_make_tree) :
    m_find k (mmap t) = tfind k t := by simp only [m_find, mmap]
@[grind] theorem mmap_empty : mmap .Leaf = m_empty := by
  funext k; simp only [mmap, tfind, m_empty]
@[grind] theorem mmap_tins (k v : Int) (t : Vox_Lean_map_make_tree) :
    mmap (tins k v t) = m_add k v (mmap t) := by
  funext k'; simp only [mmap, m_add]; exact tfind_tins k k' v t

@[grind] theorem m_find_add_eq (k v : Int) (m : MMap) :
    m_find k (m_add k v m) = .MSome v := by simp only [m_find, m_add]; grind
grind_pattern m_find_add_eq => m_find k (m_add k v m)
@[grind] theorem m_find_add_ne (k k' v : Int) (m : MMap) (h : k ≠ k') :
    m_find k (m_add k' v m) = m_find k m := by simp only [m_find, m_add]; grind
grind_pattern m_find_add_ne => m_find k (m_add k' v m)
|lean}]

module type MAP = sig
  type key
  type t : value refines (mmap)
  val empty : (u : unit) -> t{ _ = m_empty }
  val find : (k : key) -> (m : t) -> mopt{ _ = m_find k m }
  val add : (k : key) -> (v : int) -> (m : t) -> t{ _ = m_add k v m }
end

module Make (Ord : ORD) : MAP with type key = Ord.t = struct
  type key = Ord.t
  type tree = Leaf | Node of tree * Ord.t * int * tree
  type t = tree{ 0 = 0 } [@vox.via (mmap : mmap)]

  let empty : (u : unit) -> t{ _ = m_empty } =
    fun _ -> (Leaf : t{ _ = m_empty })

  let find : (k : Ord.t) -> (m : t) -> mopt{ _ = m_find k m } =
    fun k m ->
      let refine_ t0 = m in
      let rec go : (u : tree) -> mopt{ _ = tfind k u } =
        fun u ->
          match u with
          | Leaf -> MNone
          | Node (l, k', v, r) ->
            let c = Ord.compare k k' in
            if c = 0 then MSome v else if c < 0 then go l else go r
      in
      (go t0 : mopt{ _ = m_find k m })

  let rec go_ins : (k : Ord.t) -> (v : int) -> (u : tree)
      -> tree{ _ = tins k v u } =
    fun k v u ->
      match u with
      | Leaf -> Node (Leaf, k, v, Leaf)
      | Node (l, k', v', r) ->
        let c = Ord.compare k k' in
        if c = 0 then Node (l, k, v, r)
        else if c < 0 then let l2 = go_ins k v l in Node (l2, k', v', r)
        else let r2 = go_ins k v r in Node (l, k', v', r2)

  let add : (k : Ord.t) -> (v : int) -> (m : t) -> t{ _ = m_add k v m } =
    fun k v m ->
      let refine_ t0 = m in
      let u2 : tree{ _ = tins k v t0 } = go_ins k v t0 in
      (u2 : t{ _ = m_add k v m })
end

module IntOrd = struct
  type t = int
  let compare : (x : int) -> (y : int)
      -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) } =
    fun x y -> if x < y then -1 else if x = y then 0 else 1
end

module IntMap = Make (IntOrd)

(* a client proves a lookup fact THROUGH the instantiated abstraction:
   [find k (add k v m)] normalizes to [MSome v] by [m_find_add_eq]. *)
let find_after_add : (k : int) -> (v : int) -> (m : IntMap.t) ->
    mopt{ _ = m_find k (m_add k v m) } =
  fun k v m -> let m' = IntMap.add k v m in IntMap.find k m'
