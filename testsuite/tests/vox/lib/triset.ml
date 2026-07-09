(* Experiment: a set FACE over the compiler's own Patricia trie
   (lib/ptrie), retrofitted with [via].  [t] is [Ptrie.t{ trie _ }]
   whose via image is its set of elements ([elemset]); the interface
   speaks pure set vocabulary ([smem]/[addspec]), so this trie and
   the page's BST (lib/via_set) present the SAME signature and are
   interchangeable behind [refines (iset)].

   The retrofit reuses ptrie's ENTIRE bit-level theory: the bridge
   [smem x (elemset t) <-> Ptrie.mem x t] is proved by ordinary
   structural induction (both sides range over the whole tree), and
   ptrie's [mem_insert] then carries insertion's spec across.  No new
   bit-level reasoning. *)

open Ptrie

type iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
inductive ISet where
  | snil : ISet
  | scons : Int -> ISet -> ISet

@[grind] def smem (x : Int) : ISet -> Prop
  | .snil => False
  | .scons y s => x = y ∨ smem x s

@[grind] def sunion : ISet -> ISet -> ISet
  | .snil, s => s
  | .scons x xs, s => .scons x (sunion xs s)

-- The abstraction function: the elements of a trie, as a set.
@[grind] def elemset : Vox_Ptrie_t -> ISet
  | .Empty => .snil
  | .Leaf j => .scons j .snil
  | .Branch _ _ t0 t1 => sunion (elemset t0) (elemset t1)

@[grind] theorem smem_sunion (x : Int) (a b : ISet) :
    smem x (sunion a b) = (smem x a ∨ smem x b) := by
  induction a <;> grind

-- THE bridge, reusing Ptrie.mem: set membership over the abstraction
-- equals ptrie's whole-tree membership.  Structural induction only.
@[grind] theorem smem_elemset (x : Int) (t : Vox_Ptrie_t) :
    smem x (elemset t) = mem x t := by
  induction t <;> grind
grind_pattern smem_elemset => smem x (elemset t)

-- Extensional specs (the list model has structural [=], not set [=], so
-- membership-based specs are the honest set vocabulary here).
@[grind] def isempty (s : ISet) : Prop := ∀ y, ¬ smem y s
@[grind] def addspec (r : ISet) (x : Int) (s : ISet) : Prop :=
  ∀ y, smem y r = (y = x ∨ smem y s)
|lean}]

type t = Ptrie.t{ trie _ } [@vox.via (elemset : iset)]

let mem : (x : int) -> (s : t) -> bool{ _ = smem x s } =
  fun x s ->
    let refine_ t0 = s in
    Ptrie.mem x t0

(* [empty] is the empty set; the trie [Empty] denotes [snil].  It is a
   function (not a value): a top-level via-typed VALUE binding records a
   mis-sorted definitional fact (image name = skeleton rhs), so via
   values are produced only inside function bodies here (see findings). *)
let empty : (u : unit) -> t{ isempty _ } =
  fun u -> (Empty : t{ isempty _ })

(* [add] wraps [Ptrie.insert].  Its result is an opaque trie, and a via
   value must be entered as an INLINE constructor (coercing an opaque
   skeleton value mis-sorts its subject at the image sort -- see
   findings), so the wrapped result is re-matched into constructors.
   [addspec] then composes over ptrie's [mem_insert] via the bridge. *)
let add : (x : int) -> (s : t) -> t{ addspec _ x s } =
  fun x s ->
    let refine_ t0 = s in
    let r = Ptrie.insert x t0 in
    (match r with
     | Empty -> (Empty : t{ addspec _ x s })
     | Leaf j -> (Leaf j : t{ addspec _ x s })
     | Branch (p, b, l, rr) -> (Branch (p, b, l, rr) : t{ addspec _ x s }))
