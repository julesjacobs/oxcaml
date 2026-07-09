(* A Patricia trie -- the integer-set structure the compiler itself
   runs on (middle_end/flambda2/algorithms/patricia_tree.ml), as the
   classic little-endian toy: no GADT plumbing, just [empty], [insert]
   and [mem], with the real bit twiddling ([i land b = 0],
   [i land (b - 1)], and the lowest-set-bit trick [x land (-x)] that
   appears verbatim in the compiler's [unpack]).

   The block below is the whole logical story, proved once here and
   carried to the implementation and every client through this .cmi.
   The bit operations are MODELLED arithmetically -- under floor
   division, an integer IS its infinite two's-complement bit string,
   so masking low bits is [%], testing a bit is one inequality, and
   the branching bit is the lowest differing bit.  Every fact about
   them is a theorem; nothing bit-level is axiomatized. *)

type t =
  | Empty
  | Leaf of int
  | Branch of int * int * t * t

[%%vox.lean {lean|
-- Bit-level model.  [mask i b] keeps the bits of [i] below the
-- power-of-two [b]; [zbit i b] says bit [b] of [i] is clear.
@[expose] public def mask (i b : Int) : Int := i % b

@[expose] public def zbit (i b : Int) : Prop := i % (2*b) < b

public instance (i b : Int) : Decidable (zbit i b) := by unfold zbit; infer_instance

public inductive isbit : Int -> Prop where
  | one : isbit 1
  | dbl : {b : Int} -> isbit b -> isbit (2*b)

-- Lowest differing bit of two distinct integers (junk 1 when equal).
@[expose] public def bbit (p0 p1 : Int) : Int :=
  if p0 = p1 then 1
  else if p0 % 2 = p1 % 2 then 2 * bbit (p0/2) (p1/2)
  else 1
termination_by (p0.natAbs + p1.natAbs)
decreasing_by omega

-- Tree model: naive whole-tree membership, and the Patricia invariant.
@[grind, expose] public def mem : Int -> Vox_Ptrie_t -> Prop
  | _, .Empty => False
  | i, .Leaf j => i = j
  | i, .Branch _ _ t0 t1 => mem i t0 ∨ mem i t1

@[grind, expose] public def allmatch : Vox_Ptrie_t -> Int -> Int -> Prop
  | .Empty, _, _ => True
  | .Leaf j, p, b => mask j b = p
  | .Branch _ _ t0 t1, p, b => allmatch t0 p b ∧ allmatch t1 p b

@[grind, expose] public def allzero : Vox_Ptrie_t -> Int -> Prop
  | .Empty, _ => True
  | .Leaf j, b => zbit j b
  | .Branch _ _ t0 t1, b => allzero t0 b ∧ allzero t1 b

@[grind, expose] public def allone : Vox_Ptrie_t -> Int -> Prop
  | .Empty, _ => True
  | .Leaf j, b => ¬ zbit j b
  | .Branch _ _ t0 t1, b => allone t0 b ∧ allone t1 b

@[grind, expose] public def trie : Vox_Ptrie_t -> Prop
  | .Empty => True
  | .Leaf _ => True
  | .Branch p b t0 t1 =>
      isbit b ∧ mask p b = p ∧
      allmatch t0 p b ∧ allmatch t1 p b ∧
      allzero t0 b ∧ allone t1 b ∧ trie t0 ∧ trie t1

@[grind, expose] public def join (p0 : Int) (t0 : Vox_Ptrie_t) (p1 : Int) (t1 : Vox_Ptrie_t) :
    Vox_Ptrie_t :=
  if zbit p0 (bbit p0 p1)
  then .Branch (mask p0 (bbit p0 p1)) (bbit p0 p1) t0 t1
  else .Branch (mask p0 (bbit p0 p1)) (bbit p0 p1) t1 t0

@[grind, expose] public def insert (i : Int) : Vox_Ptrie_t -> Vox_Ptrie_t
  | .Empty => .Leaf i
  | .Leaf j => if i = j then .Leaf i else join i (.Leaf i) j (.Leaf j)
  | .Branch p b t0 t1 =>
      if mask i b = p then
        if zbit i b then .Branch p b (insert i t0) t1
        else .Branch p b t0 (insert i t1)
      else join i (.Leaf i) p (.Branch p b t0 t1)

-- The one client-facing law (obligation; the .ml discharges it by
-- induction).  All the bit-algebra and invariant scaffolding that proves
-- it lives PRIVATELY in the .ml.
public axiom mem_insert (x y : Int) (t : Vox_Ptrie_t) :
    mem y (insert x t) ↔ (y = x ∨ mem y t)
grind_pattern mem_insert => mem y (insert x t)
|lean}]

(* The API type is the refined abbreviation: a set IS a tree
   satisfying the Patricia invariant. *)
type set = t{ trie _ }

val empty : set{ _ = Empty }

(* One-path search, proved equal to the model membership that
   quantifies the WHOLE tree: the invariant's lemmas bridge the path
   the code takes to the subtrees it skips. *)
val mem : (i : int) -> (s : set) -> bool{ _ = mem i s }

(* Insertion returns exactly the model's insert; [mem_insert] then
   characterizes the result completely at every client. *)
val insert : (i : int) -> (s : set) -> set{ _ = insert i s && mem i _ }
