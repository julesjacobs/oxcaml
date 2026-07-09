(* The compiler's OWN Patricia representation, verified: BIG-endian
   (branch on the highest differing bit), with the prefix and the
   branching bit PACKED into one int exactly as
   middle_end/flambda2/algorithms/patricia_tree.ml packs them --
   [Branch] carries a single [prefix_and_bit], [pack] is [lor], and
   [unpack] recovers the bit with the compiler's [x land (-x)] trick.
   Where the little-endian toy (ptrie.mli) stores the pair unpacked,
   here the invariant itself rides the packed int: for a well-formed
   node [0 < x], the branching bit IS [lbit x] (lowest set bit) and
   the prefix IS [x - lbit x] -- both theorems, not fields.

   The bit operations are modelled arithmetically, as in ptrie.mli:
   under floor division an integer is its infinite two's-complement
   bit string.  One consequence is honest scope: keys are NONNEGATIVE
   (a negative and a nonnegative key differ at every sufficiently
   high bit, so no highest differing bit exists on ideal integers --
   the compiler's sign bit is a property of its finite word, and
   width is outside the model, as always). *)

type t =
  | Empty
  | Leaf of int
  | Branch of int * t * t

[%%vox.lean {lean|
-- Bit-level theory, BIG-endian: [hmask i b] keeps the bits of [i]
-- strictly ABOVE the power-of-two bit [b]; [zbit i b] says bit [b] of
-- [i] is clear.  Euclidean / and % are floor semantics for positive
-- divisors, i.e. exactly infinite two's complement.
@[expose] public def hmask (i b : Int) : Int := i - i % (2*b)

@[expose] public def zbit (i b : Int) : Prop := i % (2*b) < b

public instance (i b : Int) : Decidable (zbit i b) := by unfold zbit; infer_instance

public inductive isbit : Int -> Prop where
  | one : isbit 1
  | dbl : {b : Int} -> isbit b -> isbit (2*b)

-- Highest differing bit of two distinct NONNEGATIVE integers (junk 1
-- when equal or when either is negative: two integers of opposite
-- sign differ at every sufficiently high bit, so no highest one
-- exists on ideal integers).
@[expose] public def hbit (p0 p1 : Int) : Int :=
  if p0 < 0 ∨ p1 < 0 ∨ p0/2 = p1/2 then 1
  else 2 * hbit (p0/2) (p1/2)
termination_by (p0.natAbs + p1.natAbs)
decreasing_by omega

-- Lowest set bit of a POSITIVE integer (junk 1 otherwise): the
-- arithmetic meaning of the compiler's [x land (-x)].
@[expose] public def lbit (x : Int) : Int :=
  if x ≤ 0 then 1
  else if x % 2 = 1 then 1
  else 2 * lbit (x/2)
termination_by x.natAbs
decreasing_by omega

-- [pack]'s disjointness precondition, as a named Prop the surface
-- contract can state.
@[grind, expose] public def packmod (p b : Int) : Prop := p % (2*b) = 0

-- Tree model: naive membership over the WHOLE tree, and the Patricia
-- invariant over the PACKED node int [x]: [lbit x] is the branching
-- bit and [x - lbit x] the prefix (bits strictly above it) -- their
-- shape facts ([isbit], disjointness, nonnegativity) are the [lbit]
-- theorems above, so the invariant itself only demands [0 < x].
-- Keys are NONNEGATIVE: on ideal integers a negative and a
-- nonnegative key differ at every sufficiently high bit, so no
-- highest differing bit exists (the compiler's finite word makes the
-- sign bit an ordinary bit; width is outside the model, as always).
@[grind, expose] public def mem : Int -> Vox_Ptrie_packed_t -> Prop
  | _, .Empty => False
  | i, .Leaf j => i = j
  | i, .Branch _ t0 t1 => mem i t0 ∨ mem i t1

@[grind, expose] public def allmatch : Vox_Ptrie_packed_t -> Int -> Int -> Prop
  | .Empty, _, _ => True
  | .Leaf j, p, b => hmask j b = p
  | .Branch _ t0 t1, p, b => allmatch t0 p b ∧ allmatch t1 p b

@[grind, expose] public def allzero : Vox_Ptrie_packed_t -> Int -> Prop
  | .Empty, _ => True
  | .Leaf j, b => zbit j b
  | .Branch _ t0 t1, b => allzero t0 b ∧ allzero t1 b

@[grind, expose] public def allone : Vox_Ptrie_packed_t -> Int -> Prop
  | .Empty, _ => True
  | .Leaf j, b => ¬ zbit j b
  | .Branch _ t0 t1, b => allone t0 b ∧ allone t1 b

@[grind, expose] public def allpos : Vox_Ptrie_packed_t -> Prop
  | .Empty => True
  | .Leaf j => 0 ≤ j
  | .Branch _ t0 t1 => allpos t0 ∧ allpos t1

@[grind, expose] public def trie : Vox_Ptrie_packed_t -> Prop
  | .Empty => True
  | .Leaf j => 0 ≤ j
  | .Branch x t0 t1 =>
      0 < x ∧
      allmatch t0 (x - lbit x) (lbit x) ∧
      allmatch t1 (x - lbit x) (lbit x) ∧
      allzero t0 (lbit x) ∧ allone t1 (lbit x) ∧
      trie t0 ∧ trie t1

@[grind, expose] public def join (p0 : Int) (t0 : Vox_Ptrie_packed_t)
    (p1 : Int) (t1 : Vox_Ptrie_packed_t) : Vox_Ptrie_packed_t :=
  if zbit p0 (hbit p0 p1)
  then .Branch (hmask p0 (hbit p0 p1) + hbit p0 p1) t0 t1
  else .Branch (hmask p0 (hbit p0 p1) + hbit p0 p1) t1 t0

@[grind, expose] public def insert (i : Int) : Vox_Ptrie_packed_t -> Vox_Ptrie_packed_t
  | .Empty => .Leaf i
  | .Leaf j => if i = j then .Leaf i else join i (.Leaf i) j (.Leaf j)
  | .Branch x t0 t1 =>
      if hmask i (lbit x) = x - lbit x then
        if zbit i (lbit x) then .Branch x (insert i t0) t1
        else .Branch x t0 (insert i t1)
      else join i (.Leaf i) (x - lbit x) (.Branch x t0 t1)

-- Insertion adds exactly one key ...
public axiom mem_insert (x y : Int) (t : Vox_Ptrie_packed_t) :
    mem y (insert x t) ↔ (y = x ∨ mem y t)
grind_pattern mem_insert => mem y (insert x t)
|lean}]

(* The API type is the refined abbreviation: a set IS a packed tree
   satisfying the Patricia invariant. *)
type set = t{ trie _ }

val empty : set{ _ = Empty }

(* One-path search, proved equal to the model membership that
   quantifies the WHOLE tree. *)
val mem : (i : int) -> (s : set) -> bool{ _ = mem i s }

(* Insertion returns exactly the model's insert; [mem_insert] then
   characterizes the result completely at every client. *)
val insert :
  (i : int{ 0 <= _ }) -> (s : set) -> set{ _ = insert i s && mem i _ }
