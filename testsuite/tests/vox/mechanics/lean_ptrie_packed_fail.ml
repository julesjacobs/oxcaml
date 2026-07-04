(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* PACKED-trie soundness probe: the invariant of lib/ptrie_packed.mli
   computes on literal trees (model inlined here over this unit's own
   datatype), so a hand-built node's packed int is CHECKED, not
   trusted.  [Branch (2, ...)] decodes as prefix 0, bit 2 and is a
   well-formed set; [Branch (3, ...)] decodes as prefix 2, bit 1 --
   and no key can match a prefix that has bits at or below the
   branching bit. *)

type t =
  | Empty
  | Leaf of int
  | Branch of int * t * t

[%%vox.lean {lean|
@[grind] def hmask (i b : Int) : Int := i - i % (2*b)

@[grind] def zbit (i b : Int) : Prop := i % (2*b) < b

instance (i b : Int) : Decidable (zbit i b) := by unfold zbit; infer_instance

@[grind] def lbit (x : Int) : Int :=
  if x ≤ 0 then 1
  else if x % 2 = 1 then 1
  else 2 * lbit (x/2)
termination_by x.natAbs
decreasing_by omega

@[grind] def mem : Int -> Vox__t -> Prop
  | _, .Empty => False
  | i, .Leaf j => i = j
  | i, .Branch _ t0 t1 => mem i t0 ∨ mem i t1

@[grind] def allmatch : Vox__t -> Int -> Int -> Prop
  | .Empty, _, _ => True
  | .Leaf j, p, b => hmask j b = p
  | .Branch _ t0 t1, p, b => allmatch t0 p b ∧ allmatch t1 p b

@[grind] def allzero : Vox__t -> Int -> Prop
  | .Empty, _ => True
  | .Leaf j, b => zbit j b
  | .Branch _ t0 t1, b => allzero t0 b ∧ allzero t1 b

@[grind] def allone : Vox__t -> Int -> Prop
  | .Empty, _ => True
  | .Leaf j, b => ¬ zbit j b
  | .Branch _ t0 t1, b => allone t0 b ∧ allone t1 b

@[grind] def trie : Vox__t -> Prop
  | .Empty => True
  | .Leaf j => 0 ≤ j
  | .Branch x t0 t1 =>
      0 < x ∧
      allmatch t0 (x - lbit x) (lbit x) ∧
      allmatch t1 (x - lbit x) (lbit x) ∧
      allzero t0 (lbit x) ∧ allone t1 (lbit x) ∧
      trie t0 ∧ trie t1
|lean}]

type set = t{ trie _ }

(* Well packed: prefix 0, bit 2; key 1 rides the zero side, key 3 the
   one side. *)
let ok : set{ mem 1 _ } = Branch (2, Leaf 1, Leaf 3)
[%%expect{|
type t = Empty | Leaf of int | Branch of int * t * t
type set = t{ trie _ }
val ok : t{ (trie _) && (mem 1 _) } = Branch (2, Leaf 1, Leaf 3)
|}]

(* Mis-packed: 3 decodes as prefix 2, bit 1 -- the invariant refuses. *)
let bad : set = Branch (3, Leaf 1, Leaf 3)
[%%expect{|
Line 1, characters 16-42:
1 | let bad : set = Branch (3, Leaf 1, Leaf 3)
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: trie (Branch (3, Leaf 1, Leaf 3))
Hypotheses:
  ok = (Branch (2, Leaf 1, Leaf 3))
  (trie ok) && (mem 1 ok)
Possible counterexample:
  lbit 2 = 2
  lbit 3 = 1
  lbit 1 = 1
  hmask 1 (lbit 2) = 0
  hmask 1 (lbit 3) = 0
  hmask 3 (lbit 2) = 0
  hmask 3 (lbit 3) = 2
(lean: error: `grind` failed)
|}]
