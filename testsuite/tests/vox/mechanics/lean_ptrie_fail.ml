(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Patricia-trie soundness probes: the classic bit-trie bugs, each
   caught by the model from demo/ptrie.mli (inlined here over this
   unit's own datatype, with [mask]/[zbit] left transparent so the
   invariant computes on literal trees).  The broken variants are
   exactly the code a tired programmer writes. *)

type t =
  | Empty
  | Leaf of int
  | Branch of int * int * t * t

[%%vox.lean {lean|
@[grind] def mask (i b : Int) : Int := i % b

@[grind] def zbit (i b : Int) : Prop := i % (2*b) < b

instance (i b : Int) : Decidable (zbit i b) := by unfold zbit; infer_instance

inductive isbit : Int -> Prop where
  | one : isbit 1
  | dbl : {b : Int} -> isbit b -> isbit (2*b)

@[grind .] theorem isbit_one : isbit 1 := .one

def bbit (p0 p1 : Int) : Int :=
  if p0 = p1 then 1
  else if p0 % 2 = p1 % 2 then 2 * bbit (p0/2) (p1/2)
  else 1
termination_by (p0.natAbs + p1.natAbs)
decreasing_by omega

theorem bbit_isbit (p0 p1 : Int) : isbit (bbit p0 p1) := by
  fun_induction bbit p0 p1 with
  | case1 => exact isbit.one
  | case2 _ _ _ _ ih => exact isbit.dbl ih
  | case3 => exact isbit.one
grind_pattern bbit_isbit => bbit p0 p1

@[grind] def mem : Int -> Vox_t -> Prop
  | _, .Empty => False
  | i, .Leaf j => i = j
  | i, .Branch _ _ t0 t1 => mem i t0 ∨ mem i t1

@[grind] def allmatch : Vox_t -> Int -> Int -> Prop
  | .Empty, _, _ => True
  | .Leaf j, p, b => mask j b = p
  | .Branch _ _ t0 t1, p, b => allmatch t0 p b ∧ allmatch t1 p b

@[grind] def allzero : Vox_t -> Int -> Prop
  | .Empty, _ => True
  | .Leaf j, b => zbit j b
  | .Branch _ _ t0 t1, b => allzero t0 b ∧ allzero t1 b

@[grind] def allone : Vox_t -> Int -> Prop
  | .Empty, _ => True
  | .Leaf j, b => ¬ zbit j b
  | .Branch _ _ t0 t1, b => allone t0 b ∧ allone t1 b

@[grind] def trie : Vox_t -> Prop
  | .Empty => True
  | .Leaf _ => True
  | .Branch p b t0 t1 =>
      isbit b ∧ mask p b = p ∧
      allmatch t0 p b ∧ allmatch t1 p b ∧
      allzero t0 b ∧ allone t1 b ∧ trie t0 ∧ trie t1

@[grind] def join (p0 : Int) (t0 : Vox_t) (p1 : Int) (t1 : Vox_t) : Vox_t :=
  if zbit p0 (bbit p0 p1)
  then .Branch (mask p0 (bbit p0 p1)) (bbit p0 p1) t0 t1
  else .Branch (mask p0 (bbit p0 p1)) (bbit p0 p1) t1 t0

@[grind] def insert (i : Int) : Vox_t -> Vox_t
  | .Empty => .Leaf i
  | .Leaf j => if i = j then .Leaf i else join i (.Leaf i) j (.Leaf j)
  | .Branch p b t0 t1 =>
      if mask i b = p then
        if zbit i b then .Branch p b (insert i t0) t1
        else .Branch p b t0 (insert i t1)
      else join i (.Leaf i) p (.Branch p b t0 t1)

theorem not_mem_mismatch (i p b : Int) (t : Vox_t)
    (h : allmatch t p b) (hm : mask i b ≠ p) : ¬ mem i t := by
  induction t <;> grind
grind_pattern not_mem_mismatch => mem i t, allmatch t p b

theorem not_mem_zero (i b : Int) (t : Vox_t)
    (h : allzero t b) (hz : ¬ zbit i b) : ¬ mem i t := by
  induction t <;> grind
grind_pattern not_mem_zero => mem i t, allzero t b

theorem not_mem_one (i b : Int) (t : Vox_t)
    (h : allone t b) (hz : zbit i b) : ¬ mem i t := by
  induction t <;> grind
grind_pattern not_mem_one => mem i t, allone t b
|lean}]
[%%expect{|
type t = Empty | Leaf of int | Branch of int * int * t * t
|}]

let zero_bit : (i : int) -> (b : int{ isbit _ }) -> bool{ _ = zbit i b } =
  fun i b -> assume_unchecked_ (i land b = 0)

let mask : (i : int) -> (b : int{ isbit _ }) -> int{ _ = mask i b } =
  fun i b -> assume_unchecked_ (i land (b - 1))
[%%expect{|
val zero_bit : (i : int) -> (b : int{ isbit _ }) -> bool{ _ = (zbit i b) } =
  <fun>
val mask : (i : int) -> (b : int{ isbit _ }) -> int{ _ = (mask i b) } = <fun>
|}]

(* A well-formed literal branch on bit 1: key 0 on the zero side, key
   1 on the one side.  The invariant computes and accepts. *)
let ok : t{ trie _ } = Branch (0, 1, Leaf 0, Leaf 1)
[%%expect{|
val ok : t{ trie _ } = Branch (0, 1, Leaf 0, Leaf 1)
|}]

(* The same branch with the leaves SWAPPED: key 1 sits on the zero
   side of bit 1.  [trie] refuses. *)
let forged : t{ trie _ } = Branch (0, 1, Leaf 1, Leaf 0)
[%%expect{|
Line 1, characters 27-56:
1 | let forged : t{ trie _ } = Branch (0, 1, Leaf 1, Leaf 0)
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: trie (Branch (0, 1, Leaf 1, Leaf 0))
Hypotheses:
  ok = (Branch (0, 1, Leaf 0, Leaf 1))
  trie ok
Possible counterexample:
  mask 0 1 = 0
  mask 1 1 = 0
(lean: error: `grind` failed)
|}]

(* Descending the WRONG side of the branching bit: a key with bit [b]
   clear can only live in [t0]; searching [t1] is incomplete. *)
let rec mem_wrong : (i : int) -> (s : t{ trie _ }) -> bool{ _ = mem i s } =
  fun i s ->
    match s with
    | Empty -> false
    | Leaf j -> i = j
    | Branch (p, b, t0, t1) ->
      let m = mask i b in
      if m <> p then false
      else begin
        let z = zero_bit i b in
        if z then mem_wrong i t1 else mem_wrong i t0
      end
[%%expect{|
Line 11, characters 18-32:
11 |         if z then mem_wrong i t1 else mem_wrong i t0
                       ^^^^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: *unknown7* = (mem i s)
Hypotheses:
  *unknown7* = (mem i t1)
  z
  z = (zbit i b)
  not (m <> p)
  m = (mask i b)
  s = (Branch (p, b, t0, t1))
  not (s is Empty)
  not (s is Leaf)
  trie s
  ok = (Branch (0, 1, Leaf 0, Leaf 1))
  trie ok
Possible counterexample:
  i = -1
  b = 0
  p = -1
  m = -1
  mask i b = -1
  w = -1
  w_1 = 0
  mask p b = -1
  mask 0 1 = 0
  mask 1 1 = 0
(lean: error: `grind` failed)
|}]

let branching_bit : (p0 : int) -> (p1 : int{ _ <> p0 }) -> int{ _ = bbit p0 p1 } =
  fun p0 p1 -> assume_unchecked_ (let x = p0 lxor p1 in x land (-x))

let join : (p0 : int) -> (t0 : t) -> (p1 : int{ _ <> p0 }) -> (t1 : t)
           -> t{ _ = join p0 t0 p1 t1 } =
  fun p0 t0 p1 t1 ->
    let b = branching_bit p0 p1 in
    let p = mask p0 b in
    let z = zero_bit p0 b in
    if z then Branch (p, b, t0, t1) else Branch (p, b, t1, t0)
[%%expect{|
val branching_bit :
  (p0 : int) -> (p1 : int{ _ <> p0 }) -> int{ _ = (bbit p0 p1) } = <fun>
val join :
  (p0 : int) ->
  (t0 : t) ->
  (p1 : int{ _ <> p0 }) -> (t1 : t) -> t{ _ = (join p0 t0 p1 t1) } = <fun>
|}]

(* Forgetting the prefix guard: descending by the branching bit alone
   files the key under a prefix it does not match -- THE Patricia
   pitfall ([insert] must [join] on a mismatch, not recurse). *)
let rec ins_unguarded : (i : int) -> (s : t{ trie _ }) -> t{ _ = insert i s } =
  fun i s ->
    match s with
    | Empty -> Leaf i
    | Leaf j ->
      if i = j then s
      else begin
        let l = Leaf i in
        join i l j s
      end
    | Branch (p, b, t0, t1) ->
      let z = zero_bit i b in
      if z then begin
        let t0' = ins_unguarded i t0 in
        Branch (p, b, t0', t1)
      end
      else begin
        let t1' = ins_unguarded i t1 in
        Branch (p, b, t0, t1')
      end
[%%expect{|
Line 15, characters 8-30:
15 |         Branch (p, b, t0', t1)
             ^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: (Branch (p, b, t0', t1)) = (insert i s)
Hypotheses:
  t0' = (insert i t0)
  z
  z = (zbit i b)
  s = (Branch (p, b, t0, t1))
  not (s is Empty)
  not (s is Leaf)
  trie s
  ok = (Branch (0, 1, Leaf 0, Leaf 1))
  trie ok
Possible counterexample:
  i = -1
  b = 0
  p = 0
  w = 0
  w_1 = 0
  mask i b = -1
  mask p b = 0
  mask 0 1 = 0
  bbit i p = 0
  mask i (bbit i p) = -1
  mask 1 1 = 0
(lean: error: `grind` failed)
|}]
