(* Per-module SMOKE client (blueprint §6.7 dead-law check) for the ORDER-FREE
   element-polymorphic set.  Verified at BOTH [int Vpset.t] and [string Vpset.t]
   against Vpset.cmi + VoxSig_Vpset.olean, sources deleted.

   There is NO runnable [mem] query (the module ships membership only as a Prop
   spec -- see the .mli header), so op specs are forced two ways: refined-RETURN
   goals whose declared postcondition follows ONLY from the op's spec, and client
   block theorems that consume the relational defs.  Each is load-bearing by
   deletion (remove the referenced spec/def in Vpset ⇒ the goal fails; a
   hypothesis-free relational goal is rejected -- see notes/vpset.md negatives).

   - [add_superset_*] force [ps_addspec]: [s ⊆ add x s].
   - [union_superset_*] force [ps_unionspec]: [s1 ⊆ union s1 s2].
   - [sing_not_empty_*] force [is_empty] + [ps_singletonspec] + the bridge
     [ps_isnil_isempty] together: a singleton is not empty.
   - block theorems force the def vocabulary ([ps_singletonspec]/[ps_mem],
     [ps_unionspec], [ps_equal]/[ps_subset]) as consumable client goals. *)

open Vpset

(* Helper the two [sing_not_empty_*] goals consume: a singleton is not
   structurally nil.  Proved explicitly (the negated-∀ in [ps_isempty] needs the
   head witness [x], which grind will not self-instantiate) and marked [@grind]
   so the [is_empty (singleton x)] op goals close. *)
[%%vox.lean {lean|
@[grind] theorem ps_singleton_not_isnil {a : Type} (s : PSet a) (x : a)
    (h : ps_singletonspec s x) : ps_isnil s = False := by
  have hx : ps_mem x s := by grind
  have hne : ¬ ps_isempty s := fun hemp => hemp x hx
  grind [ps_isnil_isempty]
|lean}]

(* ---- int Vpset.t ---- *)
let add_superset_int (x : int) (s : int Vpset.t) : int Vpset.t{ ps_subset s _ } =
  Vpset.add x s

let union_superset_int (s1 : int Vpset.t) (s2 : int Vpset.t)
  : int Vpset.t{ ps_subset s1 _ } =
  Vpset.union s1 s2

let sing_not_empty_int (x : int) : bool{ _ = false } =
  let s = Vpset.singleton x in
  Vpset.is_empty s

(* ---- string Vpset.t ---- *)
let add_superset_str (x : string) (s : string Vpset.t) : string Vpset.t{ ps_subset s _ } =
  Vpset.add x s

let union_superset_str (s1 : string Vpset.t) (s2 : string Vpset.t)
  : string Vpset.t{ ps_subset s1 _ } =
  Vpset.union s1 s2

let sing_not_empty_str (x : string) : bool{ _ = false } =
  let s = Vpset.singleton x in
  Vpset.is_empty s

[%%vox.lean {lean|
theorem smoke_singleton_mem {a : Type} (r : PSet a) (x : a)
    (h : ps_singletonspec r x) : ps_mem x r := by grind
theorem smoke_union_mem_l {a : Type} (r p q : PSet a) (y : a)
    (h : ps_unionspec r p q) (hy : ps_mem y p) : ps_mem y r := by grind
theorem smoke_equal_to_subset {a : Type} (p q : PSet a)
    (h : ps_equal p q) : ps_subset p q := by grind
|lean}]
