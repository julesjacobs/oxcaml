(* Per-module SMOKE client (blueprint §6.7 dead-law check): few-line goals that
   FORCE each shipped op spec to fire, verified against Vset.cmi +
   VoxSig_Vset.olean (+ the Vset_bst artifacts Vset depends on), sources
   deleted.

   - [after_add_has_x] forces the quantified [vs_addspec]: after [add x], the
     element [x] is a member. grind must instantiate addspec at y = x.
   - [empty_has_no_x] forces the quantified [vs_isempty]: nothing is a member
     of [empty]. grind must instantiate isempty at y = x.
   - [elements_agrees] forces [vs_elements_spec]: reading membership back off
     the enumerated Vlist agrees with set membership (the eliminator bridge).
   - [add_is_superset] forces [vs_subset] against the real add algebra: the
     added set is a superset of the original.
   - [removed_absent] forces [vs_removespec] at y = x (symbolic s): the removed
     element is absent from the result.
   - [smoke_remove_survivor] (a client block theorem) forces [vs_removespec]'s
     survivor conjunct: a member y <> x stays after removing x.
   - [smoke_equal_to_subset] (a client block theorem) forces [vs_equal] and
     [vs_subset] as CONSUMABLE goals: a client discharges them from the vs_mem
     algebra without writing its own quantifier (F-3).

   Post-#53 (finding C1): Vset's ops (add/remove/empty/elements) carry
   RELATIONAL (forall) result contracts (vs_addspec / vs_removespec /
   vs_isempty / vs_elements_spec), NOT an exact equational one, so #53 canNOT
   substitute them -- their results are STILL let-bound before the dependent
   [mem] call (the relational-contract boundary; see LANGUAGE_NEEDS). *)

open Vset
open Vlist

let after_add_has_x (x : int) (s : Vset.t) : bool{ _ = true } =
  let s' = Vset.add x s in
  Vset.mem x s'

let empty_has_no_x (x : int) : bool{ _ = false } =
  let e = Vset.empty () in
  Vset.mem x e

let elements_agrees (x : int) (s : Vset.t) : bool{ _ = vs_mem x s } =
  let l = Vset.elements s in
  Vlist.mem x l

let add_is_superset (x : int) (s : Vset.t) : Vset.t{ vs_subset s _ } =
  Vset.add x s

let removed_absent (x : int) (s : Vset.t) : bool{ _ = false } =
  let s' = Vset.remove x s in
  Vset.mem x s'

[%%vox.lean {lean|
theorem smoke_equal_to_subset (a b : ISet) (h : vs_equal a b) : vs_subset a b := by grind
theorem smoke_remove_survivor (x y : Int) (s r : ISet)
    (hr : vs_removespec r x s) (hne : y ≠ x) (hmem : vs_mem y s) : vs_mem y r := by grind
|lean}]
