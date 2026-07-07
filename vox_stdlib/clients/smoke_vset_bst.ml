(* Per-module SMOKE client (dead-law check, blueprint §6.7): short goals that
   FORCE each shipped .mli law of Vset_bst to fire, verified against
   Vset_bst.cmi + VoxSig_Vset_bst.olean (no source).

   The membership laws are forced by observable [member] results; the [bok_*]
   invariant laws are MODEL-level facts about the bare [bins]/[bdel] terms, so
   they are forced by a goal that demands [bok (bins x s)] / [bok (bdel x s)]
   for a SYMBOLIC s -- grind cannot induct on a variable, so it must use the
   shipped lemma (an op's own [set] result type would NOT force them, since
   the interface hands back [bok] for free; see notes).

   - [inserted_is_member] forces bmem_insert at y = x.
   - [removed_is_absent] forces bmem_delete at y = x (the y = x branch of the
     <-> collapses to False).
   - [member_survives_other_remove] forces bmem_delete at y <> x (survival
     branch) and bmem_insert.
   - [ok_after_insert] forces bok_insert (bok of a symbolic bins).
   - [ok_after_remove] forces bok_delete (bok of a symbolic bdel).

   Call results feeding a dependent parameter ([member]/[insert]/[remove] take
   a [set]) are let-bound to a variable first (workaround C1; see notes). *)
open Vset_bst

let inserted_is_member (x : int) (s : set) : bool{ _ = true } =
  let s' = insert x s in
  member x s'

let removed_is_absent (x : int) (s : set) : bool{ _ = false } =
  let s' = remove x s in
  member x s'

let member_survives_other_remove (x : int) (y : int{ _ <> x }) (s : set)
  : bool{ _ = true } =
  let s1 = insert y s in
  let s2 = remove x s1 in
  member y s2

let ok_after_insert (x : int) (s : set) : unit{ bok (bins x s) } = ()

let ok_after_remove (x : int) (s : set) : unit{ bok (bdel x s) } = ()
