(* Wave-3 F-2 DEDUP client (integrator-owned) — the usability reviewer's task, now
   EXPRESSIBLE on the origin/vox compiler. of_list traverses a Vlist with the (now total)
   Vlist.head/tail eliminator and folds each element into a Vset; dedup_elems then
   enumerates the set back to a Vlist. Vlist -> Vset -> Vlist, end to end. This was
   BLOCKED before: head/tail carried a `not (ll_isnil _)` precondition whose fact the `if
   is_empty` branch could not thread (#32); making head/tail total removed the
   precondition, so the guarded traversal verifies. Verified against Vset + Vlist +
   Vset_bst artifacts (sources deleted). Dependent args are let-bound (C1). *)

let rec of_list (l : Vlist.t) : Vset.t =
  if Vlist.is_empty l
  then Vset.empty ()
  else (
    let h = Vlist.head l in
    let tl = Vlist.tail l in
    let rest = of_list tl in
    Vset.add h rest)
;;

(* the reviewer's dedup: elements of the set of a list's elements *)
let dedup_elems (l : Vlist.t) : Vlist.t =
  let s = of_list l in
  Vset.elements s
;;
