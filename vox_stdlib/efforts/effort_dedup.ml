(* WP-6 Effort C -- deduplication over an abstract element, driven at a concrete
   type. Exercises Vplist.dedup (the WP-2 gate op) end to end: the polymorphic
   dedup is proven ONCE at the abstract element inside Vplist; a client uses it at
   int with a call-site decider lambda (a total decider cannot be forwarded, so
   the client supplies it concretely -- the realistic shape). The correctness
   story: dedup returns a SUBSET of its input (up to the decider), and the
   distinct count is well-defined. Composes Vplist.dedup/mem/length. *)
[@@@warning "-6-32-26-27"]
open Vhof
open Vplist

[%%vox.lean {lean|
abbrev intEq : Int -> Int -> Prop := fun a b => a = b
|lean}]

(* distinct: drop duplicates. Result is a subset of the input (pl_dedup_sub). *)
let distinct (l : int Vplist.t) : int Vplist.t{ pl_dedup_sub intEq l _ } =
  Vplist.dedup (fun a b -> a = b) (fun a b -> a = b) l

(* the distinct count is a well-defined non-negative number. *)
let count_distinct (l : int Vplist.t) : int{ _ >= 0 } =
  Vplist.length (distinct l)

(* membership query on a (deduped) list, up to the decider. A client composes
   [present x (distinct l)]; passing the deduped list as an ARGUMENT avoids the C1
   re-reference of the relational-result [distinct l] (see FINDINGS). *)
let present (x : int) (d : int Vplist.t) : bool{ _ = pl_memr intEq x d } =
  Vplist.mem (fun a b -> a = b) (fun a b -> a = b) x d

(* an element removed from a list is not in what remains (up to the decider). *)
let after_remove (x : int) (d : int Vplist.t) : int Vplist.t{ not (pl_memr intEq x _) } =
  Vplist.remove (fun a b -> a = b) (fun a b -> a = b) x d

(* composition: dedup then query -- returns a plain bool (the relational result of
   distinct cannot be re-named in a refinement, so the composed goal stays
   unrefined; the pieces above carry the specs). *)
let distinct_has (x : int) (l : int Vplist.t) : bool =
  let d = distinct l in present x d

(* ============================ FINDINGS (Effort C) ============================
   Vplist.dedup (the WP-2 gate op) works end to end from a client. Verified:
   distinct (subset spec), a non-negative distinct count, membership + remove on
   the deduped list, and composition.

   F-C1 (MEDIUM, known from WP-2): a [@vox.total] decider cannot be FORWARDED, so
   a client generic over 'a cannot thread a comparator param. The op is proven
   once at the abstract element inside Vplist; the client instantiates at a
   concrete element (int) with a call-site decider lambda. Realistic, but a fully
   generic client wrapper is impossible.

   F-C2 (MINOR): a RELATIONAL-result op ([distinct], contract pl_dedup_sub) cannot
   be RE-REFERENCED in a downstream refinement (the C1 boundary): stating a goal
   about [pl_memr .. (distinct l)] while also calling [distinct l] leaves two
   unequatable occurrences. Route around: pass the deduped list as an ARGUMENT to
   the query fns ([present]/[after_remove] take [d], client composes
   [present x (distinct l)]); the composed fn's own result then stays unrefined.

   F-C3 (MEDIUM, from WP-2): dedup ships SUBSET-only (result ⊆ input). A superset
   / membership-equality spec needs the decider to be an EQUIVALENCE; the negative
   control (claiming superset) fails closed. A client wanting exact set-equality
   supplies an equivalence e and its own block law.

   F-C4 (NONE, positive): the eq-param route delivers — bool mem/remove/dedup at a
   concrete element are fully consumable; the DecidableEq wall is gone in practice. *)
