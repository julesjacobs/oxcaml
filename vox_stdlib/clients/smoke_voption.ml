(* Per-module SMOKE client (dead-law check, blueprint §6.7): a few-line goal
   per shipped Voption law, so each law has a forcing consumer. Verified
   against voption.cmi + VoxSig_Voption.olean only.
   - some_is_some    forces vo_is_some_some  (is_some (Vsome x) = true)
   - none_is_not_some forces vo_not_some_none (is_some Vnone = false)
   - get_or_some      forces vo_get_or_some   (get_or d (Vsome x) = x)
   - get_some         forces vo_get_some at `get`'s result and vo_is_some_some
     at its precondition (the model defs are not `expose`d, so both must fire
     as named laws — liveness re-verified: dropping any one law breaks smoke).
   Post-#53 (finding C1): a constructor application (Vsome x / Vnone) is a
   reflectable expression, so it now passes INLINE to a dependent parameter --
   the C1 let-binds are removed here (see notes/voption.md). *)
open Voption
let some_is_some : (x : int) -> bool{ _ = true } =
  fun x -> is_some (Vsome x)
let none_is_not_some : bool{ _ = false } =
  is_some Vnone
let get_or_some : (d : int) -> (x : int) -> int{ _ = x } =
  fun d x -> get_or d (Vsome x)
let get_some : (x : int) -> int{ _ = x } =
  fun x -> get (Vsome x)
