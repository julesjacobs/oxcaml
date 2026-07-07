(* Per-module SMOKE client (dead-law check, blueprint §6.7): a few-line goal
   per shipped Voption law, so each law has a forcing consumer. Verified
   against voption.cmi + VoxSig_Voption.olean only.
   - some_is_some    forces vo_is_some_some  (is_some (Vsome x) = true)
   - none_is_not_some forces vo_not_some_none (is_some Vnone = false)
   - get_or_some      forces vo_get_or_some   (get_or d (Vsome x) = x)
   - get_some         forces vo_get_some at `get`'s result and vo_is_some_some
     at its precondition (the model defs are not `expose`d, so both must fire
     as named laws — liveness re-verified: dropping any one law breaks smoke).
   The `let o = Vsome x in ...` bindings are the C1 workaround: a constructor
   application cannot be passed inline to a dependent parameter (see
   notes/voption.md). *)
open Voption
let some_is_some : (x : int) -> bool{ _ = true } =
  fun x -> let o = Vsome x in is_some o
let none_is_not_some : bool{ _ = false } =
  let o = Vnone in is_some o
let get_or_some : (d : int) -> (x : int) -> int{ _ = x } =
  fun d x -> let o = Vsome x in get_or d o
let get_some : (x : int) -> int{ _ = x } =
  fun x -> let o = Vsome x in get o
