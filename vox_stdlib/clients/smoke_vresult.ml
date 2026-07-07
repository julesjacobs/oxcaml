(* Per-module SMOKE client (dead-law check §6.7): a few-line goal per shipped
   law, forcing each to fire. Verified against Vresult.cmi + VoxSig_Vresult.olean.
   Because the model defs are NOT `expose`d, a client cannot unfold them; each
   goal is dischargeable ONLY via the corresponding law, so these goals prove
   the laws are LIVE (removing any one law makes the matching goal fail).
   - smoke_ok     forces vr_is_ok_ok     (vr_is_ok (.Vok x) holds).
   - smoke_iserr  forces vr_not_ok_error (¬ vr_is_ok (.Verror e)).
   - smoke_get    forces vr_get_ok_ok    (vr_get_ok d (.Vok x) = x).
   - smoke_geterr forces vr_get_err_err  (vr_get_err d (.Verror e) = e).
   Post-#53 (finding C1): a raw constructor application (Vok x / Verror e) is
   reflectable and now passes INLINE to the dependent op -- C1 let-binds
   removed. *)
open Vresult
let smoke_ok (x : int) : bool{ _ = true } =
  Vresult.is_ok (Vresult.Vok x)
let smoke_iserr (e : int) : bool{ _ = true } =
  Vresult.is_error (Vresult.Verror e)
let smoke_get (d : int) (x : int) : int{ _ = x } =
  Vresult.get_ok_or d (Vresult.Vok x)
let smoke_geterr (d : int) (e : int) : int{ _ = e } =
  Vresult.get_err_or d (Vresult.Verror e)
