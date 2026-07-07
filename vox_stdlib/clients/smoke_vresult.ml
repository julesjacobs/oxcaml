(* Per-module SMOKE client (dead-law check §6.7): a few-line goal per shipped
   law, forcing each to fire. Verified against Vresult.cmi + VoxSig_Vresult.olean.
   Because the model defs are NOT `expose`d, a client cannot unfold them; each
   goal is dischargeable ONLY via the corresponding law, so these goals prove
   the laws are LIVE (removing any one law makes the matching goal fail).
   - smoke_ok     forces vr_is_ok_ok     (vr_is_ok (.Vok x) holds).
   - smoke_iserr  forces vr_not_ok_error (¬ vr_is_ok (.Verror e)).
   - smoke_get    forces vr_get_ok_ok    (vr_get_ok d (.Vok x) = x).
   - smoke_geterr forces vr_get_err_err  (vr_get_err d (.Verror e) = e).
   Constructor arguments to the dependent ops are let-bound first: a raw
   constructor application in argument position is rejected (C1). *)
open Vresult
let smoke_ok (x : int) : bool{ _ = true } =
  let r = Vresult.Vok x in
  Vresult.is_ok r
let smoke_iserr (e : int) : bool{ _ = true } =
  let r = Vresult.Verror e in
  Vresult.is_error r
let smoke_get (d : int) (x : int) : int{ _ = x } =
  let r = Vresult.Vok x in
  Vresult.get_ok_or d r
let smoke_geterr (d : int) (e : int) : int{ _ = e } =
  let r = Vresult.Verror e in
  Vresult.get_err_or d r
