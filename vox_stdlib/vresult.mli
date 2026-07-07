(* Vresult -- exposed-ADT result over int payloads (ok / error). The dual of
   Voption: two int-carrying constructors instead of one. Definitional laws
   reference the exposed constructors DOTTED (.Vok / .Verror); a bare
   constructor fails ("Function expected at Vok ... unknown"). Inline public
   theorems are used under the narrow blueprint §4 exception (leaf module, no
   private scaffolding, one-line proofs). Verified with the real solver.

   The model defs are `@[grind] public` but deliberately NOT `expose`: with
   `expose` a client's grind unfolds these non-recursive defs directly and
   never needs a law, so every shipped law is DEAD (Phase-C soundness finding).
   Without `expose` the body does not cross the unit boundary, so a client can
   reason only via the exported reduction laws below -- which makes them live
   (confirmed: removing any law breaks the smoke client). The defs stay
   transparent WITHIN this unit, so the inline proofs and the .ml op VCs still
   discharge by grind. *)
type t = Vok of int | Verror of int
[%%vox.lean {lean|
@[grind] public def vr_is_ok : Vox_Vresult_t -> Prop
  | .Vok _ => True
  | .Verror _ => False
@[grind] public def vr_get_ok (d : Int) : Vox_Vresult_t -> Int
  | .Vok x => x
  | .Verror _ => d
@[grind] public def vr_get_err (d : Int) : Vox_Vresult_t -> Int
  | .Vok _ => d
  | .Verror e => e
public theorem vr_is_ok_ok (x : Int) : vr_is_ok (.Vok x) := by grind
grind_pattern vr_is_ok_ok => vr_is_ok (.Vok x)
public theorem vr_not_ok_error (e : Int) : ¬ vr_is_ok (.Verror e) := by grind
grind_pattern vr_not_ok_error => vr_is_ok (.Verror e)
public theorem vr_get_ok_ok (d x : Int) : vr_get_ok d (.Vok x) = x := by grind
grind_pattern vr_get_ok_ok => vr_get_ok d (.Vok x)
public theorem vr_get_err_err (d e : Int) : vr_get_err d (.Verror e) = e := by grind
grind_pattern vr_get_err_err => vr_get_err d (.Verror e)
|lean}]
val is_ok : (r : t) -> bool{ _ = vr_is_ok r }
val is_error : (r : t) -> bool{ _ = not (vr_is_ok r) }
val get_ok_or : (d : int) -> (r : t) -> int{ _ = vr_get_ok d r }
val get_err_or : (d : int) -> (r : t) -> int{ _ = vr_get_err d r }
