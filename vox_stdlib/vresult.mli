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
   discharge by grind.

   HOF surface (WP-1): map (on Ok) / map_error (on Error) / bind / fold /
   to_option, via the HOF kit (notes/hof_kit.md). Exposed ADT, so the lift defs
   reduce on .Vok/.Verror -- exact output available. to_option enumerates into a
   Voption (open Voption): Vresult DEPENDS ON Voption. The reverse
   (Voption.to_result) is deliberately NOT shipped, to keep the Voption<->Vresult
   dependency acyclic (Voption is the lower layer). *)
open Vhof
open Voption
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

-- ===== per-container relational lifts over the result model =====
-- map: transform the Ok payload r-related; leave Error unchanged.
@[grind, expose] public def vr_maprel (r : IntRel) : Vox_Vresult_t -> Vox_Vresult_t -> Prop
  | .Vok x, o => (exists y, o = .Vok y /\ r x y)
  | .Verror e, o => o = .Verror e
-- map_error: transform the Error payload r-related; leave Ok unchanged.
@[grind, expose] public def vr_maperr (r : IntRel) : Vox_Vresult_t -> Vox_Vresult_t -> Prop
  | .Vok x, o => o = .Vok x
  | .Verror e, o => (exists e', o = .Verror e' /\ r e e')
-- bind: on Ok x, f produces a result whose Ok payload (if any) is r-related to
-- x; on Error, unchanged.
@[grind, expose] public def vr_resrel (r : IntRel) (x : Int) : Vox_Vresult_t -> Prop
  | .Vok y => r x y
  | .Verror _ => True
@[grind, expose] public def vr_bindrel (r : IntRel) : Vox_Vresult_t -> Vox_Vresult_t -> Prop
  | .Vok x, o => vr_resrel r x o
  | .Verror e, o => o = .Verror e
-- fold: on Ok x, one r-step from init; on Error, init unchanged.
@[grind, expose] public def vr_foldrel (r : IntRel3) : Vox_Vresult_t -> Int -> Int -> Prop
  | .Vok x, init, final => r init x final
  | .Verror _, init, final => init = final
-- to_option: Ok x -> Some x, Error _ -> None (bridges to the Voption model).
@[grind, expose] public def vr_to_opt_rel : Vox_Vresult_t -> Vox_Voption_t -> Prop
  | .Vok x, o => o = .Vsome x
  | .Verror _, o => o = .Vnone
|lean}]
val is_ok : (r : t) -> bool{ _ = vr_is_ok r }
val is_error : (r : t) -> bool{ _ = not (vr_is_ok r) }
val get_ok_or : (d : int) -> (r : t) -> int{ _ = vr_get_ok d r }
val get_err_or : (d : int) -> (r : t) -> int{ _ = vr_get_err d r }

(* ===== HOF surface (WP-1) ===== *)
val map :
  (r : ((int -> int -> bool) [@vox.total])) ->
  (f : ((x : int) -> int{ rHolds r x _ })) ->
  (s : t) -> t{ vr_maprel r s _ }
val map_error :
  (r : ((int -> int -> bool) [@vox.total])) ->
  (f : ((e : int) -> int{ rHolds r e _ })) ->
  (s : t) -> t{ vr_maperr r s _ }
val bind :
  (r : ((int -> int -> bool) [@vox.total])) ->
  (f : ((x : int) -> t{ vr_resrel r x _ })) ->
  (s : t) -> t{ vr_bindrel r s _ }
val fold :
  (r : ((int -> int -> int -> bool) [@vox.total])) ->
  (f : ((acc : int) -> (x : int) -> int{ r3Holds r acc x _ })) ->
  (init : int) -> (s : t) -> int{ vr_foldrel r s init _ }
val to_option : (s : t) -> Voption.t{ vr_to_opt_rel s _ }
