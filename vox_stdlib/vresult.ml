type t = Vok of int | Verror of int
[%%vox.lean {lean|
-- Model defs restated (model-dup tax); the .mli's inline public theorems are
-- re-elaborated against these by the seal, so no theorem copies are needed here.
@[grind] def vr_is_ok : Vox_Vresult_t -> Prop
  | .Vok _ => True
  | .Verror _ => False
@[grind] def vr_get_ok (d : Int) : Vox_Vresult_t -> Int
  | .Vok x => x
  | .Verror _ => d
@[grind] def vr_get_err (d : Int) : Vox_Vresult_t -> Int
  | .Vok _ => d
  | .Verror e => e
|lean}]
let is_ok : (r : t) -> bool{ _ = vr_is_ok r } =
  fun r -> match r with Vok _ -> true | Verror _ -> false
let is_error : (r : t) -> bool{ _ = not (vr_is_ok r) } =
  fun r -> match r with Vok _ -> false | Verror _ -> true
let get_ok_or : (d : int) -> (r : t) -> int{ _ = vr_get_ok d r } =
  fun d r -> match r with Vok x -> x | Verror _ -> d
let get_err_or : (d : int) -> (r : t) -> int{ _ = vr_get_err d r } =
  fun d r -> match r with Vok _ -> d | Verror e -> e
