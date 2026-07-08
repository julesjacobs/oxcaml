type t = Vnone | Vsome of int
[%%vox.lean {lean|
@[grind] def vo_is_some : Vox_Voption_t -> Prop
  | .Vnone => False
  | .Vsome _ => True
@[grind] def vo_get_or (d : Int) : Vox_Voption_t -> Int
  | .Vnone => d
  | .Vsome x => x
@[grind] def vo_get : Vox_Voption_t -> Int
  | .Vnone => 0
  | .Vsome x => x
theorem vo_is_some_some (x : Int) : vo_is_some (.Vsome x) := by grind
grind_pattern vo_is_some_some => vo_is_some (.Vsome x)
@[grind] theorem vo_not_some_none : ¬ vo_is_some .Vnone := by grind
theorem vo_get_or_some (d x : Int) : vo_get_or d (.Vsome x) = x := by grind
grind_pattern vo_get_or_some => vo_get_or d (.Vsome x)
theorem vo_get_some (x : Int) : vo_get (.Vsome x) = x := by grind
grind_pattern vo_get_some => vo_get (.Vsome x)
|lean}]
let is_some : (o : t) -> bool{ _ = vo_is_some o } =
  fun o -> match o with Vnone -> false | Vsome _ -> true
let is_none : (o : t) -> bool{ _ = not (vo_is_some o) } =
  fun o -> match o with Vnone -> true | Vsome _ -> false
let get_or : (d : int) -> (o : t) -> int{ _ = vo_get_or d o } =
  fun d o -> match o with Vnone -> d | Vsome x -> x
let get : (o : t{ vo_is_some _ }) -> int{ _ = vo_get o } =
  fun o -> match o with Vsome x -> x | Vnone -> 0
