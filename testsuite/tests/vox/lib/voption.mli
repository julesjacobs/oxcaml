(* Voption -- verified int option, wave-1 exposed-ADT leaf module.
   Repr is exposed (type t = Vnone | Vsome of int); the model block gives
   clients the vocabulary (vo_is_some / vo_get_or / vo_get) plus the four
   definitional laws. Obligation form (the blueprint default): the laws are
   public axioms here and are discharged by same-named theorems in the .ml.
   The model defs are public but deliberately NOT `expose`d: exposing them
   would let a client's grind unfold the def and discharge every law without
   ever matching it (dead laws, amendment A), so the reduction facts clients
   compute with ship as the named laws below. Without `expose`, `get` is
   unusable unless vo_get's reduction rides as a law, hence vo_get_some.
   Zero trust. *)
type t = Vnone | Vsome of int
[%%vox.lean {lean|
@[grind] public def vo_is_some : Vox_Voption_t -> Prop
  | .Vnone => False
  | .Vsome _ => True
@[grind] public def vo_get_or (d : Int) : Vox_Voption_t -> Int
  | .Vnone => d
  | .Vsome x => x
@[grind] public def vo_get : Vox_Voption_t -> Int
  | .Vnone => 0
  | .Vsome x => x
public axiom vo_is_some_some (x : Int) : vo_is_some (.Vsome x)
grind_pattern vo_is_some_some => vo_is_some (.Vsome x)
@[grind] public axiom vo_not_some_none : ¬ vo_is_some .Vnone
public axiom vo_get_or_some (d x : Int) : vo_get_or d (.Vsome x) = x
grind_pattern vo_get_or_some => vo_get_or d (.Vsome x)
public axiom vo_get_some (x : Int) : vo_get (.Vsome x) = x
grind_pattern vo_get_some => vo_get (.Vsome x)
|lean}]
val is_some : (o : t) -> bool{ _ = vo_is_some o }
val is_none : (o : t) -> bool{ _ = not (vo_is_some o) }
val get_or : (d : int) -> (o : t) -> int{ _ = vo_get_or d o }
val get : (o : t{ vo_is_some _ }) -> int{ _ = vo_get o }
