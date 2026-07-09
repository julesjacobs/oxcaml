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
   Zero trust.

   HOF surface (WP-1): map / bind / filter / fold / is_some_and, via the HOF
   kit (notes/hof_kit.md). Voption is an EXPOSED ADT, so the relational lift
   defs (vo_maprel / vo_bindrel / vo_filterrel / vo_foldrel) reduce on the
   Vnone/Vsome constructors at the client -- exact output IS available (with
   the callback's graph the result is pinned), unlike the via-abstracted
   Vlist. The lift defs are `@[grind, expose]` (spec vocabulary the client
   reasons THROUGH, the Vrel/kit pattern); relation/predicate spec params carry
   [@vox.total]. Voption stays a LEAF (option->result conversion lives in
   Vresult to avoid a Voption<->Vresult cycle). *)
open Vhof
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


-- ===== per-container relational lifts over the option model =====
-- map: the result option is r-related to the input (None -> None).
@[grind, expose] public def vo_maprel (r : IntRel) : Vox_Voption_t -> Vox_Voption_t -> Prop
  | .Vnone, o => o = .Vnone
  | .Vsome x, o => (exists y, o = .Vsome y /\ r x y)
-- bind: f maps x to an option whose contents (if any) are r-related to x.
@[grind, expose] public def vo_optrel (r : IntRel) (x : Int) : Vox_Voption_t -> Prop
  | .Vnone => True
  | .Vsome y => r x y
@[grind, expose] public def vo_bindrel (r : IntRel) : Vox_Voption_t -> Vox_Voption_t -> Prop
  | .Vnone, o => o = .Vnone
  | .Vsome x, o => vo_optrel r x o
-- filter: keep Some x iff p x.
@[grind, expose] public def vo_filterrel (p : IntPred) : Vox_Voption_t -> Vox_Voption_t -> Prop
  | .Vnone, o => o = .Vnone
  | .Vsome x, o => (pHolds p x /\ o = .Vsome x) \/ ((¬ pHolds p x) /\ o = .Vnone)
-- fold: at most one step (None -> init; Some x -> one r-step from init).
@[grind, expose] public def vo_foldrel (r : IntRel3) : Vox_Voption_t -> Int -> Int -> Prop
  | .Vnone, init, final => init = final
  | .Vsome x, init, final => r init x final
-- is_some_and: Some x with p x.
@[grind, expose] public def vo_is_some_and (p : IntPred) : Vox_Voption_t -> Prop
  | .Vnone => False
  | .Vsome x => pHolds p x
|lean}]
val is_some : (o : t) -> bool{ _ = vo_is_some o }
val is_none : (o : t) -> bool{ _ = not (vo_is_some o) }
val get_or : (d : int) -> (o : t) -> int{ _ = vo_get_or d o }
val get : (o : t{ vo_is_some _ }) -> int{ _ = vo_get o }

(* ===== HOF surface (WP-1) ===== *)
val map :
  (r : ((int -> int -> bool) [@vox.total])) ->
  (f : ((x : int) -> int{ rHolds r x _ })) ->
  (o : t) -> t{ vo_maprel r o _ }
val bind :
  (r : ((int -> int -> bool) [@vox.total])) ->
  (f : ((x : int) -> t{ vo_optrel r x _ })) ->
  (o : t) -> t{ vo_bindrel r o _ }
val filter :
  (p : ((int -> bool) [@vox.total])) ->
  (test : ((x : int) -> bool{ _ = pHolds p x })) ->
  (o : t) -> t{ vo_filterrel p o _ }
val fold :
  (r : ((int -> int -> int -> bool) [@vox.total])) ->
  (f : ((acc : int) -> (x : int) -> int{ r3Holds r acc x _ })) ->
  (init : int) -> (o : t) -> int{ vo_foldrel r o init _ }
val is_some_and :
  (p : ((int -> bool) [@vox.total])) ->
  (test : ((x : int) -> bool{ _ = pHolds p x })) ->
  (o : t) -> bool{ _ = vo_is_some_and p o }
