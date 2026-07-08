open Vhof
open Voption
type t = Vok of int | Verror of int
[%%vox.lean {lean|
@[grind] def vr_is_ok : Vox_Vresult_t -> Prop
  | .Vok _ => True
  | .Verror _ => False
@[grind] def vr_get_ok (d : Int) : Vox_Vresult_t -> Int
  | .Vok x => x
  | .Verror _ => d
@[grind] def vr_get_err (d : Int) : Vox_Vresult_t -> Int
  | .Vok _ => d
  | .Verror e => e

@[grind, expose] def vr_maprel (r : IntRel) : Vox_Vresult_t -> Vox_Vresult_t -> Prop
  | .Vok x, o => (exists y, o = .Vok y /\ r x y)
  | .Verror e, o => o = .Verror e
@[grind, expose] def vr_maperr (r : IntRel) : Vox_Vresult_t -> Vox_Vresult_t -> Prop
  | .Vok x, o => o = .Vok x
  | .Verror e, o => (exists e', o = .Verror e' /\ r e e')
@[grind, expose] def vr_resrel (r : IntRel) (x : Int) : Vox_Vresult_t -> Prop
  | .Vok y => r x y
  | .Verror _ => True
@[grind, expose] def vr_bindrel (r : IntRel) : Vox_Vresult_t -> Vox_Vresult_t -> Prop
  | .Vok x, o => vr_resrel r x o
  | .Verror e, o => o = .Verror e
@[grind, expose] def vr_foldrel (r : IntRel3) : Vox_Vresult_t -> Int -> Int -> Prop
  | .Vok x, init, final => r init x final
  | .Verror _, init, final => init = final
@[grind, expose] def vr_to_opt_rel : Vox_Vresult_t -> Vox_Voption_t -> Prop
  | .Vok x, o => o = .Vsome x
  | .Verror _, o => o = .Vnone
|lean}]
let is_ok : (r : t) -> bool{ _ = vr_is_ok r } =
  fun r -> match r with Vok _ -> true | Verror _ -> false
let is_error : (r : t) -> bool{ _ = not (vr_is_ok r) } =
  fun r -> match r with Vok _ -> false | Verror _ -> true
let get_ok_or : (d : int) -> (r : t) -> int{ _ = vr_get_ok d r } =
  fun d r -> match r with Vok x -> x | Verror _ -> d
let get_err_or : (d : int) -> (r : t) -> int{ _ = vr_get_err d r } =
  fun d r -> match r with Vok _ -> d | Verror e -> e

let map :
      (r : ((int -> int -> bool) [@vox.total])) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (s : t) -> t{ vr_maprel r s _ } =
  fun r f s ->
    ignore r;
    match s with
    | Vok x -> let y = f x in (Vok y : t{ vr_maprel r s _ })
    | Verror e -> (Verror e : t{ vr_maprel r s _ })

let map_error :
      (r : ((int -> int -> bool) [@vox.total])) ->
      (f : ((e : int) -> int{ rHolds r e _ })) ->
      (s : t) -> t{ vr_maperr r s _ } =
  fun r f s ->
    ignore r;
    match s with
    | Vok x -> (Vok x : t{ vr_maperr r s _ })
    | Verror e -> let e' = f e in (Verror e' : t{ vr_maperr r s _ })

let bind :
      (r : ((int -> int -> bool) [@vox.total])) ->
      (f : ((x : int) -> t{ vr_resrel r x _ })) ->
      (s : t) -> t{ vr_bindrel r s _ } =
  fun r f s ->
    ignore r;
    match s with
    | Vok x -> let res = f x in (res : t{ vr_bindrel r s _ })
    | Verror e -> (Verror e : t{ vr_bindrel r s _ })

let fold :
      (r : ((int -> int -> int -> bool) [@vox.total])) ->
      (f : ((acc : int) -> (x : int) -> int{ r3Holds r acc x _ })) ->
      (init : int) -> (s : t) -> int{ vr_foldrel r s init _ } =
  fun r f init s ->
    ignore r;
    match s with
    | Vok x -> let res = f init x in (res : int{ vr_foldrel r s init _ })
    | Verror e -> (init : int{ vr_foldrel r s init _ })

let to_option : (s : t) -> Voption.t{ vr_to_opt_rel s _ } =
  fun s ->
    match s with
    | Vok x -> (Voption.Vsome x : Voption.t{ vr_to_opt_rel s _ })
    | Verror e -> (Voption.Vnone : Voption.t{ vr_to_opt_rel s _ })
