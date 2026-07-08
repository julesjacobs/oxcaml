open Vhof
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

@[grind, expose] def vo_maprel (r : IntRel) : Vox_Voption_t -> Vox_Voption_t -> Prop
  | .Vnone, o => o = .Vnone
  | .Vsome x, o => (exists y, o = .Vsome y /\ r x y)
@[grind, expose] def vo_optrel (r : IntRel) (x : Int) : Vox_Voption_t -> Prop
  | .Vnone => True
  | .Vsome y => r x y
@[grind, expose] def vo_bindrel (r : IntRel) : Vox_Voption_t -> Vox_Voption_t -> Prop
  | .Vnone, o => o = .Vnone
  | .Vsome x, o => vo_optrel r x o
@[grind, expose] def vo_filterrel (p : IntPred) : Vox_Voption_t -> Vox_Voption_t -> Prop
  | .Vnone, o => o = .Vnone
  | .Vsome x, o => (pHolds p x /\ o = .Vsome x) \/ ((¬ pHolds p x) /\ o = .Vnone)
@[grind, expose] def vo_foldrel (r : IntRel3) : Vox_Voption_t -> Int -> Int -> Prop
  | .Vnone, init, final => init = final
  | .Vsome x, init, final => r init x final
@[grind, expose] def vo_is_some_and (p : IntPred) : Vox_Voption_t -> Prop
  | .Vnone => False
  | .Vsome x => pHolds p x
|lean}]
let is_some : (o : t) -> bool{ _ = vo_is_some o } =
  fun o -> match o with Vnone -> false | Vsome _ -> true
let is_none : (o : t) -> bool{ _ = not (vo_is_some o) } =
  fun o -> match o with Vnone -> true | Vsome _ -> false
let get_or : (d : int) -> (o : t) -> int{ _ = vo_get_or d o } =
  fun d o -> match o with Vnone -> d | Vsome x -> x
let get : (o : t{ vo_is_some _ }) -> int{ _ = vo_get o } =
  fun o -> match o with Vsome x -> x | Vnone -> 0

let map :
      (r : ((int -> int -> bool) [@vox.total])) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (o : t) -> t{ vo_maprel r o _ } =
  fun r f o ->
    ignore r;
    match o with
    | Vnone -> (Vnone : t{ vo_maprel r o _ })
    | Vsome x -> let y = f x in (Vsome y : t{ vo_maprel r o _ })

let bind :
      (r : ((int -> int -> bool) [@vox.total])) ->
      (f : ((x : int) -> t{ vo_optrel r x _ })) ->
      (o : t) -> t{ vo_bindrel r o _ } =
  fun r f o ->
    ignore r;
    match o with
    | Vnone -> (Vnone : t{ vo_bindrel r o _ })
    | Vsome x -> let res = f x in (res : t{ vo_bindrel r o _ })

let filter :
      (p : ((int -> bool) [@vox.total])) ->
      (test : ((x : int) -> bool{ _ = pHolds p x })) ->
      (o : t) -> t{ vo_filterrel p o _ } =
  fun p test o ->
    ignore p;
    match o with
    | Vnone -> (Vnone : t{ vo_filterrel p o _ })
    | Vsome x -> if test x then (Vsome x : t{ vo_filterrel p o _ })
                 else (Vnone : t{ vo_filterrel p o _ })

let fold :
      (r : ((int -> int -> int -> bool) [@vox.total])) ->
      (f : ((acc : int) -> (x : int) -> int{ r3Holds r acc x _ })) ->
      (init : int) -> (o : t) -> int{ vo_foldrel r o init _ } =
  fun r f init o ->
    ignore r;
    match o with
    | Vnone -> (init : int{ vo_foldrel r o init _ })
    | Vsome x -> let res = f init x in (res : int{ vo_foldrel r o init _ })

let is_some_and :
      (p : ((int -> bool) [@vox.total])) ->
      (test : ((x : int) -> bool{ _ = pHolds p x })) ->
      (o : t) -> bool{ _ = vo_is_some_and p o } =
  fun p test o ->
    ignore p;
    match o with Vnone -> false | Vsome x -> test x
