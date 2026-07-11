(* Backed by ['a array], but the type is abstract: the only bridges to the array are the
   two [%identity] externals below, and [inj]/[prj] are never exported (iarr.mli exposes
   neither). Covariance (+'a) is asserted here because the value is never mutated through
   this module (I1/I2); stock OCaml's [array] is invariant, so the assertion is what buys
   us the sound covariant view. *)

type +'a t

external inj : 'a array -> 'a t = "%identity"
external prj : 'a t -> 'a array = "%identity"

(* Both public constructors copy: a fresh array whose only reference is inside [t], so no
   caller alias can ever reach it (ADR-0003, fixes B1). *)
let of_list l = inj (Array.of_list l)
let of_array a = inj (Array.copy a)
let to_list t = Array.to_list (prj t)
let length t = Array.length (prj t)
let get t i = (prj t).(i)
let iter f t = Array.iter f (prj t)
let iteri f t = Array.iteri f (prj t)
let fold f acc t = Array.fold_left f acc (prj t)
let map f t = inj (Array.map f (prj t))
let exists f t = Array.exists f (prj t)
let for_all f t = Array.for_all f (prj t)

let equal eq a b =
  let a = prj a
  and b = prj b in
  Array.length a = Array.length b
  &&
  let rec go i = i = Array.length a || (eq a.(i) b.(i) && go (i + 1)) in
  go 0
;;

let compare cmp a b =
  let a = prj a
  and b = prj b in
  let la = Array.length a
  and lb = Array.length b in
  let rec go i =
    if i = la || i = lb
    then Stdlib.compare la lb
    else (
      let c = cmp a.(i) b.(i) in
      if c <> 0 then c else go (i + 1))
  in
  go 0
;;

(* Length is folded in so that [ [||] ] and a singleton whose element hashes to the seed
   do not collide, and so element order matters. *)
let hash_fold f acc t =
  let a = prj t in
  let acc = ref ((acc * 31) + Array.length a) in
  Array.iter (fun x -> acc := f !acc x) a;
  !acc
;;
