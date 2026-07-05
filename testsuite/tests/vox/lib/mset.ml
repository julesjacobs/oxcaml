open Mset_lib

(* The carrier is opaque (Mset_lib.varr, sort VoxU); [setof] is
   Mset_lib's owned-contents ghost.  A binder of [t] denotes its ISet
   image; [refine_] reaches the carrier, carrying the link
   [setof r0 = s]. *)
type t = varr{ 0 = 0 } [@vox.via (setof : iset)]

let create : unit -> t{ card _ = 0 } @ unique =
  fun () ->
    let r = empty () in
    (r : t{ card _ = 0 })

let insert : (x : int) -> (s : t) @ unique -> t{ _ = ins x s } @ unique =
  fun x s ->
    let refine_ r0 = s in
    let p = new_proph () in
    let (r1, u) =
      borrow p r0 (fun m ->
        let m1 = sinsert m x in
        let _u = sdrop m1 in
        (() : unit{ spv p = ins x (setof r0) }))
    in
    ignore u;
    (r1 : t{ _ = ins x s })

let member :
  (x : int) -> (s : t) @ unique ->
  (bool{ _ = mem x s } * t{ _ = s }) @ unique =
  fun x s ->
    let refine_ r0 = s in
    let p = new_proph () in
    let (r1, b) =
      borrow p r0 (fun m ->
        let (b, m1) = smem m x in
        let _u = sdrop m1 in
        (b : bool{ _ = mem x (setof r0) && spv p = setof r0 }))
    in
    ((b : bool{ _ = mem x s }), (r1 : t{ _ = s }))
