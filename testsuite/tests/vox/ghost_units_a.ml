(* A ghost parameter used from another unit. *)
let f (x : int @ ghost) = 0
let use () = f (ghost_ 1)

(* A record with a ghost field, used from the other unit: pins that
   [ld_ghost] round-trips through the .cmi (a dropped flag would give the
   consumer a slotful layout and a mismatched construction). *)
type rec_g = { ga : int; gp : string @@ ghost; gb : int }
let mk_rec ga gb = { ga; gp = "cmi"; gb }
