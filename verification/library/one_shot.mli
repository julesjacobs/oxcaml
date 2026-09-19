@@ portable

(** Affine endpoints. Messages may contain further endpoints. *)
type ('a : value mod portable contended) send : value mod contended total
type ('a : value mod portable contended) recv : value mod contended total

(** Allocate one sender and one receiver for a single ownership transfer. *)
val create : ('a : value mod portable contended).
  unit -> ('a send * 'a recv) @ unique
(** Consume the sender and publish the unique payload without waiting. *)
val send : ('a : value mod portable contended).
  'a send @ unique -> 'a @ unique -> unit
(** Consume the receiver and wait for its payload. The result retains the
    payload type and its refinements. No termination guarantee is made. *)
val recv : ('a : value mod portable contended).
  'a recv @ unique -> 'a @ unique
