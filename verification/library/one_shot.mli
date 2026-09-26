@@ portable

(** Affine endpoints for one ownership transfer. Messages may contain further
    endpoints. Payload refinements are preserved by the type parameter; this
    API does not expose a trace model or prove progress. Trusted cell moves
    and strong sequentially consistent atomic events are specified in
    [Unique_cell] and [Verified_atomic]. All contracts concern normal returns.
    Dropping endpoints or authority, exceptions, and cancellation may prevent
    delivery and leak manually managed payloads. No recovery or leak-freedom
    guarantee is supplied. *)
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
