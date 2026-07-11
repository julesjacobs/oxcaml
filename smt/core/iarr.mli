(** Portable immutable array (ADR-0003 Decision on [Iarr]; ADR-0002 pins stock OCaml 5.4,
    so this is not OxCaml [iarray]).

    The type is {b abstract} and covariant: values can only be built by the two copying
    constructors below, and there is no mutator and no [to_array]. No read path hands back
    the backing array and no write path exists, so a term's hash-consed [Iarr] payload
    cannot be corrupted in place (INVARIANTS.md I1/I2). Covariance is sound precisely
    because the value is immutable.

    There is deliberately {b no} aliasing / unsafe constructor on this public surface. A
    no-copy cast lives in the library-private [Iarr_unsafe] module (dune
    [private_modules]) and is invisible to every consumer. *)

type +'a t

val of_list : 'a list -> 'a t

(** [of_array a] copies [a] ([Array.copy]); the caller keeps ownership of its array and
    may mutate it afterwards without affecting the result. *)
val of_array : 'a array -> 'a t

val to_list : 'a t -> 'a list
val length : 'a t -> int

(** [get t i] is O(1). Raises [Invalid_argument] if out of bounds. *)
val get : 'a t -> int -> 'a

val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val map : ('a -> 'b) -> 'a t -> 'b t
val exists : ('a -> bool) -> 'a t -> bool
val for_all : ('a -> bool) -> 'a t -> bool
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

(** [hash_fold f acc t] folds [f] over {e every} element (order and count included), so
    distinct arrays hash distinctly (ADR-0003 required #8). *)
val hash_fold : (int -> 'a -> int) -> int -> 'a t -> int
