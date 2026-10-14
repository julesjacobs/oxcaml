(** Exact arithmetic with no foreign runtime dependency. Poll callbacks may
    raise to interrupt an operation; operands are immutable. *)
module Z : sig
  type t

  val zero : t
  val one : t
  val minus_one : t
  val of_int : int -> t
  val of_string : ?poll:(unit -> unit) -> string -> t
  val to_int_opt : t -> int option
  val to_string : ?poll:(unit -> unit) -> t -> string
  val sign : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_zero : t -> bool
  val neg : t -> t
  val abs : t -> t
  val add : ?poll:(unit -> unit) -> t -> t -> t
  val sub : ?poll:(unit -> unit) -> t -> t -> t
  val mul : ?poll:(unit -> unit) -> t -> t -> t

  (** Quotient truncated towards zero; remainder has the dividend's sign. *)
  val quot_rem : ?poll:(unit -> unit) -> t -> t -> t * t

  (** Euclidean division, with [0 <= remainder < abs divisor]. *)
  val div_rem : ?poll:(unit -> unit) -> t -> t -> t * t
  val floor_div : ?poll:(unit -> unit) -> t -> t -> t
  val ceil_div : ?poll:(unit -> unit) -> t -> t -> t
  val gcd : ?poll:(unit -> unit) -> t -> t -> t
  val pow : ?poll:(unit -> unit) -> t -> int -> t
  val log2 : ?poll:(unit -> unit) -> t -> t

  (** Exact integer extensions. [None] bounds materialization of results
      exceeding 1,048,576 bits; it does not supply an approximate value. *)
  val pow2 : ?poll:(unit -> unit) -> t -> t option
  val piand : ?poll:(unit -> unit) -> t -> t -> t -> t option
end

module Q : sig
  type t

  val zero : t
  val one : t
  val of_int : int -> t
  val of_z : Z.t -> t
  val make : ?poll:(unit -> unit) -> Z.t -> Z.t -> t
  val numerator : t -> Z.t
  val denominator : t -> Z.t
  val to_string : t -> string
  val sign : t -> int
  val is_zero : t -> bool
  val is_integer : t -> bool
  val equal : t -> t -> bool
  val compare : ?poll:(unit -> unit) -> t -> t -> int
  val neg : t -> t
  val add : ?poll:(unit -> unit) -> t -> t -> t
  val sub : ?poll:(unit -> unit) -> t -> t -> t
  val mul : ?poll:(unit -> unit) -> t -> t -> t
  val div : ?poll:(unit -> unit) -> t -> t -> t
  val floor : ?poll:(unit -> unit) -> t -> Z.t
  val ceil : ?poll:(unit -> unit) -> t -> Z.t
end
