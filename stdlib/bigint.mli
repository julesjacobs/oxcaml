(** Arbitrary-precision signed integers.

    Values are mathematical integers: arithmetic does not wrap at the bounds
    of the machine [int] type.  Refinement checking interprets the total
    arithmetic and comparison operations below as mathematical-integer
    primitives.  The executable implementation is checked against an
    independent arbitrary-precision oracle by the runtime test suite. *)

type t : immutable_data

val zero : t @@ logical
val one : t @@ logical
val of_int : int -> t @@ total
val is_zero : t @ logical -> bool @@ total

(** The runtime representation is canonical, so polymorphic equality agrees
    with [equal].  Polymorphic ordering compares the private representation,
    not mathematical integers; use [lt], [le], [gt], or [ge] instead. *)
val equal : t @ logical -> t @ logical -> bool @@ total
val compare : t @ logical -> t @ logical -> int @@ total
val lt : t @ logical -> t @ logical -> bool @@ total
val le : t @ logical -> t @ logical -> bool @@ total
val gt : t @ logical -> t @ logical -> bool @@ total
val ge : t @ logical -> t @ logical -> bool @@ total
val neg : t @ logical -> t @@ total
val abs : t @ logical -> t @@ total
val add : t @ logical -> t @ logical -> t @@ total
val sub : t @ logical -> t @ logical -> t @@ total
val mul : t @ logical -> t @ logical -> t @@ total

(** [to_int_opt value] is [Some integer] exactly when [value] is representable
    as a machine [int]. *)
val to_int_opt : t -> int option

(** Canonical decimal conversion. [of_string] accepts an optional leading
    minus followed by decimal digits, with no redundant leading zeroes. *)
val to_string : t -> string
val of_string : string -> t
