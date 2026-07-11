(** Exact rational arithmetic for the LIA simplex (Dutertre-de Moura [DdM06]).

    Values are normalized fractions [num/den] with [den > 0] and [gcd(|num|, den) = 1];
    [zero = 0/1]. Every arithmetic operation is {b overflow-guarded}: it raises
    {!Overflow} {e before} producing a wrapped (silently wrong) result. This is the LIA
    analogue of the [Term.Overflow] session-boundary contract (INVARIANTS.md I8, ADR-0003
    Overflow/Unsupported): the numeric layer never wraps unsoundly; the exception is
    raised before any mutable solver state is touched, so the catch boundary degrades to
    verdict [unknown] with state intact. We define a local exception (rather than reuse
    [Term.Overflow]) because this module is term-agnostic; the boundary contract is
    identical.

    Native [int] numerators/denominators are a v1 decision (ADR-0003): refinement VCs
    carry tiny coefficients. The revisit trigger is a real VC overflowing native [int]
    (swap for a stdlib bignum), which this interface is shaped to allow. *)

exception Overflow

type t

val zero : t
val one : t
val of_int : int -> t

(** [of_frac num den] is [num/den] normalized; [den <> 0] required ([Invalid_argument]
    otherwise). Raises {!Overflow} if normalization would wrap (e.g. numerator/denominator
    = [min_int]). *)
val of_frac : int -> int -> t

val num : t -> int
val den : t -> int
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t

(** [div a b]: [b <> zero] required ([Invalid_argument] otherwise). *)
val div : t -> t -> t

val neg : t -> t
val abs : t -> t
val compare : t -> t -> int
val equal : t -> t -> bool

(** [sign t] is [-1], [0], or [1]. *)
val sign : t -> int

val is_zero : t -> bool

(** [is_int t] iff [den t = 1]. *)
val is_int : t -> bool

(** [floor t] / [ceil t]: greatest integer [<= t] / least integer [>= t]. *)
val floor : t -> int

val ceil : t -> int
val min : t -> t -> t
val max : t -> t -> t
val to_string : t -> string
