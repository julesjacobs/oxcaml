(** Hand-rolled arbitrary-precision signed integers. Two consumers: the [Big]-tier
    fallback arithmetic for {!Rational} (core-bignum W2), and the arbitrary-precision
    coefficients/constants of the core term layer ([Term.Int_const] and [linear], since
    the core arithmetic representation was widened off native [int] to admit >int63 input
    literals). It lives in [oxsmt_core] so both the term layer and the LIA theory can name
    it; stdlib-only (INVARIANTS.md I3): no Zarith.

    Sign-magnitude, little-endian base-2^31 limbs (core-bignum-review.md R3: 2^31, not
    2^62 — OCaml's 63-bit [int] has no double-width product). Values are {b canonical}: no
    trailing zero limbs, a unique zero, [sign] in [-1|0|+1] with [sign = 0] iff the value
    is zero. Canonicity makes {!compare}/{!equal} and the {!to_string} decimal grammar
    well-defined; a mis-canonicalized value is unconstructible.

    Determinism (I6): pure integer arithmetic, no hashing/float/allocation-order
    dependence. Do NOT use polymorphic [(=)] / [Stdlib.compare] / [Hashtbl.hash] on [t] —
    use {!equal} / {!compare} (they are value-correct; polymorphic ops would also be
    well-defined given canonicity, but the discipline is preserved deliberately, per R5). *)

type t

val zero : t
val one : t
val of_int : int -> t
val sign : t -> int
val is_zero : t -> bool
val equal : t -> t -> bool
val compare : t -> t -> int

(** Value-based structural hash (sign + limbs); equal values hash equal (canonicity), and
    it is deterministic (I6). This is the sanctioned value hash for the core term
    hash-cons bucket over [Int_const]/linear coefficients — do not substitute polymorphic
    [Hashtbl.hash] on [t]. *)
val hash : t -> int

val neg : t -> t
val abs : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t

(** [divmod x y] is [(q, r)] with [x = q*y + r], [q] truncated TOWARD ZERO, and [r]
    carrying the sign of [x] (matching OCaml [/]/[mod]); [y <> zero] required
    ([Invalid_argument]). *)
val divmod : t -> t -> t * t

(** Nonnegative greatest common divisor; [gcd zero zero = zero]. *)
val gcd : t -> t -> t

(** [Some n] iff the value fits a native [int] (int63), else [None]. *)
val to_int_opt : t -> int option

(** [fits_int t] iff {!to_int_opt} would return [Some]. *)
val fits_int : t -> bool

(** Canonical decimal (core-bignum-review.md R7, shared cert wire format — ADR-0006 #7):
    optional leading ['-'] then digits, {b no leading zeros}, zero renders exactly ["0"]
    (never ["-0"]), sign on the number only. *)
val to_string : t -> string

(** Parse the {!to_string} grammar STRICTLY: rejects empty, non-digits, leading zeros, and
    ["-0"] ([Invalid_argument]). Round-trips with {!to_string}. *)
val of_string : string -> t
