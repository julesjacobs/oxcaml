(** δ-rationals [c + k·δ] for strict bounds (Dutertre-de Moura [DdM06] §5).

    [δ] is a symbolic positive infinitesimal: [c1 + k1·δ < c2 + k2·δ] iff [c1 < c2], or
    [c1 = c2] and [k1 < k2]. Strict bounds enter the tableau as non-strict δ-bounds:
    [x > b] becomes [x >= b + δ] (i.e. [(b, 1)]), and [x < b] becomes [x <= b - δ] (i.e.
    [(b, -1)]). Non-strict bounds have [k = 0], so a single mechanism handles both
    (INVARIANTS.md I6: no float, exact comparison).

    Over ℤ the LIA atom path never needs δ (a negated [t <= 0] is the {e integer}
    complement [t >= 1], non-strict); δ is exercised by directly-asserted strict bounds
    and is what makes the simplex layer a faithful general-simplex over ℚ. *)

type t

val of_rat : Rational.t -> t

(** [make c k] is [c + k·δ]. *)
val make : Rational.t -> Rational.t -> t

(** The rational (finite) part [c]. *)
val c_part : t -> Rational.t

(** The infinitesimal coefficient [k]. *)
val k_part : t -> Rational.t

val zero : t
val add : t -> t -> t
val sub : t -> t -> t

(** [scale r t] is [r · t] (both components scaled). *)
val scale : Rational.t -> t -> t

val neg : t -> t
val compare : t -> t -> int
val equal : t -> t -> bool
val le : t -> t -> bool
val lt : t -> t -> bool

(** [is_rational t] iff [k = 0] (no infinitesimal part). *)
val is_rational : t -> bool

val to_string : t -> string
