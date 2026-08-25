(** Exact rational arithmetic for the LIA simplex (Dutertre-de Moura [DdM06]).

    Values are normalized fractions [num/den] with [den > 0] and [gcd(|num|, den) = 1];
    [zero = 0/1]. Two-tier (core-bignum W2): a native-int [Small] fast path with an
    arbitrary-precision [Big] fallback (hand-rolled {!Bigint}, stdlib-only). The tier is
    an implementation detail — [t] is abstract and values are canonical (fits-int63 ⟺
    [Small]), so the tier is invisible to verdicts, models, and pivot order (INVARIANTS.md
    I6).

    {b Overflow contract (core-bignum-review.md R1).} INTERNAL arithmetic never wraps and
    never raises: [add]/[sub]/[mul]/[div]/[neg]/[abs]/[compare]/[of_frac] promote to [Big]
    on native overflow and continue exactly. The exception {!Overflow} is raised ONLY at
    the OUTPUT-PROJECTION boundary — [num], [den], [floor], [ceil] return a native [int]
    and raise iff the (integer) value does not fit int63 — so a [Big] model value or B&B
    branch bound degrades to verdict [unknown] at those sinks (retaining the pre-W2 poison
    there), and NEVER truncates. This preserves the [Term.Overflow] session-boundary
    contract (INVARIANTS.md I8, ADR-0003): the exception is raised before any mutable
    solver state is touched, so the catch boundary degrades with state intact. Local
    exception (not [Term.Overflow]) because this module is term-agnostic; the boundary
    contract is identical.

    Do NOT use polymorphic [(=)] / [Stdlib.compare] / [Hashtbl.hash] on [t] (a two-tier
    variant makes them tier-sensitive): use {!equal} / {!compare} (value-based, R5). *)

exception Overflow

type t

val zero : t
val one : t
val of_int : int -> t

(** [of_bigint n] is the integer [n] (den = 1); never raises (demotes to [Small] iff it
    fits int63). The ingestion path for core term coefficients that exceed int63. *)
val of_bigint : Oxsmt_core.Bigint.t -> t

(** [of_frac num den] is [num/den] normalized; [den <> 0] required ([Invalid_argument]
    otherwise). Never raises {!Overflow} (promotes to [Big] if the native normalization
    would wrap). *)
val of_frac : int -> int -> t

(** [num t] / [den t]: the numerator / denominator as a native [int]. OUTPUT-PROJECTION
    boundary (R1): raises {!Overflow} iff the component does not fit int63 (only possible
    in the [Big] tier). Callers at native-int sinks catch this and degrade to [unknown]. *)
val num : t -> int

(** [num_bigint t] is the numerator as an arbitrary-precision {!Oxsmt_core.Bigint.t} — the
    same value as {!num} but WITHOUT the int63 output-projection: it never raises
    {!Overflow}, so a model value exceeding int63 (e.g. a uint256 constant) is
    representable. Used at the Bigint model sink ({!Lia.model_bigint}). *)
val num_bigint : t -> Oxsmt_core.Bigint.t

(** [floor_bigint t] is [floor t] as an arbitrary-precision {!Oxsmt_core.Bigint.t} —
    without {!floor}'s int63 output projection, so a >int63 branch point does not raise
    {!Overflow}. Used by B&B branching ({!Lia.suggest_branch}) on uint256-range values. *)
val floor_bigint : t -> Oxsmt_core.Bigint.t

val den : t -> int
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t

(** [div a b]: [b <> zero] required ([Invalid_argument] otherwise). *)
val div : t -> t -> t

val neg : t -> t
val abs : t -> t

(** Value-based total order / equality — never raise (promote to a common tier). *)
val compare : t -> t -> int

val equal : t -> t -> bool

(** [sign t] is [-1], [0], or [1]. *)
val sign : t -> int

val is_zero : t -> bool

(** [is_int t] iff [den t = 1]. Never raises. *)
val is_int : t -> bool

(** [floor t] / [ceil t]: greatest integer [<= t] / least integer [>= t], as a native
    [int]. OUTPUT-PROJECTION boundary (R1): raises {!Overflow} iff that integer does not
    fit int63; never truncates. *)
val floor : t -> int

val ceil : t -> int
val min : t -> t -> t
val max : t -> t -> t

(** Canonical decimal (core-bignum-review.md R7, the shared certificate wire format,
    ADR-0006 #7): ["num"] when [den = 1], else ["num/den"]; each component has no leading
    zeros, the sign is on the numerator only, and [den > 0]; zero renders exactly ["0"]. *)
val to_string : t -> string

(** Parse the {!to_string} grammar (decimal ["num"] or ["num/den"]); [den > 0] required.
    Round-trips with {!to_string}. *)
val of_string : string -> t
