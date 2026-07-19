(** Incremental linear real arithmetic over exact rationals.

    This is the rational-feasibility layer of the LIA solver without any integrality
    enforcement.  Linear forms are sent directly to {!Simplex}; strict inequalities use
    {!Delta}'s symbolic positive infinitesimal.  Consequently {!check} is a decision
    procedure for conjunctions of linear real constraints, rather than a relaxation that
    needs branch-and-bound.

    Variables are allocated in deterministic creation order and the underlying simplex
    uses Bland's rule.  Coefficients, bounds, models, and Farkas multipliers are all exact
    {!Rational.t}s. *)

type var = int

type comparison =
  | Le
  | Lt
  | Ge
  | Gt
  | Eq
  | Ne

(** [coeffs] denotes [sum (q, x) in coeffs. q * x].  Repeated variables are summed and
    zero coefficients are discarded when a constraint is asserted. *)
type constraint_ =
  { coeffs : (var * Rational.t) list
  ; comparison : comparison
  ; rhs : Rational.t
  }

(** An oriented half-plane [sum a_i*x_i + constant <= 0].  The constant is a
    delta-rational so that a strict input remains visible in a certificate: for example
    [x < b] is represented by [x - b + delta <= 0]. *)
type 'premise half_plane =
  { premise : 'premise
  ; coeffs : (var * Rational.t) list
  ; constant : Delta.t
  }

(** A self-checked Farkas certificate.  The three lists are parallel.  Every multiplier
    is nonnegative; their weighted half-planes have zero coefficient for every variable
    and a strictly positive delta-rational constant.  {!Simplex} checks those properties
    before this value is exposed.  [half_planes] is included so equality orientations
    and strict premises can also be checked independently by a caller. *)
type 'premise conflict =
  { premises : 'premise list
  ; half_planes : 'premise half_plane list
  ; farkas : Rational.t list
  }

(** Assertion of a disequality does not silently strengthen or discard it.  It returns
    [Split (c_lt, c_gt)], the exhaustive split [lhs < rhs or lhs > rhs], without changing
    the solver.  All other comparisons are installed and return [Asserted], except that
    a bound that immediately closes an interval returns its already self-checked
    certificate in [Immediate_conflict].  The constraint remains installed in that
    case, so the next {!check} also returns [Unsat]. *)
type 'premise assertion =
  | Asserted
  | Immediate_conflict of 'premise conflict
  | Split of constraint_ * constraint_

type 'premise result =
  | Sat
  | Unsat of 'premise conflict

type 'premise t

(** Raised when an escaped exact-arithmetic failure left the underlying mutable tableau
    unsafe to reuse.  Discard the instance. *)
exception Poisoned

val create : unit -> 'premise t

(** Allocate an unconstrained real variable. *)
val new_var : 'premise t -> var

(** Assert one linear constraint, attributing each generated bound to [premise].
    [Invalid_argument] is raised if a coefficient names a variable not allocated by this
    instance.  See {!assertion} for disequalities. *)
val assert_constraint
  :  'premise t
  -> constraint_
  -> premise:'premise
  -> 'premise assertion

(** Exact rational feasibility.  [Sat] means feasible over the reals; there is no
    integrality step. *)
val check : 'premise t -> 'premise result

(** A total exact-rational assignment, in variable creation order.  Valid only after the
    latest variable allocation, state-changing assertion, or pop has been followed by
    [check t = Sat].

    The simplex assignment is delta-rational.  Model extraction chooses one global,
    deterministic positive rational epsilon small enough for every active constraint,
    substitutes it for delta in every variable, and then rechecks all active constraints
    exactly.  Thus the result satisfies strict inequalities too; it is not merely the
    finite part of the symbolic assignment. *)
val model : 'premise t -> (var * Rational.t) list

(** [value t v] is [v]'s entry in {!model}, with the same validity precondition. *)
val value : 'premise t -> var -> Rational.t

(** [fixed_value t ~coeffs ~constant] returns the exact value of
    [sum coeffs + constant], together with the premises for its active lower and upper
    bounds, exactly when both bounds are non-strict and coincide. *)
val fixed_value
  :  'premise t
  -> coeffs:(var * Rational.t) list
  -> constant:Rational.t
  -> (Rational.t * 'premise * 'premise) option

(** Read one active non-strict oriented bound of [sum coeffs + constant].  Strict delta
    bounds do not constitute exact-value witnesses. *)
val oriented_bound
  :  'premise t
  -> coeffs:(var * Rational.t) list
  -> constant:Rational.t
  -> [ `Lower | `Upper ]
  -> ('premise * Rational.t) option

(** Backtracking affects asserted bounds and the active-constraint set.  Allocated
    variables and internal slack rows persist. *)
val push : 'premise t -> unit

val pop : 'premise t -> int -> unit

(** A sub-frame watermark for chronological rewind.  It restores simplex bounds and the
    active-constraint log without changing the ordinary push/pop frame stack. *)
type checkpoint

val checkpoint : 'premise t -> checkpoint
val rewind_to_checkpoint : 'premise t -> checkpoint -> unit

(** Deterministic cumulative simplex pivot count. *)
val pivot_count : 'premise t -> int

val is_poisoned : 'premise t -> bool
