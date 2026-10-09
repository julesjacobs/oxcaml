(** Incremental general simplex over δ-rationals (Dutertre-de Moura [DdM06]).

    The two-layer DdM design: a tableau of {e basic} variables expressed over {e nonbasic}
    variables (each basic [= Σ coeffᵢ · nonbasicᵢ]), with an optional lower and upper
    δ-bound per variable. Atoms map to bounds on variables (the LIA layer builds the
    variables); {!check} restores feasibility by Bland-rule pivoting.
    Backtracking-friendly: {!push}/{!pop} save and restore bounds only — the tableau and
    assignment stay a valid relaxation, so no value/basis trailing is needed (DdM06 §5,
    backtracking).

    Variables are dense integer ids assigned in creation order. Bland's rule (smallest id
    first, both leaving and entering) guarantees termination and makes the pivot sequence
    a deterministic function of the input (INVARIANTS.md I6).

    ['a] is the caller's opaque {e premise token} type: each bound carries the token of
    the atom that set it, and a {!conflict} reports the tokens of the bounds in the
    infeasible set together with their Farkas multipliers (DESIGN.md §7, INVARIANTS.md
    I4). The Farkas certificate is {b self-checked at production} (always on): the
    multiplier-weighted sum of the contributing half-planes is verified to cancel all
    variables and leave a strictly positive constant, else {!Farkas_error} is raised (this
    is the mutation-testing tripwire for a flipped simplex comparison, DESIGN.md §10).

    Overflow: any {!Rational.Overflow} from the exact arithmetic propagates out; the LIA /
    session layer catches it and degrades to [unknown] (INVARIANTS.md I8). *)

type 'a t

(** [{ premises; farkas }]: parallel lists — [farkas] are nonnegative multipliers, one per
    premise, such that [Σ farkasᵢ · (premiseᵢ's half-plane ≤ 0)] is a variable-free
    strictly positive constant (a Farkas certificate of infeasibility, self-checked). *)
type 'a conflict =
  { premises : 'a list
  ; farkas : Rational.t list
  }

(** Raised (always, not only in debug) when a produced Farkas certificate fails its
    self-check — a solver bug, never a normal outcome. *)
exception Farkas_error of string

val create : unit -> 'a t

(** [new_problem_var t] allocates a fresh nonbasic problem variable (value 0, no bounds);
    its Farkas half-plane basis is itself. *)
val new_problem_var : 'a t -> int

(** [new_slack t def] allocates a fresh basic variable [s] with [s = Σ coeffᵢ · varᵢ] over
    the given (problem-variable id, coefficient) pairs; [def]'s vars must already exist.
    Its value is initialized consistently with the current assignment. *)
val new_slack : 'a t -> (int * Rational.t) list -> int

(** [assert_lower t v d tok] tightens [v]'s lower bound to [d] (attributed to [tok]); a
    no-op if [d] is not tighter. Returns [Some conflict] iff this makes [v]'s lower bound
    exceed its upper bound (the immediate two-bound contradiction, DdM06 [AssertLower]);
    otherwise [None] (feasibility restored by a later {!check}). *)
val assert_lower : 'a t -> int -> Delta.t -> 'a -> 'a conflict option

val assert_upper : 'a t -> int -> Delta.t -> 'a -> 'a conflict option

(** [check t] pivots to a feasible assignment; [None] iff feasible (rational/δ SAT), else
    [Some conflict] (the Farkas-certified infeasible bound set). Idempotent once feasible. *)
val check : 'a t -> 'a conflict option

(** [value t v] is [v]'s current assignment β(v). Meaningful as a model value only after a
    {!check} returned [None]. *)
val value : 'a t -> int -> Delta.t

(** [get_lower t v] / [get_upper t v]: the current bound and the token that set it, if
    any. *)
val get_lower : 'a t -> int -> ('a * Delta.t) option

val get_upper : 'a t -> int -> ('a * Delta.t) option

(** [push t] opens a backtrack frame; [pop t n] undoes the last [n] frames, restoring
    bounds (and clearing any pending conflict). *)
val push : 'a t -> unit

val pop : 'a t -> int -> unit

(** Total pivots performed (determinism/perf stat, DESIGN.md §8). *)
val pivot_count : 'a t -> int

(** [true] once a {!Rational.Overflow} has escaped a state-mutating op ({!assert_lower},
    {!assert_upper}, {!check}, {!new_slack}): the tableau may be left mid-pivot with
    INV-EQ broken, so every later result would be unsound. Callers must refuse to reason
    on a poisoned instance (the {!Lia} layer raises [Lia.Poisoned]); the flag is never
    cleared. *)
val is_poisoned : 'a t -> bool

(** Brick the instance explicitly (sets {!is_poisoned}). For the {!Lia} layer to poison on
    a {!Rational.Overflow} that escapes its own arithmetic — atom translation, B&B branch
    bounds — which happens outside a guarded simplex op. *)
val poison : 'a t -> unit

(** Number of variables allocated. *)
val num_vars : 'a t -> int

(** [cube_test t problem_vars] — the Bromberger–Fleury unit cube test (TACAS 2016). A
    {e sufficient} integer-feasibility test that finds a model without branch-and-bound:
    shrink every constraint interval inward by half the 1-norm of its coefficient row and
    test rational feasibility of the shrunk system. [Some assignment] (a rounded integer
    value per id in [problem_vars]) iff the shrunk system is feasible AND the rounded
    point re-verifies feasible against the ORIGINAL bounds; [None] otherwise (inconclusive
    — the caller falls back to branching). Runs under an internal {!push}/{!pop} so no
    tightened bound persists and the tableau is left feasible; never poisons (all
    arithmetic is exact/non-raising, and the sole int63 projection is caught).
    [problem_vars] must be the integer variables to round; every coefficient row's def
    references only these. *)
val cube_test : 'a t -> int list -> (int * Rational.t) list option
