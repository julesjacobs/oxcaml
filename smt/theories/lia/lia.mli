(** Linear integer arithmetic decision procedure: Dutertre-de Moura incremental simplex
    over ℚ (rational feasibility) + branch-and-bound for integrality (DESIGN.md §6).

    This is the {b adapter-facing} algorithm surface (the THEORY functor that binds it to
    the frozen engine interface, ADR-0005, is a separate M4 concern). It consumes atoms as
    frozen-core {!Term.t}s in the ADR-0003 [Le]-normal form ([inner <= 0], gcd-tightened)
    and Int-sorted equalities, and speaks a caller-chosen opaque {b premise token} ['tok]
    (the adapter passes its [Lit.t]; tests pass strings/ints). Every conflict is a Farkas
    certificate over premise tokens, self-checked at production (INVARIANTS.md I4, §7).

    Determinism (INVARIANTS.md I6): variables are numbered in atom-arrival order, Bland's
    rule drives pivoting, and branch selection is by lowest {!Term} tag — a given atom
    sequence yields identical pivots, conflicts, and models every run.

    Overflow / the native-int incompleteness ceiling (INVARIANTS.md I8): coefficients grow
    inside DdM pivoting and Farkas combinations, so guarded native-[int] rational
    arithmetic {b will} overflow on some small, non-adversarial QF_LIA inputs — this is a
    {e known incompleteness ceiling}, not a bug, and it stands only until rationals go
    arbitrary-precision (tracked as the core-bignum row, post-M4). Raising is sound:
    {!Rational.Overflow} is raised before any silent wrap. {!assert_atom}/{!check} let it
    propagate (the session boundary catches it and degrades to [unknown]);
    {!solve_integer} — the complete decision driver — itself degrades an overflow to
    {!Int_unknown} and counts it (see {!overflow_count}), so a benchmark pass-rate gap is
    attributable rather than mysterious. The overflow lives in the numeric layer, which
    never touches the {!Context} intern table, so core state (I8) is intact and a fresh
    solver is unaffected — but the solver {e instance} whose {!check} let an overflow
    escape is left mid-pivot and {b must be discarded, not reused} (a subsequent operation
    on it may reach a different, spurious verdict). {!solve_integer}, which catches the
    overflow itself, has no such hazard. *)

open Oxsmt_core

type 'tok t

(** A Farkas-certified infeasible core: [farkasᵢ >= 0] is the multiplier for [premisesᵢ]'s
    half-plane; [Σ farkasᵢ · half-planeᵢ] is a variable-free positive constant. *)
type 'tok conflict =
  { premises : 'tok list
  ; farkas : Rational.t list
  }

type 'tok result =
  | Sat_candidate (** the asserted atoms are feasible over ℚ (δ-rational model) *)
  | Conflict of 'tok conflict

type 'tok integer_result =
  | Int_sat of (Term.t * int) list (** a total integer model over the problem variables *)
  | Int_unsat of 'tok conflict option
  (** infeasible; [Some] certificate iff already infeasible over ℚ (no branching) *)
  | Int_unknown (** split budget exhausted (a sound incomplete answer, DESIGN.md §1) *)

(** Raised for an atom outside the handled fragment (e.g. an Int-[Ite] that preprocessing
    should have removed, or a disequality — which the engine resolves by a trichotomy
    split, ADR-0005 CONTRACT-SPLIT — reaching the direct assert path). Same session-caught
    contract as {!Rational.Overflow}. *)
exception Unsupported of string

(** [create ctx] is an empty solver threading the session {!Context.t} (ADR-0003 D6): all
    branch atoms built by {!suggest_branch} go through [ctx], sharing its tag stream. *)
val create : Context.t -> 'tok t

(** [assert_atom t atom ~polarity ~premise] asserts [atom] (a [Le] atom or Int [Eq]) with
    the given [polarity], attributing it to [premise]. Side-effect only (state update);
    any resulting inconsistency is reported by the next {!check} (ADR-0005 D3). A negated
    [Le] becomes the exact ℤ complement ([¬(t<=0)] ≡ [t>=1]); a positive Int [Eq] becomes
    a pair of bounds. Raises {!Unsupported} for a negated equality. *)
val assert_atom : 'tok t -> Term.t -> polarity:bool -> premise:'tok -> unit

(** [register_atom t atom] pre-declares [atom]'s variable/bound structure without
    asserting it, so {!propagate} can later report it as theory-implied. Idempotent. *)
val register_atom : 'tok t -> Term.t -> unit

(** [check t] tests rational (δ) feasibility of the asserted atoms. *)
val check : 'tok t -> 'tok result

(** [solve_integer ?budget t] runs branch-and-bound for an integer model. Branches
    (deterministically, lowest {!Term} tag first) on a non-integer variable, exploring
    [x <= floor v] before [x >= ceil v] under {!push}/{!pop}. Returns [Int_unknown] once
    [budget] splits are spent (default {!default_budget}). An internal
    {!Rational.Overflow} is degraded to {!Int_unknown} and counted ({!overflow_count}). *)
val solve_integer : ?budget:int -> 'tok t -> 'tok integer_result

val default_budget : int

(** [suggest_branch t] — after a {!Sat_candidate} — is the B&B split request for the
    lowest-tag non-integer variable [x] with value [v]: the atom pair
    [(x <= floor v, x >= floor v + 1)] built through the session {!Context}, mirroring the
    adapter's [Split] (ADR-0005 D5). [None] iff the current rational model is already
    integral. *)
val suggest_branch : 'tok t -> (Term.t * Term.t) option

(** [propagate t] returns atoms (with polarity and premise witnesses) that the currently
    asserted bounds theory-imply but that are not yet asserted — bound-to-bound
    propagation with lazy explanations (ADR-0005 D3, [Lia_bound]). Registered atoms only
    ({!register_atom}). *)
val propagate : 'tok t -> (Term.t * bool * 'tok list) list

(** [model t] is the integer assignment of the problem variables; valid only after
    {!solve_integer} returned [Int_sat] (raises {!Failure} if a value is non-integral). *)
val model : 'tok t -> (Term.t * int) list

(** [rational_value t term] is the current δ-rational assignment of [term]'s variable (its
    finite part), for inspection/tests; [Rational.zero] for an unseen term. *)
val rational_value : 'tok t -> Term.t -> Rational.t

(** [push t] / [pop t n]: backtrack frames (ADR-0005 D6), delegated to the simplex bound
    stack; created variables persist (idempotent re-registration). *)
val push : 'tok t -> unit

val pop : 'tok t -> int -> unit

(** Total simplex pivots performed (determinism/perf stat, DESIGN.md §8). *)
val pivot_count : 'tok t -> int

(** Number of times {!solve_integer} degraded an internal overflow to {!Int_unknown} — the
    distinct stat attributing the native-int incompleteness gap (DESIGN.md §8 bench
    digest). *)
val overflow_count : 'tok t -> int
