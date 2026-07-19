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
    solver is unaffected.

    {b Poisoned instances (brick semantics).} When a {!Rational.Overflow} escapes a
    state-mutating op the tableau may be left mid-pivot (INV-EQ broken), so the
    {e instance} is bricked: it is flagged poisoned and {b every} later public entry
    raises {!Poisoned} rather than return a value computed from corrupt state. This covers
    {b both} paths equally — the {!check}/{!assert_atom} path that lets the overflow
    propagate {b and} the {!solve_integer} path that catches it internally (the earlier
    claim that {!solve_integer} "has no such hazard" was wrong: only its own
    {!Int_unknown} return for the call that {e hit} the overflow is safe; reuse of that
    instance is not). The call that hits the overflow behaves as documented (the
    propagating ops re-raise {!Rational.Overflow}; {!solve_integer} returns {!Int_unknown}
    and bumps {!overflow_count}); only {e subsequent} operations raise {!Poisoned}.
    Diagnostics ({!pivot_count}, {!overflow_count}, {!is_poisoned}) stay readable. The
    flag is never cleared — discard the instance and build a fresh one. *)

open Oxsmt_core

type 'tok t

(** A Farkas-certified infeasible core. For an inequality premise, [farkasᵢ >= 0] is its
    asserted half-plane multiplier. For a positive Int equality [a = b], [farkasᵢ] is an
    unrestricted signed multiplier on the equation [a - b = 0]. Their sum is a
    variable-free positive constant. *)
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

(** Raised by every public entry (other than the diagnostics {!pivot_count},
    {!overflow_count}, {!is_poisoned}) when the instance has been poisoned by an escaped
    {!Rational.Overflow} — see the "Poisoned instances" note above. Converts silent
    mid-pivot corruption into a loud, sound failure; the fix is to discard the instance. *)
exception Poisoned

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
    asserting it, so {!propagate} can later report it as theory-implied. This covers [Le]
    atoms, and under [OXSMT_LIA_EQ_PROP=1] also Int equalities. Idempotent. *)
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

(** [diophantine_conflict t] — after a {!Sat_candidate} whose ℚ-model is non-integral —
    runs a GCD / Diophantine integer-feasibility test over the asserted positive Int
    equalities and returns a conflict iff one equality is ℤ-infeasible (its free-variable
    coefficient gcd does not divide the residual once simplex-pinned variables are
    substituted). SOUND integer-infeasibility certificate, orthogonal to combination: it
    only ever reports a conflict on a genuinely ℤ-infeasible state (never merges/injects).
    The conflict's premises are the equality literal plus the oriented-bound tokens of the
    substituted variables; [farkas] is empty (the state is ℚ-feasible, so there is no
    rational multiplier — this conflict is certified by the divisibility argument, not
    Farkas, and its consumers on the solve path read only the premise set). [None] when no
    recorded equality is infeasible, or on any overflow / non-integer coefficient (skipped
    soundly). *)
val diophantine_conflict : 'tok t -> 'tok conflict option

(** [hnf_cut t] — after a {!Sat_candidate} whose ℚ-model is non-integral — a Stage B HNF
    integer cut (charter logs/lia-cuts-charter.md, spec logs/lia-cuts-hnf-spec.md) over
    the TIGHT constraint rows (asserted equalities AND active one-sided bounds). It
    surfaces a MULTI-ROW integer-lattice infeasibility that {!diophantine_conflict}'s
    single-row gcd test cannot see, returned as [Some (cut_atom, antecedent_tokens)] for
    emission through the CONTRACT-LEMMA seam: [cut_atom] is the bound atom [f·x <= k]
    (built through the session {!Context}) and [antecedent_tokens] are the tight rows'
    premise tokens whose lattice combination proves it. The caller emits
    [Lemma [(cut_atom, true); ¬antecedentᵢ …]].

    SOUND by a self-checked certificate INDEPENDENT of the HNF kernel: the emitted cut is
    a rational multiplier [μ] of the contributing equality rows with [μ·A = f] (integer)
    and [μ·c = β ∉ ℤ]; because the rows are equalities, [f·x = β] for every feasible [x],
    so no integer point satisfies the antecedents (the clause is T-valid) and the LP
    vertex is separated. [μ] is re-verified against the original rows before emission — a
    wrong cut is dropped, never emitted. [None] when no cut is found, the system exceeds
    the z3-parity caps ([Hnf.max_rows]/[Hnf.max_cols]), or the self-check fails (cut-only
    degradation, never the verdict). Read-only over the engine state. *)
val hnf_cut : 'tok t -> (Term.t * 'tok list) option

(** [cg_cut t] — the Stage B3 Chvátal–Gomory SEPARATION cut (charter
    logs/lia-cuts-charter.md; the rings prize, logs/lia-cuts-b2-log.md §next rung). Same
    tight-constraint system, emission contract, and fail-closed self-check as {!hnf_cut},
    but where {!hnf_cut} REJECTS an HNF-row multiplier that is negative on some inequality
    row, [cg_cut] shifts it into the tight cone by the minimal nonnegative INTEGER shift
    on the restricted rows — this preserves the multiplier's integer image ([μ·A]) and the
    fractional part of [μ·c] (the shift adds an integer to it), so the cut stays a
    T-valid, vertex-separating Chvátal–Gomory cut while becoming emittable. Every
    fractional HNF row then yields a valid cut over a MULTI-ROW combination (the cuts
    {!hnf_cut} could not emit); the smallest-[‖f'‖₁] one is returned. Re-verified against
    the original A/c before emission ([μ' ≥ 0] on inequalities, [μ'·A] integer,
    [μ'·c ∉ ℤ]); a failure drops the cut. [None] when no fractional row yields a cut, the
    z3-parity caps are exceeded, or a contributing row is not a real trail literal.
    Read-only over the engine state.

    [cut_gate] (task #60 cut-policy) is an optional emission filter on the selected best
    candidate: it is called with the cut's coefficient count [nnz], its antecedent-row
    support size [ants], and the tight system's [m] rows / [n] cols, and the cut is
    emitted only if it returns [true]. The default always emits — verdict+search-identical
    to callers that do not pass it (not allocation-identical: the support scan + gate
    callback still run). Rejecting a cut yields [None] (the caller branches instead), a
    strictly weaker action, so the gate is soundness-neutral. *)
val cg_cut
  :  ?cut_gate:(nnz:int -> ants:int -> m:int -> n:int -> bool)
  -> 'tok t
  -> (Term.t * 'tok list) option

(** [suggest_branch t] — after a {!Sat_candidate} — is the B&B split request for the
    lowest-tag non-integer variable [x] with value [v]: the atom pair
    [(x <= floor v, x >= floor v + 1)] built through the session {!Context}, mirroring the
    adapter's [Split] (ADR-0005 D5). [None] iff the current rational model is already
    integral. *)
val suggest_branch : 'tok t -> (Term.t * Term.t) option

(** [propagate t] returns atoms (with polarity and premise witnesses) that the currently
    asserted bounds theory-imply but that are not yet asserted — bound-to-bound
    propagation with lazy explanations (ADR-0005 D3, [Lia_bound]). Under
    [OXSMT_LIA_EQ_PROP=1], coincident lower/upper bounds propagate registered Int
    equalities with both premises, and an excluding bound propagates their negation with
    one premise. Registered atoms only ({!register_atom}). *)
val propagate : 'tok t -> (Term.t * bool * 'tok list) list

(** [cube_model t] — after a {!Sat_candidate} whose ℚ-model is not integral — runs the
    Bromberger–Fleury unit cube test ({!Simplex.cube_test}) to find an integer model with
    no branch-and-bound. [Some model] (a total integer model over the problem variables,
    also returned by a subsequent {!model} until the next {!check}) iff the test succeeds;
    [None] means fall back to {!suggest_branch}. Sound: the point is re-verified feasible
    by the simplex and by the session's independent R1 check, so a wrong point degrades to
    [unknown], never a wrong [sat]. Sets the {!solve_integer}-style dirty flag (the test
    push/pops simplex bounds). *)
val cube_model : 'tok t -> (Term.t * int) list option

(** [model_find ?node_budget t] — after a {!Sat_candidate} whose ℚ-model is not integral —
    runs a cut-free, arbitrary-precision, round-to-nearest DIVING branch-and-bound
    entirely inside the theory (no CDCL(T) round-trip, no cut generation), bounded by
    [node_budget] B&B nodes. Returns [true] iff it found an integer model of the currently
    asserted atom set — stashed for a subsequent {!model_bigint} until the next {!check};
    [false] means "no model within budget" and the caller MUST fall back to
    {!suggest_branch} (never treat [false] as unsat). Re-runnable at every [Final] (unlike
    {!cube_model}'s once-guard): each call re-solves from the CURRENT simplex bounds, so
    it composes with the combiner's disequality-resolution splits between Finals. Intended
    for the convert class (bounded ℤ-feasible conjunctions with 2^32/2^64-scale
    coefficients, where the CDCL(T)-delegated B&B wanders); the engine behind
    [Lia_adapter]'s OXSMT_LIA_MODELFIND mode. Sound: the leaf assignment is
    simplex-feasible and integral, and every branch bound only restricts, so it is a
    genuine ℤ model of the SIMPLEX constraints; the session's independent R1 check
    re-validates, so a bug degrades to [unknown], never a wrong [sat]. Overflow-safe by
    construction (all branch points/values go through the [Bigint] projections), so unlike
    {!solve_integer} it does not degrade on the 2^64-coefficient inputs. Sets the
    {!solve_integer}-style dirty flag.

    {b Known limitation (logs/convert-impl-report.md).} The dive sees only the simplex
    (=/≤/≥) constraints. Int DISEQUALITIES are routed to the EUF congruence child + the
    combiner's pins ({!Combine}), NOT to LIA, so a returned model can violate an asserted
    [px <> py]; the combiner's [find_disagreement] then rejects it and splits. On a
    formula with hundreds of pinned disequalities (convert), the dive/split interleaving
    does not converge — a disequality-AWARE dive is the follow-up. *)
val model_find : ?node_budget:int -> 'tok t -> bool

(** [set_pin_hint t pairs] installs a READ-ONLY snapshot of the combinator's pinned Int
    disequality pairs [(px, py)] (meaning [px <> py]) for the next {!model_find} dive to
    steer branching away from equating them. A hint only — soundness is unchanged (the
    combinator's [find_disagreement] and the session R1 check validate every model); pairs
    whose terms are not problem variables are ignored by the dive. Empty by default. *)
val set_pin_hint : 'tok t -> (Term.t * Term.t) list -> unit

(** [model t] is the integer assignment of the problem variables; valid only after
    {!solve_integer} returned [Int_sat] or {!cube_model} returned [Some] (raises
    {!Failure} if a value is non-integral and no cube model is stashed). *)
val model : 'tok t -> (Term.t * int) list

(** [model_bigint t] is {!model} with arbitrary-precision integer values (ADR-0018): same
    validity precondition, but a value exceeding int63 (e.g. a uint256 constant) is
    projected via {!Rational.num_bigint} and never overflows. This is the form the
    combinator consumes at the [Model.Int] sink; the int-tier B&B/cube drivers keep
    {!model}. *)
val model_bigint : 'tok t -> (Term.t * Oxsmt_core.Bigint.t) list

(** [rational_value t term] is the current δ-rational assignment of [term]'s variable (its
    finite part), for inspection/tests; [Rational.zero] for an unseen term. *)
val rational_value : 'tok t -> Term.t -> Rational.t

(** [fixed_bounds t term] returns [(value, lower_reason, upper_reason)] exactly when
    [term]'s tightest ACTIVE ASSERTED (User) lower and upper bounds coincide on an
    integer. The reasons are oriented: [lower_reason] proves [term >= value] and
    [upper_reason] proves [term <= value]. Slack-aware ([x >= c] lands on the [-x] slack)
    and const-aware ([x] and [x + 1] share one variable). This is the fabric fix-TRIGGER. *)
val fixed_bounds : 'tok t -> Term.t -> (Rational.t * 'tok * 'tok) option

(** [oriented_bound_value t term which] — ADR-0014 Stage 1b F1-SEM independent oriented-
    bound accessor (§B.1 C1/Rev5-B3). Returns [(token, value)] for [term]'s tightest
    active asserted bound on [which] side, with NO cross-side equality bundling — a
    SEPARATE consumer from {!fixed_bounds}, so the fabric's semantic verifier can
    re-derive a fixed-value pair's oriented premises independently of the trigger's tuple
    and REJECT a wrong value / swapped-or-foreign token / dropped bound. *)
val oriented_bound_value
  :  'tok t
  -> Term.t
  -> [ `Lower | `Upper ]
  -> ('tok * Rational.t) option

(** [notify_equality t eq ~premise] asserts an EUF-entailed positive Int equality
    (ADR-0014 Stage 2 fabric [new_eq]) into the tableau, attributed to [premise]. Behaves
    like {!assert_atom} for a positive equality, EXCEPT one case: a [0 = 0] TAUTOLOGY (the
    equality's variable combination AND constants both cancel) is a NO-OP instead of
    raising {!Unsupported}. The merge callback re-surfaces such an equality when
    congruence unions two terms LIA already relates; the re-notification carries no
    constraint, so skipping it is sound and complete and avoids degrading the query to
    [unknown]. Every UNSATISFIABLE constant equality is NOT skipped — it keeps raising
    {!Unsupported} (fail closed to [unknown], as {!assert_atom} does), because silently
    dropping it would be a wrong-verdict hole. This covers both an unfolded [0 = k]
    ([k <> 0]) and a [Context.eq]-FOLDED [c1 = c2] ([c1 <> c2]), which arrives as a
    [Bool_const false]; only a [true]-folded / [0 = 0] tautology is the no-op. *)
val notify_equality : 'tok t -> Term.t -> premise:'tok -> unit

(** [push t] / [pop t n]: backtrack frames (ADR-0005 D6), delegated to the simplex bound
    stack; created variables persist (idempotent re-registration). *)
val push : 'tok t -> unit

val pop : 'tok t -> int -> unit

(** ADR-0014 Stage 4.2 sub-frame checkpoint/rewind (chrono earliest-removed incremental
    undo). [checkpoint t] captures the simplex bound watermark plus the reported/eq/false
    bookkeeping counts; [rewind_to_checkpoint t c] restores exactly to it (draining the
    simplex bound trail and un-reporting the atoms recorded since, as {!pop} does),
    without touching the frame stack. Requires the theory to be at a single base frame
    (the CB checkpoint-driver invariant); raises otherwise. *)
type checkpoint

val checkpoint : 'tok t -> checkpoint
val rewind_to_checkpoint : 'tok t -> checkpoint -> unit

(** Total simplex pivots performed (determinism/perf stat, DESIGN.md §8). *)
val pivot_count : 'tok t -> int

(** Number of times {!solve_integer} degraded an internal overflow to {!Int_unknown} — the
    distinct stat attributing the native-int incompleteness gap (DESIGN.md §8 bench
    digest). *)
val overflow_count : 'tok t -> int

(** [true] once an escaped overflow has bricked the instance (see the "Poisoned instances"
    note); safe to call at any time, never raises {!Poisoned}. *)
val is_poisoned : 'tok t -> bool

module For_testing : sig
  (** The equality and hash used by the slack-dedup table, including canonical ordering.
      Exposed only for the soundness-critical key discrimination test. *)
  val slack_key_equal : (int * Rational.t) list -> (int * Rational.t) list -> bool

  val slack_key_hash : (int * Rational.t) list -> int
end
