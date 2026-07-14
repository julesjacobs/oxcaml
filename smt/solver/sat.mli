(** Propositional CDCL SAT core — MiniSat design (Eén–Sörensson, "An Extensible
    SAT-solver", SAT 2003), deliberately novelty-free (DESIGN.md §5).

    Two-watched-literal propagation, 1UIP conflict analysis with clause learning and local
    (self-subsumption) minimization, VSIDS activity branching with exponential decay,
    phase saving, Luby restarts, and activity-based learned- clause deletion.
    Propositional only: it sees no theories and no terms. The clausifier (smt/preprocess)
    is its sole intended client and talks to it only through this interface.

    {b Determinism (I6).} No wall-clock and no randomness: restart and activity schedules
    are purely count-based, so two identical runs produce identical verdicts, models, and
    stats.

    {b Incrementality.} [add_clause] is permanent and may be called between [solve]s.
    [solve] takes optional assumptions; a fresh assumption set each call. Learned clauses
    persist across [solve]s.

    {b Encoding.} Variables are consecutive nonnegative ints from 0. A literal packs a
    variable and a polarity as [2*v] (positive) / [2*v+1] (negative); treat [lit] as
    opaque and build it with {!pos}/{!neg}. *)

type t
type var = int

(** A signed variable. Opaque; build with {!pos}/{!neg}, negate with {!neg_lit}. *)
type lit = private int

val pos : var -> lit
val neg : var -> lit
val neg_lit : lit -> lit
val var_of_lit : lit -> var

(** [true] when the literal is positive (i.e. built by {!pos}). *)
val sign_of_lit : lit -> bool

type result =
  | Sat
  | Unsat

(** A fresh solver with no variables and no clauses. *)
val create : unit -> t

(** Allocate and return the next variable. Variables are also auto-allocated on demand by
    {!add_clause} and {!solve} when a literal names one not yet created, so explicit calls
    are optional. *)
val new_var : t -> var

(** Number of variables allocated so far. *)
val num_vars : t -> int

(** The provenance of an added clause (ADR-0013 §4.0, RR5). [Query] is a genuine query
    clause from the clausifier/session; [Theory_lemma] is a CONTRACT-SPLIT / theory lemma
    added mid-solve. The certificate emitter routes a [Query] clause to an [Input] intro
    and a [Theory_lemma] clause to a [Valid_lemma] [Theory] intro — never an [Input]. *)
type origin =
  | Query
  | Theory_lemma

(** Add a permanent clause (disjunction of literals). Legal between [solve]s. Level-0
    simplification (tautology/duplicate/falsified-literal removal, unit propagation)
    happens here; an empty clause makes the instance permanently unsat.

    [origin] (default [Query]) tags the clause's provenance for certificate emission
    (ADR-0013 §4.0). It has no effect on solving — it is the frozen seam consumed by the
    trace's {!field-on_input}; a defaulted call is behaviourally identical to the untagged
    form. *)
val add_clause : ?origin:origin -> t -> lit list -> unit

(** [solve ?assumptions t] decides satisfiability under the given unit assumptions
    (default none). After [Sat], query the model with {!value}. After [Unsat] with
    assumptions, {!failed_assumptions} gives an unsat subset of them. *)
val solve : ?assumptions:lit list -> t -> result

(** Truth value of a variable in the model of the most recent [Sat]. Unspecified before
    any [Sat]. *)
val value : t -> var -> bool

(** The model of the most recent [Sat] as a [var]-indexed array (element [v] is
    {!value}[ t v]). Empty before any [Sat]. *)
val model : t -> bool array

(** A subset of the assumptions that is jointly unsatisfiable, valid after [solve]
    returned [Unsat] with a nonempty assumption set (the failed- assumption core, §7).
    Empty otherwise. *)
val failed_assumptions : t -> lit list

module Stats : sig
  (** The harness counter trio (DESIGN.md §8). Monotonic across [solve]s. *)
  type t =
    { conflicts : int
    ; decisions : int
    ; propagations : int
    }
end

val stats : t -> Stats.t

(** [var_activity t v] is the current VSIDS activity of variable [v] ([0.0] for a variable
    not yet allocated or never bumped). A read-only side channel — reading it never
    mutates the solver and has no effect on search; a client that does not call it is
    unaffected. Its intended use is a {!set_branch_filter} / relevancy driver that wants
    to align its own choices (e.g. which candidate atom to make branchable) with the
    solver's activity order rather than an arbitrary tie-break. Only the ordinal
    comparison of activities is meaningful across a run — the absolute value drifts with
    the global rescaling. *)
val var_activity : t -> var -> float

(** {2 Proof-readiness / certificate-emission hooks (§7; ADR-0013 §4.0)}

    A compile-out-able trace of the search that certificate emission (ADR-0013) attaches
    to. [None] by default —
    {b zero cost and bit-identical verdicts, models, and stats when unset}, and every hook
    is a pure side channel that never feeds back into search. The record is a frozen
    {e seam}: the emission bodies (the four terminal steps, the E3 [analyze_final] walk,
    the [on_input]/[on_unit] firing) land later as [sat.ml] internals (editable), so the
    {b signatures} here are complete against all four [Unsat] exits and the
    [Decision]/[Implied_by]/[Theory_prop] reason walk WITHOUT a further unfreeze.

    {b Id-resolvability invariant.} Every clause [id] a hook cites is resolvable against a
    {e content-bearing} event elsewhere in the stream — {!field-on_input} (id + clause +
    origin), {!field-on_learned} (id + clause), or {!field-on_theory_clause} (id +
    clause + role). No hook emits a bare id whose clause was never surfaced, so
    {!unsat_conclusion} carries ids only. *)

(** Which theory-transient clause {!field-on_theory_clause} surfaced, so the emitter picks
    the right leaf shape. [Reason] is the propagation clause [p ∨ ¬p₁ ∨ … ∨ ¬pₖ] (the
    implied literal at slot 0 — the EUF/LIA "¬Γ ∨ p" propagation leaf); [Conflict] is the
    falsified premise clause [¬p₁ ∨ … ∨ ¬pₙ] (the theory conflict leaf). *)
type theory_clause_role =
  | Reason
  | Conflict

(** The empty-clause conclusion of a solve, one constructor per [Sat] [Unsat] exit
    (ADR-0013 §4.0 E1–E4). Each carries exactly what the terminal [||] step needs; the ids
    resolve per the id-resolvability invariant above.

    - [Root_empty] — E1 (a [Query] clause) / E4 (a [Theory_lemma]) filtered to [] under
      level-0 simplification; the terminal step is level-0 RUP of [input_id] against the
      checker's re-derived unit closure. E1 vs E4 is the [origin] recorded for [input_id].
    - [Level0_conflict] — E2, a level-0 conflict clause (a Boolean clause, or a theory
      conflict transient — including the empty clause of an unconditional
      [T_conflict []]); terminal step is level-0 RUP of [conflict_id].
    - [Failed_assumption] — E3, the universal session exit: [antecedents] is the
      assumption-forcing reason chain in RUP-consumption order ([Implied_by] clause ids
      and materialized [Theory_prop] reason ids); after the selector strip it derives []. *)
type unsat_conclusion =
  | Root_empty of { input_id : int }
  | Level0_conflict of { conflict_id : int }
  | Failed_assumption of { antecedents : int list }

type trace =
  { on_input : id:int -> clause:lit array -> origin:origin -> unit
    (** fires for every asserted input clause with a stable [id], {e before} level-0
      filtering — including a clause that filters to [] and is therefore not retained
      (E1/E4 [Root_empty] id-resolvability depends on this: the terminal step cites that
      clause's [id]). [origin] splits genuine query inputs from theory Split/lemma
      clauses. *)
  ; on_unit : id:int -> lit:lit -> unit
    (** fires once per standing level-0 unit; the checker re-derives the unit closure by
      propagation, so no forcing-clause provenance is carried. *)
  ; on_learned : id:int -> clause:lit array -> antecedents:int list -> btlevel:int -> unit
    (** fires once per learned clause with a fresh clause [id], the learned [clause]
      (asserting literal at index 0), the [antecedents] resolved to derive it, and the
      [btlevel] the solver then backjumps to. Contract (ADR-0013 §1.4): [antecedents] in
      ordered-RUP order (the reason clauses in reverse-resolution order, conflict last),
      and when a trace is active the emitted-and-stored clause is the {e unminimized} 1UIP
      clause. Learned units fire it too. Zero cost when no trace is set — antecedents are
      not even accumulated. *)
  ; on_theory_clause : id:int -> clause:lit array -> role:theory_clause_role -> unit
    (** fires when a lazy theory reason / conflict clause is materialized, surfacing its id
      ↔ clause so any hint that cites a theory transient (in {!field-on_learned}'s
      antecedents or an {!unsat_conclusion}) resolves to an emitted leaf. The theory-side
      witness (EUF proof tree / LIA multipliers) is attached off-seam by the adapter. *)
  ; on_unsat : unsat_conclusion -> unit
    (** fires at whichever [Sat] [Unsat] exit fires, carrying the terminal [||]-step data. *)
  }

(** Install (or, with [None], remove) the trace; see the bit-identical-when-unset note
    above. {b Lifecycle contract:} a trace must be attached before the first
    {!add_clause}. Attaching one after clauses exist — or detaching and re-enabling
    mid-lifecycle — is unsupported: the emitter relies on observing every input from the
    start, so it must never reach a state where a conclusion cites the [id] of a clause
    added while untraced. *)
val set_trace : t -> trace option -> unit

(** {2 Theory seam — CDCL(T) (ADR-0005 §3; the seam for the M4 EUF/LIA adapters)}

    The same style of event interface as {!trace}: a settable record, [None] by default,
    so the pure propositional core is unchanged (one [None] branch of overhead when unset,
    and — crucially — bit-identical verdicts, models, and counters). When set, [solve]'s
    propagation loop and its full-model checkpoint consult it, which is why this is not an
    additive edit to [solve]/propagate (and why [sat.mli] freezes at M4).

    The seam is soundness-preserving by construction: a theory conflict is learned exactly
    like a propositional one (1UIP over the negated premise set), and a theory propagation
    carries a lazy reason retrieved only if conflict analysis needs it. Every [lit]
    crossing the seam names a SAT var the adapter registered 1:1 with a theory atom
    (ADR-0005 CONTRACT-ATOM); the core never inspects which vars are theory atoms — the
    adapter filters. *)

type theory_result =
  | T_consistent of lit list
  (** consistent; theory-implied literals to enqueue as true. The reason is LAZY: the core
      calls {!field-explain} only if the literal enters 1UIP analysis (ADR-0005 D3). An
      empty list is the plain "consistent, nothing implied". *)
  | T_conflict of lit list
  (** inconsistent: the asserted premise set whose conjunction is T-unsat
      (precedence-valid, CONTRACT-EX). The core injects its negation [¬l₁ ∨ … ∨ ¬lₙ] as
      the falsified conflict clause and drives backjumping. The empty set is an
      unconditional theory contradiction. *)
  | T_lemma of lit list list
  (** clauses to add mid-solve: CONTRACT-SPLIT disjunctions (a B&B branch or an N-O
      ℤ-trichotomy). Each inner list is one clause over atoms the adapter has already
      internalized via {!new_var}. Returned at [~final:true] (a Final-effort Split). *)

type theory =
  { on_assign : lit -> unit
    (** trail-extension notify: [lit] was just placed on the trail (decision, propagation,
      assumption, or learned unit). Fires in trail order. The adapter forwards its own
      atoms to [THEORY.assert_lit] and ignores the rest. *)
  ; on_backtrack : level:int -> unit
    (** backjump notify: the trail has just been unwound to decision [level]. The adapter
      forwards to [THEORY.pop], discarding theory state asserted above [level]. Fires on
      every real unwind (backjump, restart, split, end of solve). *)
  ; check : final:bool -> theory_result
    (** [~final:false]: cheap in-search check (ADR-0005 [Propagate] effort), driven to a
      fixpoint interleaved with Boolean propagation. [~final:true]: a complete check at a
      full Boolean model (ADR-0005 [Final]: B&B integrality, model-based N-O) —
      [T_consistent []] here means the theory accepts the model (the query is SAT). *)
  ; explain : lit -> lit list
    (** the lazy, precedence-valid reason for a literal this theory propagated via
      [T_consistent] (CONTRACT-EX: every returned lit must be currently true and asserted
      STRICTLY before [lit] on the trail). Called only during conflict analysis; a
      violation raises {!Theory_contract_violation} rather than corrupting 1UIP. *)
  }

(** Raised when a plugged theory violates a seam soundness contract the core cannot
    otherwise uphold: a [T_conflict]/propagation whose premise set is not all currently
    true, or an [explain] premise not asserted strictly before the literal it explains
    (CONTRACT-EX). Unconditional (not an [assert] the runtime could drop) — learning from
    a corrupt explanation is a soundness break. The engine's CONTRACT-POISON handling
    catches it and degrades the query to [unknown]. *)
exception Theory_contract_violation of string

(** Attach (or, with [None], detach) a theory. Must be called on a PRISTINE solver — no
    clauses added and an empty trail — else it raises [Invalid_argument]. Lifecycle
    contract: attaching after clauses/units exist would leave the theory unaware of trail
    literals it never heard (a wrong-[Sat] risk on theory-unsat instances), and detaching
    mid-lifecycle would strand theory-propagated literals whose lazy reasons can no longer
    be reconstructed. The driver installs the theory first, before asserting. *)
val set_theory : t -> theory option -> unit

(** {2 Effort-budget tick hook (board #60)}

    A settable [unit -> unit] side-channel, modeled on {!trace}/{!set_theory}: [None] by
    default, so the pure propositional core is bit-identical (one [None] branch of
    overhead when unset — no counter, no allocation, no behavior change). When set,
    [solve] calls it at each SAT {b conflict} and each SAT {b decision} — the two
    unbounded-in-principle events of Boolean search. The driver installs a closure that
    ticks a deterministic effort counter and raises to unwind [solve] once a
    per-[check_sat] cap is exceeded (the counted, load-independent cutoff replacing the
    wall clock for corpus measurement).

    The core treats the hook as opaque: it stores no counter itself and knows nothing of
    the budget, so [oxsmt_solver] keeps its stdlib-only, dependency-firewall-clean surface
    (I3). Any exception the hook raises propagates out of [solve] uncaught — the driver's
    [check_sat] boundary is the sole intended catch site. Ticking does not touch the
    search path, so with the hook unset (or an unbounded cap) verdicts, models, and the
    counter trio are unchanged. *)
val set_budget_tick : t -> (unit -> unit) option -> unit

(** The current decision level (0 at the base, before any decision). Exposed so a theory
    adapter can tag each {!field-on_assign}ed literal with the level at which it was
    asserted — the level {!field-on_backtrack} later references to undo trail-synchronized
    theory state. Reading it inside [on_assign] is a pure query (no re-entrancy). *)
val decision_level : t -> int

(** {2 Decision branch-filter hook (relevancy)}

    A settable [var -> bool] predicate, same [None]-by-default side-channel discipline as
    {!set_trace}/{!set_theory}/{!set_budget_tick}: with the hook unset the branching
    engine is {b bit-identical} to today (verdicts, models, and the conflicts/decisions/
    propagations trio unchanged) — one [None] branch of overhead in {!field-pick}.

    When set, the branching heuristic will not {b decide} an unassigned variable [v] for
    which [filter v] is [false]; such a variable is skipped and kept as a future candidate
    (re-inserted into the activity order), so once [filter v] becomes [true] it is
    branched again. When every remaining unassigned variable is filtered out, branching
    yields no decision — the search reports a complete assignment over the {e branchable}
    variables and hands off to the theory's [Final] check exactly as it does when the
    VSIDS order is exhausted. The intended client is a relevancy driver that maintains,
    over the assignment trail (via {!field-on_assign}/{!field-on_backtrack}), which atoms
    are relevant to satisfying the top-level formula under the current partial model, and
    filters out the irrelevant ones (z3's [smt_relevancy]): a satisfied [(or a1 … a5)]
    makes its unset disjuncts irrelevant, so they are not decided and cannot spuriously
    over-constrain.

    {b Soundness — what the core does and does not guarantee.} The filter adds no literal
    to the trail and no clause, so it cannot manufacture a conflict: an [Unsat] the
    filtered search reaches is over genuinely-asserted literals, exactly as without the
    filter — {b no wrong [Unsat]}, unconditionally. It is {b not}, however, safe to trust
    a filtered [Sat] without a model check. When only filtered-out variables remain,
    branching yields no decision and the core reports the current {e partial} assignment
    as complete WITHOUT checking that every clause is satisfied (it hands off exactly as
    on an exhausted VSIDS order — via the theory [Final] check, or, with no theory,
    directly). So a filter that leaves all of some clause's literals unassigned can drive
    the core to report [Sat] on an assignment that falsifies that clause (a wrong-[Sat]
    reachable from this API — codex S1). Therefore any client that installs a filter MUST
    re-validate a [Sat] against the original formula with a full (total) model check and
    treat a failure as [unknown]; the core does not itself certify that the
    branchable-only assignment models the clause set. oxsmt's session does this —
    [commit_sat]'s in-process [Model_check] gates every reported [Sat], fail-closed — so a
    filter that wrongly marks a needed atom irrelevant costs at most a query degraded to
    [unknown], never a wrong verdict, {e for that consumer}.

    {b Filter totality / exception-safety.} The filter is called mid-scan in [pick_branch]
    on a variable already popped from the decision heap. The core is exception-safe: on
    any exit — including the filter {e raising} — every variable popped in that call (the
    stashed ones and the one in flight) is re-inserted into the heap before the exception
    propagates, so no variable is lost and the heap remains complete for the next solve
    (untrailed popped vars would otherwise NOT be restored by [cancel_until 0]). The
    filter SHOULD nonetheless be total (a pure lookup into precomputed marks); a raise
    degrades the surrounding solve, it does not corrupt the core.

    {b Certificate independence.} The trace/certificate machinery (ADR-0013 §4.0)
    validates the {e clauses} learned and the input/unit closure, never the {e order} in
    which decisions were taken; a branch-filter changes only which variable is decided
    next, so an installed filter leaves every {!trace} hook's contract and the replayed
    proof unaffected. A pure side channel: it never feeds conflict analysis and never
    alters a learned clause. *)
val set_branch_filter : t -> (var -> bool) option -> unit

(** {2 CNF preprocessing / inprocessing — eliminable-variable marking (DESIGN.md A10)}

    Mark variable [v] as eligible for CNF-level variable elimination (bounded variable
    elimination / blocked-clause elimination, Jacobs 2021 "Bounded clause elimination").
    The core DEFAULTS every variable {b frozen} — never eliminated — so a client that
    never calls this (the default) leaves the whole feature inert: preprocessing
    eliminates nothing and the search is {b bit-identical} to today (verdicts, models, and
    the conflicts/decisions/propagations trio unchanged). This is the "when in doubt,
    freeze" discipline made structural: only a variable a client has {e explicitly}
    certified as invisible outside the SAT core — a pure auxiliary (Tseitin) structure
    variable that no model path reads, that is not a theory-seam atom, an
    assumption/selector literal, or a variable any re-added clause can name — may be
    marked eliminable, and a forgotten marking costs only effectiveness, never soundness.

    Preprocessing itself is env-gated ([OXSMT_SATPRE], default OFF) and runs at [solve]
    entry (decision level 0); it is additionally disabled whenever a {!set_trace}
    certificate trace is installed (the added resolvents / deleted clauses are not yet
    routed through certificate emission). When it eliminates a marked variable [v] it
    records the deleted clauses on a per-instance reconstruction stack; the model snapshot
    taken at [Sat] reconstructs [v]'s value (flip-to-satisfy, per the note's Lemma 1)
    before {!value}/{!model} read it, so a reported model is correct over {e every}
    variable including eliminated ones — unconditionally, with no downstream check
    required (the raw-SAT-API contract). Marking is idempotent and legal at any time. The
    two elimination forms differ on the incremental re-add of a clause naming an
    eliminated variable: {b bounded variable elimination} RESTORES the variable (its
    deleted clauses are re-added) so the elimination stays sound under incremental
    additions, whereas {b equivalent-literal substitution} instead RAISES
    [Invalid_argument] on such a re-reference — its equivalence-establishing clauses were
    rewritten away, so sound reactivation would need incremental-ELS machinery
    (Fazekas–Biere–Scholl, SAT 2019) that is not built, and failing loud is preferred over
    a silent wrong result. Both cases are contractually unreachable for a conforming
    client, because an eliminable variable is by the paragraph above one that no re-added
    clause can name. *)
val set_eliminable : t -> var -> unit
