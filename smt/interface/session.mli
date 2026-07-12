(** Session API (DESIGN.md §3): declare symbols, assert terms, check-sat, push/pop.

    A session bundles one {!Oxsmt_core.Env.t} + {!Oxsmt_core.Context.t}, a
    {!Oxsmt_preprocess.Preprocess} handle, the {!Oxsmt_preprocess.Cnf} clausifier, the
    {!Oxsmt_solver.Sat} core, and — the M4 change — the Nelson-Oppen combined EUF+LIA
    theory stack driven through the CDCL(T) seam ({!Cdclt}). All term construction threads
    the one context (ADR-0003 Decision 6), so the same atom is the same SAT variable AND
    the same theory atom throughout the session. Shipped, stdlib-only (INVARIANTS.md I3):
    it never links the test-only SMT-LIB parser.

    {b THE SOUNDNESS RULE (v1 is now a real CDCL(T) solver over EUF+LIA).}

    {i M1 regime (superseded).} Before the theories were wired, the boolean skeleton
    over-approximated: theory atoms were opaque Booleans with no arithmetic/congruence
    meaning, so a propositional [Sat] could be theory-inconsistent and had to be
    downgraded to [Unknown]. Only propositional [Unsat] and pure-Boolean formulas got real
    verdicts.

    {i M4 regime (current).} The combined theory is installed on the SAT core before any
    clause (pristine-attach) and {b every} theory atom (order [Le], non-Bool [Eq], applied
    predicate) is registered with it as it is clausified. The CDCL(T) loop then
    interleaves Boolean search with theory propagation and, at each full Boolean model, a
    complete [Final] theory check (LIA branch-and-bound for integrality, model-based
    Nelson-Oppen for the EUF/LIA arrangement). Consequences:

    {ul
     {- {b [Unsat]} is sound as before — theory conflicts only remove models — and is now
        also derived {e via} theory conflicts (a propositionally-satisfiable skeleton
        whose theory is inconsistent comes out [Unsat], not [Unknown]). This is the regime
        flip: the degradation honeypots ([degrade_*], propositionally-sat / theory-unsat)
        are now real [Unsat].
    }
     {- {b [Sat]} is reported {e only} when the [Final] theory check accepts a full
        Boolean model {e and} a self-checkable model is reconstructable — one with no
        applied uninterpreted symbol (see {!get_model}). Such a model is verifiable by the
        §8 layer-1 evaluator and is function-free, hence in the fragment the Nelson-Oppen
        combination decides soundly; a query whose model would need a function table is
        degraded to [Unknown] rather than reported on an unself-checked [Sat]. (This also
        firewalls the combination's current incompleteness on function applications that
        appear only inside an arithmetic atom — no purification pass exists yet, so their
        congruence can be missed; degrading to [Unknown] keeps every reported verdict
        sound.) A v1 completeness limit on the [Sat] direction, never a soundness one.
    }
     {- {b [Unknown]} is the disciplined fallback (never a guessed verdict). It is
        returned when — and only when — the theory stack cannot certify a definite answer:
       - a {!Oxsmt_core.Term.Overflow}/{!Oxsmt_core.Term.Unsupported} in preprocessing or
         an out-of-fragment atom the adapters reject (I8);
       - the {b CONTRACT-POISON firewall}: {e any} exception escaping the untrusted theory
         callbacks that {!check_sat}'s [Sat.solve] drives — a declared poison
         ([Lia.Poisoned], [Rational.Overflow], [Lia.Unsupported],
         [Combine.Combination_unsound], [Sat.Theory_contract_violation]) or an unforeseen
         [Failure]/[Invalid_argument]/[Not_found]/[Term.Overflow] from a bug in theory
         code — bricks the query to [Unknown] (I8: degrade, never crash, never a verdict
         from a bricked theory). The firewall is a catch-all, {e except} that
         [Out_of_memory] and [Stack_overflow] are re-raised (the process state is
         untrustworthy) and are the only escapees. Its boundary is precisely the
         [Sat.solve] call: model reconstruction and the session's own bookkeeping run
         outside it, so a programming error there surfaces as a crash rather than a
         silently-swallowed [Unknown];
       - a {b deliberate completeness degrade}: the internalization combinator raises
         [Combine.Incomplete] for a shape it soundly chooses not to decide (ADR-0010 §3.6:
         a structured Bool compound under a UF argument). This is a "known [Unknown]",
         distinct from the CONTRACT-POISON faults above; because [register_atom] runs both
         at {!assert_term} (base-frame interning) and mid-[Sat.solve] (split-atom
         re-registration), it is caught on BOTH paths and degrades sticky;
       - the deterministic {b split budget} is exhausted (the [Final]-check split loop has
         no intrinsic termination bound; see {!budget_exhausted}).

       A degraded session stays [Unknown] for the rest of its life (the poison is sticky).

       Distinct from all of the above is the {b effort budget} (board #60, see {!create}'s
       [max_effort] and {!effort_exhausted}): a deterministic cap on total search effort
       (SAT conflicts + decisions + seam [Final]-rounds). When it fires the query is
       [Unknown] with the BUDGET tag, but the session is NOT degraded — the cutoff poisons
       nothing, so the SAME query re-run at a larger [max_effort] can still be decided. It
       is a measurement/termination tool, never a soundness one: like the split budget and
       the poison firewall it only ever turns a would-be answer into [Unknown], never a
       [Sat]/[Unsat] from an unfinished search.
    }
    }

    Determinism (I6): no wall-clock anywhere; the split and effort budgets are counters;
    all theory iteration is deterministic. *)

type t

type verdict =
  | Sat
  | Unsat
  | Unknown

(** A model value / table cell (re-exported from {!Cdclt}). [VUninterp i] is a 0-based
    element index of its uninterpreted sort's finite universe. *)
type model_value = Cdclt.value =
  | VBool of bool
  | VInt of int
  | VUninterp of int

(** A total interpretation of one uninterpreted function/predicate (re-exported). *)
type fun_table = Cdclt.fun_table =
  { default : model_value
  ; cases : (model_value list * model_value) list
  }

(** A model binding (re-exported from {!Cdclt}): a nullary symbol's value, or a
    function/predicate table. *)
type model_binding = Cdclt.binding =
  | Const of string * model_value
  | Fun of string * fun_table

(** The finite-universe cardinality of one uninterpreted sort (re-exported). *)
type sort_card = Cdclt.sort_card =
  { sort_name : string
  ; card : int
  }

(** The full reconstructed model: uninterpreted-sort cardinalities + symbol bindings. *)
type model = sort_card list * model_binding list

(** A fresh session: empty env (with the reserved [div]/[mod] built-ins), fresh context,
    fresh SAT core with the combined EUF+LIA theory installed, one active (base) assertion
    frame. [split_budget] overrides the deterministic per-[check_sat] theory-split cap
    (default 10_000); a tiny value drives the split-budget path (see {!budget_exhausted})
    in tests.

    [max_effort] is the board #60 counted cutoff: the per-[check_sat] cap on total search
    effort (SAT conflicts + decisions + seam [Final]-rounds).
    {b Absent (the default) is UNBOUNDED}: the counter still runs — so {!effort} is always
    available for instrumented calibration — but never cuts off, and since the count is
    never surfaced by default the interactive / [make test] path is byte-identical to a
    build without the budget. A finite [max_effort] makes exhaustion return [Unknown] with
    the BUDGET tag ({!effort_exhausted}); per-check and poison-free (re-runnable at a
    larger cap). *)
val create : ?split_budget:int -> ?max_effort:int -> unit -> t

(** The session's {!Oxsmt_core.Env.t}. Exposed so a front end (e.g. the test-only SMT-LIB
    parser) can declare symbols and build assertion terms in the {e same} context the
    session solves over. *)
val env : t -> Oxsmt_core.Env.t

(** The session's {!Oxsmt_core.Context.t} (same rationale as {!env}). *)
val context : t -> Oxsmt_core.Context.t

(** [declare_sort]/[declare_fun]/[declare_const] declare into {!env}. They reject the
    reserved fresh-symbol namespace ([".oxsmt.*"], board #48) with [Invalid_argument] so a
    user symbol cannot collide with one preprocessing invents. *)
val declare_sort : t -> string -> Oxsmt_core.Symbol.t

val declare_fun : t -> string -> Oxsmt_core.Rank.t -> Oxsmt_core.Symbol.t
val declare_const : t -> string -> Oxsmt_core.Sort.t -> Oxsmt_core.Symbol.t

(** [assert_term t phi] preprocesses [phi] (ADR-0003 §5 passes), clausifies the boolean
    skeleton, registers each theory atom with the combined theory, and adds the clauses to
    the current frame. [phi] must be Bool-sorted and built through {!context}. An
    [Overflow]/[Unsupported]/rejected atom degrades the session to [Unknown] (I8). Legal
    before or after {!check_sat} (assert-after-check). *)
val assert_term : t -> Oxsmt_core.Term.t -> unit

(** An opaque handle to a lemma stored by {!assert_lemma}, for the tranche-1 manual
    instantiation scaffold {!instantiate}. *)
type lemma

(** Lemma provenance (for cores / messages). *)
type origin =
  | Named of string
  | Anonymous

(** What {!assert_lemma}'s [build] returns: the well-sorted Bool [body] over the minted
    qvars plus ground symbols, and the multi-triggers ([Term.t list list]: outer =
    alternative triggers, inner = conjunctive). Empty [triggers] requests auto-selection
    (a tranche-3 feature; tranche 1 stores them verbatim). *)
type lemma_def =
  { body : Oxsmt_core.Term.t
  ; triggers : Oxsmt_core.Term.t list list
  }

(** [assert_lemma t ~qvars ~build] states a universally-quantified lemma
    [forall qvars. body] (ADR-0012 §1.3, mint-before-build binder-builder form). The
    session mints one placeholder {!Oxsmt_ematch.Qvar.t} per [(name, sort)] in [qvars]
    FIRST, hands the array to [build], and [build] constructs [body]/[triggers] {e using}
    those handles (through {!context}), so occurrence-binding is by construction — the
    caller never spells a reserved placeholder name (R1). [body] must be Bool-sorted
    ([Invalid_argument] otherwise). The lemma is recorded in the CURRENT assertion frame;
    {!pop} retracts it and every instance drawn from it together (§1.5).

    While any lemma is live (in an active frame), THE SOUNDNESS RULE (§2) degrades a
    {!check_sat} of [Sat] to [Unknown] — E-matching is refutation-only, so satisfiability
    can never be concluded with a quantifier live. [Unsat] is reported unchanged (a ground
    instance is a valid consequence).

    Returns the stored {!lemma} handle (the ADR's [unit] widened additively for the
    tranche-1 manual path; a caller may ignore it). *)
val assert_lemma
  :  t
  -> qvars:(string * Oxsmt_core.Sort.t) list
  -> build:(Oxsmt_ematch.Qvar.t array -> lemma_def)
  -> lemma

(** {b Tranche-1 scaffold} (ADR-0012 §8 manual-instances path).
    [instantiate t lemma sigma] seeds a ground instance of [lemma] at substitution [sigma]
    (ground terms in the lemma's qvars order); the next {!check_sat} draws it through the
    real dedup + frame-scoped assertion pipeline. This stands in for the matcher until
    tranche 2, which generates substitutions by E-matching and retires this entry point.
    Each [sigma.(k)] must be ground; a [sigma] whose image still contains a placeholder is
    an internal bug ([Failure] from the instance minter). *)
val instantiate : t -> lemma -> Oxsmt_core.Term.t array -> unit

(** Open a new assertion frame. Assertions added until the matching {!pop} are retracted
    by it. Implemented with a fresh selector variable: frame clauses are guarded by the
    selector, which {!check_sat} assumes true while the frame is active (standard
    MiniSat-style retraction — nothing is physically removed). *)
val push : t -> unit

(** Close the innermost frame, deactivating its assertions. [Invalid_argument] if there is
    no matching {!push}. *)
val pop : t -> unit

(** Decide satisfiability of the active assertions via CDCL(T) under THE SOUNDNESS RULE.
    Repeatable; more assertions or push/pop may follow. *)
val check_sat : t -> verdict

(** The model of the most recent {!check_sat}, iff that call returned [Sat] {e and} a
    checkable model is reconstructable. Bindings are one [Const (symbol-name, value)] per
    constrained nullary symbol, sorted by name: the theory's Int / uninterpreted-sort
    constants {e unioned with} a [Bool] per declared propositional variable (a mixed
    Boolean/theory [Sat] covers both, so the §8 evaluator sees every declared symbol).
    Reserved internal witnesses ([".oxsmt.*"], e.g. an ITE lift) are excluded — the model
    names only user-declared symbols. [None] after [Unsat]/[Unknown], before any
    [check_sat], or when the [Sat]'s model would require a function table (any applied
    uninterpreted symbol is constrained) — a v1 completeness limit of the model
    reconstruction, not of the verdict. *)
val get_model : t -> model option

(** The SAT core's counter trio, monotonic across the session (DESIGN.md §8). *)
val stats : t -> Oxsmt_solver.Sat.Stats.t

(** Theory splits consumed by the most recent {!check_sat} (determinism/perf stat). *)
val splits : t -> int

(** [true] iff the most recent {!check_sat} degraded to [Unknown] by exhausting the split
    budget (the distinct split-budget stat; the query is otherwise unresolved). *)
val budget_exhausted : t -> bool

(** Effort consumed by the most recent {!check_sat}: SAT conflicts + decisions + seam
    [Final]-rounds (board #60). A deterministic function of the input (I6) — this is the
    per-file value the calibration run records to pick the cutoff, and the determinism
    check is that two runs report the same number. 0 before any {!check_sat}. *)
val effort : t -> int

(** [true] iff the most recent {!check_sat} returned [Unknown] because the {!create}
    [max_effort] cap fired (the BUDGET tag). Unlike {!budget_exhausted} this is NOT sticky
    and does not degrade the session — the same query is re-runnable at a larger cap. *)
val effort_exhausted : t -> bool

(** Lemma-tier instantiation stats (ADR-0012 §O4), distinct from {!splits}: [live_lemmas]
    currently in an active frame, and the cumulative [instances] generated / [rounds] run
    across the session. *)
type lemma_stats =
  { live_lemmas : int
  ; instances : int
  ; rounds : int
  }

val lemma_stats : t -> lemma_stats
