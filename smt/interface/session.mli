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

(** A Boolean atom together with its asserted polarity. [true] means the atom itself and
    [false] its negation. Keeping polarity out of band lets assumption cores be read
    without interning a [Not] term. *)
type assumption = Oxsmt_core.Term.t * bool

(** The result of {!check_sat_assuming}. [unsat_core] is [Some core] exactly when
    [verdict = Unsat]. *)
type assumption_check =
  { verdict : verdict
  ; unsat_core : assumption list option
  }

(** A model value / table cell (re-exported from {!Cdclt}). [VUninterp i] is a 0-based
    element index of its uninterpreted sort's finite universe. *)
type model_value = Cdclt.value =
  | VBool of bool
  | VInt of Oxsmt_core.Bigint.t (* arbitrary precision (core-bignum W2) *)
  | VReal of Oxsmt_lia.Rational.t
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
    larger cap).

    [lemma_gen_budget] caps the number of ground lemma instances generated per [check_sat]
    (ADR-0012 §1.4); on exhaustion the instantiation loop degrades to [Unknown] rather
    than hanging (a matching-loop lemma such as associativity never runs away). Absent =
    the manager's generous deterministic default.

    [enable_relevancy] installs the dynamic-relevancy branch filter (task #24, QF_UF): the
    decision heuristic only branches on atoms relevant to satisfying the formula under the
    current partial assignment. Absent, it defaults to the [OXSMT_RELEVANCY] environment
    gate (OFF unless that names an on value), so the shipped / [make test] path is
    byte-identical to a build without it. Soundness is backstopped by the fail-closed
    [Model_check] on every reported [Sat], so a wrong relevancy marking can only cost a
    solve to [unknown], never a wrong verdict; verified for the pure QF_UF path (see
    logs/quf-propagation-log.md). Tests pass it explicitly to exercise both settings. *)
val create
  :  ?split_budget:int
  -> ?max_effort:int
  -> ?lemma_gen_budget:int
  -> ?enable_relevancy:bool
  -> ?seed_lemmas:bool
  -> unit
  -> t
(** [seed_lemmas] overrides the [OXSMT_LEMMA_SEED] gate for MBQI-lite ground-term seeding
    of trigger-inert universals (chunk 3); absent, it defaults to that env gate
    (DEFAULT-ON). Tests pass it explicitly to exercise the seeding-on and seeding-disabled
    (RED) paths. *)

(** The session's {!Oxsmt_core.Env.t}. Exposed so a front end (e.g. the test-only SMT-LIB
    parser) can declare symbols and build assertion terms in the {e same} context the
    session solves over. *)
val env : t -> Oxsmt_core.Env.t

(** The session's {!Oxsmt_core.Context.t} (same rationale as {!env}). *)
val context : t -> Oxsmt_core.Context.t

(** [parse_minter t] is the cap-backed minter for theory-internal reserved symbols
    ([".oxsmt.<theory>.*"], board #58 O-MINTER), for a front end that must mint one
    mid-parse — the SMT-LIB parser's [?internal_mint] hook, because arrays op symbols are
    per-sort instantiations discovered only at first [select]/[store] use and so cannot be
    pre-minted at a declaration site. It returns an OPAQUE
    {!Oxsmt_core.Internal_minter.t}, NOT a bare [string -> Rank.t -> Symbol.t] closure:
    the holder can mint only the marker names the session sanctions (via the minter's
    [admit] gate) and never obtains the {!Oxsmt_core.Env.reserved_cap} or a re-delegatable
    general closure — so a caller holding only a [t] cannot forge an arbitrary reserved
    name (the O-MINTER narrowing; ADR-0012: [Session] stays the sole cap holder). The
    sensitive reserved namespaces (arrays ext witness, datatype testers, qvars,
    preprocessing witnesses) are minted directly through [Env.declare_reserved] by trusted
    code and are NEVER admitted through this door. The sanctioned set is the parse-time
    theory vocabulary: it admits the bit-vector markers ([Oxsmt_core.Bv.is_bv_name],
    [.oxsmt.bv|...]); a further theory (arrays) widens it with its own grammar.

    A holder can still mint any ADMITTED shape, so admitting a grammar is PAIRED with that
    theory's consuming-side check (bit-vectors: [Oxsmt_core.Bv.view] verifies
    operand/result sorts and arity), which keeps a name/rank-mismatched mint inert. *)
val parse_minter : t -> Oxsmt_core.Internal_minter.t

(** [set_arrays t defs] installs the array [select]/[store] symbol registry the front end
    parsed ({!Oxsmt_core.Array_defs}), routing the session onto the standalone arrays
    theory (QF_AX: read-over-write + extensionality). Must precede {!assert_term}. A
    non-empty registry also degrades any [Final]->[Sat] on the problem to [Unknown] in v1
    (sat models on arrays are not yet self-checked); UNSAT is unaffected. A no-op with an
    empty registry.

    RESET-PER-QUERY (task #54, contract-A): see {!set_datatypes} — replacing the array
    registry after a prior query instantiated a theory invalidates it (fresh rebuild at
    the next intern), and raises [Invalid_argument] if attempted with live assertions. *)
val set_arrays : t -> Oxsmt_core.Array_defs.t -> unit

(** [declare_sort]/[declare_fun]/[declare_const] declare into {!env}. They reject the
    reserved fresh-symbol namespace ([".oxsmt.*"], board #48) with [Invalid_argument] so a
    user symbol cannot collide with one preprocessing invents. *)
val declare_sort : t -> string -> Oxsmt_core.Symbol.t

val declare_fun : t -> string -> Oxsmt_core.Rank.t -> Oxsmt_core.Symbol.t
val declare_const : t -> string -> Oxsmt_core.Sort.t -> Oxsmt_core.Symbol.t

(** [set_datatypes t defs] installs the algebraic-datatype shapes (GOALS Datatypes) the
    front end parsed ({!Oxsmt_core.Datatype_defs}). The sorts/constructors/selectors/
    testers must already be declared as ordinary symbols (via {!declare_sort}/
    {!declare_fun}); this records their datatype structure. A non-empty [defs] installs
    the DT theory (an e-graph client: EUF congruence + the datatype axioms) for this
    session in place of the EUF+LIA stack, so it must precede {!assert_term}/{!check_sat}.

    RESET-PER-QUERY (task #54, contract-A ruling). Each query's sort/datatype declarations
    are self-contained and PRECEDE its assertions. Calling this (or {!declare_datatype} /
    {!set_arrays}) to mutate the datatype/array registry AFTER a prior query has already
    instantiated a theory INVALIDATES that cached theory: it is dropped along with the
    SAT-var<->atom bijection, and the next {!check_sat} rebuilds the theory fresh against
    the new registry (so a re-used term re-classifies correctly — the none->DT,
    DT->arrays, and loader-overwrite patterns all return correct verdicts rather than the
    #51 interim [unknown]). This is sound only BETWEEN queries: mutating the registry with
    live state bound to the dropped bijection — live ground assertions (no {!pop} since
    the last {!check_sat}) OR a live quantified lemma ({!assert_lemma}; user input,
    outside the assertion set, and a base-frame lemma survives {!pop}) — raises
    [Invalid_argument] rather than resetting under it. Declarations (and lemmas) must
    precede assertions within a query. The common single-query path (all declarations
    before the first {!check_sat}, no cross-query lemma) never triggers a reset and is
    byte-identical to before. *)
val set_datatypes : t -> Oxsmt_core.Datatype_defs.t -> unit

(** [true] iff a datatype has been declared for this session ([set_datatypes] /
    [declare_datatype] with a non-empty registry) — i.e. the standalone DT theory is
    installed. A [Sat] from a DT session is self-checked by the in-process DT constructor-
    tree checker, but its tree model is not yet carried by the scalar [model] type, so
    {!get_model} is [None]; a front end uses this to report [sat] on the verdict alone
    (matching the headline classifier) rather than treating a modelless [Sat] as a
    non-self-checkable UF sat. *)
val uses_datatypes : t -> bool

(** [true] iff an array select/store registry has been installed ([set_arrays] with a
    non-empty registry) — i.e. the standalone arrays theory is installed. Like a datatype
    [Sat], an array [Sat] is self-checked in process (by the array model checker) but its
    map model is not carried by the scalar [model] type, so {!get_model} is [None]; a
    front end uses this to report [sat] on the verdict alone. *)
val uses_arrays : t -> bool

(** One constructor for {!declare_datatype}: its name and each field's (selector name,
    sort). A nullary constructor (an enum case) has [fields = []]. *)
type ctor_decl =
  { ctor_name : string
  ; fields : (string * Oxsmt_core.Sort.t) list
  }

(** [declare_datatype t sort constructors] declares an ADT and its constructors
    programmatically (the Session-API path, distinct from the .smt2 parser). Constructor
    and selector symbols mint normally; each TESTER mints in the RESERVED [.oxsmt.*]
    namespace via the session's capability (ADR-0012), so a user function cannot forge
    [is-C] and silently shadow the tester in the printed session the Lean oracle checks.
    [sort] must be the datatype's [Sort.Datatype] (declare it first via {!declare_sort} +
    {!Oxsmt_core.Sort.datatype_} so a recursive field can reference it). Returns the built
    {!Oxsmt_core.Datatype_defs.datatype} (all minted symbols, for building terms) and adds
    it to the session registry, installing the DT theory. Must precede
    {!assert_term}/{!check_sat}. Adding a datatype AFTER a prior query instantiated a
    theory invalidates the cached theory (reset-per-query, task #54 — see
    {!set_datatypes}); doing so with live assertions raises [Invalid_argument]. *)
val declare_datatype
  :  t
  -> Oxsmt_core.Sort.t
  -> ctor_decl list
  -> Oxsmt_core.Datatype_defs.datatype

(** [assert_term t phi] preprocesses [phi] (ADR-0003 §5 passes), clausifies the boolean
    skeleton, registers each theory atom with the combined theory, and adds the clauses to
    the current frame. [phi] must be Bool-sorted and built through {!context}. An
    [Overflow]/[Unsupported]/rejected atom degrades the session to [Unknown] (I8). Legal
    before or after {!check_sat} (assert-after-check). *)
val assert_term : t -> Oxsmt_core.Term.t -> unit

(** Scan a complete assertion batch before preprocessing and select its actual arithmetic
    family. Mixed Int/Real content, Real with arrays/datatypes, a live-theory swap, or
    Real while [OXSMT_LRA] is disabled degrades the session to [unknown]. Idempotent. *)
val preselect_arithmetic : t -> Oxsmt_core.Term.t list -> unit

(** [assert_presolved t terms] asserts a WHOLE batch of terms through the W1b
    equality-elimination presolve (logs/w1b-design.md): it runs the {!Oxsmt_interface}
    presolve over [terms] to drop top-level unconditional Int-variable aliases [(= x t)]
    and substitute [x ↦ t] into the rest, internalizes the reduced (equisatisfiable) set,
    and keeps the ORIGINAL [terms] for the R1 self-check. Eliminated variables are
    re-derived into the model at {!get_model} / R1 time. Semantically equivalent to
    [List.iter (assert_term t) terms] — on a zero-alias input it is byte-identical — but
    solves a smaller problem when aliases are present.

    Unlike {!assert_term}, this needs the full set at once (aliases are collected across
    all terms), so it is the BATCH entry point (the .smt2 CLI); the incremental
    {!assert_term}/{!push}/{!pop}/lemma API is unchanged. Legal once per base frame before
    {!check_sat}; the reserved-symbol gate applies exactly as in {!assert_term}. *)
val assert_presolved : t -> Oxsmt_core.Term.t list -> unit

(** The names of the variables the most recent {!assert_presolved} eliminated, in
    elimination order (empty after a zero-alias batch or when {!assert_term} was used).
    Introspection for tests / metrics — NOT part of the client verdict flow. *)
val eliminated_vars : t -> string list

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

(** [check_sat_assuming t assumptions] decides the active assertions conjoined with the
    supplied Boolean literals. Each term must be a Bool-sorted atom built in [context t];
    Boolean connectives are rejected with [Invalid_argument]. Exact duplicate literals are
    ignored, while opposite polarities remain distinct.

    On [Unsat], [unsat_core] is a subset-minimal, duplicate-free subset of [assumptions],
    in input order: the active assertions conjoined with the core are unsatisfiable, and
    deleting any one core literal makes the remainder [Sat]. The empty core therefore
    means the active assertions are already unsatisfiable. The initial candidate comes
    from the SAT core's failed assumptions and is then deletion-minimized. If any deletion
    cannot be certified [Sat] or [Unsat], the whole call fails closed to
    [{ verdict = Unknown; unsat_core = None }].

    This entry point is additive: ordinary {!check_sat} does not consult assumption state
    and retains its existing search path. Nonempty assumption queries currently decline
    with [Unknown] when a certificate trace or quantified lemma is active, when the full
    query is in the pure bit-vector dispatch fragment, or when preprocessing an atom with
    a value [Ite]/[div]/[mod] introduces side constraints that cannot be represented by
    one assumption literal. *)
val check_sat_assuming : t -> assumption list -> assumption_check

(** The model of the most recent {!check_sat} or {!check_sat_assuming}, iff that call
    returned [Sat] {e and} a checkable model is reconstructable. Bindings are one
    [Const (symbol-name, value)] per constrained nullary symbol, sorted by name: the
    theory's Int / uninterpreted-sort constants {e unioned with} a [Bool] per declared
    propositional variable (a mixed Boolean/theory [Sat] covers both, so the §8 evaluator
    sees every declared symbol). Reserved internal witnesses ([".oxsmt.*"], e.g. an ITE
    lift) are excluded — the model names only user-declared symbols. [None] after
    [Unsat]/[Unknown], before any [check_sat], or when the [Sat]'s model would require a
    function table (any applied uninterpreted symbol is constrained) — a v1 completeness
    limit of the model reconstruction, not of the verdict. *)
val get_model : t -> model option

(** census (task #78): a short tag naming WHY the most recent {!check_sat} or
    {!check_sat_assuming} returned [Unknown] (e.g. ["r1-model-check-failed"],
    ["effort-budget"], ["lemma-saturated"], ["combine-incomplete-solve"]). Empty string
    when the last verdict was not [Unknown]. Diagnostic introspection only — the solver
    never reads it, so it cannot influence a verdict; the dev CLI surfaces it
    unconditionally on stderr to bucket structural unknowns by cause. *)
val last_unknown_reason : t -> string

(** {2 Certificate emission (ADR-0013)}
    — additive, compile-out-able side channel. *)

type certificate_presolve_definition =
  { name : string
  ; sort : Oxsmt_core.Sort.t
  ; value : Oxsmt_core.Term.t
  }

type presolve_certificate_trace =
  { on_equality_elimination :
      context:Oxsmt_core.Context.t
      -> original:Oxsmt_core.Term.t list
      -> reduced:Oxsmt_core.Term.t list
      -> definitions:certificate_presolve_definition list
      -> int
  ; on_clausify_begin :
      rewrite_id:int -> source:Oxsmt_core.Term.t -> preprocessed:Oxsmt_core.Term.t -> unit
  ; on_clausify_bindings :
      selector:Oxsmt_solver.Sat.var
      -> bindings:(Oxsmt_core.Term.t * Oxsmt_solver.Sat.var) list
      -> unit
  ; on_clausify_end : unit -> unit
  }

(** Install (or, with [None], remove) a certificate-emission trace on the inner SAT core.
    {b Must be called on a PRISTINE session} — before the first {!assert_term}/{!push} or
    nonempty {!check_sat_assuming} — per the {!Oxsmt_solver.Sat.set_trace} lifecycle
    contract (the recorder must observe every input from the start, or a conclusion could
    cite an untraced clause's id). A [None] default means byte-identical solving; a set
    trace bypasses learned-clause minimization (ADR-0013 §1.4(b), a weaker-but-sound
    solver), so verdicts are preserved but counters may differ from an untraced run. *)
val install_cert_trace : t -> Oxsmt_solver.Sat.trace option -> unit

(** The off-frozen-seam companion to {!install_cert_trace}: carries theory-atom meanings,
    pure-EUF leaf claims, and LIA Farkas evidence to the certificate recorder. Install it
    after a non-[None] SAT certificate trace and before assertions. [None] is the inert
    default. This channel is observational and never feeds solving. *)
val install_leaf_certificate_trace : t -> Cdclt.leaf_certificate_trace option -> unit

(** Off-seam evidence for W1b equality-elimination replay. The first callback records the
    original/reduced term statement separately from its eliminated definitions; the
    begin/end callbacks associate each reduced term with the SAT Query inputs emitted by
    its later preprocessing and clausification. Install after {!install_cert_trace}, on a
    pristine session. [None] is inert. *)
val install_presolve_certificate_trace : t -> presolve_certificate_trace option -> unit

(** Backward-compatible LIA-only companion to {!install_leaf_certificate_trace}. *)
val install_lia_certificate_trace : t -> Cdclt.lia_certificate_trace option -> unit

(** The active frame-selector assumptions the most recent (and next) {!check_sat} solves
    under ([List.map Sat.pos frames]). The certificate's terminal [Failed_assumption] (E3)
    step is conditioned on these being true — the checker seeds them to realize the §1.0
    selector strip. *)
val cert_assumptions : t -> Oxsmt_solver.Sat.lit list

(** The failed-assumption (selector) core of the most recent ordinary {!check_sat} that
    returned [Unsat] under a nonempty frame-assumption set; empty otherwise. User literals
    from {!check_sat_assuming} and the internal symmetry-breaking activation selector are
    filtered out. *)
val failed_assumptions : t -> Oxsmt_solver.Sat.lit list

(** {2 Theory infeasibility evidence (task #106)}
    — additive, observational; reading it never perturbs solving.

    After a {!check_sat} or {!check_sat_assuming} that returned [Unsat]
    {e via a LIA theory conflict}, these surface the refuting conflict's evidence for a
    downstream consumer (e.g. a CHC/Horn solver building Farkas interpolants). The
    evidence is recorded off the frozen, payload-free {!Explanation} (ADR-0006) by the LIA
    adapter at conflict-production time and read back here; a [None]-by-default channel
    that a caller may ignore entirely.

    {b Premise rendering.} Each premise is a [(atom, polarity)] pair: [polarity = true]
    means the atom was asserted true, [false] means asserted false (an [Le] atom's ℤ-
    complement, [¬(e <= 0) ≡ e >= 1]). Polarity is carried out of band — NOT folded into a
    negated term — precisely so reading these accessors interns nothing and cannot bump
    the context tag counter (an earlier version negated via [Context.not_], which on a
    cache miss interned a fresh [Not] node and perturbed later term tags / CNF ordering).

    {b What "core" means.} The atoms are the premises of the {e most recent} LIA conflict
    — a genuinely theory-infeasible set (their conjunction at the given polarities is
    T-unsat, re-checkable on a fresh session). For a conjunctive LIA query refuted at
    decision level 0 (the counterexample-to-induction shape a CHC consumer generates) that
    conflict is the whole-query refutation, so the atoms are exactly the asserted-formula
    subset that clashes. For a query with Boolean structure it is the last theory lemma
    the search derived — still a sound theory core, but not necessarily a minimal
    whole-formula core. [None] when the last verdict was not [Unsat], the refutation was
    purely propositional (no LIA conflict), or a premise is not representable as a term
    (an EUF-congruence fabric-edge handle). *)

(** [last_unsat_core t] is the theory-unsat core of the most recent {!check_sat} or
    {!check_sat_assuming}: the [(atom, polarity)] premises of its refuting LIA conflict.
    The conjunction of the atoms at their polarities is T-unsatisfiable. [None] per the
    rules above. *)
val last_unsat_core : t -> (Oxsmt_core.Term.t * bool) list option

(** [last_farkas t] is the Farkas certificate of the most recent {!check_sat} or
    {!check_sat_assuming} refutation: [(coeffᵢ, (atomᵢ, polarityᵢ))] pairs where an
    inequality's [coeffᵢ >= 0] multiplies its asserted half-plane, while a positive Int
    equality's unrestricted signed [coeffᵢ] multiplies the equation [a - b = 0]. The
    resulting sum is a variable-free false constant — the rational-infeasibility proof.
    Premises use the same rendering as {!last_unsat_core} and are index-aligned. [None]
    when {!last_unsat_core} is [None], the refutation was a Diophantine/divisibility
    conflict, or the evidence has an unsupported shape. The coefficient type is
    {!Oxsmt_lia.Rational.t}. *)
val last_farkas : t -> (Oxsmt_lia.Rational.t * (Oxsmt_core.Term.t * bool)) list option

(** Test-only (task #25): whether a symmetry-breaking emission is currently active (its
    activation selector is still assumed). Used by [symbreak_test] to check the R2
    emission restriction (no emission under a pushed frame or with lemmas registered). *)
val symbreak_active_for_test : t -> bool

(** The SAT core's counter trio, monotonic across the session (DESIGN.md §8). *)
val stats : t -> Oxsmt_solver.Sat.Stats.t

(** Theory splits consumed by the most recent {!check_sat} or {!check_sat_assuming}
    (determinism/perf stat). *)
val splits : t -> int

(** Incremental re-solves the most recent {!check_sat_assuming} spent minimizing its
    assumption core: the initial assumption solve, every deletion/refinement probe, and
    the final core replay. [0] after a call that never reached minimization (empty
    assumptions, an early [Unknown] decline, or a [Sat] verdict). Diagnostic/perf
    introspection only — never consulted by the solver, so it cannot affect a verdict;
    exposed so the core-min property test and benchmark can compare the linear and
    clause-set-refinement strategies (see [OXSMT_CORE_MIN_LINEAR]). *)
val minimize_probes : t -> int

(** [true] iff the most recent {!check_sat} or {!check_sat_assuming} degraded to [Unknown]
    by exhausting the split budget (the distinct split-budget stat; the query is otherwise
    unresolved). *)
val budget_exhausted : t -> bool

(** Effort consumed by the most recent {!check_sat} or {!check_sat_assuming}: SAT
    conflicts + decisions + seam [Final]-rounds (board #60). A deterministic function of
    the input (I6) — this is the per-file value the calibration run records to pick the
    cutoff, and the determinism check is that two runs report the same number. 0 before
    any {!check_sat}. *)
val effort : t -> int

(** [true] iff the most recent {!check_sat} or {!check_sat_assuming} returned [Unknown]
    because the {!create} [max_effort] cap fired (the BUDGET tag). Unlike
    {!budget_exhausted} this is NOT sticky and does not degrade the session — the same
    query is re-runnable at a larger cap. *)
val effort_exhausted : t -> bool

(** Lemma-tier instantiation stats (ADR-0012 §O4), distinct from {!splits}: [live_lemmas]
    currently in an active frame, and the cumulative [instances] generated / [rounds] run
    across the session. [seeds] is the subset of [instances] produced by MBQI-lite seeding
    (chunk 3) of trigger-inert universals rather than by E-matching. *)
type lemma_stats =
  { live_lemmas : int
  ; instances : int
  ; rounds : int
  ; seeds : int
  }

val lemma_stats : t -> lemma_stats

(** Provenance of one ground instance generated from a lemma (ADR-0012): the source
    lemma's id, the substitution (qvar images in binder order), and the resulting ground
    body. Certificate replay of instantiations is a later tranche; the record exists now. *)
type instantiation =
  { lemma_id : int
  ; subst : Oxsmt_core.Term.t array
  ; instance : Oxsmt_core.Term.t
  }

(** [lemma_instantiations t] is the instantiation trace, oldest-first: every ground
    instance actually asserted this session, tagged with which lemma and substitution
    produced it (a budget-aborted round's instances are absent). *)
val lemma_instantiations : t -> instantiation list

(** Test-only whitebox hook. NOT for solver code. *)
module For_test : sig
  (** The canonical model-reconstruction default for an unconstrained variable of a given
      sort (used by the W1b eliminated-def splice). Exposed so the wiring test can assert
      it fails closed on a datatype sort — a datatype has no scalar default, so it must
      raise rather than fabricate [VUninterp 0] (the silent wrong-value class, codex). *)
  val default_value : Oxsmt_core.Sort.t -> model_value

  (** Substitute (or, with [None], restore) the DT model self-checker that {!check_sat}'s
      commit consults for a datatype [Sat] (GOALS Datatypes). Exposed ONLY to pin the
      commit -> checker WIRING: a fault-injection test installs a reject-all stub and
      asserts the session then reports [Unknown] on a genuinely-sat query — a regression
      that bypassed the checker would ignore the stub and wrongly report [Sat]. [None]
      (the default, and the only production state) uses the real {!Dt_model_check}; NOT
      for solver code. *)
  val set_dt_checker
    :  (Oxsmt_core.Datatype_defs.t
        -> (Oxsmt_core.Term.t * Oxsmt_dt.Dt.ctor_tree) list
        -> Oxsmt_core.Term.t list
        -> bool)
         option
    -> unit

  (** Substitute (or, with [None], restore) the arrays model self-checker that
      {!check_sat}'s commit consults for an array [Sat]. Exposed ONLY to pin the commit ->
      checker WIRING (a fault-injection test installs a reject-all stub and asserts the
      session then reports [Unknown] on a genuinely-sat array query). [None] (the default,
      and the only production state) uses the real {!Array_model_check}. *)
  val set_array_checker
    :  (Oxsmt_core.Array_defs.t
        -> (Oxsmt_core.Term.t * Oxsmt_arr.Arr.value) list
        -> Oxsmt_core.Term.t list
        -> bool)
         option
    -> unit
end
