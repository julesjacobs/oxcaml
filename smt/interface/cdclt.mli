(** CDCL(T) seam glue (ADR-0005 §3): drives the Nelson-Oppen combined theory
    [Combine (Uflia_router) (Euf_adapter) (Lia_adapter)] behind the propositional SAT
    core's {!Oxsmt_solver.Sat.theory} callback. Owns the theory-atom <-> SAT-var bijection
    (1:1, CONTRACT-ATOM), maps every seam event onto the frozen THEORY, keeps one
    backtrack frame per SAT decision level, caps mid-solve splits, and snapshots the model
    at the accepting Final->Sat. Internal to {!Session}; see the .ml for the full
    contract. *)

open Oxsmt_core

type t

(** A model value / table cell (eval-agnostic; the CLI renders it to the §8 self-check
    sidecar grammar). [VUninterp i] is a 0-based ELEMENT INDEX into its uninterpreted
    sort's finite universe (not the raw e-graph class id — {!model} remaps). *)
type value =
  | VBool of bool
  | VInt of Bigint.t (* arbitrary precision (core-bignum W2) *)
  | VUninterp of int

(** A total interpretation of one uninterpreted function/predicate: [cases] maps
    argument-index tuples to results (structural first-match), [default] covers the rest
    (ADR-UF-models §0/§1). *)
type fun_table =
  { default : value
  ; cases : (value list * value) list
  }

(** A model binding: a nullary symbol's value, or a function/predicate's table. *)
type binding =
  | Const of string * value
  | Fun of string * fun_table

(** The finite-universe cardinality of one uninterpreted sort (SMT-LIB sorts are inhabited
    ⇒ [card >= 1]). *)
type sort_card =
  { sort_name : string
  ; card : int
  }

(** The per-check-sat split budget was exhausted (the [T_lemma] loop has no intrinsic
    bound); caught at the {!Session} boundary and turned into verdict [unknown]. *)
exception Split_budget_exceeded

(** [create ctx env sat ~split_budget ~budget] builds the combined theory over [ctx]/[env]
    and installs it into [sat] via [Sat.set_theory]. [sat] MUST be pristine (no clauses,
    empty trail) — the seam's attach contract. [budget] is the shared effort budget (board
    #60): [create] installs a tick closure onto [sat] (counting SAT conflicts/decisions)
    and this module ticks it once per [Final]-round, so a [Budget.Exceeded] unwinds
    [Sat.solve] at the cap; {!Session} catches it. [budget] is reset per check by
    {!begin_check}.

    [registry] / [array_registry] carry the session's datatype / array declarations (each
    empty for a problem not using that theory). The theory the seam drives is chosen
    lazily at the first [intern_atom]: the standalone arrays theory when [array_registry]
    is non-empty, else the standalone DT theory when [registry] is non-empty, else the
    EUF+LIA combined stack. *)
val create
  :  Context.t
  -> Env.t
  -> Oxsmt_solver.Sat.t
  -> split_budget:int
  -> budget:Budget.t
  -> registry:Oxsmt_core.Datatype_defs.t ref
  -> array_registry:Oxsmt_core.Array_defs.t ref
  -> cap:Oxsmt_core.Env.reserved_cap
  -> t

(** [theory_instantiated t] is [true] once the standalone/combined theory has been chosen
    (at the first [intern_atom]) and cached. While [false], no theory atom has been
    interned or cataloged, so replacing the datatype/array registry is safe; once [true],
    the theory has classified terms against the current registry, so a NON-MONOTONIC
    registry replacement invalidates it (see {!reset_for_new_query} and
    {!Session.set_datatypes}). *)
val theory_instantiated : t -> bool

(** [reset_for_new_query t] drops the cached theory instance and the SAT-var<->theory-atom
    bijection (task #54, reset-per-query), so the next [intern] rebuilds the theory fresh
    from the (just-replaced) registry and re-interns every term against it. This is the
    correct-everywhere replacement for the #51 interim fail-closed guard: it turns the
    none->DT / DT->arrays / loader-overwrite degrade patterns into correct verdicts
    instead of [unknown], and dissolves the #51 stranded-[ctor_terms] wrong-[unsat]
    landmine (the old [Dt.t] carrying those tables is discarded entirely). PRECONDITION:
    called by {!Session} only between queries — SAT core at decision level 0, no live
    assertions bound to the dropped bijection (Session fail-LOUDs a mid-query
    replacement). The prior query's now-inert SAT vars/clauses stay allocated but cannot
    affect a later solve (their frame selector is free, they are absent from the cleared
    bijection). *)
val reset_for_new_query : t -> unit

(** Install (or, with [None], detach) the dynamic relevancy driver (task #24). When set,
    the theory-seam trail events ([on_assign]/[on_backtrack]) are also streamed to it so
    it can maintain relevancy marks in lockstep with the SAT trail. The branch filter that
    consults the driver is installed on the SAT core directly by {!Session}. [None] (the
    default) is byte-identical to the pre-relevancy glue. *)
val set_relevancy : t -> Relevancy.t option -> unit

(** [intern_atom t term] returns the SAT var 1:1 with theory atom [term], registering it
    with the combined theory on first sight (base frame — survives backjumps). The
    clausifier calls this for each theory atom before solving. Idempotent by hash-cons. *)
val intern_atom : t -> Term.t -> Oxsmt_solver.Sat.var

(** [bind_bool_var_atom t term v] registers [term] — a bare nullary Bool-sorted variable
    used as an uninterpreted-function argument — as an EUF [K_bool] theory atom bound to
    the ALREADY-ALLOCATED SAT var [v] (its propositional variable from {!Session}), so EUF
    binds it to true/false when [v] is assigned. Unlike {!intern_atom} it reuses [v]
    instead of minting a fresh var, keeping ONE SAT variable per term (the model reads its
    value from the propositional side, EUF from the same var — they cannot diverge). This
    closes the completeness half of the Bool-cardinality rule for buried bare Bool
    variables (combine's H2 guard), the applied-predicate analogue of which
    {!Session.register_bool_terms} already routes through {!intern_atom}. Idempotent: a
    no-op if [term] is already a theory atom or [v] already owns one. *)
val bind_bool_var_atom : t -> Oxsmt_core.Term.t -> Oxsmt_solver.Sat.var -> unit

(** Reset the split counter, the effort budget, and the stale model snapshot; call at the
    start of each check-sat. *)
val begin_check : t -> unit

(** task #106: reset the observational LIA conflict-evidence stash (see
    {!last_conflict_core}). Separate from {!begin_check} so {!Session.check_sat} can call
    it unconditionally at its top — including on the pure-BV fast path that never reaches
    {!begin_check} — guaranteeing no stale core leaks across checks. Observational only;
    never affects solving. *)
val clear_last_conflict : t -> unit

(** Splits emitted during the last check-sat (stat / determinism witness). *)
val splits_used : t -> int

(** Effort consumed on the shared budget so far in the current/most-recent check-sat
    ([Budget.used]); the instrumentation read behind {!Session.effort}. *)
val effort_used : t -> int

(** task #106: the LIA adapter's observational conflict evidence, re-exported so
    {!Session} can surface it. Only the EUF+LIA stack carries it (DT/arrays give [None]).
    See {!Oxsmt_lia.Lia_adapter.conflict_core}. *)
type conflict_core = Oxsmt_lia.Lia_adapter.conflict_core =
  { farkas : Oxsmt_lia.Rational.t list option
  ; atoms : (Term.t * bool) list
  }

(** [last_conflict_core t] is the {!conflict_core} of the most recent theory conflict
    since the last {!clear_last_conflict}, or [None] if there was none / it is not
    term-representable. Observational — never affects solving. *)
val last_conflict_core : t -> conflict_core option

(** The nullary-symbol (table-free) model reconstructed from the snapshot of the accepting
    Final->Sat, or [None] (see {!model} for the full function-model reconstruction). Kept
    for the const-only path. *)
val model_bindings : t -> binding list option

(** [model t] reconstructs the FULL finite function model from the accepting-Final->Sat
    snapshot: uninterpreted-sort cardinalities + const bindings + per-symbol function /
    predicate tables (ADR-UF-models §1). Int-sorted table cells get concrete integers: a
    LIA-valued term keeps its integer, a pure-EUF Int class (LIA never valued it) is
    realized to a distinct integer (QF_UFLIA §10 ℤ-realization, task #110; see the .ml).
    [None] (=> fail-closed [unknown]) when the last check-sat was not a theory [Sat], a
    needed value is missing, or a buried (unbound) Bool-codomain predicate cell would have
    to be guessed. Deterministic (R10). *)
val model : t -> (sort_card list * binding list) option

(** [dt_model t] is the datatypes theory's constructor-tree checker model, snapshotted at
    the accepting Final->Sat when the installed theory is the standalone DT theory (GOALS
    Datatypes model construction); [None] otherwise or when it degraded (fail-closed).
    Read by {!Session}'s DT commit branch and validated by [Dt_model_check] before a [sat]
    is reported. Deterministic. *)
val dt_model : t -> (Term.t * Oxsmt_dt.Dt.ctor_tree) list option

(** [array_model t] is the arrays theory's checker model, snapshotted at the accepting
    Final->Sat when the installed theory is the standalone arrays theory (QF_AX model
    construction); [None] otherwise. Read by {!Session}'s arrays commit branch and
    validated by [Array_model_check] before a [sat] is reported. Deterministic. *)
val array_model : t -> (Term.t * Oxsmt_arr.Arr.value) list option

(** Test-only: run the CONTRACT-LEMMA/CONTRACT-SPLIT clausifier on a crafted THEORY
    [check_result] at the given effort, exactly as {!check} does (it shares the same
    internal desugar), returning the emitted SAT-core {!Oxsmt_solver.Sat.theory_result}.
    Exposed for the H1 seam tests (adr-0005-contract-lemma-erratum): a [Lemma] desugars
    identically at BOTH efforts (each [(tm, sign)] via the signed-literal path with
    Not-peeling) and is NEVER dropped at [Propagate], whereas a [Split] clausifies only at
    [Final] and is dropped at [Propagate]. Not part of the shipping contract. *)
val desugar_result_for_test
  :  t
  -> final:bool
  -> Theory.check_result
  -> Oxsmt_solver.Sat.theory_result

(** Test-only re-exports of the seam callbacks the SAT core drives internally, exposed for
    the S4.2 chrono incremental-undo REDs (H1 zero-removal wipe / H2 cross-query index
    skew): they reproduce the exact driver behaviour against a REAL Combined theory
    without staging a full chrono solve. [on_assign_for_test] /
    [on_chrono_rewind_for_test] are the very closures {!create} installs; [check_for_test]
    runs the theory check; and [ckpt_log_length_for_test] reads the driver's
    absolute-trail-indexed checkpoint log so a test can assert the index-alignment
    invariant directly. Not part of the shipping contract; only exercised under
    [OXSMT_CHRONO_INCR_UNDO]+[OXSMT_CHRONO]. *)
val on_assign_for_test : t -> Oxsmt_solver.Sat.lit -> level:int -> unit

val on_chrono_rewind_for_test : t -> int -> unit
val check_for_test : t -> final:bool -> Oxsmt_solver.Sat.theory_result
val ckpt_log_length_for_test : t -> int

(** [egraph_view t] is a read-only query view of the live congruence closure (ADR-0012
    L2/O3), for the lemma tier's E-matcher. Its accessors are non-registering — the
    matcher reads the e-graph without mutating it (R6). It is a {b live} surface, NOT a
    snapshot. The combined EUF+LIA, standalone datatype, and standalone array theory
    implementations all expose their owned congruence closure through this same view.
    snapshot: each accessor reflects the engine's current state at the moment it is
    called, so the caller must rebuild it (call [egraph_view] again) after any state
    change and must not cache results across a [check_sat]/push/pop. {!Session.check_sat}
    rebuilds it per instantiation round; see {!Oxsmt_ematch.Egraph_view} for the full
    validity window. *)
val egraph_view : t -> Oxsmt_ematch.Egraph_view.t
