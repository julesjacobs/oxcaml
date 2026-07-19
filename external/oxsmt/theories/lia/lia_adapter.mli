(** LIA {!Oxsmt_core.Theory.THEORY} adapter (ADR-0005, M4): the thin binding of the {!Lia}
    engine (Dutertre-de Moura simplex + branch-and-bound over δ-rationals) to the frozen
    CDCL(T) theory seam. All reasoning stays in {!Lia}; this layer only translates between
    the engine currency ([Atom.t]/[Lit.t]/[Explanation.t]/[Model.t]) and the [Term.t]s
    {!Lia} consumes, instantiating {!Lia}'s opaque premise token to [Lit.t].

    {b Mapping.} [create] ignores [Env] (v1 LIA needs none). [register_atom] records the
    atom⇄term maps (idempotent, C7) and internalizes the term in {!Lia}. [assert_lit]
    forwards the polarity + [Lit.t] premise to {!Lia.assert_atom}. [check Propagate]
    returns a rational-feasibility [Conflict] (Farkas-backed, [Lia_farkas]) or the
    bound-to-bound [Propagations]; [check Final] adds integrality — [Sat] when the
    rational model is integral, else a [Split] of the two distinct B&B atoms
    [x<=⌊v⌋]/[x>=⌊v⌋+1] handed to CDCL(T) (CONTRACT-SPLIT; branching stays in the SAT
    core, so {!Lia}'s own internal B&B is never run in the loop). [explain] serves the
    premise set cached at propagation time, scoped to the [push]/[pop] frame
    (precedence-valid, CONTRACT-EX). [model] reads {!Lia.model} (valid after
    [Final]→[Sat]) as [Model.Int] bindings.

    {b Soundness / CONTRACT-POISON.} The adapter never turns a poisoned or overflowed
    state into a sat/unsat verdict. A native-int {!Rational.Overflow} escaping a
    state-touching op (and a subsequent {!Lia.Poisoned} on the bricked instance, and an
    out-of-fragment {!Lia.Unsupported}) propagates out of the THEORY op unchanged, so the
    engine bricks the instance and degrades the query to [unknown] (INVARIANTS.md I8).
    {!overflows_to_unknown} counts overflow-induced degradations — the design's distinct
    native-int-ceiling stat, separate from {!Lia.overflow_count} (which counts only the
    standalone [solve_integer] driver, unused here). *)

open Oxsmt_core
include Theory.THEORY

val check_fabric : t -> Theory.effort -> Fabric.check_result
val explain_fabric : t -> Lit.t -> Fabric.Explanation.t
val fixed_bounds : t -> Term.t -> Fabric.fixed_bounds option

(** [fabric_verify t term value lo hi] — ADR-0014 Stage 1b F1-SEM independent semantic
    verifier: re-derives, by a path SEPARATE from the {!fixed_bounds} tuple, that [term]
    is fixed to [value] with [lo]/[hi] as its oriented bound premises. Rejects a wrong
    value, a swapped/foreign token, or a dropped/non-exact bound. *)
val fabric_verify
  :  t
  -> Term.t
  -> string
  -> Fabric.justification
  -> Fabric.justification
  -> bool

(** [notify_eq t ~edge_id eq] reacts to an ADR-0014 Stage 2 hub [new_eq] (§A.3): asserts
    the (already-constructed) Int equality atom [eq] into the tableau as a pair of bounds,
    attributed to fabric [edge_id] (whose Γ is the EUF congruence proof; a later conflict
    citing it expands to the real premises). Pure mutation on LIA's own trail — reversed
    by an ordinary [pop] (F3 co-location); the combinator does the fallible construction
    before calling, so a skip leaves no partial state (H5). *)
val notify_eq : t -> edge_id:Fabric.edge_id -> Term.t -> unit

(** [note_disequalities t pairs] receives the combinator's pinned Int-disequality snapshot
    [(px, py)] (px <> py) and forwards it to the engine as a HINT for the next
    OXSMT_LIA_MODELFIND dive (see {!Lia.set_pin_hint}). Read-only w.r.t. the combinator;
    no trail/premise effect; soundness unchanged (models are still combinator- and
    R1-validated). *)
val note_disequalities : t -> (Term.t * Term.t) list -> unit

(** ADR-0014 Stage 4.2 sub-frame checkpoint/rewind (chrono earliest-removed incremental
    undo): delegate to {!Lia.checkpoint}/{!Lia.rewind_to_checkpoint} and invalidate the
    explain-cache entries snapshotted since the checkpoint. *)
type checkpoint

val checkpoint : t -> checkpoint
val rewind_to_checkpoint : t -> checkpoint -> unit

(** {2 Reason builders (exposed for the empty-premise tripwire test).}

    [conflict_explanation] / [propagation_reason] turn an engine conflict / a bound
    propagation's premise set into the frozen [Explanation.t] the CDCL(T) core consumes.
    Both enforce the codex AP4 tripwire in parity with {!Euf_adapter}: an EMPTY premise
    set is a soundness bug (a premise-free conflict would learn the empty clause ->
    spurious [unsat]; a premise-free propagation is an unconditional entailment), so they
    raise (degrading the query to [unknown] via CONTRACT-POISON) rather than return an
    unsound reason. Unconstructible from the engine; exposed only so a test can drive the
    tripwire directly. *)

val conflict_explanation : Lit.t Lia.conflict -> Explanation.t
val propagation_reason : Lit.t list -> Explanation.t

(** {2 Observational theory-infeasibility evidence (task #106).}

    A read-only side channel for surfacing the most recent theory conflict's evidence
    (Farkas dual coefficients + premise atoms) through the public {!Session} API, e.g. for
    a CHC/Horn consumer building Farkas interpolants. Recorded OFF the frozen payload-free
    {!Explanation} (ADR-0006): {!fabric_conflict_explanation} stashes the raw engine
    conflict at production time. Reading it NEVER affects solving. *)

(** The Farkas / premise evidence of one theory conflict, mapped to terms. *)
type conflict_core =
  { farkas : Rational.t list option
  (** [Some coeffs] for a Farkas-certified rational-infeasibility conflict, index-aligned
      with [atoms]. An inequality coefficient is nonnegative and multiplies its asserted
      half-plane. A positive Int equality coefficient is signed and multiplies
      [a - b = 0]. Their sum is a variable-free false constant. [None] for a Diophantine /
      divisibility conflict (empty engine vector), a shape/sign mismatch, or unsupported
      premises. *)
  ; atoms : (Term.t * bool) list
  (** each premise atom's [Term.t] and its asserted polarity, in premise order. *)
  }

(** [last_conflict_core t] is the {!conflict_core} of the MOST RECENT conflict this
    adapter produced since {!clear_last_conflict}, or [None] if no conflict was produced
    or a premise cannot be represented as a term (a fabric-edge handle, or an atom absent
    from the term map). A genuine T-infeasible core: the conjunction of its {!field:atoms}
    (at their polarities) is theory-unsatisfiable. *)
val last_conflict_core : t -> conflict_core option

(** Reset the {!last_conflict_core} stash. Called once per check-sat by
    {!Cdclt.clear_last_conflict} at the TOP of {!Session.check_sat} — before any dispatch
    path (including the pure-BV fast path that bypasses {!Cdclt.begin_check}) — so a stale
    conflict from a prior check cannot masquerade as the current query's core. *)
val clear_last_conflict : t -> unit

(** [true] once an overflow has bricked the underlying {!Lia} instance; never raises. *)
val is_poisoned : t -> bool

(** Count of overflow-induced degradations to [unknown] observed at this adapter's
    boundary (DESIGN.md §8 bench digest; the native-int incompleteness ceiling). *)
val overflows_to_unknown : t -> int

(** Total simplex pivots performed by the underlying engine (determinism/perf stat). *)
val pivot_count : t -> int

(** Number of Stage-B HNF integer cuts this adapter has emitted (as CONTRACT-LEMMA lemmas)
    since creation — the dark-lever instrumentation (0 when [OXSMT_HNF_CUTS] is off, so
    OFF is byte-identical). *)
val hnf_cuts_emitted : t -> int

(** Reset the per-query CG-cut attempt budget (task #53 H3): zeroes {!cut_attempts} so the
    [OXSMT_CG_MAX_CUTS] cap starts fresh for a new query. The budget is already fresh per
    query on the corpus/reset paths (fresh adapter; [Cdclt.reset_for_new_query] recreates
    the theory); this is the mechanism for the residual persisting-theory incremental
    case. NOT wired into the solve path (no per-check-sat theory hook exists in the frozen
    interfaces — a documented follow-up), so it changes no solver behavior; exercised by
    [cut_budget_test]. *)
val reset_cut_budget : t -> unit

(** CG-cut attempts consumed since the last {!reset_cut_budget} (0 at creation). Bounded
    by [OXSMT_CG_MAX_CUTS]. Test-observability accessor, symmetric with
    {!hnf_cuts_emitted}. *)
val cut_attempts : t -> int
