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

(** [true] once an overflow has bricked the underlying {!Lia} instance; never raises. *)
val is_poisoned : t -> bool

(** Count of overflow-induced degradations to [unknown] observed at this adapter's
    boundary (DESIGN.md §8 bench digest; the native-int incompleteness ceiling). *)
val overflows_to_unknown : t -> int

(** Total simplex pivots performed by the underlying engine (determinism/perf stat). *)
val pivot_count : t -> int
