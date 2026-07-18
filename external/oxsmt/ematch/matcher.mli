(** E-matcher v1 (ADR-0012 L3, tranche 2): a deterministic backtracking matcher that finds
    ground substitutions for a lemma's trigger patterns against the solver's congruence
    closure, {b modulo EUF-congruence equalities}, reading the e-graph through the
    non-registering {!Egraph_view} (R6 — it never mutates the e-graph).

    {b Fragment (matcher v1).} A trigger pattern is a UF application [App (f, p̄)] with an
    uninterpreted head ([f] arity ≥ 1, ADR-0010 invariant (iv)); its arguments are qvar
    placeholders, nested UF applications, or ground leaves. A pattern rooted at anything
    else (an arithmetic/order/equality node) yields no matches — arithmetic lives in the
    lemma {e body}, handled by the assert-time pipeline, not in a trigger. This is a
    completeness scope, never a soundness one (§3): fewer matches ⇒ fewer valid instances
    ⇒ at worst a missed refutation ([unknown]), never a wrong verdict.

    {b Multi-triggers (O9).} [triggers : Term.t list list]: the outer list is
    {e alternatives} (any one may fire), the inner list is a {e conjunctive} multi-trigger
    (all its patterns must match under ONE substitution).

    {b Budget (R4).} Every enumeration step — each candidate tried, each argument match,
    each class member visited — debits [budget]; on exhaustion it raises
    {!Budget_exhausted} {e mid-enumeration}, before a conjunctive cross-product fully
    materializes. The caller ({!Manager}) maps that to the generation-budget degrade
    ([unknown], §3).

    {b Determinism (I6, O10).} Candidates are iterated in [Egraph_view]'s registration/id
    order and class members in id order; alternatives and conjuncts in list order — so the
    emitted substitution sequence is a deterministic function of the view and the lemma. *)

open Oxsmt_core

(** Raised when [budget] hits 0 during matching (R4). Internal to the tier; {!Manager}
    catches it. *)
exception Budget_exhausted

(** [substitutions view lemma ~budget] is the complete ground substitutions found by
    E-matching [lemma]'s triggers against [view], each a [Term.t array] in [lemma.qvars]
    order. A substitution is emitted only if it binds {e every} qvar (a trigger set that
    fails to cover some qvar contributes nothing). Two edge cases:
    - a {b zero-qvar} lemma ([qvars = [||]], i.e. [forall (). body]) yields exactly one
      empty substitution [[ [||] ]] — its [body] is a ground fact, instantiated once,
      regardless of triggers;
    - a lemma with {e ≥1 qvar} and {b empty} [triggers] yields [[]] (no matching;
      auto-trigger selection is tranche 3). May raise {!Budget_exhausted}. *)
val substitutions : Egraph_view.t -> Lemma.t -> budget:int ref -> Term.t array list

(** [iter_substitutions view lemma ~budget ~yield] enumerates the same substitutions in
    the same order as {!substitutions}, but calls [yield] as soon as each complete
    substitution is found instead of first materializing the whole result.  Thus a caller
    may retain the already-yielded prefix if a later step raises {!Budget_exhausted}. *)
val iter_substitutions
  :  Egraph_view.t
  -> Lemma.t
  -> budget:int ref
  -> yield:(Term.t array -> unit)
  -> unit
