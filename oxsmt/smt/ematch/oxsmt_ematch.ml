(** Stage-2 lemma tier (ADR-0012): quantified lemmas instantiated by (eventually)
    E-matching, fed back through the ground CDCL(T) core. This library owns the lemma
    store and the instantiation manager; it sits above the theories and beside the
    session, is NOT a THEORY, and does not touch the frozen seam (§1.2).

    Tranche 1: the store + placeholder {!Qvar}s + ground {!Instance}s + the frame-scoped
    {!Manager} with a manual instance seed queue. Tranche 2 (this cut): the read-only
    {!Egraph_view} + the backtracking {!Matcher} (L2/L3/R4), wired into {!Manager.round} —
    real E-matching, modulo EUF congruence, budget-bounded. The soundness spine — a live
    lemma degrades a ground [Sat] to [Unknown] — lives in {!Oxsmt_interface.Session}. *)

module Qvar = Qvar
module Instance = Instance
module Lemma = Lemma
module Manager = Manager
module Egraph_view = Egraph_view
module Matcher = Matcher
