(** Stage-2 lemma tier (ADR-0012): quantified lemmas instantiated by (eventually)
    E-matching, fed back through the ground CDCL(T) core. This library owns the lemma
    store and the instantiation manager; it sits above the theories and beside the
    session, is NOT a THEORY, and does not touch the frozen seam (§1.2).

    Tranche 1 (this cut): the store + placeholder {!Qvar}s + ground {!Instance}s + the
    frame-scoped {!Manager} with a manual instance seed queue (the matcher arrives in
    tranche 2). The soundness spine — a live lemma degrades a ground [Sat] to [Unknown] —
    lives in {!Oxsmt_interface.Session}. *)

module Qvar = Qvar
module Instance = Instance
module Lemma = Lemma
module Manager = Manager
