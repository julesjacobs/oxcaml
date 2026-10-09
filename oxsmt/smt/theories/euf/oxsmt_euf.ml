(** EUF theory engine: proof-producing congruence closure via union-find with an
    explanation forest (Nieuwenhuis-Oliveras).

    Responsibility: the engine-independent equality + uninterpreted-function reasoner — an
    e-graph (union-find over term ids, congruence table, merge queue) whose every merge is
    explainable as a subset of asserted premises, so explanations are native (DESIGN.md
    §6, INVARIANTS.md I4). The M1 THEORY adapter (ADR-0005) is a thin later layer over
    this and is not part of this module.

    Owning task: TASKS.md M2-euf. *)

module Euf = Euf

(** The ADR-0005 [THEORY] adapter over {!Euf} — presents the engine to the CDCL(T) solver
    (M4) as one frozen {!Oxsmt_core.Theory.THEORY} plugin. Owning task: TASKS.md
    M4-adapters (EUF). *)
module Euf_adapter = Euf_adapter
