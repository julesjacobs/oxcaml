(** EUF theory plugin: congruence closure via proof-producing union-find
    (Nieuwenhuis-Oliveras).

    Responsibility: implement the THEORY signature for equality + uninterpreted
    functions — an e-graph (union-find over term ids, congruence table, merge
    queue) whose every merge is explainable as a subset of asserted atoms, so
    explanations are native (DESIGN.md §6, INVARIANTS.md I4).

    Status: skeleton. Owning task: TASKS.md M2-euf. *)
