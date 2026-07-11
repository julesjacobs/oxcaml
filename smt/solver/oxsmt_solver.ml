(** CDCL(T) engine: trail, boolean propagation, conflict analysis, clause
    learning, restarts.

    Responsibility: the propositional CDCL core (two-watched literals, 1UIP,
    activity branching) plus online theory integration — it sees theories only
    through the THEORY callback interface and stays novelty-free (DESIGN.md §5).
    Incrementality (push/pop, assert-after-check) falls out of the trail.

    Status: skeleton. Owning task: TASKS.md M1-cdcl. *)
