(** LIA theory plugin: incremental simplex over rationals + branch-and-bound for
    integrality (Dutertre-de Moura).

    Responsibility: implement the THEORY signature for linear integer arithmetic
    — general simplex for bound reasoning with backtracking-friendly state,
    branch-and-bound on top; conflicts emitted as infeasible bound sets
    justified by Farkas coefficients (DESIGN.md §6-7, INVARIANTS.md I4).

    Status: skeleton. Owning task: TASKS.md M3-lia. *)
