(** Session API: declare sorts/symbols, assert, check-sat, push/pop, unsat
    cores, reasons.

    Responsibility: the sole entry point clients use — the refinement checker
    translates its VC language into core terms and drives the solver through
    here; the solver never sees typechecker internals (DESIGN.md §3 boundary 1).
    Also the seam where each session serializes to SMT-LIB2.

    Status: skeleton. Owning task: TASKS.md M4-interface. *)
