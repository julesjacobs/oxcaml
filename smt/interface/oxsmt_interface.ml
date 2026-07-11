(** Session API: declare sorts/symbols, assert, check-sat, push/pop (DESIGN.md §3).

    Responsibility: the sole entry point clients use — the refinement checker translates
    its VC language into core terms and drives the solver through {!Session}; the solver
    never sees typechecker internals (DESIGN.md §3 boundary 1). Shipped, stdlib-only over
    [oxsmt_core] + [oxsmt_preprocess] + [oxsmt_solver] (INVARIANTS.md I3); it never links
    the test-only SMT-LIB parser (the CLI that does lives under [tests/]).

    Status: M1-wiring — the propositional session (see {!Session} for THE SOUNDNESS RULE
    that keeps a theoryless core sound). Unsat cores / reasons and the SMT-LIB
    serialization seam arrive with M4. *)

module Session = Session
