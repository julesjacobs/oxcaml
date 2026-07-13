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

(** The R1 in-process model self-check (ADR-UF-models §3), exposed so the test suite can
    unit-test the TCB guard directly (e.g. the [min_int * -1] fail-closed case, which no
    solver path reaches but the invariant must uphold). [oxsmt_core]-only; not part of the
    client-facing session flow, which drives it internally via {!Session}. *)
module Model_check = Model_check

(** The CDCL(T) seam glue, exposed so the test suite can pin {!Cdclt.add_ovf}/[mul_ovf] —
    the §10-v2 gap-B structural-fold overflow guards — in PARITY with the matching
    {!Model_check} guards (task #117). Not part of the client-facing session flow, which
    drives it internally via {!Session}. *)
module Cdclt = Cdclt

(** The datatypes in-process model self-check (GOALS Datatypes), exposed so the sat-model
    gate can drive it directly — in particular to prove it RED against a
    deliberately-wrong constructor tree. Not part of the client-facing session flow, which
    drives it internally via {!Session} (the DT commit branch). *)
module Dt_model_check = Dt_model_check

(** The arrays in-process model self-check (QF_AX), exposed so the array sat-model gate
    can drive it directly — in particular to prove it RED against a deliberately-wrong
    array model. Not part of the client-facing session flow, which drives it internally
    via {!Session} (the arrays commit branch). *)
module Array_model_check = Array_model_check

(** The W1b unconditional equality-elimination presolve (logs/w1b-design.md), exposed so
    the test suite can unit-test the transform directly (the reduced set / eliminated defs
    / interface-variable no-op). {!Session.assert_presolved} drives it internally on the
    batch path. *)
module Presolve = Presolve
