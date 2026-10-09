(** Eager bit-blasting of QF_BV into the propositional SAT core (GOALS: bit-vectors).

    A presolve-style whole-formula transform, NOT a theory client: each n-bit term becomes
    n Boolean SAT variables, each operator a Tseitin circuit, and the existing
    {!Oxsmt_solver.Sat} core does the solving — no SAT-core change, no combinator/theory
    involvement. Sat models read straight back as concrete bit-vectors; the Unsat path is
    pure propositional (the RUP certificate checker applies ARCHITECTURALLY unchanged, no
    theory leaves — v1 caveat: cert EMISSION for this path is not yet wired; the
    refutation is ordinary propositional resolution, nothing for the checker to
    special-case).

    Public surface:
    - {!Bv_op} — the operator vocabulary the circuit library matches on.
    - {!Blast} — the circuit library (term DAG -> SAT literals).
    - {!Bv_eval} — independent value-semantics evaluator (oracle + model check).
    - {!Bv_solve} — the solve driver (fail-closed to [Unknown], never a wrong verdict).
    - {!Bv_adapter} — the bridge from bv-front's [Oxsmt_core.Bv] vocabulary to {!Blast}. *)

module Bv_op = Bv_op
module Blast = Blast
module Bv_eval = Bv_eval
module Bv_solve = Bv_solve
module Bv_adapter = Bv_adapter
module Bv_simplify = Bv_simplify
