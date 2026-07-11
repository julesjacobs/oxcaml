(** CDCL(T) engine (DESIGN.md §5). Owning task: TASKS.md M1-cdcl.

    M1 landed the propositional CDCL SAT core ({!Sat}) — two-watched-literal propagation,
    1UIP clause learning, VSIDS branching, phase saving, Luby restarts, activity-based
    clause deletion — following the MiniSat design and deliberately novelty-free. Online
    theory integration via the THEORY callback interface arrives with the theory
    milestones (M2+); until then the core is propositional only and sees no terms. *)

module Sat = Sat
