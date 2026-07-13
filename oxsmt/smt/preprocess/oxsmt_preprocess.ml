(** Preprocessing and clausification (DESIGN.md §5). Re-exports the two public surfaces:

    - {!Preprocess} — the ADR-0003 desugaring passes (Int/value-[Ite] removal, [div]/[mod]
      elimination) and their [Pipeline]-clean composition, all threading the session
      {!Oxsmt_core.Context}.
    - {!Cnf} — the Tseitin clausifier turning a preprocessed Bool-sorted term into
      abstract CNF (its own {!Cnf.Lit}/{!Cnf.Clause} over its own variable ids) plus the
      atom map.

    Both are stdlib-only over [oxsmt_core] (INVARIANTS.md I3). The abstract CNF is wired
    to the SAT core at M1-end; nothing here depends on [smt/solver]. *)

module Preprocess = Preprocess
module Cnf = Cnf
