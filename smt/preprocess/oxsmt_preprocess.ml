(** Preprocessing and clausification (DESIGN.md §5). Re-exports {!Preprocess} — the
    ADR-0003 desugaring passes (Int/value-[Ite] removal, [div]/[mod] elimination, a
    minimal [simplify]) and their [Pipeline]-clean composition, all threading the session
    {!Oxsmt_core.Context}. Stdlib-only over [oxsmt_core] (INVARIANTS.md I3). *)

module Preprocess = Preprocess
