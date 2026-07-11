(** Core term layer: sorts, hash-consed terms, smart constructors, symbol
    environments.

    Responsibility: own the [Term.t] / [Sort.t] representation. Terms are a
    [private] type built only through sort-checking, normalizing, hash-consing
    smart constructors, so the invariant "any [Term.t] in existence is
    well-sorted and hash-consed" (INVARIANTS.md I1/I2) holds by construction.

    Status: skeleton. The frozen representation is settled by ADR-0003 (M0); do
    NOT add a Term/Sort API before then. Owning task: TASKS.md M0-core. *)
