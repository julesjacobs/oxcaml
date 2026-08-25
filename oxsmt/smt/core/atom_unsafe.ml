(* Core-private no-copy cast [int -> Atom.t] — the [Iarr_unsafe] pattern (ADR-0003 B1).
   Sound because [Atom.t] is represented as [int]. It exists only so [Lit] can rebuild the
   [Atom.t] it packed into a literal; a dune [private_modules], it is a compile error
   outside [core], so no consumer can forge an atom id (ADR-0005: [Atom.fresh] is the only
   public minter). *)

external of_int : int -> Atom.t = "%identity"
