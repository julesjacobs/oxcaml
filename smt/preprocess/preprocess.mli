(** Desugaring passes assigned to preprocessing by ADR-0003 Decision 5 (the
    "solver-pipeline invariants" the smart constructors deliberately do {e not} enforce),
    plus their composition. Every pass threads the single session {!Context.t} (ADR-0003
    Decision 6): the terms it builds share the session's tag stream and hash-consing.
    Fresh symbols are declared in the session's {!Env.t}, so {!create} takes both.

    {b Pass contracts} (equivalence vs. equisatisfiability, and fresh symbols):

    - {!ite_removal} — {e equisatisfiable}; introduces one fresh nullary symbol per
      non-Bool-sorted [Ite]. Full equivalence modulo those symbols: each is uniquely
      determined ([v = if c then a else b]), so the witness is computable.
    - {!div_mod_elimination} — {e equisatisfiable}; introduces a fresh quotient/remainder
      pair per distinct (dividend, divisor). Equivalence modulo those symbols: euclidean
      [q]/[r] are uniquely determined.
    - {!simplify} — {e fully equivalent}; introduces no fresh symbols.
    - {!run} — {!div_mod_elimination} then {!ite_removal}; equisatisfiable, fresh symbols
      from both.

    {b Output contract.} A {!run} result satisfies [Term.Debug.check ~mode:Pipeline] (no
    Int-sorted [Ite] anywhere, no residual reserved [div]/[mod] application). Neither
    single pass guarantees the full pipeline invariant on its own — [ite_removal] leaves
    [div]/[mod] applications, [div_mod_elimination] leaves [Ite] nodes — so the two must
    be composed (see {!run}); it is their composition that is [Pipeline]-clean.

    {b Fresh-symbol namespace.} Fresh symbols use a reserved [".oxsmt."] name prefix and a
    per-session monotonic counter (deterministic, INVARIANTS.md I6). Front ends (the
    SMT-LIB parser, VC generation) must not declare user symbols in that namespace; this
    is a caller contract flagged for the M1-end wiring. *)

open Oxsmt_core

type t

(** [create env ctx] makes a preprocessing handle over a session. [env] must be the same
    {!Env.t} that [ctx] was created from ({!Context.create}); fresh symbols are declared
    in it and then built through [ctx]. *)
val create : Env.t -> Context.t -> t

(** A fresh nullary symbol a pass introduced, with a term computing its value. [value] may
    use [div]/[mod] or an Int-sorted [Ite]; it is {b not} part of the preprocessed formula
    — it is the existential witness, for model reconstruction (map a solver model back to
    the eliminated construct) and for equivalence testing. *)
type definition =
  { symbol : Symbol.t
  ; value : Term.t
  }

(** Lift every non-Bool-sorted [Ite] to a fresh nullary constant with Tseitin-visible
    guarded equalities [(c -> t = a) /\ (¬c -> t = b)] conjoined into the boolean
    structure (ADR-0003 required #3). The stricter-than-ADR generalization from
    "Int-sorted" to "any non-Bool-sorted" is sound and keeps every theory atom [Ite]-free.
    Bool-sorted [Ite] stays (it is a connective the clausifier handles). *)
val ite_removal : t -> Term.t -> Term.t * definition list

(** Replace every reserved [div]/[mod] application (a nonzero constant divisor, guaranteed
    by {!Context.div}/{!Context.mod_}) by a fresh quotient/remainder with euclidean side
    constraints [x = d*q + r /\ 0 <= r < |d|] (ADR-0003 required #4). A [div] and a [mod]
    of the same (dividend, divisor) share one [q]/[r] pair. A non-constant or zero divisor
    raises {!Term.Unsupported} — but {!Context.div}/{!Context.mod_} already reject those
    at construction, so this pass never encounters one in practice. *)
val div_mod_elimination : t -> Term.t -> Term.t * definition list

(** Re-canonicalize by rebuilding bottom-up through the smart constructors. Construction
    already constant-folds, so this is effectively the identity on well-formed terms; it
    is the minimal, obviously-sound simplification hook (ADR-0003 Decision 5 defers a real
    rewriter). Introduces no fresh symbols; fully equivalence-preserving. *)
val simplify : t -> Term.t -> Term.t

(** [run] = {!div_mod_elimination} then {!ite_removal}. This order clears both pipeline
    invariants in a single pass each: [div_mod_elimination] may surface an [Ite] into a
    new euclidean side constraint (which {!ite_removal} then removes), and it removes
    every [div]/[mod] first, so {!ite_removal} reintroduces neither. The result satisfies
    [Term.Debug.check ~mode:Pipeline]. *)
val run : t -> Term.t -> Term.t

(** {!run} keeping the combined fresh-symbol definitions (div/mod first, then ite). *)
val run_with_definitions : t -> Term.t -> Term.t * definition list
