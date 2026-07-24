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
      [q]/[r] are uniquely determined. It also canonically expands bounded bilinear
      integer products marked by [.oxsmt.nia.mul]; ineligible or over-cap products remain
      unchanged and retain fail-closed model checking.
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

(** The reserved name prefix ([".oxsmt."]) that all fresh preprocessing symbols use, and
    the predicate that recognizes it. Re-exported from {!Env}, which is now the single
    source of truth (ADR-0012 R1): the public {!Env.declare_fun}/{!Env.declare_sort}
    reject this namespace by construction, so a user symbol can never collide with a fresh
    one (INVARIANTS.md I6 freshness; board #48). *)
val reserved_prefix : string

val is_reserved_name : string -> bool

type t

(** [create cap env ctx] makes a preprocessing handle over a session. [env] must be the
    same {!Env.t} that [ctx] was created from ({!Context.create}); fresh symbols are
    declared in it through the cap-gated {!Env.declare_reserved} (ADR-0012 R1 — the public
    {!Env.declare_fun} now rejects the reserved [".oxsmt.*"] namespace) and then built
    through [ctx]. [cap] must be the {!Env.reserved_cap} minted with [env]. *)
val create : Env.reserved_cap -> Env.t -> Context.t -> t

(** A fresh nullary symbol a pass introduced, with a term computing its value. [value] may
    use [div]/[mod] or an Int-sorted [Ite]; it is {b not} part of the preprocessed formula
    — it is the existential witness, for model reconstruction (map a solver model back to
    the eliminated construct) and for equivalence testing. *)
type definition =
  { symbol : Symbol.t
  ; value : Term.t
  }

(** One non-Boolean ITE lifted to [result], before its two defining implications are
    encoded. [condition], [then_branch], and [else_branch] have already been recursively
    preprocessed, so none contains a non-Boolean ITE or reserved [div]/[mod]. *)
type guarded_ite =
  { condition : Term.t
  ; result : Term.t
  ; then_branch : Term.t
  ; else_branch : Term.t
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
    at construction, so this pass never encounters one in practice. The same DAG walk
    canonically expands bounded bilinear [.oxsmt.nia.mul] applications over normalized
    integer linear forms; products outside that bounded degree-2 fragment are unchanged. *)
val div_mod_elimination : t -> Term.t -> Term.t * definition list

(** [run] = {!div_mod_elimination} then {!ite_removal}. This order clears both pipeline
    invariants in a single pass each: [div_mod_elimination] may surface an [Ite] into a
    new euclidean side constraint (which {!ite_removal} then removes), and it removes
    every [div]/[mod] first, so {!ite_removal} reintroduces neither. The result satisfies
    [Term.Debug.check ~mode:Pipeline]. *)
val run : t -> Term.t -> Term.t

(** {!run} keeping the combined fresh-symbol definitions (div/mod first, then ite). *)
val run_with_definitions : t -> Term.t -> Term.t * definition list

(** The direct-clause variant of {!run_with_definitions}. The returned root has every
    non-Boolean ITE replaced by its fresh [result], but the two defining implications are
    returned as {!guarded_ite} records instead of being conjoined into the Boolean term.
    A clausifier can therefore emit the binary clauses
    [(not condition) or (result = then_branch)] and
    [condition or (result = else_branch)] without allocating Tseitin variables for an
    [and] of two implications. The legacy functions above retain their exact output. *)
val run_with_guarded_ites
  :  t
  -> Term.t
  -> Term.t * definition list * guarded_ite list
