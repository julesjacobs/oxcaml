(** Dynamic relevancy driver for the SAT core's decision branch-filter (task #24, QF_UF).

    z3's [smt_relevancy] idea: only branch on atoms that are RELEVANT to satisfying the
    top-level formula under the current partial assignment. oxsmt's full-biconditional
    Tseitin encoding leaves the siblings of an already-satisfied disjunction as free
    variables; VSIDS keeps deciding them, spuriously over-constrains, and conflicts (6-33x
    more decisions than z3 on the losing QF_UF tail). A satisfied [(or a1 … a5)] makes its
    undecided disjuncts irrelevant, so they need never be decided.

    This module maintains, over the assignment trail, which SAT variables are relevant,
    and exposes the predicate the SAT core's {!Oxsmt_solver.Sat.set_branch_filter}
    consults. It is a pure decision-ordering side channel: it adds no literal and no
    clause to the search (so it cannot manufacture a conflict — no wrong [unsat]), and
    handing a partial (branchable-only) assignment to the theory's [Final] check / model
    reconstruction is backstopped by {!Session}'s fail-closed [Model_check] (a variable
    wrongly left irrelevant degrades a query to [unknown], never a wrong verdict). See
    logs/quf-propagation-log.md for the measured motivation and the soundness argument.

    {b Graph.} {!Session} builds the relevancy graph from the clausifier
    ({!Oxsmt_preprocess.Cnf}) as each formula is clausified: every SAT variable maps back
    to its Bool subterm, and the And/Or/iff/Ite DAG is recovered by looking children's SAT
    variables up in the subterm→var map. Atom (leaf) variables are the ones the filter
    gates; auxiliary (Tseitin connective) variables are never gated. Each top-level
    asserted formula's root variable is seeded relevant at decision level 0.

    {b Trail discipline.} Relevancy marks and the tracked variable values are maintained
    via {!on_assign}/{!on_backtrack} in lockstep with the SAT trail, and are restorable by
    [cancel_until 0] (a backtrack to level [L] undoes every mark/value recorded above [L];
    the level-0 seeds and level-0 propagations persist). Deterministic (INVARIANTS.md I6):
    children are marked in a fixed order and the justifying child of a satisfied
    disjunction is chosen by lowest SAT variable. *)

type t

(** The connective a compound (non-atom) variable stands for. [KIff] is a Bool-sorted [Eq]
    (iff); [KIte] is a Bool-sorted [Ite] whose children are [| condition; then; else |]. *)
type kind =
  | KAnd
  | KOr
  | KIff
  | KIte

(** [true] iff [OXSMT_RELEVANCY] names an on value ([1]/[true]/[yes]/[on]); the
    default-OFF gate {!Session.create} consults. *)
val enabled_from_env : unit -> bool

(** A fresh driver with an empty graph and no marks. *)
val create : unit -> t

(** Register [var] as a gated ATOM (leaf) variable: once the graph is seeded, the filter
    ({!should_branch}) permits deciding it only while it is relevant. Idempotent (a shared
    theory atom is registered once per occurrence). *)
val register_atom : t -> int -> unit

(** [register_node t ~var ~kind ~children] records that the compound (auxiliary) variable
    [var] is the [kind] connective over [children], each a [(child_var, positive)] literal
    ([positive] false encodes a [Not]-negated child). Builds the parent index used to
    re-choose a disjunction's justifying child when a child's value later settles. *)
val register_node : t -> var:int -> kind:kind -> children:(int * bool) list -> unit

(** Seed a top-level asserted formula's root [var] relevant at decision level 0 (permanent
    across backtracks to 0). Propagates immediately over any values already known at level
    0 (e.g. the root unit forced true when the clause was added). *)
val seed_root : t -> int -> unit

(** [on_assign t ~var ~value ~level]: [var] was just placed on the trail with [value] at
    decision [level]. Records the value and propagates relevancy — down from [var] if it
    is a relevant compound, and re-choosing the justifying child of any relevant parent
    whose disjunction [var] may now satisfy. *)
val on_assign : t -> var:int -> value:bool -> level:int -> unit

(** [on_backtrack t ~level]: the trail was unwound to decision [level]; undo every value
    and relevancy mark recorded above [level]. *)
val on_backtrack : t -> level:int -> unit

(** The branch-filter predicate: [true] means the SAT core may decide [var]. A non-gated
    variable (auxiliary/selector/unknown) is always branchable; a gated atom is branchable
    only while relevant. *)
val should_branch : t -> int -> bool
