(** CDCL(T) seam glue (ADR-0005 §3): drives the Nelson-Oppen combined theory
    [Combine (Uflia_router) (Euf_adapter) (Lia_adapter)] behind the propositional SAT
    core's {!Oxsmt_solver.Sat.theory} callback. Owns the theory-atom <-> SAT-var bijection
    (1:1, CONTRACT-ATOM), maps every seam event onto the frozen THEORY, keeps one
    backtrack frame per SAT decision level, caps mid-solve splits, and snapshots the model
    at the accepting Final->Sat. Internal to {!Session}; see the .ml for the full
    contract. *)

open Oxsmt_core

type t

(** A model value / table cell (eval-agnostic; the CLI renders it to the §8 self-check
    sidecar grammar). [VUninterp i] is a 0-based ELEMENT INDEX into its uninterpreted
    sort's finite universe (not the raw e-graph class id — {!model} remaps). *)
type value =
  | VBool of bool
  | VInt of int
  | VUninterp of int

(** A total interpretation of one uninterpreted function/predicate: [cases] maps
    argument-index tuples to results (structural first-match), [default] covers the rest
    (ADR-UF-models §0/§1). *)
type fun_table =
  { default : value
  ; cases : (value list * value) list
  }

(** A model binding: a nullary symbol's value, or a function/predicate's table. *)
type binding =
  | Const of string * value
  | Fun of string * fun_table

(** The finite-universe cardinality of one uninterpreted sort (SMT-LIB sorts are inhabited
    ⇒ [card >= 1]). *)
type sort_card =
  { sort_name : string
  ; card : int
  }

(** The per-check-sat split budget was exhausted (the [T_lemma] loop has no intrinsic
    bound); caught at the {!Session} boundary and turned into verdict [unknown]. *)
exception Split_budget_exceeded

(** [create ctx env sat ~split_budget ~budget] builds the combined theory over [ctx]/[env]
    and installs it into [sat] via [Sat.set_theory]. [sat] MUST be pristine (no clauses,
    empty trail) — the seam's attach contract. [budget] is the shared effort budget (board
    #60): [create] installs a tick closure onto [sat] (counting SAT conflicts/decisions)
    and this module ticks it once per [Final]-round, so a [Budget.Exceeded] unwinds
    [Sat.solve] at the cap; {!Session} catches it. [budget] is reset per check by
    {!begin_check}. *)
val create
  :  Context.t
  -> Env.t
  -> Oxsmt_solver.Sat.t
  -> split_budget:int
  -> budget:Budget.t
  -> t

(** [intern_atom t term] returns the SAT var 1:1 with theory atom [term], registering it
    with the combined theory on first sight (base frame — survives backjumps). The
    clausifier calls this for each theory atom before solving. Idempotent by hash-cons. *)
val intern_atom : t -> Term.t -> Oxsmt_solver.Sat.var

(** Reset the split counter, the effort budget, and the stale model snapshot; call at the
    start of each check-sat. *)
val begin_check : t -> unit

(** Splits emitted during the last check-sat (stat / determinism witness). *)
val splits_used : t -> int

(** Effort consumed on the shared budget so far in the current/most-recent check-sat
    ([Budget.used]); the instrumentation read behind {!Session.effort}. *)
val effort_used : t -> int

(** The nullary-symbol (table-free) model reconstructed from the snapshot of the accepting
    Final->Sat, or [None] (see {!model} for the full function-model reconstruction). Kept
    for the const-only path. *)
val model_bindings : t -> binding list option

(** [model t] reconstructs the FULL finite function model from the accepting-Final->Sat
    snapshot: uninterpreted-sort cardinalities + const bindings + per-symbol function /
    predicate tables (ADR-UF-models §1). Int-sorted table cells get concrete integers: a
    LIA-valued term keeps its integer, a pure-EUF Int class (LIA never valued it) is
    realized to a distinct integer (QF_UFLIA §10 ℤ-realization, task #110; see the .ml).
    [None] (=> fail-closed [unknown]) when the last check-sat was not a theory [Sat], a
    needed value is missing, or a buried (unbound) Bool-codomain predicate cell would have
    to be guessed. Deterministic (R10). *)
val model : t -> (sort_card list * binding list) option

(** Overflow-guarded int add/mul used by {!model}'s §10-v2 gap-B structural Arith fold;
    [None] on overflow (task #117). Exposed so the wiring-test parity oracle can pin them
    equal to [Model_check.add_ovf]/[mul_ovf] on the [min_int] edge — R1 re-folds every
    table key, so extraction's fold MUST agree with R1's or a valid model is gratuitously
    rejected. INCLUDING the [min_int * -1] / [-1 * min_int] wrap clause. *)
val add_ovf : int -> int -> int option

val mul_ovf : int -> int -> int option

(** [egraph_view t] is a read-only query view of the live congruence closure (ADR-0012
    L2/O3), for the lemma tier's E-matcher. Its accessors are non-registering — the matcher
    reads the e-graph without mutating it (R6). Rebuild it per instantiation round (the
    e-graph grows as instances are asserted). *)
val egraph_view : t -> Oxsmt_ematch.Egraph_view.t
