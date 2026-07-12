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

(** [create ctx env sat ~split_budget] builds the combined theory over [ctx]/[env] and
    installs it into [sat] via [Sat.set_theory]. [sat] MUST be pristine (no clauses, empty
    trail) — the seam's attach contract. *)
val create : Context.t -> Env.t -> Oxsmt_solver.Sat.t -> split_budget:int -> t

(** [intern_atom t term] returns the SAT var 1:1 with theory atom [term], registering it
    with the combined theory on first sight (base frame — survives backjumps). The
    clausifier calls this for each theory atom before solving. Idempotent by hash-cons. *)
val intern_atom : t -> Term.t -> Oxsmt_solver.Sat.var

(** Reset the split counter and stale model snapshot; call at the start of each check-sat. *)
val begin_check : t -> unit

(** Splits emitted during the last check-sat (stat / determinism witness). *)
val splits_used : t -> int

(** The nullary-symbol (table-free) model reconstructed from the snapshot of the accepting
    Final->Sat, or [None] (see {!model} for the full function-model reconstruction). Kept
    for the const-only path. *)
val model_bindings : t -> binding list option

(** [model t] reconstructs the FULL finite function model from the accepting-Final->Sat
    snapshot: uninterpreted-sort cardinalities + const bindings + per-symbol function /
    predicate tables (ADR-UF-models §1). [None] (=> fail-closed [unknown]) when the last
    check-sat was not a theory [Sat], a needed value is missing, or a buried (unbound)
    Bool-codomain predicate cell would have to be guessed. Deterministic (R10). *)
val model : t -> (sort_card list * binding list) option
