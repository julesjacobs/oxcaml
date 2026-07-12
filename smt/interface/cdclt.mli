(** CDCL(T) seam glue (ADR-0005 §3): drives the Nelson-Oppen combined theory
    [Combine (Uflia_router) (Euf_adapter) (Lia_adapter)] behind the propositional SAT
    core's {!Oxsmt_solver.Sat.theory} callback. Owns the theory-atom <-> SAT-var bijection
    (1:1, CONTRACT-ATOM), maps every seam event onto the frozen THEORY, keeps one
    backtrack frame per SAT decision level, caps mid-solve splits, and snapshots the model
    at the accepting Final->Sat. Internal to {!Session}; see the .ml for the full
    contract. *)

open Oxsmt_core

type t

(** A nullary-symbol model value (eval-agnostic; the CLI renders it to the §8 self-check
    sidecar grammar). *)
type value =
  | VBool of bool
  | VInt of int
  | VUninterp of int

(** A model binding. v1 emits only nullary [Const] bindings (see {!model_bindings}). *)
type binding = Const of string * value

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

(** The nullary-symbol model reconstructed from the snapshot of the accepting Final->Sat,
    or [None] if the last check-sat was not a theory [Sat] or no table-free model is
    reconstructable (see {!binding}). *)
val model_bindings : t -> binding list option
