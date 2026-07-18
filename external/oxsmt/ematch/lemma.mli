(** A stored universally-quantified lemma (ADR-0012 §1.1). The quantifier structure lives
    entirely here, outside the frozen ground {!Oxsmt_core.Term.t} (representation (A),
    §3a): a lemma is a body over placeholder qvars plus ground symbols, instantiated by
    substitution ({!Instance}). *)

open Oxsmt_core
module Sat = Oxsmt_solver.Sat

(** Provenance for cores / messages ([:named] or an anonymous VC-supplied lemma). *)
type origin =
  | Named of string
  | Anonymous

type t =
  { qvars : Qvar.t array (* the forall-bound variables, minted before the body *)
  ; body : Term.t (* well-sorted Bool term over qvars + ground symbols *)
  ; triggers : Term.t list list (* multi-triggers; inner list = conjunctive trigger *)
  ; id : int (* dense, deterministic; certificate + dedup key *)
  ; frame : Sat.var
    (* owning frame's SELECTOR var; instances guarded by [Sat.neg frame] *)
  ; origin : origin
  }
