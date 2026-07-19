(** Exact LRA adapter for the frozen theory seam and the Nelson--Oppen fabric. *)

open Oxsmt_core
include Theory.THEORY

val check_fabric : t -> Theory.effort -> Fabric.check_result
val explain_fabric : t -> Lit.t -> Fabric.Explanation.t
val fixed_bounds : t -> Term.t -> Fabric.fixed_bounds option

val fabric_verify
  :  t
  -> Term.t
  -> string
  -> Fabric.justification
  -> Fabric.justification
  -> bool

val notify_eq : t -> edge_id:Fabric.edge_id -> Term.t -> unit

type checkpoint

val checkpoint : t -> checkpoint
val rewind_to_checkpoint : t -> checkpoint -> unit

type conflict_core =
  { farkas : Rational.t list option
  ; atoms : (Term.t * bool) list
  }

val clear_last_conflict : t -> unit
val last_conflict_core : t -> conflict_core option
val is_poisoned : t -> bool
val pivot_count : t -> int
