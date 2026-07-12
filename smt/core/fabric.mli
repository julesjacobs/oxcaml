(** Internal justification currency for ADR-0014's theory fabric.

    This module is deliberately outside the frozen {!Theory.THEORY} seam.  Children use
    it only when driven by {!Oxsmt_combine.Combine}; ordinary direct-drive callers keep
    seeing {!Explanation.t}. *)

type edge_id = int

type justification =
  | Real of Lit.t
  | Fabric of edge_id

module Explanation : sig
  type t =
    { premises : justification list
    ; rule : Explanation.Rule_tag.t
    }
end

type check_result =
  | Sat
  | Propagations of Lit.t list
  | Conflict of Explanation.t
  | Split of Term.t list

(** Adapter-facing fixed-bound witness. [value] is the canonical rational spelling;
    [lower]/[upper] are the active exact bounds [term >= value]/[term <= value]. *)
type fixed_bounds =
  { value : string
  ; lower : justification
  ; upper : justification
  }

(** Emission-only Stage-1b witness.  The four literals are the two oriented derivations:
    [s_upper,w_lower] prove [s <= w], and [s_lower,w_upper] prove [s >= w]. *)
type equality_witness =
  { value : string
  ; s_lower : Lit.t
  ; s_upper : Lit.t
  ; t_lower : Lit.t
  ; t_upper : Lit.t
  }

type eq_event =
  { edge_id : edge_id
  ; s : Term.t
  ; t : Term.t
  ; gamma : Lit.t list
  ; witness : equality_witness
  }

type trace = { on_fabric_eq : eq_event -> unit }
