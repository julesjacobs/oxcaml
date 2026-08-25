type edge_id = int

type justification =
  | Real of Lit.t
  | Fabric of edge_id

module Explanation = struct
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

type fixed_bounds =
  { value : string
  ; lower : justification
  ; upper : justification
  }

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

type merge_event =
  { kept : Term.t
  ; merged : Term.t
  ; kept_tag : Term.t option
  ; merged_tag : Term.t option
  }
