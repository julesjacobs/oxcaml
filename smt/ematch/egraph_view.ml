(* Read-only e-graph view (ADR-0012 L2). See egraph_view.mli. *)

open Oxsmt_core

type t =
  { app_terms_by_symbol : Symbol.t -> Term.t list
  ; find_class_opt : Term.t -> int option
  ; equal_if_registered : Term.t -> Term.t -> bool
  ; class_members : Term.t -> Term.t list
  ; ground_terms_by_sort : Sort.t -> Term.t list
  }

let empty =
  { app_terms_by_symbol = (fun _ -> [])
  ; find_class_opt = (fun _ -> None)
  ; equal_if_registered = (fun a b -> Term.equal a b)
  ; class_members = (fun t -> [ t ])
  ; ground_terms_by_sort = (fun _ -> [])
  }
;;
