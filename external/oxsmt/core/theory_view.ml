type atom =
  | Equality of Term.t * Term.t
  | Le_zero of Term.t
  | Predicate of Symbol.t * Term.t Iarr.t
  | Bool_lit of bool

(* Frozen is_atom (ADR-0003 Decision 2). Guarded by Bool sort: Le/Eq/Bool_const are
   already Bool by construction, App may be either. A Bool-argument Eq is an iff
   (connective) so it is NOT an atom; a non-Bool Eq (e.g. Int equality) is. *)
let is_atom (t : Term.t) =
  match t.node with
  | Le _ -> true
  | Eq (a, _) -> not (Sort.equal a.sort Sort.bool)
  | App (_, _) -> Sort.equal t.sort Sort.bool
  | Bool_const _ -> true
  | And _ | Or _ | Not _ | Ite _ | Arith _ | Real_arith _ | Int_const _ | Real_const _ ->
    false
;;

let atom (t : Term.t) =
  match t.node with
  | Le arg -> Le_zero arg
  | Eq (a, b) -> Equality (a, b)
  | App (sym, args) -> Predicate (sym, args)
  | Bool_const b -> Bool_lit b
  | _ -> invalid_arg "Theory_view.atom: not an atom"
;;

let is_app (t : Term.t) =
  match t.node with
  | App _ -> true
  | _ -> false
;;

let linear (t : Term.t) =
  match t.node with
  | Arith l -> Some l
  | ( Bool_const _
    | Int_const _
    | Real_const _
    | App _
    | Real_arith _
    | Le _
    | Eq _
    | Not _
    | And _
    | Or _
    | Ite _ ) -> None
;;
