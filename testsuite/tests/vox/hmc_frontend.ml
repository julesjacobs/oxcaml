module D = Hm_declarative
module V = Verified_hm
module G = Hmc_grounding
module A = Hmc_admission
module T = Hmc_templates

type result = Unbound_variable | Type_error | Entry_type_mismatch
  | Unsupported_fragment of A.error | Prepared of T.program [@@inductive]

let prepare : (term : D.term) @ immutable ->
    {r : result | match r with Prepared p -> T.ready p && T.rebuild p.T.globals p.T.entry === term
      | _ -> true} @ immutable = fun term ->
  if not (D.scoped_term D.Z term) then Unbound_variable else
  let inferred = V.infer term in
  match V.elaborate term (borrow_ inferred) with
  | None -> Type_error
  | Some checked -> match G.ground checked with
    | G.Entry_type_mismatch -> Entry_type_mismatch
    | G.Grounded grounded -> match A.admit grounded with
      | A.Rejected error -> Unsupported_fragment error
      | A.Admitted admitted -> Prepared (T.extract admitted)
