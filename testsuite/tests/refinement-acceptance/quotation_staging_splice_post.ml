#syntax quotations on

(* A fact established inside a splice cannot escape the quotation into the
   surrounding construction program. *)
let splice_fact_must_not_survive_quotation () =
  let _quotation =
    (<[ $(ignore Quotation_staging_vehicle.law_p; <[ 0 ]>) ]>
     [@magic_staged_modes])
  in
  ignore (() : unit{ Quotation_staging_vehicle.p = true })
