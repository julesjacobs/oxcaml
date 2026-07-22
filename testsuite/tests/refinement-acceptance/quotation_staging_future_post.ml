#syntax quotations on

(* Facts established by generated code belong to its future execution and do
   not flow backward through quotation construction. *)
let future_fact_does_not_escape_after_quote () =
  let _code = <[ ignore Quotation_staging_vehicle.law_p ]> in
  ignore (() : unit{ Quotation_staging_vehicle.p = true })
