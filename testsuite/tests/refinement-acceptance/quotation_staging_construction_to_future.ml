#syntax quotations on

(* Facts available while constructing a quotation do not become assumptions
   of the generated program. *)
let construction_fact_must_not_prove_future_code () =
  ignore Quotation_staging_vehicle.law_p;
  <[ ignore (() : unit{ Quotation_staging_vehicle.p = true }) ]>
