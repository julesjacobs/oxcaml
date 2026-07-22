#syntax quotations on

(* A fact established while constructing a splice stays inside that payload;
   it cannot prove an obligation in the generated program. *)
let splice_fact_must_not_prove_future_code () =
  (<[
    ignore $(ignore Quotation_staging_vehicle.law_p; <[ () ]>);
    ignore (() : unit{ Quotation_staging_vehicle.p = true })
  ]>
  [@magic_staged_modes])
