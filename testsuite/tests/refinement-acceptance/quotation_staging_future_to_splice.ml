#syntax quotations on

(* A fact established by future generated code cannot flow backward into a
   splice which executes while that code is being constructed. *)
let future_fact_must_not_prove_current_splice () =
  <[
    ignore Quotation_staging_vehicle.law_p;
    $(ignore (() : unit{ Quotation_staging_vehicle.p = true }); <[ 0 ]>)
  ]>
