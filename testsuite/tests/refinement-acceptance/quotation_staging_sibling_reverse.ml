#syntax quotations on

let right_sibling_fact_cannot_prove_left_sibling () =
  (<[
     ( $(ignore (() : unit{ Quotation_staging_vehicle.p = true });
         <[ () ]>),
       $(ignore Quotation_staging_vehicle.law_p; <[ () ]>) )
   ]>
   [@magic_staged_modes])
