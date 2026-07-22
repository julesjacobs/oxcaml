#syntax quotations on

let left_sibling_fact_cannot_prove_right_sibling () =
  (<[
     ( $(ignore Quotation_staging_vehicle.law_p; <[ () ]>),
       $(ignore (() : unit{ Quotation_staging_vehicle.p = true });
         <[ () ]>) )
   ]>
   [@magic_staged_modes])
