#syntax quotations on

let nested_splice_fact_does_not_run () =
  let _code =
    (<[
       <[
         $(ignore Quotation_staging_vehicle.law_p; <[ 0 ]>)
       ]>
     ]>
     [@magic_staged_modes])
  in
  ignore (() : unit{ Quotation_staging_vehicle.p = true })
