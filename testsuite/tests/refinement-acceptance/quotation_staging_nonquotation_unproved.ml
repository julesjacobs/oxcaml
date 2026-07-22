(* Control: the same missing fact remains an ordinary rejection outside the
   quotation machinery. *)
let ordinary_unproved_annotation () =
  ignore (() : unit{ Quotation_staging_vehicle.p = true })
