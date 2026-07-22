#syntax quotations on

let quoted_body_must_not_prove_splice_obligation () =
  <[
    ignore (Quotation_staging_vehicle.impossible ());
    $(ignore (0 : int{ false }); <[ 0 ]>)
  ]>
