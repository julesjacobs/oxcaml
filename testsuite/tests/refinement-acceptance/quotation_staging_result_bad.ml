#syntax quotations on

(* A normally returning quotation still checks its own result refinement. *)
let returning_quotation_must_check_result_mark () =
  (<[ 0 ]> : <[ int ]> expr{ false })
