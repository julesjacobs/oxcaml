#syntax quotations on

(* Skipping a current splice payload during the future-code walk must not skip
   a refinement attached to the generated antiquotation expression itself. *)
let antiquotation_wrapper_mark_must_fail () =
  <[
    ($(Quote.Expr.int 0) : int{ false })
  ]>
