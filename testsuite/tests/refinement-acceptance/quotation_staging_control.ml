#syntax quotations on

let current_stage_splice_obligation_must_fail () =
  <[ $(ignore (0 : int{ false }); <[ 0 ]>) ]>
