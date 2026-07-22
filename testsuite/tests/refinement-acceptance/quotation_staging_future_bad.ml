#syntax quotations on

(* Quoted source is verified in its own future-stage fact channel. *)
let generated_code_obligation_must_fail () =
  <[ (0 : int{ false }) ]>
