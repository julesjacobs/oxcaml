#syntax quotations on

external raise_false_bool : exn -> bool{ false } = "%raise"

let bfalse () : bool{ false } = raise_false_bool Exit

let quotation_is_a_completing_value (flag : bool) =
  let _code =
    if flag then <[ Stdlib.raise Exit ]>
    else (fun _ -> <[ 0 ]>) (bfalse ())
  in
  (0 : int{ false })
