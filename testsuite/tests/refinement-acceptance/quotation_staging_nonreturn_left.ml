#syntax quotations on

external stop : exn -> <[ unit ]> expr{ false } = "%raise"

let nonreturning_left_splice_cannot_prove_right_sibling () =
  (<[
     ( $(stop Exit),
       $(ignore (0 : int{ false }); <[ () ]>) )
   ]>
   [@magic_staged_modes])
