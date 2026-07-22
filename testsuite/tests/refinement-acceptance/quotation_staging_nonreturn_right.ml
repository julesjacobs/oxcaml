#syntax quotations on

external stop : exn -> <[ unit ]> expr{ false } = "%raise"

let nonreturning_right_splice_cannot_prove_left_sibling () =
  (<[
     ( $(ignore (0 : int{ false }); <[ () ]>),
       $(stop Exit) )
   ]>
   [@magic_staged_modes])
