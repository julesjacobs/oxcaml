(* Implementation of utf8.mli, checked against its interface's model
   (the .cmi carries the model and its theorems; there is no local
   prelude and nothing is assumed).  The encoder uses only reflected
   arithmetic; the decoders mirror the model's [dec1]/[dec2]/[dec3]. *)

type bytes_ = Bnil | Bcons of int * bytes_
type res = Bad | Good of int * bytes_

let encode1 : (c : int{ valid_cp _ }) -> bytes_{ _ = enc_cp c } =
  fun c ->
    if c < 128 then Bcons (c, Bnil)
    else if c < 2048
    then Bcons (192 + c / 64, Bcons (128 + c mod 64, Bnil))
    else
      Bcons
        ( 224 + c / 4096,
          Bcons (128 + (c / 64) mod 64, Bcons (128 + c mod 64, Bnil)) )

let decode2 : (b0 : int) -> (rest : bytes_) -> res{ _ = dec2 b0 rest } =
  fun b0 rest ->
    match rest with
    | Bnil -> Bad
    | Bcons (b1, rest1) ->
      if 192 <= b0 && 128 <= b1 && b1 < 192
      then
        let c = ((b0 - 192) * 64) + (b1 - 128) in
        if c < 128 then Bad else Good (c, rest1)
      else Bad

let decode3 : (b0 : int) -> (rest : bytes_) -> res{ _ = dec3 b0 rest } =
  fun b0 rest ->
    match rest with
    | Bnil -> Bad
    | Bcons (b1, rest1) -> (
      match rest1 with
      | Bnil -> Bad
      | Bcons (b2, rest2) ->
        if 128 <= b1 && b1 < 192 && 128 <= b2 && b2 < 192
        then
          let c = ((b0 - 224) * 4096) + ((b1 - 128) * 64) + (b2 - 128) in
          if c < 2048 || (55296 <= c && c <= 57343)
          then Bad
          else Good (c, rest2)
        else Bad)

let decode1 : (bs : bytes_) -> res{ _ = dec1 bs } =
  fun bs ->
    match bs with
    | Bnil -> Bad
    | Bcons (b0, rest) ->
      if 0 <= b0 && b0 < 128
      then Good (b0, rest)
      else if b0 < 224
      then decode2 b0 rest
      else if b0 < 240
      then decode3 b0 rest
      else Bad
