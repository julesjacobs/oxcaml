(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* A VERIFIED UTF-8 codec, proved against a Lean specification.

   Scope: 1..3-byte sequences, covering codepoints U+0000..U+FFFF with
   surrogates (U+D800..U+DFFF) excluded -- enough to exhibit UTF-8's
   whole essence: multi-byte length dispatch, continuation-byte
   validation, and (the headline for a security audience) rejection of
   OVERLONG and SURROGATE encodings.  The 4-byte plane is omitted only
   to keep proof size down; nothing here relies on its absence.

   The MODEL (below, in Lean):
   - [enc_cp c] : the MINIMAL byte encoding of a codepoint, expressed
     purely arithmetically (b / 64, b mod 64, ...) so it reflects into
     the logic natively -- the trust surface is ZERO ([grep] this file
     for [assume]: none).
   - [dec1] : the model decoder.  Its overlong guard ([c < 128] /
     [c < 2048]) and surrogate guard are exactly what "minimal, valid"
     means; validity of a byte sequence is being in [enc_cp]'s image.
   - Two theorems, each proved once by case analysis / induction:
       [dec1_enc_cp] : ROUNDTRIP -- [dec1 (enc_cp c) = Good c].
       [dec1_sound]  : SOUNDNESS -- if the decoder accepts, the bytes it
                       consumed ARE the minimal encoding of a valid
                       codepoint.  This BUILDS IN overlong/surrogate
                       rejection: no non-minimal sequence can decode.

   THE SPEC IS SIMPLER THAN THE CODE.  What you must READ to trust the
   guarantee is only [enc_cp] (5 lines, no error handling) and the
   domain side conditions in [valid_cp] -- exactly three: [0 <= c],
   [c < 0x10000] (the cap; 0x10000 not 0x10FFFF because 4-byte is out of
   scope), and surrogate exclusion [not (0xD800 <= c <= 0xDFFF)].  The
   decoder below is 37 lines, mostly error handling (9 reject paths),
   ALL proved -- not read.  Scope note: dropping 4-byte only tightens the
   cap; surrogate exclusion and the 2/3-byte overlong guards stay. *)

type bytes_ = Bnil | Bcons of int * bytes_
type res = Bad | Good of int * bytes_
[%%vox.lean {lean|
@[grind] def bapp (xs ys : Vox_bytes_) : Vox_bytes_ :=
  match xs with
  | .Bnil => ys
  | .Bcons x xs => .Bcons x (bapp xs ys)

@[grind] theorem bapp_nil (xs : Vox_bytes_) : bapp xs .Bnil = xs := by
  induction xs <;> grind [bapp]

@[grind] def valid_cp (c : Int) : Prop :=
  0 <= c /\ c < 65536 /\ ¬ (55296 <= c /\ c <= 57343)

@[grind] def enc_cp (c : Int) : Vox_bytes_ :=
  if c < 128 then .Bcons c .Bnil
  else if c < 2048 then .Bcons (192 + c / 64) (.Bcons (128 + c % 64) .Bnil)
  else .Bcons (224 + c / 4096)
         (.Bcons (128 + (c / 64) % 64) (.Bcons (128 + c % 64) .Bnil))

@[grind] def dec2 (b0 : Int) (rest : Vox_bytes_) : Vox_res :=
  match rest with
  | .Bnil => .Bad
  | .Bcons b1 rest1 =>
    if 192 <= b0 /\ 128 <= b1 /\ b1 < 192 then
      let c := (b0 - 192) * 64 + (b1 - 128)
      if c < 128 then .Bad else .Good c rest1
    else .Bad

@[grind] def dec3 (b0 : Int) (rest : Vox_bytes_) : Vox_res :=
  match rest with
  | .Bnil => .Bad
  | .Bcons b1 rest1 =>
    match rest1 with
    | .Bnil => .Bad
    | .Bcons b2 rest2 =>
      if 128 <= b1 /\ b1 < 192 /\ 128 <= b2 /\ b2 < 192 then
        let c := (b0 - 224) * 4096 + (b1 - 128) * 64 + (b2 - 128)
        if c < 2048 \/ (55296 <= c /\ c <= 57343) then .Bad else .Good c rest2
      else .Bad

@[grind] def dec1 (bs : Vox_bytes_) : Vox_res :=
  match bs with
  | .Bnil => .Bad
  | .Bcons b0 rest =>
    if 0 <= b0 /\ b0 < 128 then .Good b0 rest
    else if b0 < 224 then dec2 b0 rest
    else if b0 < 240 then dec3 b0 rest
    else .Bad

@[grind] theorem dec1_bapp (c : Int) (rest : Vox_bytes_) (h : valid_cp c) :
    dec1 (bapp (enc_cp c) rest) = .Good c rest := by
  unfold valid_cp at h
  unfold enc_cp
  split
  next => simp only [bapp]; unfold dec1; grind
  next =>
    split
    next => simp only [bapp]; unfold dec1 dec2; grind
    next => simp only [bapp]; unfold dec1 dec3; grind

@[grind] theorem dec1_enc_cp (c : Int) (h : valid_cp c) :
    dec1 (enc_cp c) = .Good c .Bnil := by
  have hb := dec1_bapp c .Bnil h
  grind [bapp_nil]

@[grind] theorem dec1_sound (bs : Vox_bytes_) (c : Int) (rest : Vox_bytes_)
    (h : dec1 bs = .Good c rest) :
    valid_cp c /\ bapp (enc_cp c) rest = bs := by
  unfold valid_cp enc_cp
  rcases bs with _ | ⟨b0, rest0⟩
  · simp [dec1] at h
  · rcases rest0 with _ | ⟨b1, rest1⟩
    · simp only [dec1, dec2, dec3] at h; grind
    · rcases rest1 with _ | ⟨b2, rest2⟩
      · simp only [dec1, dec2, dec3] at h; grind
      · simp only [dec1, dec2, dec3] at h
        split at h <;> (try split at h) <;> (try split at h) <;> (try split at h) <;> (try split at h) <;> grind
|lean}]
[%%expect{|
type bytes_ = Bnil | Bcons of int * bytes_
type res = Bad | Good of int * bytes_
|}]

(* Encoder: proved to produce exactly the model's minimal encoding. *)
let encode1 : (c : int{ valid_cp _ }) -> bytes_{ _ = enc_cp c } =
  fun c ->
    if c < 128 then Bcons (c, Bnil)
    else if c < 2048
    then Bcons (192 + c / 64, Bcons (128 + c mod 64, Bnil))
    else
      Bcons
        ( 224 + c / 4096,
          Bcons (128 + (c / 64) mod 64, Bcons (128 + c mod 64, Bnil)) )
[%%expect{|
val encode1 : (c : int{ valid_cp _ }) -> bytes_{ _ = (enc_cp c) } = <fun>
|}]

(* Continuation-byte decoders, proved equal to the model's [dec2]/[dec3]. *)
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

(* The head decoder, proved equal to the model [dec1]. *)
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
[%%expect{|
val decode2 : (b0 : int) -> (rest : bytes_) -> res{ _ = (dec2 b0 rest) } =
  <fun>
val decode3 : (b0 : int) -> (rest : bytes_) -> res{ _ = (dec3 b0 rest) } =
  <fun>
val decode1 : (bs : bytes_) -> res{ _ = (dec1 bs) } = <fun>
|}]

(* ROUNDTRIP on real characters, PROVED (not tested):
   U+00E9 e-acute -> 0xC3 0xA9 ; U+20AC euro sign -> 0xE2 0x82 0xAC. *)
let e_acute_bytes : bytes_{ _ = Bcons (195, Bcons (169, Bnil)) } =
  encode1 233

let euro_bytes : bytes_{ _ = Bcons (226, Bcons (130, Bcons (172, Bnil))) } =
  encode1 8364

let roundtrip_e_acute : res{ _ = Good (233, Bnil) } =
  let b = encode1 233 in
  decode1 b

let roundtrip_euro : res{ _ = Good (8364, Bnil) } =
  let b = encode1 8364 in
  decode1 b
[%%expect{|
val e_acute_bytes : bytes_{ _ = (Bcons (195, Bcons (169, Bnil))) } =
  Bcons (195, Bcons (169, Bnil))
val euro_bytes : bytes_{ _ = (Bcons (226, Bcons (130, Bcons (172, Bnil)))) } =
  Bcons (226, Bcons (130, Bcons (172, Bnil)))
val roundtrip_e_acute : res{ _ = (Good (233, Bnil)) } = Good (233, Bnil)
val roundtrip_euro : res{ _ = (Good (8364, Bnil)) } = Good (8364, Bnil)
|}]

(* REJECTION, as a PROOF consequence of the model (the bytes are
   literal): the correct decoder returns Bad on each ill-formed input.
   - 0xC0 0x80 : overlong NUL (U+0000 stuffed into 2 bytes).
   - 0xE0 0x80 0x80 : overlong NUL in 3 bytes.
   - 0xED 0xA0 0x80 : the surrogate U+D800, forbidden in UTF-8.
   - 0x80 : a lone continuation byte with no lead. *)
let overlong_nul_2 : res{ _ = Bad } =
  let b = Bcons (192, Bcons (128, Bnil)) in
  decode1 b

let overlong_nul_3 : res{ _ = Bad } =
  let b = Bcons (224, Bcons (128, Bcons (128, Bnil))) in
  decode1 b

let surrogate_d800 : res{ _ = Bad } =
  let b = Bcons (237, Bcons (160, Bcons (128, Bnil))) in
  decode1 b

let lone_continuation : res{ _ = Bad } =
  let b = Bcons (128, Bnil) in
  decode1 b
[%%expect{|
val overlong_nul_2 : res{ _ = Bad } = Bad
val overlong_nul_3 : res{ _ = Bad } = Bad
val surrogate_d800 : res{ _ = Bad } = Bad
val lone_continuation : res{ _ = Bad } = Bad
|}]
