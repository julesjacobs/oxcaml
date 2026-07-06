(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* THE MONEY SHOT: a decoder that ACCEPTS an overlong (or surrogate)
   encoding cannot be verified against the model -- and the solver hands
   back the exact ill-formed bytes as a counterexample.  This is what
   makes "overlong rejected" a PROOF obligation rather than a code
   review note.  Each buggy variant below reuses the SAME model
   ([dec2]/[dec3], which carry the minimality guards) as the correct
   decoder in demo/lean_utf8.ml; only the implementation drops a check. *)

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
|lean}][%%expect{|
type bytes_ = Bnil | Bcons of int * bytes_
type res = Bad | Good of int * bytes_
|}]

(* BUG 1: the 2-byte decoder drops the overlong guard [if c < 128 then
   Bad].  The contract still claims [_ = dec2 b0 rest], but the model
   rejects overlongs, so verification FAILS -- the counterexample is
   b0 = 192, b1 = 128, i.e. the bytes 0xC0 0x80, the overlong NUL. *)
let decode2_accepts_overlong :
  (b0 : int) -> (rest : bytes_) -> res{ _ = dec2 b0 rest } =
  fun b0 rest ->
    match rest with
    | Bnil -> Bad
    | Bcons (b1, rest1) ->
      if 192 <= b0 && 128 <= b1 && b1 < 192
      then
        let c = ((b0 - 192) * 64) + (b1 - 128) in
        Good (c, rest1)
      else Bad
[%%expect{|
Line 10, characters 8-23:
10 |         Good (c, rest1)
             ^^^^^^^^^^^^^^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: Good (c, rest1) = dec2 b0 rest
Hypotheses:
  c = (b0 - 192) * 64 + (b1 - 128)
  192 <= b0 && (128 <= b1 && b1 < 192)
  rest = Bcons (b1, rest1)
(lean: error: `grind` failed)
|}]

(* BUG 2: the 3-byte decoder drops the surrogate guard (it keeps the
   overlong [c < 2048] check but forgets [55296 <= c <= 57343]).  The
   counterexample is a surrogate codepoint, e.g. the bytes 0xED 0xA0
   0x80 = U+D800. *)
let decode3_accepts_surrogate :
  (b0 : int) -> (rest : bytes_) -> res{ _ = dec3 b0 rest } =
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
          if c < 2048 then Bad else Good (c, rest2)
        else Bad)
[%%expect{|
Line 13, characters 36-51:
13 |           if c < 2048 then Bad else Good (c, rest2)
                                         ^^^^^^^^^^^^^^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: Good (c, rest2) = dec3 b0 rest
Hypotheses:
  not (c < 2048)
  c = (b0 - 224) * 4096 + (b1 - 128) * 64 + (b2 - 128)
  128 <= b1 && (b1 < 192 && (128 <= b2 && b2 < 192))
  rest1 = Bcons (b2, rest2)
  rest = Bcons (b1, rest1)
(lean: error: `grind` failed)
|}]

(* BUG 3: an ENCODER that claims a codepoint's canonical encoding but
   emits a non-minimal (overlong) two-byte form for a small codepoint.
   Its contract [_ = enc_cp c] fails: the model's [enc_cp] is minimal,
   so a codepoint below 128 must be one byte. *)
let encode_overlong : (c : int{ valid_cp _ }) -> bytes_{ _ = enc_cp c } =
  fun c -> Bcons (192 + c / 64, Bcons (128 + c mod 64, Bnil))
[%%expect{|
Line 2, characters 11-61:
2 |   fun c -> Bcons (192 + c / 64, Bcons (128 + c mod 64, Bnil))
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: Bcons (192 + c / 64, Bcons (128 + c mod 64, Bnil)) = enc_cp c
Hypotheses:
  valid_cp c
(lean: error: `grind` failed)
|}]
