(* A verified UTF-8 codec behind a specced interface.

   THE SPEC IS SIMPLER THAN THE CODE, AND THAT GAP IS THE POINT.  The
   generation-direction spec [enc_cp : codepoint -> bytes] is 5 lines of
   pure arithmetic with ZERO error handling; a byte sequence is VALID
   exactly when it is [enc_cp]'s output (image membership), so minimality
   -- hence overlong rejection -- is STRUCTURAL, not a check the spec
   performs.  The decoder [decode1] is 37 lines that are mostly error
   handling (9 distinct reject paths) and is PROVED equal to a model
   decoder meeting the spec; none of it is trusted.

   AUDIT SURFACE -- the ENTIRE set of lines you must read to trust the
   guarantee; everything else is machine-checked:
     - [enc_cp]   (5 lines) : the minimal encoding, no error handling.
     - [valid_cp] (1 line)  : the domain side conditions, exactly THREE:
         * 0 <= c                        (codepoints are non-negative)
         * c < 0x10000                   (upper cap -- see SCOPE: 0x10000,
                                          not the RFC's 0x10FFFF, because
                                          4-byte sequences are omitted)
         * not (0xD800 <= c <= 0xDFFF)   (surrogates forbidden in UTF-8)
     - the STATEMENTS of [dec1_enc_cp] (roundtrip: dec1 (enc_cp c)=Good c)
       and [dec1_sound] (accept => consumed bytes are the minimal encoding
       of a valid codepoint), plus list-append [bapp] they mention
       (trivial by inspection).
   [dec1]/[dec2]/[dec3]'s guards, the decoders, and all proofs are
   verified against those ~11 lines.  Trust surface is otherwise zero:
   no assume_/assume_unchecked_; [enc_cp] is natively reflected
   arithmetic.  The model lives HERE in the interface and travels to the
   implementation and every client through this .cmi.

   SCOPE: 1..3-byte sequences, U+0000..U+FFFF.  Dropping the 4-byte plane
   only TIGHTENS one side condition (cap 0x10000 vs 0x10FFFF) and drops
   nothing else: surrogate exclusion stays (surrogates are in the 3-byte
   range) and the 2- and 3-byte overlong guards stay.  Adding 4-byte
   would raise the cap to 0x10FFFF and add one branch with its own
   overlong guard. *)

type bytes_ = Bnil | Bcons of int * bytes_
type res = Bad | Good of int * bytes_

[%%vox.lean {lean|
@[grind, expose] public def bapp (xs ys : Vox_Utf8_bytes_) : Vox_Utf8_bytes_ :=
  match xs with
  | .Bnil => ys
  | .Bcons x xs => .Bcons x (bapp xs ys)

public theorem bapp_nil (xs : Vox_Utf8_bytes_) : bapp xs .Bnil = xs := by
  induction xs <;> grind [bapp]
grind_pattern bapp_nil => bapp xs .Bnil

@[grind, expose] public def valid_cp (c : Int) : Prop :=
  0 <= c /\ c < 65536 /\ ¬ (55296 <= c /\ c <= 57343)

@[grind, expose] public def enc_cp (c : Int) : Vox_Utf8_bytes_ :=
  if c < 128 then .Bcons c .Bnil
  else if c < 2048 then .Bcons (192 + c / 64) (.Bcons (128 + c % 64) .Bnil)
  else .Bcons (224 + c / 4096)
         (.Bcons (128 + (c / 64) % 64) (.Bcons (128 + c % 64) .Bnil))

@[grind, expose] public def dec2 (b0 : Int) (rest : Vox_Utf8_bytes_) : Vox_Utf8_res :=
  match rest with
  | .Bnil => .Bad
  | .Bcons b1 rest1 =>
    if 192 <= b0 /\ 128 <= b1 /\ b1 < 192 then
      let c := (b0 - 192) * 64 + (b1 - 128)
      if c < 128 then .Bad else .Good c rest1
    else .Bad

@[grind, expose] public def dec3 (b0 : Int) (rest : Vox_Utf8_bytes_) : Vox_Utf8_res :=
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

@[grind, expose] public def dec1 (bs : Vox_Utf8_bytes_) : Vox_Utf8_res :=
  match bs with
  | .Bnil => .Bad
  | .Bcons b0 rest =>
    if 0 <= b0 /\ b0 < 128 then .Good b0 rest
    else if b0 < 224 then dec2 b0 rest
    else if b0 < 240 then dec3 b0 rest
    else .Bad

-- ROUNDTRIP: a valid codepoint's canonical encoding decodes back to it.
public theorem dec1_bapp (c : Int) (rest : Vox_Utf8_bytes_) (h : valid_cp c) :
    dec1 (bapp (enc_cp c) rest) = .Good c rest := by
  unfold valid_cp at h
  unfold enc_cp
  split
  next => simp only [bapp]; unfold dec1; grind
  next =>
    split
    next => simp only [bapp]; unfold dec1 dec2; grind
    next => simp only [bapp]; unfold dec1 dec3; grind
grind_pattern dec1_bapp => dec1 (bapp (enc_cp c) rest)

public theorem dec1_enc_cp (c : Int) (h : valid_cp c) :
    dec1 (enc_cp c) = .Good c .Bnil := by
  have hb := dec1_bapp c .Bnil h
  grind [bapp_nil]
grind_pattern dec1_enc_cp => dec1 (enc_cp c)

-- SOUNDNESS: if the decoder accepts, the consumed bytes ARE the minimal
-- encoding of a valid codepoint -- overlong/surrogate rejection built in.
public theorem dec1_sound (bs : Vox_Utf8_bytes_) (c : Int) (rest : Vox_Utf8_bytes_)
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

(* Emit the minimal UTF-8 encoding of one codepoint. *)
val encode1 : (c : int{ valid_cp _ }) -> bytes_{ _ = enc_cp c }

(* Read one codepoint from the front of a byte sequence; proved to
   implement the model decoder [dec1]. *)
val decode1 : (bs : bytes_) -> res{ _ = dec1 bs }
