(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/utf8.mli ../lib/utf8.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of the specced UTF-8 codec: the model and its theorems arrive
   through Utf8's .cmi (no prelude flag anywhere, nothing assumed).  The
   client proves roundtrip on real characters and rejection of overlong
   / surrogate encodings THROUGH the imported spec -- it never sees the
   decoder's internals, only [enc_cp]/[dec1] and the exported theorems. *)

open Utf8

(* ROUNDTRIP: e-acute (U+00E9) and euro (U+20AC) encode to their RFC
   bytes and decode straight back -- via the imported [dec1_enc_cp]. *)
let roundtrip_e_acute : res{ _ = Good (233, Bnil) } =
  let b = encode1 233 in
  decode1 b

let roundtrip_euro : res{ _ = Good (8364, Bnil) } =
  let b = encode1 8364 in
  decode1 b

let euro_bytes : bytes_{ _ = Bcons (226, Bcons (130, Bcons (172, Bnil))) } =
  encode1 8364

(* REJECTION, proved through the imported model: overlong NUL 0xC0 0x80
   and the surrogate U+D800 (0xED 0xA0 0x80) both decode to Bad. *)
let overlong_nul : res{ _ = Bad } =
  let b = Bcons (192, Bcons (128, Bnil)) in
  decode1 b

let surrogate_d800 : res{ _ = Bad } =
  let b = Bcons (237, Bcons (160, Bcons (128, Bnil))) in
  decode1 b
