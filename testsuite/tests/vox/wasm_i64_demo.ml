(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "wasm_u32.ml hmc_word64.ml wasm_i64.ml wasm_i64_demo.ml";
 { bytecode; }
*)
module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_i64
let check (lo : W.limb) (hi : W.limb) =
  let word = {W.lo; hi} in
  let tail = B.Byte (42, B.End) in
  if I.decode (I.encode word tail) <> Some (word, tail) then failwith "signed LEB64 roundtrip"
let invalid (last : B.byte) =
  B.Byte (128, B.Byte (128, B.Byte (128, B.Byte (128, B.Byte (128,
    B.Byte (128, B.Byte (128, B.Byte (128, B.Byte (128, B.Byte (last, B.End))))))))))
let () =
  check 0 0; check 63 0; check 64 0; check 4294967295 0; check 0 1;
  check 4294967295 2147483647; check 0 2147483648; check 4294967295 4294967295;
  check 305419896 2882400001;
  if I.decode (invalid 1) <> None || I.decode (invalid 126) <> None || I.decode (invalid 128) <> None then failwith "invalid signed LEB64 sign extension";
  if I.decode (B.Byte (0, B.End)) <> None then failwith "non-fixed-width LEB64 accepted";
  print_endline "fixed-width signed LEB64: full-width words, sign extension, suffixes, and malformed encodings passed"
