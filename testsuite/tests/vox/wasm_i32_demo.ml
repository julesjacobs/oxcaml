(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "wasm_u32.ml wasm_i32.ml wasm_i32_demo.ml";
 { bytecode; }
*)
module B = Wasm_u32
module I = Wasm_i32
let check (n : B.u32) =
  let tail = B.Byte (42, B.End) in
  let bytes = I.encode n tail in
  if I.decode bytes <> Some (n, tail) then failwith "signed LEB32 roundtrip";
  match bytes with
  | B.Byte (_, B.Byte (_, B.Byte (_, B.Byte (_, B.Byte (last, actual_tail))))) ->
    if actual_tail <> tail then failwith "signed LEB32 suffix";
    if (n >= 2147483648) <> (last >= 120) then failwith "signed LEB32 sign extension"
  | _ -> failwith "signed LEB32 width"
let () =
  check 0; check 63; check 64; check 127; check 128; check 268435455;
  check 2147483647; check 2147483648; check 4294967295;
  let bad last = B.Byte (128, B.Byte (128, B.Byte (128, B.Byte (128, B.Byte (last, B.End))))) in
  if I.decode (bad 8) <> None || I.decode (bad 15) <> None || I.decode (bad 112) <> None || I.decode (bad 128) <> None
  then failwith "invalid signed LEB32 accepted";
  if I.decode (B.Byte (0, B.End)) <> None then failwith "non-fixed-width encoding accepted";
  print_endline "fixed-width signed LEB32: bit patterns, suffixes, sign extension, and malformed encodings passed"
