(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "wasm_u32.ml wasm_u32_demo.ml";
 { bytecode; }
*)
open Wasm_u32

let expect (n : u32) =
  let tail = Byte (42, End) in
  match decode_5 (encode_u32 n tail) with
  | Some (m, Byte (42, End)) when m = n -> ()
  | _ -> failwith "u32 roundtrip"

let () =
  expect 0; expect 127; expect 128; expect 16383; expect 16384;
  expect 2097151; expect 2097152; expect 268435455; expect 268435456;
  expect 4294967295;
  (match decode_5 (Byte (229, Byte (142, Byte (38, End)))) with
   | Some (624485, End) -> () | _ -> failwith "canonical u32");
  (match decode_5 (Byte (128, End)) with
   | None -> () | _ -> failwith "truncated u32 accepted");
  (match decode_5 (Byte (128, Byte (128, Byte (128, Byte (128,
      Byte (16, End)))))) with
   | None -> () | _ -> failwith "overflowing u32 accepted");
  (match decode_5 (Byte (128, Byte (128, Byte (128, Byte (128,
      Byte (128, Byte (0, End))))))) with
   | None -> () | _ -> failwith "overlong u32 accepted")
