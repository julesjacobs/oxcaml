(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "wasm_u32.ml hmc_word64.ml wasm_i32.ml wasm_i64.ml wasm_instruction.ml wasm_code.ml wasm_address.ml wasm_code_demo.ml";
 { bytecode; }
 { native; }
*)
module B = Wasm_u32
module C = Wasm_code
module A = Wasm_address
open Wasm_instruction
let () =
  let tail = B.Byte (255, B.End) in
  let code = C.Next (I32_const 1, C.Next (I32_const 41,
    C.Next (Plain I32_add, C.Next (Plain End, C.Empty)))) in
  if C.decode (C.length code) (C.encode code tail) <> Some (code, tail) then
    failwith "code roundtrip";
  if C.decode (C.Succ C.Zero) B.End <> None then failwith "truncated code";
  if C.decode (C.Succ C.Zero) tail <> None then failwith "unknown opcode";
  if C.decode C.Zero tail <> Some (C.Empty, tail) then failwith "empty code suffix";
  if A.resolve 65536 65532 0 4 <> Some 65532 then failwith "exact memory end";
  if A.resolve 65536 65533 0 4 <> None then failwith "memory overrun";
  if A.resolve 65536 1 4294967295 4 <> None then failwith "wrapped effective address";
  if A.resolve 4294967296 4294967288 0 8 <> Some 4294967288 then failwith "full memory exact end";
  if A.resolve 4294967296 4294967295 0 1 <> Some 4294967295 then failwith "last byte";
  if A.resolve 4294967296 4294967295 1 1 <> None then failwith "effective address overflow";
  if A.resolve 0 0 0 1 <> None then failwith "empty memory";
  print_endline "Wasm code and addresses: sequence suffixes, truncation, exact memory bounds, and non-wrapping offsets passed"
