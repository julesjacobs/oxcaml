(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "wasm_u32.ml hmc_word64.ml wasm_i32.ml wasm_i64.ml wasm_instruction.ml wasm_instruction_demo.ml";
 { bytecode; }
*)
module B = Wasm_u32
open Wasm_instruction
let check instruction =
  let tail = B.Byte (255, B.Byte (0, B.End)) in
  if decode (encode instruction tail) <> Some (instruction, tail) then
    failwith "instruction roundtrip"
let rec bytes = function
  | [] -> B.End
  | b :: rest -> if 0 <= b && b < 256 then B.Byte (b, bytes rest)
    else failwith "fixture byte"
let reject input = if decode (bytes input) <> None then failwith "malformed instruction accepted"
let indexed (n : B.u32) =
  List.iter check [Br n; Br_if n; Call n; Local_get n; Local_set n;
    Local_tee n; Global_get n; Global_set n; Call_indirect n; I32_const n;
    I32_load (0, n); I32_load (2, n); I64_load (0, n); I64_load (3, n);
    I32_store (0, n); I32_store (2, n); I64_store (0, n); I64_store (3, n)]
let () =
  List.iter (fun p -> check (Plain p)) [Unreachable; Nop; Else; End; Return; Drop; Select; I32_eqz; I32_eq; I32_ne; I32_lt_u; I32_gt_u; I32_le_u; I32_ge_u; I64_eqz; I64_eq; I64_lt_u; I32_add; I32_sub; I32_mul; I32_and; I32_or; I64_add; I64_sub; I64_and; I64_or; I64_shl; I64_shr_u; I32_wrap_i64; I64_extend_i32_u];
  List.iter check [Block; Loop; If;
    I64_const {Hmc_word64.lo = 0; hi = 0};
    I64_const {Hmc_word64.lo = 4294967295; hi = 4294967295}];
  indexed 0; indexed 127; indexed 128; indexed 2147483648; indexed 4294967295;
  List.iter reject [[]; [255]; [12]; [12; 128]; [12; 255; 255; 255; 255; 16];
    [2]; [2; 127]; [3; 0]; [4; 126]; [17; 0]; [17; 0; 1];
    [40; 3; 0]; [41; 4; 0]; [54; 3; 0]; [55; 4; 0]; [40; 0];
    [65; 0]; [66; 0]];
  print_endline "Wasm instruction codec: opcodes, immediate bounds, suffixes, and malformed encodings passed"
