(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "wasm_u32.ml hmc_word64.ml wasm_i32.ml wasm_i64.ml wasm_instruction.ml wasm_code.ml wasm_scalar.ml wasm_word_lowering.ml wasm_scalar_demo.ml";
 { bytecode; }
*)
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_word_lowering
let word (lo : W.limb) (hi : W.limb) = {W.lo; hi}
let check operation a b expected =
  let stack = S.Push (S.I32 91, S.Empty) in
  let continuation = C.Next (I.Plain I.Drop, C.Next (I.I32_const 42, C.Empty)) in
  let code = L.lower operation a b C.Empty (ghost_ stack) in
  if S.run code stack <> S.Done (S.Push (expected, stack)) then failwith "word result";
  let code = L.lower operation a b continuation (ghost_ stack) in
  if S.run code stack <> S.Done (S.Push (S.I32 42, stack)) then failwith "word continuation"
let () =
  check L.Add (word 4294967295 4294967295) (word 1 0) (S.I64 (word 0 0));
  check L.Subtract (word 0 0) (word 1 0) (S.I64 (word 4294967295 4294967295));
  check L.Subtract (word 45 0) (word 3 0) (S.I64 (word 42 0));
  check L.Unsigned_less (word 0 2147483648) (word 0 0) (S.I32 0);
  check L.Equal (word 5 6) (word 5 6) (S.I32 1);
  if S.step (I.Plain I.I64_add) S.Empty <> S.Type_error then failwith "underflow";
  if S.step (I.Plain I.I64_add) (S.Push (S.I32 0, S.Push (S.I64 (word 0 0), S.Empty))) <> S.Type_error then failwith "operand type";
  if S.step I.Block S.Empty <> S.Not_scalar then failwith "non-scalar instruction";
  if S.step (I.Plain I.Select) (S.Push (S.I32 1, S.Push (S.I32 9, S.Push (S.I32 42, S.Empty)))) <>
      S.Done (S.Push (S.I32 42, S.Empty)) then failwith "select order";
  print_endline "Wasm scalar lowering: full-width arithmetic, operand order, continuations, and operand errors passed"
