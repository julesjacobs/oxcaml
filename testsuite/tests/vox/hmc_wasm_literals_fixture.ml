module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module Wire = Hmc_heap_wire
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module F = Hmc_wasm_frame_fixture
module R = Hmc_wasm_moves_fixture
module Literal = Hmc_wasm_literal_load
module Writes = Wasm_frame_literals
module G = Hmc_cfg_ir
let word (lo : W.limb) : W.t = {W.lo; hi = 0}
let fixture (base : B.u32) atom =
  if base > 4294967248 then failwith "fixture address" else
  match Literal.literal atom with
  | None -> failwith "not a literal"
  | Some value ->
    let env = H.Cell (V.Word (word 10), H.Cell (V.Word (word 20), H.Empty)) in
    ghost_ (Literal.source atom env value ());
    let current = V.Closure_pointer 64 in
    let old = V.Word (word 42) in
    let before_cells = H.Cell (current, H.Cell (old, env)) in
    let after_cells = H.Cell (current, H.Cell (value, env)) in
    let before_object = Wire.Closure (7, before_cells) in
    let after_object = Wire.Closure (11, after_cells) in
    let tail = B.Byte (42, B.End) in
    let before_frame = Wire.encode before_object tail in
    let after_frame = Wire.encode after_object tail in
    let memory = F.prefix base before_frame in
    let expected = Wasm_memory_splice.replace memory base before_frame after_frame () in
    ghost_ (Wire.schema_def before_object; Wire.schema_def after_object;
      H.length_def before_cells; H.length_def (H.Cell (old, env));
      H.length_def after_cells; H.length_def (H.Cell (value, env)));
    let locals = S.Push (S.I32 base, S.Push (S.I64 (word 37), S.Push (S.I64 (word 99), S.Empty))) in
    let state = {X.memory = memory; machine = {E.locals; stack = S.Push (S.I32 91, S.Empty)}} in
    ghost_ (Wasm_locals.get_def locals 0);
    ghost_ (Hmc_wasm_frame_update.correct (H.length env) 7 11 current old value env state expected tail 0 base before_frame after_frame ());
    let writes = Literal.writes 11 value in
    match Writes.apply writes state.X.memory base with
    | None -> failwith "literal writes"
    | Some result_memory ->
      ghost_ (Writes.correct writes 0 state base result_memory ());
      let code = Literal.emit 11 value 0 in
      (match X.run code state with
      | X.Done after -> if after.X.memory <> expected || after.X.machine <> state.X.machine
        then failwith "literal frame encoding"
      | _ -> failwith "literal execution");
      {R.memory = memory; base; first = word 37; second = word 99; code;
       expected = [8, word 11; 24, word 64; 32, V.tag value; 40, V.payload value;
         56, word 10; 72, word 20]}
let fixtures () =
  let atoms = [G.Truth; G.False; G.Nil; G.Word {W.lo = 4294967295; hi = 4294967295}] in
  List.map (fixture 0) atoms @ List.map (fixture 7) atoms
