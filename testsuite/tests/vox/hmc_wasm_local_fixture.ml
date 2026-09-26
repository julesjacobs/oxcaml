module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module R = Hmc_wasm_moves_fixture
module F = Hmc_wasm_frame_fixture
module Lower = Hmc_wasm_simple_lower
let word (lo : W.limb) : W.t = {W.lo; hi = 0}
let fixture (base : B.u32) second first_value first_type second_value second_type =
  if base > 4294967200 then failwith "fixture address" else
  let number : Lower.slot = if second then 1 else 0 in
  let index = if second then D.S D.Z else D.Z in
  let value = if second then second_value else first_value in
  let ty = if second then second_type else first_type in
  let current = V.Closure_pointer 64 in let old = V.Word (word 42) in
  let env_tail = Heap.Cell (second_value, Heap.Empty) in
  let env = Heap.Cell (first_value, env_tail) in
  let context_tail = D.Binding (D.Forall (D.Z, second_type), D.Empty_context) in
  let context = D.Binding (D.Forall (D.Z, first_type), context_tail) in
  let saved_tail = Heap.Cell (V.Word (word 888), Heap.Empty) in
  let saved = Heap.Cell (V.Word (word 999), saved_tail) in
  let saved_context_tail = D.Binding (D.Forall (D.Z, D.Word64), D.Empty_context) in
  let saved_context = D.Binding (D.Forall (D.Z, D.Word64), saved_context_tail) in
  let schema = G.Environment (saved_context, G.Empty_temporaries) in
  let temporaries = Frame.Environment (saved, Frame.Empty) in
  let signature = {G.locals = context; temporaries = schema; accumulator = None} in
  let next_signature = {signature with G.accumulator = Some ty} in
  let activation = {Frame.pc = Hmc_wasm_control_fixture.index 7; current; accumulator = old; env; temporaries} in
  let next = Hmc_wasm_control_fixture.index 11 in
  let padding = Heap.Cell (V.Closure_pointer 128, Heap.Empty) in
  ghost_ (Codec.shape_def signature activation; Codec.environment_def context env;
    Codec.environment_def context_tail env_tail; Codec.environment_def D.Empty_context Heap.Empty;
    Codec.temporaries_shape_def schema temporaries; Codec.temporaries_shape_def G.Empty_temporaries Frame.Empty;
    Codec.environment_def saved_context saved; Codec.environment_def saved_context_tail saved_tail;
    Hmc_u32_index.represents_def index number; Hmc_u32_index.represents_def D.Z 0;
    Hmc_heap_simple.lookup_def env index; Hmc_heap_simple.lookup_def env_tail D.Z);
  let frame_cells = Codec.encode signature activation padding () in
  ghost_ (Codec.decode_def signature activation.Frame.pc frame_cells);
  match frame_cells with
  | Heap.Cell (_, Heap.Cell (_, cells)) ->
    let after_cells = Heap.Cell (current, Heap.Cell (value, cells)) in
    let before_object = Wire.Closure (7, frame_cells) in
    let after_object = Wire.Closure (11, after_cells) in
    let tail = B.Byte (42, B.End) in
    let before_frame = Wire.encode before_object tail in
    let after_frame = Wire.encode after_object tail in
    let memory = F.prefix base before_frame in
    let expected = Wasm_memory_splice.replace memory base before_frame after_frame () in
    let locals = S.Push (S.I32 base, S.Push (S.I64 (word 37), S.Push (S.I64 (word 99), S.Empty))) in
    let state = {X.memory; machine = {E.locals; stack = S.Push (S.I32 91, S.Empty)}} in
    let source_tag = Lower.slot_tag number in let source_payload = Lower.slot_payload number in
    let derivation = D.Variable D.No_arguments in
    ghost_ (Wire.schema_def before_object; Wire.schema_def after_object;
      Heap.length_def frame_cells; Heap.length_def (Heap.Cell (old, cells));
      Heap.length_def after_cells; Heap.length_def (Heap.Cell (value, cells));
      Wasm_locals.get_def locals 0; Lower.slot_tag_def number; Lower.slot_payload_def number;
      Hmc_wasm_local_step.correct signature next_signature activation State.Halt padding cells (Heap.length cells)
        index number source_tag source_payload ty derivation next 7 11 value state expected tail 0 base before_frame after_frame ());
    let instruction = G.Load (G.Local index, ty, derivation, next) in
    let code = match Lower.lower instruction 11 with
      | None -> failwith "local instruction rejected"
      | Some fragment ->
        ghost_ (Hmc_wasm_fragment_code.local index ty derivation next number 11 fragment 0 ());
        let code : {code : Wasm_code.t | X.run code state === X.Done {X.memory = expected; machine = state.X.machine}} @ immutable
          = Lower.emit fragment 0 in code in
    (match X.run code state with
    | X.Done after -> if after.X.memory <> expected || after.X.machine <> state.X.machine then failwith "local frame encoding"
    | _ -> failwith "local execution");
    {R.memory; base; first = word 37; second = word 99; code;
      expected = [8, word 11; 24, word 64; 32, V.tag value; 40, V.payload value;
        48, V.tag first_value; 56, V.payload first_value; 64, V.tag second_value; 72, V.payload second_value;
        88, word 999; 104, word 888; 120, word 128]}
  | _ -> failwith "compiler frame encoding"
let fixtures () =
  let instruction = G.Load (G.Local D.Z, D.Word64, D.Variable D.No_arguments, Hmc_wasm_control_fixture.index 13) in
  if Lower.lower instruction 12 <> None then failwith "local successor overflow accepted";
  let pair base second = fixture base second (V.Word {W.lo = 4294967295; hi = 2882400001}) D.Word64 (V.Boolean false) D.Boolean in
  let pointers base second = fixture base second V.Nil (D.List_type D.Word64) (V.Closure_pointer 64) (D.Function (D.Word64, D.Word64)) in
  [pair 0 false; pair 7 false; pair 0 true; pair 7 true;
   pointers 0 false; pointers 7 false; pointers 0 true; pointers 7 true]
