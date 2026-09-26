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
module Lower = Hmc_wasm_relayout
let word (lo : W.limb) : W.t = {W.lo; hi = 0}
let rec padding n = if n = 0 then Heap.Empty else Heap.Cell (V.Word (word 999), padding (n - 1))
let rec drop count cells = match count, cells with
  | D.Z, _ -> cells | D.S n, Heap.Cell (_, rest) -> drop n rest | _ -> failwith "frame capacity"
let rec fields (offset : B.u32) cells : (B.u32 * W.t) list = match cells with
  | Heap.Empty -> []
  | Heap.Cell (value, rest) -> if offset <= 4294967279 then
    (offset, V.tag value) :: (offset + 8, V.payload value) :: fields (offset + 16) rest else failwith "fixture offset"
let fixture (base : B.u32) kind =
  let context1 = D.Binding (D.Forall (D.Z, D.Word64), D.Empty_context) in
  let context2 = D.Binding (D.Forall (D.Z, D.Word64), context1) in
  let env = Heap.Cell (V.Word (word 10), Heap.Cell (V.Word (word 20), Heap.Empty)) in
  let saved = Heap.Cell (V.Word (word 90), Heap.Empty) in
  let older = Heap.Cell (V.Word (word 70), Heap.Empty) in
  let rest_schema = G.Value (context1, D.Word64, G.Empty_temporaries) in
  let rest = Frame.Value (V.Word (word 30), older, Frame.Empty) in
  let temporaries = Frame.Environment (saved, rest) in
  let schema = G.Environment (context1, rest_schema) in
  let signature = {G.locals = context2; temporaries = schema; accumulator = Some D.Word64} in
  let next = Hmc_wasm_control_fixture.index 11 in
  let instruction, next_signature = match kind with
    | 0 -> G.Save_environment next, {signature with G.temporaries = G.Environment (context2, schema)}
    | 1 -> G.Save_value next, {G.locals = context1; temporaries = G.Value (context1, D.Word64, rest_schema); accumulator = None}
    | 2 -> G.Bind next, {signature with G.locals = context2; accumulator = None}
    | _ -> G.Restore next, {signature with G.locals = context1; temporaries = rest_schema} in
  let activation = {Frame.pc = Hmc_wasm_control_fixture.index 7; current = V.Closure_pointer 64;
    accumulator = V.Word (word 42); env; temporaries} in
  if not (Codec.shape signature activation) then failwith "source shape" else
  let before_padding = padding 5 in
  let before_cells = Codec.encode signature activation before_padding () in
  match Hmc_heap_simple.step instruction (State.Running (activation, State.Halt)) with
  | State.Running (next_activation, State.Halt) ->
  if not (Codec.shape next_signature next_activation) then failwith "successor shape" else
  let after_padding = drop (Codec.size next_signature) before_cells in
  let after_cells = Codec.encode next_signature next_activation after_padding () in
  ghost_ (Hmc_frame_relayout_model.successor_def signature instruction;
    let reshaped = Hmc_frame_relayout_source.correct signature instruction activation State.Halt before_cells before_padding after_padding
      next_signature next_activation () in
    Hmc_frame_decode_unique.frame next_signature next_activation.Frame.pc reshaped after_cells next_activation after_padding ());
  let tail = B.Byte (42, B.End) in
  let before_frame = Wire.encode (Wire.Closure (7, before_cells)) tail in
  let after_frame = Wire.encode (Wire.Closure (11, after_cells)) tail in
  let memory = F.prefix base before_frame in
  let expected = Wasm_memory_splice.replace memory base before_frame after_frame () in
  let locals = S.Push (S.I32 base, S.Push (S.I64 (word 37), S.Push (S.I64 (word 99), S.Empty))) in
  let state = {X.memory; machine = {E.locals; stack = S.Push (S.I32 91, S.Empty)}} in
  if Lower.build signature instruction 6 11 <> None || Lower.build signature instruction 12 10 <> None
    then failwith "invalid relayout bounds accepted";
  if kind = 0 && Lower.build signature instruction 8 11 <> None then failwith "destination capacity overflow accepted";
  let exact_capacity = if kind = 0 then 9 else 7 in
  if Lower.build signature instruction exact_capacity 11 = None then failwith "exact-fit relayout rejected";
  let fragment = match Lower.build signature instruction 12 11 with Some out -> out | None -> failwith "relayout rejected" in
  ghost_ (Wasm_locals.get_def locals 0; Wasm_sequence_update.zero_def (); Hmc_wasm_pc_update.offset_def ());
  (match Wasm_parallel_copy.apply fragment.Lower.copies memory base with
  | None -> failwith "copy model"
  | Some copied ->
  (match Lower.finish copied base fragment.Lower.pc with
  | None -> failwith "PC model"
  | Some after ->
  ghost_ (Lower.correct fragment 0 state base copied after ());
  if after <> expected then failwith "source/target relayout mismatch";
  let code = Lower.emit fragment 0 in
  (match X.run code state with
  | X.Done result -> if result.X.memory <> expected || result.X.machine <> state.X.machine then failwith "relayout execution"
  | _ -> failwith "relayout trap");
  {R.memory; base; first = word 37; second = word 99; code; expected = (8, word 11) :: fields 16 after_cells}))
  | _ -> failwith "source transition"
let fixtures () =
  let signature = {G.locals = D.Empty_context; temporaries = G.Empty_temporaries; accumulator = None} in
  if Lower.build signature (G.Bind D.Z) 2 0 <> None
    || Lower.build signature (G.Save_value D.Z) 2 0 <> None
    || Lower.build signature (G.Restore D.Z) 2 0 <> None
    || Lower.build signature (G.Call D.Z) 2 0 <> None then failwith "unsupported frame schema accepted";
  (match Lower.build signature (G.Save_environment D.Z) 2 0 with
  | Some fragment -> if fragment.Lower.copies <> Wasm_parallel_copy.End || fragment.Lower.required <> 2 then failwith "empty environment plan"
  | None -> failwith "empty environment rejected");
  [fixture 0 0; fixture 7 0; fixture 0 1; fixture 7 1; fixture 0 2; fixture 7 2; fixture 0 3; fixture 7 3] @ Hmc_wasm_cell_copy_fixture.fixtures () @ Hmc_wasm_range_fixture.fixtures () @ Hmc_wasm_range_pair_fixture.fixtures () @ Hmc_wasm_relayout_pair_fixture.fixtures () @ Hmc_wasm_relayout_value_fixture.fixtures () @ Hmc_wasm_relayout_save_step_fixture.fixtures () @ Hmc_wasm_relayout_saved_step_fixture.fixtures () @ Hmc_wasm_primitive_fixture.fixtures () @ Hmc_wasm_primitive_step_fixture.fixtures () @ Hmc_wasm_global_fixture.fixtures () @ Hmc_wasm_list_relayout_fixture.fixtures ()
