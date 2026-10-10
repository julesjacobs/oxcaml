module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module Heap = Hmc_heap_objects
module V = Hmc_tagged_cell
module Wire = Hmc_heap_wire
module Seg = Hmc_frame_segments
module Codec = Hmc_pointer_frame_codec
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Simple = Hmc_heap_simple
module Model = Hmc_frame_relayout_model
module Index = Hmc_u32_index
module Lower = Hmc_wasm_relayout
module Block = Hmc_wasm_block_lower
module Header = Hmc_wasm_header_update
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module R = Hmc_wasm_moves_fixture
let word (lo : W.limb) : W.t = {W.lo; hi = 0}
let fixture (base : B.u32) kind =
  if base > 7 then failwith "fixture base" else
  let context1 = D.Binding (D.Forall (D.Z, D.Word64), D.Empty_context) in
  let context2 = D.Binding (D.Forall (D.Z, D.Word64), context1) in
  let schema = G.Value (context1, D.Word64, G.Empty_temporaries) in
  let signature = {G.locals = context2; temporaries = G.Environment (context1, schema); accumulator = Some D.Word64} in
  let next = Hmc_wasm_control_fixture.index 11 in
  let instruction = if kind = 0 then G.Restore next else if kind = 1 then G.Save_value next else G.Bind next in
  let activation = {Frame.pc = Hmc_wasm_control_fixture.index 7; current = V.Closure_pointer 64; accumulator = V.Word (word 42);
    env = Heap.Cell (V.Word (word 10), Heap.Cell (V.Word (word 20), Heap.Empty));
    temporaries = Frame.Environment (Heap.Cell (V.Word (word 90), Heap.Empty),
      Frame.Value (V.Word (word 30), Heap.Cell (V.Word (word 70), Heap.Empty), Frame.Empty))} in
  if not (Codec.shape signature activation) then failwith "fixture shape" else
  let old_padding = Heap.Cell (V.Word (word 999), Heap.Cell (V.Word (word 999),
    Heap.Cell (V.Word (word 999), Heap.Cell (V.Word (word 999), Heap.Empty)))) in
  let cells = Codec.encode signature activation old_padding () in
  match Index.encode 11 (Heap.length cells) with
  | None -> failwith "fixture capacity"
  | Some capacity ->
    (match Block.lower Hmc_heap_machine.Empty_globals signature instruction capacity 11, Model.successor signature instruction, Simple.step instruction (State.Running (activation, State.Halt)) with
    | Some (Block.Relayout fragment as lowered), Some next_signature, State.Running (next_activation, State.Halt) ->
      let before_cells = Heap.Cell (V.Word (Header.number 7), cells) in
      let tail = B.Byte (42, B.End) in
      let before_frame = Wire.encode_cells before_cells tail in
      let memory = Hmc_wasm_frame_fixture.prefix base before_frame in
      let locals = S.Push (S.I32 base, S.Push (S.I64 (word 37), S.Push (S.I64 (word 99), S.Empty))) in
      let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
      ghost_ (Block.corresponds_def Hmc_heap_machine.Empty_globals signature instruction capacity 11 lowered; Model.successor_def signature instruction;
        Heap.length_def before_cells; Wasm_locals.get_def locals 0);
      let table = Hmc_wasm_table_lower.Add (7, lowered, Hmc_wasm_table_lower.Empty) in
      ghost_ (Hmc_wasm_table_lower.lookup_def table 7);
      let result = Hmc_wasm_dispatch_saved_source.correct signature next_signature activation next_activation State.Halt instruction next context1 schema table Wasm_control.No_labels fragment capacity 11 7
        cells old_padding state 0 base before_frame tail () in
      let expected_state = {state with X.memory = result.Hmc_wasm_relayout_saved_invariant.memory} in
      (match Wasm_control.run (Hmc_wasm_dispatch_loop.cost table 7 0 lowered)
          (Hmc_wasm_dispatch_loop.configuration table 0 Wasm_control.No_labels state) with
      | Wasm_control.Running actual ->
        if actual <> Hmc_wasm_dispatch_loop.configuration table 0 Wasm_control.No_labels expected_state then failwith "source dispatcher iteration"
      | _ -> failwith "source dispatcher stopped");
      ghost_ (Block.emit_def lowered 0);
      let code : {code : Wasm_code.t | X.run code state === X.Done {X.memory = result.Hmc_wasm_relayout_saved_invariant.memory; machine = state.X.machine}} @ immutable = Block.emit lowered 0 in
      (match X.run code state with
      | X.Done actual -> if actual.X.memory <> result.Hmc_wasm_relayout_saved_invariant.memory || actual.X.machine <> state.X.machine then failwith "source step representation"
      | _ -> failwith "source step execution");
      let expected_cells = Heap.Cell (V.Word (Header.number fragment.Lower.pc), result.Hmc_wasm_relayout_saved_invariant.cells) in
      {R.memory; base; first = word 37; second = word 99; code; expected = Hmc_wasm_range_fixture.fields 0 expected_cells}
    | _ -> failwith "source transition")
let fixtures () = [fixture 0 0; fixture 7 0; fixture 0 1; fixture 7 1; fixture 0 2; fixture 7 2]
