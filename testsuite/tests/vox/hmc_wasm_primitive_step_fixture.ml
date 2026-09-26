module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Codec = Hmc_pointer_frame_codec
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Simple = Hmc_heap_simple
module Model = Hmc_frame_primitive_model
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Lower = Hmc_wasm_primitive_lower
module Block = Hmc_wasm_block_lower
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
module R = Hmc_wasm_moves_fixture
let fixture (base : B.u32) operation left right =
  if base > 7 then failwith "fixture base" else
  let context = D.Binding (D.Forall (D.Z, D.Word64), D.Empty_context) in
  let rest_schema = G.Value (D.Empty_context, D.Word64, G.Empty_temporaries) in
  let signature = {G.locals = context; temporaries = G.Value (context, D.Word64, rest_schema); accumulator = Some D.Word64} in
  let next = Hmc_wasm_control_fixture.index 11 in
  let instruction = G.Primitive (operation, next) in
  let activation = {Frame.pc = Hmc_wasm_control_fixture.index 7; current = V.Closure_pointer 64; accumulator = V.Word right;
    env = Heap.Cell (V.Word (Header.number 99), Heap.Empty);
    temporaries = Frame.Value (V.Word left, Heap.Cell (V.Word (Header.number 88), Heap.Empty),
      Frame.Value (V.Word (Header.number 33), Heap.Empty, Frame.Empty))} in
  if not (Codec.shape signature activation) then failwith "primitive source shape" else
  let old_padding = Heap.Cell (V.Nil, Heap.Cell (V.Nil, Heap.Empty)) in
  let cells = Codec.encode signature activation old_padding () in
  if Lower.build signature instruction 5 11 <> None || Lower.build signature instruction 8 10 <> None then failwith "primitive bounds accepted";
  match Hmc_u32_index.encode 8 (Heap.length cells) with
  | None -> failwith "primitive capacity"
  | Some capacity ->
  match Block.lower Hmc_heap_machine.Empty_globals signature instruction capacity 11, Model.successor signature operation, Simple.step instruction (State.Running (activation, State.Halt)) with
  | Some (Block.Primitive fragment as lowered), Some next_signature, State.Running (next_activation, State.Halt) ->
    let tail = B.Byte (42, B.End) in
    let full = Heap.Cell (V.Word (Header.number 7), cells) in
    let before_frame = Wire.encode_cells full tail in
    let memory = Hmc_wasm_frame_fixture.prefix base before_frame in
    let locals = S.Push (S.I32 base, S.Push (S.I64 (Header.number 37), S.Push (S.I64 (Header.number 99), S.Empty))) in
    let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
    ghost_ (
        Heap.length_def full; Wasm_locals.get_def locals 0;
        Model.successor_def signature operation;
        Block.corresponds_def Hmc_heap_machine.Empty_globals signature instruction capacity 11 lowered);
      let table = Hmc_wasm_table_lower.Add (7, lowered, Hmc_wasm_table_lower.Empty) in
      ghost_ (Hmc_wasm_table_lower.lookup_def table 7);
      let result = Hmc_wasm_dispatch_primitive_source.correct signature next_signature activation next_activation State.Halt operation next context rest_schema
        table Wasm_control.No_labels fragment capacity 11 7 left right cells old_padding state 0 base before_frame tail () in
      let expected_state = {state with X.memory = result.Hmc_wasm_primitive_invariant.memory} in
      (match Wasm_control.run (Hmc_wasm_dispatch_loop.cost table 7 0 lowered)
          (Hmc_wasm_dispatch_loop.configuration table 0 Wasm_control.No_labels state) with
      | Wasm_control.Running actual ->
        if actual <> Hmc_wasm_dispatch_loop.configuration table 0 Wasm_control.No_labels expected_state then failwith "primitive dispatcher iteration"
      | _ -> failwith "primitive dispatcher stopped");
      if not (Codec.shape next_signature next_activation) then failwith "primitive successor shape" else
      let expected_cells = Codec.encode next_signature next_activation result.Hmc_wasm_primitive_invariant.padding () in
      ghost_ (Hmc_frame_decode_unique.frame next_signature next_activation.Frame.pc result.Hmc_wasm_primitive_invariant.cells expected_cells
        next_activation result.Hmc_wasm_primitive_invariant.padding ());
      ghost_ (Block.emit_def lowered 0);
      let code : {code : Wasm_code.t | X.run code state === X.Done {X.memory = result.Hmc_wasm_primitive_invariant.memory; machine = state.X.machine}} @ immutable =
        Block.emit lowered 0 in
      (match X.run code state with
      | X.Done actual -> if actual.X.memory <> result.Hmc_wasm_primitive_invariant.memory || actual.X.machine <> state.X.machine then failwith "primitive full execution"
      | _ -> failwith "primitive full trap");
      {R.memory; base; first = Header.number 37; second = Header.number 99; code;
        expected = Hmc_wasm_range_fixture.fields 0 (Heap.Cell (V.Word (Header.number fragment.Lower.pc), expected_cells))}
  | _ -> failwith "primitive transition"
let fixtures () =
  let max : W.t = {W.lo = 4294967295; hi = 4294967295} in
  let cases = [D.Add, max, Header.number 1; D.Subtract, Header.number 0, Header.number 1;
    D.Equal_word, max, max; D.Unsigned_less, max, Header.number 0] in
  List.concat_map (fun (operation, left, right) -> [fixture 0 operation left right; fixture 7 operation left right]) cases
