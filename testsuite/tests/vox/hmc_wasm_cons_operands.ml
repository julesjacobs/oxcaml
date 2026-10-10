module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Index = Hmc_u32_index
module Slots = Hmc_wasm_simple_lower
module Capture = Hmc_wasm_cons_capture
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
let (correct @ total) : (signature : G.signature) @ immutable -> (activation : Frame.activation) @ immutable ->
    (context : D.context) @ immutable -> (ty : D.mono) @ immutable -> (schema : G.temporaries) @ immutable ->
    (head : V.value) @ immutable -> (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    (old_pc : W.limb) -> (env_count : Slots.slot) -> (state : X.state) @ immutable -> (base : B.u32) ->
    (frame_local : B.u32) -> (heap_local : B.u32) -> (limit_local : B.u32) -> (slots : Capture.slots) @ immutable ->
    (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | Codec.decode signature activation.Frame.pc cells === Some (activation, padding)
      && signature.G.temporaries === G.Value (context, ty, schema)
      && (match activation.Frame.temporaries with Frame.Value (value, _, _) -> value === head | _ -> false)
      && Index.represents activation.Frame.pc old_pc && Index.represents (Codec.locals_size signature.G.locals) env_count
      && base + 48 + 16 * env_count <= 4294967280
      && Bytes.drop state.X.memory base === Some bytes
      && Wire.decode_cells (D.S (Heap.length cells)) bytes === Some (Heap.Cell (V.Word (Header.number old_pc), cells), suffix)
      && Capture.distinct slots && Capture.separate slots frame_local && Capture.separate slots heap_local && Capture.separate slots limit_local
      && Capture.writable slots state.X.machine.E.locals && L.get state.X.machine.E.locals frame_local === Some (S.I32 base)} ->
    {out : S.stack | X.run (Capture.emit (Slots.slot_tag env_count) (Slots.slot_payload env_count) slots frame_local) state
        === X.Done {X.memory = state.X.memory; machine = {E.locals = out; stack = state.X.machine.E.stack}}
      && L.get out slots.Capture.head_tag === Some (S.I64 (V.tag head)) && L.get out slots.Capture.head_payload === Some (S.I64 (V.payload head))
      && L.get out slots.Capture.tail_tag === Some (S.I64 (V.tag activation.Frame.accumulator))
      && L.get out slots.Capture.tail_payload === Some (S.I64 (V.payload activation.Frame.accumulator))
      && L.get out frame_local === Some (S.I32 base)
      && L.get out heap_local === L.get state.X.machine.E.locals heap_local && L.get out limit_local === L.get state.X.machine.E.locals limit_local} @ immutable =
  fun signature activation context ty schema head cells padding old_pc env_count state base frame_local heap_local limit_local slots bytes suffix premise ->
    let view = Hmc_frame_slices.decode signature activation cells padding () in
    ghost_ (Codec.decode_temporaries_def signature.G.temporaries view.Hmc_frame_slices.temporaries);
    match view.Hmc_frame_slices.temporaries with
    | Heap.Cell (_, saved) ->
      ghost_ (Heap.length_def cells; Heap.length_def (Heap.Cell (activation.Frame.accumulator, view.Hmc_frame_slices.body));
        Slots.slot_tag_def env_count; Slots.slot_payload_def env_count;
        Hmc_wasm_cons_read.correct state.X.memory base bytes old_pc activation.Frame.current activation.Frame.accumulator head
          view.Hmc_frame_slices.body saved suffix (Codec.locals_size signature.G.locals) env_count (Slots.slot_tag env_count) (Slots.slot_payload env_count) ());
      Capture.correct head activation.Frame.accumulator state base frame_local heap_local limit_local (Slots.slot_tag env_count) (Slots.slot_payload env_count) slots ()
    | _ -> unreachable_ ()
