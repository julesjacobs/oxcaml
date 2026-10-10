module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module V = Hmc_tagged_cell
module Header = Hmc_wasm_header_update
module Index = Hmc_u32_index
module Lower = Hmc_wasm_closure_write
module View = Hmc_frame_slices
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module Memory = Hmc_wasm_closure_memory
let (correct @ total) : (signature : G.signature) @ immutable -> (activation : Frame.activation) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (old_pc : W.limb) ->
    (id : D.index) @ immutable -> (capacity : Hmc_wasm_relayout.count) -> (max_code : W.limb) -> (fragment : Lower.fragment) @ immutable ->
    (state : X.state) @ immutable -> (frame_base : B.u32) -> (heap_base : B.u32) -> (limit : B.u32) -> (frame_local : B.u32) -> (heap_local : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Lower.matches signature.G.locals id capacity max_code fragment
      && Codec.decode signature activation.Frame.pc cells === Some (activation, padding)
      && frame_base + 16 + 16 * capacity <= 4294967296
      && heap_base + fragment.Lower.bytes <= limit && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
      && L.get state.X.machine.E.locals heap_local === Some (S.I32 heap_base)
      && Bytes.drop state.X.memory frame_base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : Memory.result | X.run (Lower.emit fragment frame_local heap_local) state === X.Done {X.memory = out.Memory.memory; machine = state.X.machine}
      && Wire.decode (Wire.Closure_schema (Heap.length activation.Frame.env)) out.Memory.bytes === Some (Wire.Closure (fragment.Lower.code, activation.Frame.env), out.Memory.suffix)
      && V.length out.Memory.memory === V.length state.X.memory && Hmc_linear_preservation.equal_prefix heap_base state.X.memory out.Memory.memory
      && Bytes.drop out.Memory.memory heap_base === Some out.Memory.bytes
      && Bytes.drop state.X.memory (S.add32 heap_base fragment.Lower.bytes) === Some out.Memory.suffix
      && Bytes.drop out.Memory.memory (S.add32 heap_base fragment.Lower.bytes) === Some out.Memory.suffix
      && Bounds.covers out.Memory.memory limit} @ immutable =
  fun signature activation cells padding old_pc id capacity max_code fragment state frame_base heap_base limit frame_local heap_local before_frame tail premise ->
    ghost_ (Lower.matches_def signature.G.locals id capacity max_code fragment);
    let view = View.decode signature activation cells padding () in
    let count = Hmc_wasm_schema_counts.encode (Codec.locals_size signature.G.locals) capacity () in
    ghost_ (
      let full = Heap.Cell (V.Word (Header.number old_pc), cells) in
      Heap.length_def full;
      Hmc_frame_header_patch.drop D.Z (V.Word (Header.number old_pc)) activation.Frame.current activation.Frame.accumulator view.View.body;
      Hmc_frame_segments.drop_def D.Z view.View.body;
      Index.represents_def D.Z 0; Index.represents_def (D.S D.Z) 1; Index.represents_def (D.S (D.S D.Z)) 2;
      Hmc_wasm_range_read.correct view.View.env 2 count (D.S (D.S D.Z)) full view.View.body state.X.memory frame_base before_frame tail ();
      Memory.environment_position_def ());
    Memory.correct view.View.env count fragment state frame_base heap_base limit frame_local heap_local ()
