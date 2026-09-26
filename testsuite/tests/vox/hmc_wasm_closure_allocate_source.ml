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
module Allocate = Hmc_wasm_closure_allocate
module A = Hmc_heap_allocate
module Machine = Hmc_heap_machine
module Image = Hmc_heap_image
module K = Hmc_closure_ir
let (correct @ total) : (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable -> (signature : G.signature) @ immutable -> (activation : Frame.activation) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (old_pc : W.limb) ->
    (id : D.index) @ immutable -> (capacity : Hmc_wasm_relayout.count) -> (max_code : W.limb) -> (fragment : Lower.fragment) @ immutable ->
    (state : X.state) @ immutable -> (frame_base : B.u32) -> (heap_base : B.u32) -> (limit : B.u32) -> (frame_local : B.u32) -> (heap_local : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Heap.valid table heap && Heap.object_valid table (Heap.view heap) (Heap.Closure (id, activation.Frame.env))
      && Heap.used heap = heap_base && Image.related state.X.memory heap
      && Lower.matches signature.G.locals id capacity max_code fragment
      && Codec.decode signature activation.Frame.pc cells === Some (activation, padding)
      && frame_base + 16 + 16 * capacity <= 4294967296
      && heap_base + fragment.Lower.bytes <= limit && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
      && L.get state.X.machine.E.locals heap_local === Some (S.I32 heap_base)
      && Bytes.drop state.X.memory frame_base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : Allocate.result | Machine.allocate heap limit (Heap.Closure (id, activation.Frame.env)) === A.Allocated out.Allocate.allocation
      && A.correct table heap limit (Heap.Closure (id, activation.Frame.env)) (A.Allocated out.Allocate.allocation)
      && X.run (Allocate.emit fragment frame_local heap_local) state === X.Done out.Allocate.state
      && Image.related out.Allocate.state.X.memory out.Allocate.allocation.A.heap && Bounds.covers out.Allocate.state.X.memory limit
      && V.length out.Allocate.state.X.memory === V.length state.X.memory
      && Hmc_linear_preservation.equal_prefix heap_base state.X.memory out.Allocate.state.X.memory
      && Hmc_linear_bytes.drop out.Allocate.state.X.memory (S.add32 heap_base fragment.Lower.bytes) === Hmc_linear_bytes.drop state.X.memory (S.add32 heap_base fragment.Lower.bytes)
      && Heap.used out.Allocate.allocation.A.heap = heap_base + fragment.Lower.bytes
      && out.Allocate.allocation.A.reference === V.Closure_pointer heap_base
      && L.get out.Allocate.state.X.machine.E.locals heap_local === Some (S.I32 (Heap.used out.Allocate.allocation.A.heap))
      && L.replaced state.X.machine.E.locals heap_local (S.I32 (Heap.used out.Allocate.allocation.A.heap)) out.Allocate.state.X.machine.E.locals
      && out.Allocate.state.X.machine.E.stack === state.X.machine.E.stack} @ immutable =
  fun table heap signature activation cells padding old_pc id capacity max_code fragment state frame_base heap_base limit frame_local heap_local before_frame tail premise ->
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
    Allocate.correct table heap id view.View.env count fragment state frame_base heap_base limit frame_local heap_local ()
