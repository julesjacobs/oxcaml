module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Index = Hmc_u32_index
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Write = Hmc_wasm_closure_write
module Allocate = Hmc_wasm_closure_allocate
module Source = Hmc_wasm_closure_allocate_source
module Finish = Hmc_wasm_closure_finish
module A = Hmc_heap_allocate
module Machine = Hmc_heap_machine
module Program = Hmc_tail_ir
module K = Hmc_closure_ir
module Image = Hmc_heap_image
module Above = Hmc_heap_image_suffix
module P = Hmc_linear_preservation
module Transport = Hmc_wasm_frame_transport
let[@def] (emit @ total) (fragment : Write.fragment @ immutable) (pc : W.limb) (frame_local : B.u32) (heap_local : B.u32) =
  E.append (Allocate.emit fragment frame_local heap_local) (Finish.emit fragment.Write.bytes pc frame_local heap_local)
type result = {allocated : B.bytes; allocation : A.allocation; state : X.state; frame : Finish.result; tail : B.bytes}
let (correct @ total) : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable -> (stack_limit : D.index) @ immutable ->
    (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable -> (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (id : D.index) @ immutable -> (type_ : D.mono) @ immutable -> (typing : D.typing) @ immutable -> (next : D.index) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (old_pc : W.limb) -> (pc : W.limb) ->
    (capacity : Hmc_wasm_relayout.count) -> (max_code : W.limb) -> (fragment : Write.fragment) @ immutable ->
    (state : X.state) @ immutable -> (frame_base : B.u32) -> (frame_stop : B.u32) -> (heap_base : B.u32) -> (limit : B.u32) -> (frame_local : B.u32) -> (heap_local : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Heap.valid table heap && Heap.object_valid table (Heap.view heap) (Heap.Closure (id, activation.Frame.env))
      && Program.lookup program.Program.code activation.Frame.pc === Some (Program.Keep (G.Load (G.Closure id, type_, typing, next)))
      && Heap.used heap = heap_base && Image.related state.X.memory heap && Above.above heap frame_stop
      && Write.matches signature.G.locals id capacity max_code fragment
      && Codec.decode signature activation.Frame.pc cells === Some (activation, padding)
      && next_signature.G.locals === signature.G.locals && next_signature.G.temporaries === signature.G.temporaries
      && Index.represents next pc && Index.represents (Heap.length cells) capacity
      && frame_stop = frame_base + 16 + 16 * capacity && frame_base <= 4294967248
      && frame_stop <= heap_base && frame_local <> heap_local
      && heap_base + fragment.Write.bytes <= limit && Hmc_linear_bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
      && L.get state.X.machine.E.locals heap_local === Some (S.I32 heap_base)
      && Bytes.drop state.X.memory frame_base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : result | P.equal_prefix heap_base state.X.memory out.allocated
      && P.equal_prefix frame_base state.X.memory out.state.X.memory
      && Bytes.drop out.allocated frame_stop === Some out.tail && Bytes.drop out.state.X.memory frame_stop === Some out.tail
      && Hmc_linear_bounds.covers out.allocated frame_stop && Hmc_linear_bounds.covers out.state.X.memory frame_stop
      && V.length out.state.X.memory === V.length state.X.memory
      && Machine.allocate heap limit (Heap.Closure (id, activation.Frame.env)) === A.Allocated out.allocation
      && Machine.step program globals limit stack_limit {Machine.heap; state = State.Running (activation, frames)} ===
        Machine.Advanced {Machine.heap = out.allocation.A.heap; state = State.Running (out.frame.Finish.activation, frames)}
      && A.correct table heap limit (Heap.Closure (id, activation.Frame.env)) (A.Allocated out.allocation)
      && Image.related out.state.X.memory out.allocation.A.heap
      && out.allocation.A.reference === V.Closure_pointer heap_base
      && Heap.used out.allocation.A.heap = heap_base + fragment.Write.bytes
      && X.run (emit fragment pc frame_local heap_local) state === X.Done out.state
      && out.state.X.memory === out.frame.Finish.memory
      && L.get out.state.X.machine.E.locals heap_local === Some (S.I32 (Heap.used out.allocation.A.heap))
      && L.get out.state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
      && L.replaced state.X.machine.E.locals heap_local (S.I32 (Heap.used out.allocation.A.heap)) out.state.X.machine.E.locals
      && out.state.X.machine.E.stack === state.X.machine.E.stack
      && out.frame.Finish.activation === {activation with Frame.pc = next; accumulator = V.Closure_pointer heap_base}
      && Codec.decode next_signature next out.frame.Finish.cells === Some (out.frame.Finish.activation, padding)
      && Heap.length out.frame.Finish.cells === Heap.length cells
      && Bytes.drop out.state.X.memory frame_base === Some out.frame.Finish.bytes
      && Wire.decode_cells (D.S (Heap.length out.frame.Finish.cells)) out.frame.Finish.bytes ===
        Some (Heap.Cell (V.Word (Header.number pc), out.frame.Finish.cells), out.tail)} @ immutable =
  fun program globals stack_limit table heap signature next_signature activation frames id type_ typing next cells padding old_pc pc capacity max_code fragment state frame_base frame_stop heap_base limit frame_local heap_local before_frame tail premise ->
    let allocated = Source.correct table heap signature activation cells padding old_pc id capacity max_code fragment state frame_base heap_base limit frame_local heap_local before_frame tail () in
    let full = Heap.Cell (V.Word (Header.number old_pc), cells) in
    ghost_ (Heap.length_def full; Index.represents_def (D.S (Heap.length cells)) (capacity + 1);
      P.shrink heap_base frame_stop state.X.memory allocated.Allocate.state.X.memory ());
    let transported = Transport.correct state.X.memory allocated.Allocate.state.X.memory frame_base frame_stop (capacity + 1) full before_frame tail () in
    ghost_ (A.correct_def table heap limit (Heap.Closure (id, activation.Frame.env)) (A.Allocated allocated.Allocate.allocation);
      Above.above_def allocated.Allocate.allocation.A.heap frame_stop;
      L.other_local state.X.machine.E.locals heap_local (S.I32 (Heap.used allocated.Allocate.allocation.A.heap)) allocated.Allocate.state.X.machine.E.locals frame_local ());
    let frame = Finish.correct signature next_signature activation next old_pc pc cells padding fragment.Write.bytes heap_base (Heap.used allocated.Allocate.allocation.A.heap) heap_local allocated.Allocate.state frame_local frame_base transported.Transport.bytes transported.Transport.tail () in
    let final = {X.memory = frame.Finish.memory; machine = allocated.Allocate.state.X.machine} in
    ghost_ (
      let after_full = Heap.Cell (V.Word (Header.number pc), frame.Finish.cells) in
      Heap.length_def after_full;
      Hmc_wasm_frame_suffix.preserve_heap allocated.Allocate.state.X.memory final.X.memory allocated.Allocate.allocation.A.heap frame_base frame_stop (capacity + 1) full after_full transported.Transport.bytes frame.Finish.bytes transported.Transport.tail ();
      Hmc_wasm_frame_preservation.suffix allocated.Allocate.state.X.memory frame_base frame_stop transported.Transport.bytes full transported.Transport.tail (capacity + 1) ();
      Hmc_linear_bounds.covers_def final.X.memory frame_stop;
      Hmc_wasm_frame_preservation.same_length allocated.Allocate.state.X.memory final.X.memory frame_stop transported.Transport.tail ();
      P.shrink heap_base frame_base state.X.memory allocated.Allocate.state.X.memory ();
      Hmc_wasm_cons_success.prefix_transitive state.X.memory allocated.Allocate.state.X.memory final.X.memory frame_base ();
      Hmc_heap_closure_transition.success program globals heap limit stack_limit activation frames id type_ typing next allocated.Allocate.allocation ();
      emit_def fragment pc frame_local heap_local;
      X.append_correct (Allocate.emit fragment frame_local heap_local) (Finish.emit fragment.Write.bytes pc frame_local heap_local) state);
    {allocated = allocated.Allocate.state.X.memory; allocation = allocated.Allocate.allocation; state = final; frame; tail = transported.Transport.tail}
