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
module Success = Hmc_wasm_closure_success
module T = Wasm_control
module C = Wasm_code
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
module Select = Hmc_wasm_allocation_select
module Branch = Wasm_control_branch_finish
let[@def] (emit @ total) (fragment : Write.fragment @ immutable) (pc : W.limb) (frame_local : B.u32) (heap_local : B.u32) (limit_local : B.u32) =
  Select.emit fragment.Write.bytes heap_local limit_local (Lift.embed (Success.emit fragment pc frame_local heap_local) T.Empty) T.Empty T.Empty
let[@def] (cost @ total) (fragment : Write.fragment @ immutable) (pc : W.limb) (frame_local : B.u32) (heap_local : B.u32) (limit_local : B.u32) (base : B.u32) (limit : B.u32) =
  Fuel.add (Select.cost fragment.Write.bytes heap_local limit_local)
    (Branch.cost (if base + fragment.Write.bytes <= limit then Success.emit fragment pc frame_local heap_local else C.Empty))
type result = {source : Machine.result; state : X.state; success : Success.result option}
let (correct @ total) : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable -> (stack_limit : D.index) @ immutable ->
    (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable -> (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (id : D.index) @ immutable -> (type_ : D.mono) @ immutable -> (typing : D.typing) @ immutable -> (next : D.index) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (old_pc : W.limb) -> (pc : W.limb) ->
    (capacity : Hmc_wasm_relayout.count) -> (max_code : W.limb) -> (fragment : Write.fragment) @ immutable ->
    (state : X.state) @ immutable -> (frame_base : B.u32) -> (frame_stop : B.u32) -> (heap_base : B.u32) -> (limit : B.u32) -> (frame_local : B.u32) -> (heap_local : B.u32) -> (limit_local : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | state.X.machine.E.stack === S.Empty
      && L.get state.X.machine.E.locals limit_local === Some (S.I32 limit)
      && Heap.valid table heap && Heap.object_valid table (Heap.view heap) (Heap.Closure (id, activation.Frame.env))
      && Program.lookup program.Program.code activation.Frame.pc === Some (Program.Keep (G.Load (G.Closure id, type_, typing, next)))
      && Heap.used heap = heap_base && Image.related state.X.memory heap && Above.above heap frame_stop
      && Write.matches signature.G.locals id capacity max_code fragment
      && Codec.decode signature activation.Frame.pc cells === Some (activation, padding)
      && next_signature.G.locals === signature.G.locals && next_signature.G.temporaries === signature.G.temporaries
      && Index.represents next pc && Index.represents (Heap.length cells) capacity
      && frame_stop = frame_base + 16 + 16 * capacity && frame_base <= 4294967248
      && frame_stop <= heap_base && frame_local <> heap_local && heap_local <> limit_local
      && heap_base <= limit && Hmc_linear_bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
      && L.get state.X.machine.E.locals heap_local === Some (S.I32 heap_base)
      && Bytes.drop state.X.memory frame_base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : result | Machine.step program globals limit stack_limit {Machine.heap; state = State.Running (activation, frames)} === out.source
      && L.get out.state.X.machine.E.locals limit_local === Some (S.I32 limit)
      && (match out.source with Machine.Advanced after -> Heap.valid table after.Machine.heap && Heap.used after.Machine.heap <= limit | _ -> true)
      && T.run (cost fragment pc frame_local heap_local limit_local heap_base limit)
        {T.code = emit fragment pc frame_local heap_local limit_local; labels = T.No_labels; state} === T.Finished out.state
      && (match out.success with
        | None -> out.source === Machine.Exhausted Machine.Heap && out.state === state && heap_base + fragment.Write.bytes > limit
        | Some success -> out.state === success.Success.state
          && X.run (Success.emit fragment pc frame_local heap_local) state === X.Done out.state
          && L.replaced state.X.machine.E.locals heap_local (S.I32 (Heap.used success.Success.allocation.A.heap)) out.state.X.machine.E.locals
          && A.correct table heap limit (Heap.Closure (id, activation.Frame.env)) (A.Allocated success.Success.allocation)
          && P.equal_prefix heap_base state.X.memory success.Success.allocated
          && P.equal_prefix frame_base state.X.memory out.state.X.memory
          && Bytes.drop success.Success.allocated frame_stop === Some success.Success.tail
          && Bytes.drop out.state.X.memory frame_stop === Some success.Success.tail
          && Hmc_linear_bounds.covers success.Success.allocated frame_stop && Hmc_linear_bounds.covers out.state.X.memory frame_stop
          && V.length out.state.X.memory === V.length state.X.memory
          && out.source === Machine.Advanced {Machine.heap = success.Success.allocation.A.heap; state = State.Running (success.Success.frame.Finish.activation, frames)}
          && Image.related out.state.X.memory success.Success.allocation.A.heap
          && success.Success.frame.Finish.activation === {activation with Frame.pc = next; accumulator = V.Closure_pointer heap_base}
          && Codec.decode next_signature next success.Success.frame.Finish.cells === Some (success.Success.frame.Finish.activation, padding)
          && Heap.length success.Success.frame.Finish.cells === Heap.length cells
          && Bytes.drop out.state.X.memory frame_base === Some success.Success.frame.Finish.bytes
          && Wire.decode_cells (D.S (Heap.length success.Success.frame.Finish.cells)) success.Success.frame.Finish.bytes ===
            Some (Heap.Cell (V.Word (Header.number pc), success.Success.frame.Finish.cells), success.Success.tail)
          && L.get out.state.X.machine.E.locals heap_local === Some (S.I32 (Heap.used success.Success.allocation.A.heap))
          && Heap.used success.Success.allocation.A.heap = heap_base + fragment.Write.bytes
          && L.get out.state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
          && out.state.X.machine.E.stack === S.Empty)} @ immutable =
  fun program globals stack_limit table heap signature next_signature activation frames id type_ typing next cells padding old_pc pc capacity max_code fragment state frame_base frame_stop heap_base limit frame_local heap_local limit_local before_frame tail premise ->
    let body = Success.emit fragment pc frame_local heap_local in
    let start = {T.code = emit fragment pc frame_local heap_local limit_local; labels = T.No_labels; state} in
    ghost_ (Select.correct fragment.Write.bytes heap_local limit_local heap_base limit state T.No_labels (Lift.embed body T.Empty) T.Empty T.Empty ();
      emit_def fragment pc frame_local heap_local limit_local;
      cost_def fragment pc frame_local heap_local limit_local heap_base limit;
      Branch.labels_def ());
    if heap_base + fragment.Write.bytes <= limit then (
      let success = Success.correct program globals stack_limit table heap signature next_signature activation frames id type_ typing next cells padding old_pc pc capacity max_code fragment state frame_base frame_stop heap_base limit frame_local heap_local before_frame tail () in
      ghost_ (L.other_local state.X.machine.E.locals heap_local (S.I32 (Heap.used success.Success.allocation.A.heap)) success.Success.state.X.machine.E.locals limit_local ();
        A.correct_def table heap limit (Heap.Closure (id, activation.Frame.env)) (A.Allocated success.Success.allocation);
        Branch.correct body state success.Success.state ();
        Fuel.correct (Select.cost fragment.Write.bytes heap_local limit_local) (Branch.cost body) start);
      {source = Machine.Advanced {Machine.heap = success.Success.allocation.A.heap; state = State.Running (success.Success.frame.Finish.activation, frames)};
        state = success.Success.state; success = Some success})
    else (
      ghost_ (Write.matches_def signature.G.locals id capacity max_code fragment);
      let view = Hmc_frame_slices.decode signature activation cells padding () in
      let count = Hmc_wasm_schema_counts.encode (Codec.locals_size signature.G.locals) capacity () in
      ghost_ (Allocate.exhausted heap id view.Hmc_frame_slices.env count heap_base limit ();
        Hmc_heap_closure_transition.exhausted program globals heap limit stack_limit activation frames id type_ typing next ();
        X.run_def C.Empty state; Lift.embed_def C.Empty T.Empty;
        Branch.correct C.Empty state state ();
        Fuel.correct (Select.cost fragment.Write.bytes heap_local limit_local) (Branch.cost C.Empty) start);
      {source = Machine.Exhausted Machine.Heap; state; success = None})
