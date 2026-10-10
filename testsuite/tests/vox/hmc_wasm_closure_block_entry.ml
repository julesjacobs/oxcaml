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
module Guarded = Hmc_wasm_closure_guarded
module Exit = Hmc_wasm_allocation_exit
module Continue = Wasm_control_branch_continue
module Block = Hmc_wasm_structured_block
module Lower = Hmc_wasm_closure_lower
module Entry = Hmc_wasm_closure_continue
let[@def] (locals @ total) (frame : B.u32) (heap : B.u32) (limit : B.u32) : Block.locals @ immutable =
  {Block.frame; heap; limit; object_ = frame; scratch = {Hmc_wasm_cons_capture.head_tag = frame; head_payload = frame; tail_tag = frame; tail_payload = frame}}
let (correct @ total) : (outer : T.labels) @ immutable -> (depth : B.u32) -> (exhausted : T.configuration) @ immutable -> (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable -> (stack_limit : D.index) @ immutable ->
    (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable -> (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (id : D.index) @ immutable -> (type_ : D.mono) @ immutable -> (typing : D.typing) @ immutable -> (next : D.index) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (old_pc : W.limb) -> (pc : W.limb) ->
    (capacity : Hmc_wasm_relayout.count) -> (max_code : W.limb) -> (fragment : Write.fragment) @ immutable ->
    (state : X.state) @ immutable -> (frame_base : B.u32) -> (frame_stop : B.u32) -> (heap_base : B.u32) -> (limit : B.u32) -> (frame_local : B.u32) -> (heap_local : B.u32) -> (limit_local : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | T.branch depth (Continue.labels T.Empty outer) state === T.Running exhausted
      && state.X.machine.E.stack === S.Empty
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
    {out : Guarded.result | Machine.step program globals limit stack_limit {Machine.heap; state = State.Running (activation, frames)} === out.Guarded.source
      && L.get out.Guarded.state.X.machine.E.locals limit_local === Some (S.I32 limit)
      && (match out.Guarded.source with Machine.Advanced after -> Heap.valid table after.Machine.heap && Heap.used after.Machine.heap <= limit | _ -> true)
      && T.run (Exit.cost fragment.Write.bytes heap_local limit_local (Success.emit fragment pc frame_local heap_local) heap_base limit)
        {T.code = Block.emit (Block.Closure {Lower.object_ = fragment; pc}) (locals frame_local heap_local limit_local) depth; labels = outer; state}
        === T.Running (match out.Guarded.success with None -> exhausted | Some _ -> {T.code = T.Empty; labels = outer; state = out.Guarded.state})
      && (match out.Guarded.success with
        | None -> out.Guarded.source === Machine.Exhausted Machine.Heap && out.Guarded.state === state && heap_base + fragment.Write.bytes > limit
        | Some success -> out.Guarded.state === success.Success.state
          && X.run (Success.emit fragment pc frame_local heap_local) state === X.Done out.Guarded.state
          && out.Guarded.source === Machine.Advanced {Machine.heap = success.Success.allocation.A.heap; state = State.Running (success.Success.frame.Finish.activation, frames)}
          && Image.related out.Guarded.state.X.memory success.Success.allocation.A.heap
          && success.Success.frame.Finish.activation === {activation with Frame.pc = next; accumulator = V.Closure_pointer heap_base}
          && Codec.decode next_signature next success.Success.frame.Finish.cells === Some (success.Success.frame.Finish.activation, padding)
          && Heap.length success.Success.frame.Finish.cells === Heap.length cells
          && Bytes.drop out.Guarded.state.X.memory frame_base === Some success.Success.frame.Finish.bytes
          && Wire.decode_cells (D.S (Heap.length success.Success.frame.Finish.cells)) success.Success.frame.Finish.bytes ===
            Some (Heap.Cell (V.Word (Header.number pc), success.Success.frame.Finish.cells), success.Success.tail)
          && L.get out.Guarded.state.X.machine.E.locals heap_local === Some (S.I32 (Heap.used success.Success.allocation.A.heap))
          && Heap.used success.Success.allocation.A.heap = heap_base + fragment.Write.bytes
          && L.get out.Guarded.state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
          && out.Guarded.state.X.machine.E.stack === S.Empty)} @ immutable =
  fun outer depth exhausted program globals stack_limit table heap signature next_signature activation frames id type_ typing next cells padding old_pc pc capacity max_code fragment state frame_base frame_stop heap_base limit frame_local heap_local limit_local before_frame tail premise ->
    let out = Entry.correct T.Empty outer depth exhausted program globals stack_limit table heap signature next_signature activation frames id type_ typing next cells padding old_pc pc capacity max_code fragment state frame_base frame_stop heap_base limit frame_local heap_local limit_local before_frame tail () in
    ghost_ (locals_def frame_local heap_local limit_local;
      Block.emit_def (Block.Closure {Lower.object_ = fragment; pc}) (locals frame_local heap_local limit_local) depth);
    out
