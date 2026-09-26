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
module Entry = Hmc_wasm_closure_continue
module Status = Hmc_wasm_program_status
module Emit = Hmc_wasm_program_emit
module Lower = Hmc_wasm_program_lower
module Block = Hmc_wasm_structured_block
module LP = Wasm_local_preservation
let[@def] (failure @ total) (unit : unit) : B.u32 = 2
type result = {prepared : X.state; source : Guarded.result; state : X.state; fuel : C.count}
let (correct @ total) : (lowered : Lower.program) @ immutable -> (locals : Emit.locals) @ immutable -> (table_base : B.u32) -> (stack_base : B.u32) -> (outer : T.labels) @ immutable -> (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable -> (stack_limit : D.index) @ immutable ->
    (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable -> (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (id : D.index) @ immutable -> (type_ : D.mono) @ immutable -> (typing : D.typing) @ immutable -> (next : D.index) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (old_pc : W.limb) -> (pc : W.limb) ->
    (capacity : Hmc_wasm_relayout.count) -> (max_code : W.limb) -> (fragment : Write.fragment) @ immutable ->
    (state : X.state) @ immutable -> (frame_base : B.u32) -> (frame_stop : B.u32) -> (heap_base : B.u32) -> (limit : B.u32) -> (frame_local : B.u32) -> (heap_local : B.u32) -> (limit_local : B.u32) ->
    (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | frame_local = locals.Emit.structured.Block.frame && heap_local = locals.Emit.structured.Block.heap && limit_local = locals.Emit.structured.Block.limit
      && locals.Emit.status <> frame_local && locals.Emit.status <> heap_local && locals.Emit.status <> limit_local
      && L.can_set state.X.machine.E.locals locals.Emit.status (S.I32 (failure ()))
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
    {out : result | L.replaced state.X.machine.E.locals locals.Emit.status (S.I32 (failure ())) out.prepared.X.machine.E.locals
      && Machine.step program globals limit stack_limit {Machine.heap; state = State.Running (activation, frames)} === out.source.Guarded.source
      && L.get out.source.Guarded.state.X.machine.E.locals limit_local === Some (S.I32 limit)
      && (match out.source.Guarded.source with Machine.Advanced after -> Heap.valid table after.Machine.heap && Heap.used after.Machine.heap <= limit | _ -> true)
      && T.run out.fuel {T.code = Emit.emit lowered (Hmc_wasm_program_block.Structured (Block.Closure {Hmc_wasm_closure_lower.object_ = fragment; pc})) locals table_base stack_base; labels = outer; state}
        === T.Running {T.code = T.Empty; labels = outer; state = out.state}
      && out.state.X.memory === out.source.Guarded.state.X.memory && out.state.X.machine.E.stack === S.Empty
      && (match out.source.Guarded.success with
        | None -> out.state.X.machine.E.locals === out.prepared.X.machine.E.locals
          && L.get out.state.X.machine.E.locals heap_local === Some (S.I32 heap_base)
          && out.source.Guarded.source === Machine.Exhausted Machine.Heap && out.state.X.memory === state.X.memory && L.get out.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (failure ())) && heap_base + fragment.Write.bytes > limit
        | Some success ->
          L.replaced out.prepared.X.machine.E.locals heap_local (S.I32 (Heap.used success.Success.allocation.A.heap)) out.source.Guarded.state.X.machine.E.locals
          && L.replaced out.source.Guarded.state.X.machine.E.locals locals.Emit.status (S.I32 (Status.zero ())) out.state.X.machine.E.locals
          && A.correct table heap limit (Heap.Closure (id, activation.Frame.env)) (A.Allocated success.Success.allocation)
          && P.equal_prefix heap_base state.X.memory success.Success.allocated
          && P.equal_prefix frame_base state.X.memory out.state.X.memory
          && Bytes.drop success.Success.allocated frame_stop === Some success.Success.tail
          && Bytes.drop out.state.X.memory frame_stop === Some success.Success.tail
          && Hmc_linear_bounds.covers success.Success.allocated frame_stop && Hmc_linear_bounds.covers out.state.X.memory frame_stop
          && V.length out.state.X.memory === V.length state.X.memory
          && L.get out.state.X.machine.E.locals heap_local === Some (S.I32 (Heap.used success.Success.allocation.A.heap))
          && L.get out.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (Status.zero ()))
          && out.source.Guarded.state === success.Success.state
          && out.source.Guarded.source === Machine.Advanced {Machine.heap = success.Success.allocation.A.heap; state = State.Running (success.Success.frame.Finish.activation, frames)}
          && Image.related out.source.Guarded.state.X.memory success.Success.allocation.A.heap
          && success.Success.frame.Finish.activation === {activation with Frame.pc = next; accumulator = V.Closure_pointer heap_base}
          && Codec.decode next_signature next success.Success.frame.Finish.cells === Some (success.Success.frame.Finish.activation, padding)
          && Heap.length success.Success.frame.Finish.cells === Heap.length cells
          && Bytes.drop out.source.Guarded.state.X.memory frame_base === Some success.Success.frame.Finish.bytes
          && Wire.decode_cells (D.S (Heap.length success.Success.frame.Finish.cells)) success.Success.frame.Finish.bytes ===
            Some (Heap.Cell (V.Word (Header.number pc), success.Success.frame.Finish.cells), success.Success.tail)
          && L.get out.source.Guarded.state.X.machine.E.locals heap_local === Some (S.I32 (Heap.used success.Success.allocation.A.heap))
          && Heap.used success.Success.allocation.A.heap = heap_base + fragment.Write.bytes
          && L.get out.source.Guarded.state.X.machine.E.locals frame_local === Some (S.I32 frame_base)
          && out.source.Guarded.state.X.machine.E.stack === S.Empty)} @ immutable =
  fun lowered locals table_base stack_base outer program globals stack_limit table heap signature next_signature activation frames id type_ typing next cells padding old_pc pc capacity max_code fragment state frame_base frame_stop heap_base limit frame_local heap_local limit_local before_frame tail premise ->
    let local = locals.Emit.status in
    let lowered_fragment = Hmc_wasm_program_block.Structured (Block.Closure {Hmc_wasm_closure_lower.object_ = fragment; pc}) in
    let body = Block.emit (Block.Closure {Hmc_wasm_closure_lower.object_ = fragment; pc}) locals.Emit.structured 2 in
    let prepared = Status.prepare local (failure ()) body outer state () in
    let exhausted = {T.code = T.Empty; labels = outer; state = T.stack prepared S.Empty} in
    ghost_ (L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals frame_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals heap_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals limit_local ();
      Status.scope_def local outer; Status.zero_def ();
      Continue.labels_def T.Empty outer;
      Continue.labels_def (Emit.status local (Status.zero ()) T.Empty) (Continue.labels T.Empty outer);
      Continue.labels_def T.Empty (Status.scope local outer);
      T.branch_def 2 (Continue.labels T.Empty (Status.scope local outer)) prepared;
      T.branch_def 1 (Status.scope local outer) prepared;
      T.branch_def 0 (Continue.labels T.Empty outer) prepared;
      T.stack_def prepared S.Empty);
    let source = Entry.correct T.Empty (Status.scope local outer) 2 exhausted program globals stack_limit table heap signature next_signature activation frames id type_ typing next cells padding old_pc pc capacity max_code fragment prepared frame_base frame_stop heap_base limit frame_local heap_local limit_local before_frame tail () in
    let body_fuel = Exit.cost fragment.Write.bytes heap_local limit_local (Success.emit fragment pc frame_local heap_local) heap_base limit in
    ghost_ (Block.emit_def (Block.Closure {Hmc_wasm_closure_lower.object_ = fragment; pc}) locals.Emit.structured 2;
      failure_def (); Emit.emit_def lowered lowered_fragment locals table_base stack_base);
    match source.Guarded.success with
    | Some success ->
      ghost_ (L.other_local prepared.X.machine.E.locals heap_local (S.I32 (Heap.used success.Success.allocation.A.heap)) source.Guarded.state.X.machine.E.locals local ();
        L.can_set_def source.Guarded.state.X.machine.E.locals local (S.I32 (Status.zero ()));
        S.same_type_def (S.I32 (failure ())) (S.I32 (Status.zero ())));
      let after = Status.normal local (failure ()) body outer state prepared source.Guarded.state body_fuel () in
      ghost_ (L.other_local source.Guarded.state.X.machine.E.locals local (S.I32 (Status.zero ())) after.X.machine.E.locals heap_local ());
      {prepared; source; state = after; fuel = Status.cost body_fuel}
    | None ->
      let fuel = Fuel.add (Status.four ()) body_fuel in
      ghost_ (Fuel.correct (Status.four ()) body_fuel {T.code = Emit.protected local (failure ()) body; labels = outer; state});
      {prepared; source; state = exhausted.T.state; fuel}
