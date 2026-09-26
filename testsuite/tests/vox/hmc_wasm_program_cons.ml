module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Codec = Hmc_pointer_frame_codec
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Simple = Hmc_heap_simple
module Model = Hmc_frame_value_pop
module Wire = Hmc_heap_wire
module Index = Hmc_u32_index
module Lower = Hmc_wasm_value_pop
module Relayout = Hmc_wasm_relayout
module Geometry = Hmc_wasm_relayout_geometry
module Header = Hmc_wasm_header_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module Bytes = Hmc_linear_bytes
module Invariant = Hmc_wasm_cons_finish_invariant
module Allocate = Hmc_wasm_cons_allocate
module A = Hmc_heap_allocate
module Machine = Hmc_heap_machine
module K = Hmc_closure_ir
module Image = Hmc_heap_image
module Above = Hmc_heap_image_suffix
module P = Hmc_linear_preservation
module L = Wasm_locals
module Transport = Hmc_wasm_frame_transport
module Guarded = Hmc_wasm_cons_guarded
module Success = Hmc_wasm_cons_success
module T = Wasm_control
module Exit = Hmc_wasm_allocation_exit
module Continue = Wasm_control_branch_continue
module Block = Hmc_wasm_structured_block
module Capture = Hmc_wasm_cons_capture
module Slots = Hmc_wasm_simple_lower
module Branch = Wasm_control_branch_target
module C = Wasm_code
module Fuel = Wasm_control_compose
module Lift = Wasm_control_lift
module Entry = Hmc_wasm_cons_entry
module Status = Hmc_wasm_program_status
module Emit = Hmc_wasm_program_emit
module Program_lower = Hmc_wasm_program_lower
module LP = Wasm_local_preservation
let[@def] (failure @ total) (unit : unit) : B.u32 = 2
type result = {source : Entry.result; prepared : X.state; state : X.state; fuel : C.count}
let (correct @ total) : (lowered : Program_lower.program) @ immutable -> (locals : Emit.locals) @ immutable -> (table_base : B.u32) -> (stack_base : B.u32) -> (outer : T.labels) @ immutable -> (env_count : Slots.slot) ->
    (program : Hmc_tail_ir.program) @ immutable -> (globals : Machine.globals) @ immutable -> (stack_limit : D.index) @ immutable -> (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next_activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (result_type : D.mono) @ immutable -> (head_type : D.mono) @ immutable -> (next : D.index) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable -> (fragment : Lower.fragment) @ immutable -> (capacity : Relayout.count) -> (max_pc : W.limb) ->
    (old_pc : W.limb) -> (left : V.value) @ immutable -> (right : V.value) @ immutable ->
    (heap_base : B.u32) -> (heap_local : B.u32) -> (limit_local : B.u32) -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable ->
    (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable -> (limit : B.u32) ->
    (slots : Capture.slots) @ immutable -> (frame_stop : B.u32) -> (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | base_local = locals.Emit.structured.Block.frame && heap_local = locals.Emit.structured.Block.heap
      && limit_local = locals.Emit.structured.Block.limit && slots === locals.Emit.structured.Block.scratch
      && locals.Emit.status <> base_local && locals.Emit.status <> heap_local && locals.Emit.status <> limit_local
      && Capture.separate slots locals.Emit.status
      && L.can_set state.X.machine.E.locals locals.Emit.status (S.I32 (failure ()))
      && Index.represents (Codec.locals_size signature.G.locals) env_count
      && base + 48 + 16 * env_count <= 4294967280
      && Capture.distinct slots && Capture.separate slots base_local && Capture.separate slots heap_local && Capture.separate slots limit_local
      && Capture.writable slots state.X.machine.E.locals
      && Hmc_tail_ir.lookup program.Hmc_tail_ir.code activation.Frame.pc === Some (Hmc_tail_ir.Keep (G.Cons next))
      && state.X.machine.E.stack === S.Empty
      && L.get state.X.machine.E.locals limit_local === Some (S.I32 limit)
      && Heap.valid table heap && Heap.object_valid table (Heap.view heap) (Heap.Cons (left, right))
      && Heap.used heap = heap_base && heap_base <= limit && Hmc_linear_bounds.covers state.X.memory limit
      && frame_stop <= heap_base && base_local <> heap_local
      && frame_stop = base + 16 + 16 * capacity
      && Hmc_heap_image.related state.X.memory heap && Hmc_heap_image_suffix.above heap frame_stop
      && Lower.matches signature next capacity max_pc fragment
      && signature.G.temporaries === G.Value (context, head_type, schema)
      && activation.Frame.accumulator === right
      && (match activation.Frame.temporaries with Frame.Value (w, _, _) -> w === left | _ -> false)
      && Index.represents activation.Frame.pc old_pc && Index.represents (Heap.length cells) capacity
      && Model.successor signature result_type === Some next_signature
      && Model.transition activation next (V.Cons_pointer heap_base) === Some next_activation
      && Codec.decode signature activation.Frame.pc cells === Some (activation, old_padding)
      && base + 16 + 16 * capacity <= 4294967296
      && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Wasm_locals.get state.X.machine.E.locals heap_local === Some (S.I32 heap_base)
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : result | out.source.Entry.captured.X.machine.E.stack === S.Empty
      && L.get out.source.Entry.captured.X.machine.E.locals heap_local === Some (S.I32 heap_base)
      && L.get out.source.Entry.captured.X.machine.E.locals limit_local === Some (S.I32 limit)
      && T.run (Status.four ())
        {T.code = Emit.emit lowered (Hmc_wasm_program_block.Structured (Block.Cons fragment)) locals table_base stack_base;
          labels = outer; state}
        === T.Running {T.code = Block.emit (Block.Cons fragment) locals.Emit.structured (Status.exit_depth ());
          labels = Status.scope locals.Emit.status outer; state = out.prepared}
      && L.replaced state.X.machine.E.locals locals.Emit.status (S.I32 (failure ())) out.prepared.X.machine.E.locals
      && X.run (Capture.emit fragment.Lower.head_tag fragment.Lower.head_payload slots base_local) out.prepared === X.Done out.source.Entry.captured
      && Machine.step program globals limit stack_limit {Machine.heap; state = State.Running (activation, frames)} === out.source.Entry.result.Guarded.source
      && T.run out.fuel {T.code = Emit.emit lowered (Hmc_wasm_program_block.Structured (Block.Cons fragment)) locals table_base stack_base; labels = outer; state}
        === T.Running {T.code = T.Empty; labels = outer; state = out.state}
      && out.state.X.memory === out.source.Entry.result.Guarded.state.X.memory
      && out.state.X.machine.E.stack === S.Empty
      && (match out.source.Entry.result.Guarded.allocation with
        | A.Exhausted -> heap_base + 32 > limit
          && out.source.Entry.result.Guarded.source === Machine.Exhausted Machine.Heap
          && out.state.X.machine.E.locals === out.source.Entry.captured.X.machine.E.locals
          && L.get out.state.X.machine.E.locals heap_local === Some (S.I32 heap_base)
          && out.state.X.memory === state.X.memory
          && L.get out.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (failure ()))
        | A.Allocated allocation -> L.replaced out.source.Entry.result.Guarded.state.X.machine.E.locals locals.Emit.status (S.I32 (Status.zero ())) out.state.X.machine.E.locals
          && X.run (Success.emit fragment base_local heap_local slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload) out.source.Entry.captured === X.Done out.source.Entry.result.Guarded.state
          && A.correct table heap limit (Heap.Cons (left, right)) out.source.Entry.result.Guarded.allocation
          && P.equal_prefix heap_base state.X.memory out.source.Entry.result.Guarded.allocated
          && P.equal_prefix base state.X.memory out.state.X.memory
          && Bytes.drop out.source.Entry.result.Guarded.allocated frame_stop === Some out.source.Entry.result.Guarded.tail
          && Bytes.drop out.state.X.memory frame_stop === Some out.source.Entry.result.Guarded.tail
          && Hmc_linear_bounds.covers out.source.Entry.result.Guarded.allocated frame_stop && Hmc_linear_bounds.covers out.state.X.memory frame_stop
          && V.length out.state.X.memory === V.length state.X.memory
          && out.source.Entry.result.Guarded.source === Machine.Advanced {Machine.heap = allocation.A.heap; state = State.Running (next_activation, frames)}
          && L.get out.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (Status.zero ()))
          && L.get out.state.X.machine.E.locals heap_local === Some (S.I32 (Heap.used allocation.A.heap))
          && Image.related out.state.X.memory allocation.A.heap
          && (match out.source.Entry.result.Guarded.frame with
            | None -> false
            | Some frame -> Codec.decode next_signature next_activation.Frame.pc frame.Invariant.cells === Some (next_activation, frame.Invariant.padding)
              && Heap.length frame.Invariant.cells === Heap.length cells
              && Bytes.drop out.state.X.memory base === Some frame.Invariant.bytes
              && Wire.decode_cells (D.S (Heap.length frame.Invariant.cells)) frame.Invariant.bytes ===
                Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), frame.Invariant.cells), out.source.Entry.result.Guarded.tail)))} @ immutable =
  fun lowered locals table_base stack_base outer env_count program globals stack_limit signature next_signature activation next_activation frames result_type head_type next context schema fragment capacity max_pc old_pc left right heap_base heap_local limit_local cells old_padding table heap limit slots frame_stop state base_local base before_frame tail premise ->
    let local = locals.Emit.status in
    ghost_ (Hmc_wasm_cons_locals.capture fragment.Lower.head_tag fragment.Lower.head_payload slots base_local local ();
      Hmc_wasm_cons_locals.success fragment base_local heap_local slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload local ());
    let body = Block.emit (Block.Cons fragment) locals.Emit.structured 2 in
    let prepared = Status.prepare local (failure ()) body outer state () in
    ghost_ (Capture.separate_def slots local;
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals base_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals heap_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals limit_local ();
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals slots.Capture.head_tag ();
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals slots.Capture.head_payload ();
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals slots.Capture.tail_tag ();
      L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals slots.Capture.tail_payload ();
      Capture.writable_def slots state.X.machine.E.locals; Capture.writable_def slots prepared.X.machine.E.locals;
      Capture.word_slot_def state.X.machine.E.locals slots.Capture.head_tag; Capture.word_slot_def prepared.X.machine.E.locals slots.Capture.head_tag;
      Capture.word_slot_def state.X.machine.E.locals slots.Capture.head_payload; Capture.word_slot_def prepared.X.machine.E.locals slots.Capture.head_payload;
      Capture.word_slot_def state.X.machine.E.locals slots.Capture.tail_tag; Capture.word_slot_def prepared.X.machine.E.locals slots.Capture.tail_tag;
      Capture.word_slot_def state.X.machine.E.locals slots.Capture.tail_payload; Capture.word_slot_def prepared.X.machine.E.locals slots.Capture.tail_payload;
      Status.scope_def local outer; Status.zero_def ();
      Continue.labels_def T.Empty outer;
      Continue.labels_def (Emit.status local (Status.zero ()) T.Empty) (Continue.labels T.Empty outer);
      Continue.labels_def T.Empty (Status.scope local outer);
      Branch.valid_def 2 (Continue.labels T.Empty (Status.scope local outer));
      Branch.valid_def 1 (Status.scope local outer); Branch.valid_def 0 (Continue.labels T.Empty outer));
    let source = Entry.correct (Status.scope local outer) 2 env_count program globals stack_limit signature next_signature activation next_activation frames result_type head_type next context schema fragment capacity max_pc old_pc left right heap_base heap_local limit_local cells old_padding table heap limit slots frame_stop prepared base_local base before_frame tail () in
    let body_fuel = Entry.cost fragment base_local heap_local limit_local slots heap_base limit in
    ghost_ (Entry.locals_def base_local heap_local limit_local slots;
      Block.emit_def (Block.Cons fragment) (Entry.locals base_local heap_local limit_local slots) 2;
      Block.emit_def (Block.Cons fragment) locals.Emit.structured 2;
      LP.correct (Capture.emit fragment.Lower.head_tag fragment.Lower.head_payload slots base_local) prepared source.Entry.captured local ();
      failure_def (); Status.exit_depth_def (); Emit.emit_def lowered (Hmc_wasm_program_block.Structured (Block.Cons fragment)) locals table_base stack_base);
    match source.Entry.result.Guarded.allocation with
    | A.Allocated _ ->
      ghost_ (LP.correct (Success.emit fragment base_local heap_local slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload)
        source.Entry.captured source.Entry.result.Guarded.state local ();
        L.can_set_def source.Entry.result.Guarded.state.X.machine.E.locals local (S.I32 (Status.zero ()));
        S.same_type_def (S.I32 (failure ())) (S.I32 (Status.zero ())));
      let after = Status.normal local (failure ()) body outer state prepared source.Entry.result.Guarded.state body_fuel () in
      ghost_ (L.other_local source.Entry.result.Guarded.state.X.machine.E.locals local (S.I32 (Status.zero ())) after.X.machine.E.locals heap_local ());
      {source; prepared; state = after; fuel = Status.cost body_fuel}
    | A.Exhausted ->
      let after = T.stack source.Entry.captured S.Empty in
      let fuel = Fuel.add (Status.four ()) body_fuel in
      ghost_ (T.branch_def 2 (Continue.labels T.Empty (Status.scope local outer)) source.Entry.captured;
        T.branch_def 1 (Status.scope local outer) source.Entry.captured;
        T.branch_def 0 (Continue.labels T.Empty outer) source.Entry.captured;
        T.stack_def source.Entry.captured S.Empty;
        Fuel.correct (Status.four ()) body_fuel {T.code = Emit.protected local (failure ()) body; labels = outer; state});
      {source; prepared; state = after; fuel}
