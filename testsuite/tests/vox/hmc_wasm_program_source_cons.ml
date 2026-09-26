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
module New = Hmc_wasm_program_cons
module Registers = Hmc_wasm_program_registers
module Runtime = Hmc_wasm_program_runtime
module Assembly = Hmc_wasm_program_functions
module Round = Hmc_wasm_program_roundtrip
module Dispatch = Hmc_wasm_program_dispatch
module Step = Hmc_wasm_program_register_step
module Func = Wasm_functions
module Calls = Wasm_calls
module GE = Wasm_global_execution
module Failed = Hmc_failed_guard_model
module Guard = Hmc_failed_guard_calls
module Guard_blocks = Hmc_failed_guard_blocks
let[@def] (fragment @ total) (plan : Lower.fragment @ immutable) = Hmc_wasm_program_block.Structured (Block.Cons plan)
let[@def] (slots @ total) (unit : unit) : Capture.slots @ immutable =
  {Capture.head_tag = 14; head_payload = 15; tail_tag = 16; tail_payload = 17}
type result = {body : New.result; registers : Registers.registers; fuel : {n : C.count | not (n === C.Zero)}; failed_guard : Guard.optional @@ ghost}
let (correct @ total) : (lowered : Program_lower.program) @ immutable -> (module_ : Func.module_) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (env_count : Slots.slot) ->
    (program : Hmc_tail_ir.program) @ immutable -> (globals : Machine.globals) @ immutable -> (stack_limit : D.index) @ immutable ->
    (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next_activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (result_type : D.mono) @ immutable -> (head_type : D.mono) @ immutable -> (next : D.index) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable -> (plan : Lower.fragment) @ immutable ->
    (capacity : Relayout.count) -> (max_pc : W.limb) -> (old_pc : W.limb) -> (left : V.value) @ immutable -> (right : V.value) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (heap : Heap.heap) @ immutable ->
    (frame_stop : B.u32) -> (before : Registers.registers) @ immutable -> (memory : B.bytes) @ immutable ->
    (bytes : B.bytes) @ immutable -> (tail : B.bytes) @ immutable -> (host_capacity : C.count) @ immutable -> (function_index : B.u32) ->
    {u : unit | Index.represents (Codec.locals_size signature.G.locals) env_count
      && before.Registers.frame + 48 + 16 * env_count <= 4294967280
      && before.Registers.frame <= 4294967280
      && Hmc_tail_ir.lookup program.Hmc_tail_ir.code activation.Frame.pc === Some (Hmc_tail_ir.Keep (G.Cons next))
      && Heap.valid program.Hmc_tail_ir.origin.Hmc_cfg_program.origin.Hmc_closure_program.table heap
      && Heap.object_valid program.Hmc_tail_ir.origin.Hmc_cfg_program.origin.Hmc_closure_program.table (Heap.view heap) (Heap.Cons (left, right))
      && Heap.used heap = before.Registers.heap && before.Registers.heap <= before.Registers.heap_limit
      && Hmc_linear_bounds.covers memory before.Registers.heap_limit
      && frame_stop <= before.Registers.heap && frame_stop = before.Registers.frame + 16 + 16 * capacity
      && Image.related memory heap && Above.above heap frame_stop
      && Lower.matches signature next capacity max_pc plan
      && signature.G.temporaries === G.Value (context, head_type, schema)
      && activation.Frame.accumulator === right
      && (match activation.Frame.temporaries with Frame.Value (w, _, _) -> w === left | _ -> false)
      && Index.represents activation.Frame.pc old_pc && Index.represents (Heap.length cells) capacity
      && Model.successor signature result_type === Some next_signature
      && Model.transition activation next (V.Cons_pointer before.Registers.heap) === Some next_activation
      && Codec.decode signature activation.Frame.pc cells === Some (activation, padding)
      && Bytes.drop memory before.Registers.frame === Some bytes
      && Wire.decode_cells (D.S (Heap.length cells)) bytes === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)
      && Func.signature module_.Func.signatures (Dispatch.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table old_pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered (fragment plan) (Runtime.config table_base stack_base))} ->
    {out : result | out.registers.Registers.top = before.Registers.top
      && out.registers.Registers.tag === before.Registers.tag && out.registers.Registers.payload === before.Registers.payload
      && out.registers.Registers.frame = before.Registers.frame
      && out.registers.Registers.heap_limit = before.Registers.heap_limit && out.registers.Registers.stack_limit = before.Registers.stack_limit
      && Machine.step program globals before.Registers.heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)} === out.body.New.source.Entry.result.Guarded.source
      && (match out.body.New.source.Entry.result.Guarded.allocation with
        | A.Exhausted -> out.body.New.source.Entry.result.Guarded.source === Machine.Exhausted Machine.Heap
          && out.registers.Registers.status = 2 && out.registers.Registers.heap = before.Registers.heap
          && out.body.New.state.X.memory === memory
        | A.Allocated allocation -> out.body.New.source.Entry.result.Guarded.source === Machine.Advanced {Machine.heap = allocation.A.heap; state = State.Running (next_activation, frames)}
          && A.correct program.Hmc_tail_ir.origin.Hmc_cfg_program.origin.Hmc_closure_program.table heap before.Registers.heap_limit (Heap.Cons (left, right)) out.body.New.source.Entry.result.Guarded.allocation
          && P.equal_prefix before.Registers.heap memory out.body.New.source.Entry.result.Guarded.allocated
          && P.equal_prefix before.Registers.frame memory out.body.New.state.X.memory
          && Bytes.drop out.body.New.source.Entry.result.Guarded.allocated frame_stop === Some out.body.New.source.Entry.result.Guarded.tail
          && Bytes.drop out.body.New.state.X.memory frame_stop === Some out.body.New.source.Entry.result.Guarded.tail
          && Hmc_linear_bounds.covers out.body.New.source.Entry.result.Guarded.allocated frame_stop
          && Hmc_linear_bounds.covers out.body.New.state.X.memory frame_stop
          && V.length out.body.New.state.X.memory === V.length memory
          && out.registers.Registers.status = 0 && out.registers.Registers.heap = Heap.used allocation.A.heap
          && Image.related out.body.New.state.X.memory allocation.A.heap
          && (match out.body.New.source.Entry.result.Guarded.frame with
            | None -> false
            | Some frame -> Codec.decode next_signature next_activation.Frame.pc frame.Invariant.cells === Some (next_activation, frame.Invariant.padding)
              && Heap.length frame.Invariant.cells === Heap.length cells
              && Bytes.drop out.body.New.state.X.memory before.Registers.frame === Some frame.Invariant.bytes
              && Wire.decode_cells (D.S (Heap.length frame.Invariant.cells)) frame.Invariant.bytes ===
                Some (Heap.Cell (V.Word (Header.number plan.Lower.pc), frame.Invariant.cells), out.body.New.source.Entry.result.Guarded.tail)))
      && (if out.registers.Registers.status = 2 then match out.failed_guard with
        | Guard.Absent -> false
        | Guard.Present guard -> Guard.reaches Failed.Heap module_
          (Dispatch.loop (Registers.globals before) memory (C.Succ host_capacity)) guard
        else true)
      && Calls.run out.fuel module_ (Dispatch.loop (Registers.globals before) memory (C.Succ host_capacity)) ===
        (if out.registers.Registers.status = 0 then Calls.Running (Dispatch.loop (Registers.globals out.registers) out.body.New.state.X.memory (C.Succ host_capacity))
         else Calls.Finished {GE.globals = Registers.globals out.registers; execution = {X.memory = out.body.New.state.X.memory;
           machine = {E.locals = S.Empty; stack = S.Push (S.I32 out.registers.Registers.status, S.Empty)}}})} @ immutable =
  fun lowered module_ table_base stack_base env_count program globals stack_limit signature next_signature activation next_activation frames result_type head_type next
      context schema plan capacity max_pc old_pc left right cells padding heap frame_stop before memory bytes tail host_capacity function_index premise ->
    let config = Runtime.config table_base stack_base in
    let state = {X.memory; machine = {E.locals = Registers.locals before; stack = S.Empty}} in
    ghost_ (Runtime.config_def table_base stack_base; Registers.local_values before; Registers.matches_def (Registers.locals before) before;
      New.failure_def (); L.can_set_def (Registers.locals before) 11 (S.I32 2); S.same_type_def (S.I32 before.Registers.status) (S.I32 2);
      slots_def (); Capture.distinct_def (slots ());
      Capture.separate_def (slots ()) 0; Capture.separate_def (slots ()) 1; Capture.separate_def (slots ()) 2; Capture.separate_def (slots ()) 11;
      Capture.writable_def (slots ()) (Registers.locals before);
      Capture.word_slot_def (Registers.locals before) 14; Capture.word_slot_def (Registers.locals before) 15;
      Capture.word_slot_def (Registers.locals before) 16; Capture.word_slot_def (Registers.locals before) 17);
    ghost_ (
      Capture.separate_def (slots ()) 11;
      Hmc_wasm_cons_locals.capture plan.Lower.head_tag plan.Lower.head_payload (slots ()) 0 11 ();
      Hmc_wasm_cons_locals.success plan 0 1 14 15 16 17 11 ();
      Capture.separate_def (slots ()) 4;
      Hmc_wasm_cons_locals.capture plan.Lower.head_tag plan.Lower.head_payload (slots ()) 0 4 ();
      Hmc_wasm_cons_locals.success plan 0 1 14 15 16 17 4 ();
      Capture.separate_def (slots ()) 12;
      Hmc_wasm_cons_locals.capture plan.Lower.head_tag plan.Lower.head_payload (slots ()) 0 12 ();
      Hmc_wasm_cons_locals.success plan 0 1 14 15 16 17 12 ();
      Capture.separate_def (slots ()) 13;
      Hmc_wasm_cons_locals.capture plan.Lower.head_tag plan.Lower.head_payload (slots ()) 0 13 ();
      Hmc_wasm_cons_locals.success plan 0 1 14 15 16 17 13 ());
    let body = New.correct lowered config.Assembly.locals table_base stack_base (Round.labels config) env_count program globals stack_limit signature next_signature
      activation next_activation frames result_type head_type next context schema plan capacity max_pc old_pc left right before.Registers.heap 1 2 cells padding
      program.Hmc_tail_ir.origin.Hmc_cfg_program.origin.Hmc_closure_program.table heap before.Registers.heap_limit (slots ()) frame_stop state 0 before.Registers.frame bytes tail () in
    ghost_ (fragment_def plan;
      L.other_local (Registers.locals before) 11 (S.I32 (New.failure ())) body.New.prepared.X.machine.E.locals 4 ();
      LP.correct (Capture.emit plan.Lower.head_tag plan.Lower.head_payload (slots ()) 0) body.New.prepared body.New.source.Entry.captured 4 ();
      L.other_local (Registers.locals before) 11 (S.I32 (New.failure ())) body.New.prepared.X.machine.E.locals 12 ();
      LP.correct (Capture.emit plan.Lower.head_tag plan.Lower.head_payload (slots ()) 0) body.New.prepared body.New.source.Entry.captured 12 ();
      L.other_local (Registers.locals before) 11 (S.I32 (New.failure ())) body.New.prepared.X.machine.E.locals 13 ();
      LP.correct (Capture.emit plan.Lower.head_tag plan.Lower.head_payload (slots ()) 0) body.New.prepared body.New.source.Entry.captured 13 ();
      match body.New.source.Entry.result.Guarded.allocation with
      | A.Exhausted -> ()
      | A.Allocated _ ->
        LP.correct (Success.emit plan 0 1 14 15 16 17) body.New.source.Entry.captured body.New.source.Entry.result.Guarded.state 4 ();
        L.other_local body.New.source.Entry.result.Guarded.state.X.machine.E.locals 11 (S.I32 (Status.zero ())) body.New.state.X.machine.E.locals 4 ();
        LP.correct (Success.emit plan 0 1 14 15 16 17) body.New.source.Entry.captured body.New.source.Entry.result.Guarded.state 12 ();
        L.other_local body.New.source.Entry.result.Guarded.state.X.machine.E.locals 11 (S.I32 (Status.zero ())) body.New.state.X.machine.E.locals 12 ();
        LP.correct (Success.emit plan 0 1 14 15 16 17) body.New.source.Entry.captured body.New.source.Entry.result.Guarded.state 13 ();
        L.other_local body.New.source.Entry.result.Guarded.state.X.machine.E.locals 11 (S.I32 (Status.zero ())) body.New.state.X.machine.E.locals 13 ());
    let registers = Step.complete lowered (fragment plan) module_ table_base stack_base before memory bytes cells tail host_capacity old_pc function_index body.New.fuel body.New.state () in
    ghost_ (Registers.exports_def body.New.state.X.machine.E.locals registers; Status.zero_def (); New.failure_def ());
    let failed_guard = ghost_ (match body.New.source.Entry.result.Guarded.allocation with
      | A.Allocated _ -> Guard.Absent
      | A.Exhausted ->
        Failed.frame_local_def (); Failed.cursor_local_def Failed.Heap; Failed.limit_local_def Failed.Heap;
        Failed.status_local_def (); Failed.status_def Failed.Heap;
        let local = Guard_blocks.cons lowered plan table_base stack_base before memory
          body.New.prepared body.New.source.Entry.captured () in
        Guard.Present (Guard.from_body Failed.Heap lowered (fragment plan) module_ table_base stack_base
          before memory bytes cells tail host_capacity old_pc function_index local ())) in
    {failed_guard; body; registers; fuel = Hmc_wasm_program_cost.dispatch (Round.cost config body.New.fuel) registers.Registers.status}

let (framed @ total) : (lowered : Program_lower.program) @ immutable -> (module_ : Func.module_) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (env_count : Slots.slot) ->
    (program : Hmc_tail_ir.program) @ immutable -> (globals : Machine.globals) @ immutable -> (stack_limit : D.index) @ immutable ->
    (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next_activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (result_type : D.mono) @ immutable -> (head_type : D.mono) @ immutable -> (next : D.index) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable -> (plan : Lower.fragment) @ immutable ->
    (capacity : Relayout.count) -> (max_pc : W.limb) -> (old_pc : W.limb) -> (left : V.value) @ immutable -> (right : V.value) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (heap : Heap.heap) @ immutable ->
    (frame_stop : B.u32) -> (before : Registers.registers) @ immutable -> (memory : B.bytes) @ immutable ->
    (bytes : B.bytes) @ immutable -> (tail : B.bytes) @ immutable -> (host_capacity : C.count) @ immutable -> (function_index : B.u32) ->
    (abstract : Hmc_cfg_semantics.state) @ immutable -> (runtime : Hmc_runtime_closures.table) @ immutable ->
    (table_count : Hmc_runtime_descriptor_table.count) -> (cell_count : B.u32) ->
    {u : unit | Hmc_wasm_program_resources.valid program globals lowered.Program_lower.width stack_base frame_stop abstract heap activation frames before memory
      && Hmc_wasm_program_descriptors.valid program before memory runtime table_base table_count
      && Hmc_wasm_program_frame.valid signature activation before memory frame_stop old_pc cells padding bytes tail cell_count
      && Index.represents (Codec.locals_size signature.G.locals) env_count
      && before.Registers.frame + 48 + 16 * env_count <= 4294967280
      && Hmc_tail_ir.lookup program.Hmc_tail_ir.code activation.Frame.pc === Some (Hmc_tail_ir.Keep (G.Cons next))
      && frame_stop <= before.Registers.heap && frame_stop = before.Registers.frame + 16 + 16 * capacity
      && Lower.matches signature next capacity max_pc plan
      && signature.G.temporaries === G.Value (context, head_type, schema)
      && activation.Frame.accumulator === right
      && (match activation.Frame.temporaries with Frame.Value (w, _, _) -> w === left | _ -> false)
      && Index.represents (Heap.length cells) capacity
      && Model.successor signature result_type === Some next_signature
      && Model.transition activation next (V.Cons_pointer before.Registers.heap) === Some next_activation
      && Func.signature module_.Func.signatures (Dispatch.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table old_pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered (fragment plan) (Runtime.config table_base stack_base))} ->
    {out : result | Hmc_wasm_program_descriptors.valid program out.registers out.body.New.state.X.memory runtime table_base table_count
      && out.registers.Registers.top = before.Registers.top
      && out.registers.Registers.tag === before.Registers.tag && out.registers.Registers.payload === before.Registers.payload
      && out.registers.Registers.frame = before.Registers.frame
      && out.registers.Registers.heap_limit = before.Registers.heap_limit && out.registers.Registers.stack_limit = before.Registers.stack_limit
      && Machine.step program globals before.Registers.heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)} === out.body.New.source.Entry.result.Guarded.source
      && (match out.body.New.source.Entry.result.Guarded.allocation with
        | A.Exhausted ->
          Hmc_wasm_program_resources.valid program globals lowered.Program_lower.width stack_base frame_stop abstract heap activation frames out.registers out.body.New.state.X.memory
          && Hmc_wasm_program_frame.valid signature activation out.registers out.body.New.state.X.memory frame_stop old_pc cells padding bytes tail cell_count
          && out.body.New.source.Entry.result.Guarded.source === Machine.Exhausted Machine.Heap
          && out.registers.Registers.status = 2 && out.registers.Registers.heap = before.Registers.heap
          && out.body.New.state.X.memory === memory
        | A.Allocated allocation ->
          Hmc_wasm_program_resources.valid program globals lowered.Program_lower.width stack_base frame_stop (Hmc_tail_semantics.step program abstract)
            allocation.A.heap next_activation frames out.registers out.body.New.state.X.memory
          && (match out.body.New.source.Entry.result.Guarded.frame with
            | None -> false
            | Some frame -> Hmc_wasm_program_frame.valid next_signature next_activation out.registers out.body.New.state.X.memory frame_stop plan.Lower.pc
              frame.Invariant.cells frame.Invariant.padding frame.Invariant.bytes out.body.New.source.Entry.result.Guarded.tail cell_count)
          && out.body.New.source.Entry.result.Guarded.source === Machine.Advanced {Machine.heap = allocation.A.heap; state = State.Running (next_activation, frames)}
          && A.correct program.Hmc_tail_ir.origin.Hmc_cfg_program.origin.Hmc_closure_program.table heap before.Registers.heap_limit (Heap.Cons (left, right)) out.body.New.source.Entry.result.Guarded.allocation
          && P.equal_prefix before.Registers.heap memory out.body.New.source.Entry.result.Guarded.allocated
          && P.equal_prefix before.Registers.frame memory out.body.New.state.X.memory
          && Bytes.drop out.body.New.source.Entry.result.Guarded.allocated frame_stop === Some out.body.New.source.Entry.result.Guarded.tail
          && Bytes.drop out.body.New.state.X.memory frame_stop === Some out.body.New.source.Entry.result.Guarded.tail
          && Hmc_linear_bounds.covers out.body.New.source.Entry.result.Guarded.allocated frame_stop
          && Hmc_linear_bounds.covers out.body.New.state.X.memory frame_stop
          && V.length out.body.New.state.X.memory === V.length memory
          && out.registers.Registers.status = 0 && out.registers.Registers.heap = Heap.used allocation.A.heap
          && Image.related out.body.New.state.X.memory allocation.A.heap
          && (match out.body.New.source.Entry.result.Guarded.frame with
            | None -> false
            | Some frame -> Codec.decode next_signature next_activation.Frame.pc frame.Invariant.cells === Some (next_activation, frame.Invariant.padding)
              && Heap.length frame.Invariant.cells === Heap.length cells
              && Bytes.drop out.body.New.state.X.memory before.Registers.frame === Some frame.Invariant.bytes
              && Wire.decode_cells (D.S (Heap.length frame.Invariant.cells)) frame.Invariant.bytes ===
                Some (Heap.Cell (V.Word (Header.number plan.Lower.pc), frame.Invariant.cells), out.body.New.source.Entry.result.Guarded.tail)))
      && (if out.registers.Registers.status = 2 then match out.failed_guard with
        | Guard.Absent -> false
        | Guard.Present guard -> Guard.reaches Failed.Heap module_
          (Dispatch.loop (Registers.globals before) memory (C.Succ host_capacity)) guard
        else true)
      && Calls.run out.fuel module_ (Dispatch.loop (Registers.globals before) memory (C.Succ host_capacity)) ===
        (if out.registers.Registers.status = 0 then Calls.Running (Dispatch.loop (Registers.globals out.registers) out.body.New.state.X.memory (C.Succ host_capacity))
         else Calls.Finished {GE.globals = Registers.globals out.registers; execution = {X.memory = out.body.New.state.X.memory;
           machine = {E.locals = S.Empty; stack = S.Push (S.I32 out.registers.Registers.status, S.Empty)}}})} @ immutable =
  fun lowered module_ table_base stack_base env_count program globals stack_limit signature next_signature activation next_activation frames result_type head_type next
      context schema plan capacity max_pc old_pc left right cells padding heap frame_stop before memory bytes tail host_capacity function_index
      abstract runtime table_count cell_count premise ->
    ghost_ (Hmc_wasm_program_frame.valid_def signature activation before memory frame_stop old_pc cells padding bytes tail cell_count;
      Hmc_wasm_program_resources.valid_def program globals lowered.Program_lower.width stack_base frame_stop abstract heap activation frames before memory;
      Hmc_heap_invariant.valid_def program globals before.Registers.heap_limit {Machine.heap; state = State.Running (activation, frames)} abstract;
      Hmc_heap_invariant.request_valid program heap (State.Running (activation, frames)) abstract ();
      Hmc_heap_step.request_def program (State.Running (activation, frames));
      Hmc_heap_allocating.request_def (G.Cons next) activation;
      Hmc_wasm_heap_suffix.objects_above heap frame_stop ());
    let out = correct lowered module_ table_base stack_base env_count program globals stack_limit signature next_signature activation next_activation frames
      result_type head_type next context schema plan capacity max_pc old_pc left right cells padding heap frame_stop before memory bytes tail host_capacity function_index () in
    ghost_ (
      Hmc_wasm_program_frame.valid_def signature activation before memory frame_stop old_pc cells padding bytes tail cell_count;
      match out.body.New.source.Entry.result.Guarded.allocation with
      | A.Exhausted ->
        Hmc_wasm_program_resources.valid_def program globals lowered.Program_lower.width stack_base frame_stop abstract heap activation frames before memory;
        Hmc_wasm_program_resources.valid_def program globals lowered.Program_lower.width stack_base frame_stop abstract heap activation frames out.registers out.body.New.state.X.memory;
        Hmc_wasm_program_descriptors.valid_def program before memory runtime table_base table_count;
        Hmc_wasm_program_descriptors.valid_def program out.registers out.body.New.state.X.memory runtime table_base table_count;
        Hmc_wasm_program_frame.valid_def signature activation out.registers out.body.New.state.X.memory frame_stop old_pc cells padding bytes tail cell_count
      | A.Allocated allocation ->
        Hmc_wasm_program_resources.allocate program globals lowered.Program_lower.width stack_base frame_stop abstract heap allocation (Heap.Cons (left, right))
          activation next_activation frames stack_limit before out.registers memory out.body.New.source.Entry.result.Guarded.allocated out.body.New.state.X.memory ();
        Hmc_wasm_program_descriptors.preserve program before out.registers memory out.body.New.state.X.memory runtime table_base table_count ();
        Lower.matches_def signature next capacity max_pc plan;
        Model.transition_def activation next (V.Cons_pointer before.Registers.heap);
        match out.body.New.source.Entry.result.Guarded.frame with
        | None -> ()
        | Some frame -> Hmc_wasm_program_frame.valid_def next_signature next_activation out.registers out.body.New.state.X.memory frame_stop plan.Lower.pc
            frame.Invariant.cells frame.Invariant.padding frame.Invariant.bytes out.body.New.source.Entry.result.Guarded.tail cell_count);
    out
