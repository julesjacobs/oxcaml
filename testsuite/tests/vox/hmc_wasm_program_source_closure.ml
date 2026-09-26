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
module New = Hmc_wasm_program_closure
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
let[@def] (fragment @ total) (plan : Write.fragment @ immutable) (pc : B.u32) =
  Hmc_wasm_program_block.Structured (Block.Closure {Hmc_wasm_closure_lower.object_ = plan; pc})
type result = {body : New.result; registers : Registers.registers; fuel : {n : C.count | not (n === C.Zero)}; failed_guard : Guard.optional @@ ghost}
let (correct @ total) : (lowered : Lower.program) @ immutable -> (module_ : Func.module_) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (program : Program.program) @ immutable ->
    (globals : Machine.globals) @ immutable -> (stack_limit : D.index) @ immutable -> (heap : Heap.heap) @ immutable ->
    (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (id : D.index) @ immutable -> (type_ : D.mono) @ immutable -> (typing : D.typing) @ immutable -> (next : D.index) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (old_pc : W.limb) -> (pc : W.limb) ->
    (capacity : Hmc_wasm_relayout.count) -> (max_code : W.limb) -> (plan : Write.fragment) @ immutable ->
    (before : Registers.registers) @ immutable -> (memory : B.bytes) @ immutable -> (frame_stop : B.u32) ->
    (bytes : B.bytes) @ immutable -> (tail : B.bytes) @ immutable -> (host_capacity : C.count) @ immutable -> (function_index : B.u32) ->
    {u : unit | Heap.valid program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table heap
      && Heap.object_valid program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table (Heap.view heap) (Heap.Closure (id, activation.Frame.env))
      && Program.lookup program.Program.code activation.Frame.pc === Some (Program.Keep (G.Load (G.Closure id, type_, typing, next)))
      && Heap.used heap = before.Registers.heap && Image.related memory heap && Above.above heap frame_stop
      && Write.matches signature.G.locals id capacity max_code plan
      && Codec.decode signature activation.Frame.pc cells === Some (activation, padding)
      && next_signature.G.locals === signature.G.locals && next_signature.G.temporaries === signature.G.temporaries
      && Index.represents activation.Frame.pc old_pc && Index.represents next pc && Index.represents (Heap.length cells) capacity
      && frame_stop = before.Registers.frame + 16 + 16 * capacity && before.Registers.frame <= 4294967248
      && frame_stop <= before.Registers.heap && before.Registers.heap <= before.Registers.heap_limit
      && Hmc_linear_bounds.covers memory before.Registers.heap_limit
      && Bytes.drop memory before.Registers.frame === Some bytes
      && Wire.decode_cells (D.S (Heap.length cells)) bytes === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)
      && Func.signature module_.Func.signatures (Dispatch.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table old_pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered (fragment plan pc) (Runtime.config table_base stack_base))} ->
    {out : result | out.registers.Registers.frame = before.Registers.frame && out.registers.Registers.heap_limit = before.Registers.heap_limit
      && out.registers.Registers.stack_limit = before.Registers.stack_limit && out.registers.Registers.top = before.Registers.top
      && out.registers.Registers.tag === before.Registers.tag && out.registers.Registers.payload === before.Registers.payload
      && Machine.step program globals before.Registers.heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)} === out.body.New.source.Guarded.source
      && (match out.body.New.source.Guarded.success with
        | None -> out.body.New.source.Guarded.source === Machine.Exhausted Machine.Heap
          && out.registers.Registers.status = 2 && out.registers.Registers.heap = before.Registers.heap
          && out.body.New.state.X.memory === memory && before.Registers.heap + plan.Write.bytes > before.Registers.heap_limit
        | Some success -> out.registers.Registers.status = 0 && out.registers.Registers.heap = Heap.used success.Success.allocation.A.heap
          && out.body.New.source.Guarded.source === Machine.Advanced {Machine.heap = success.Success.allocation.A.heap; state = State.Running (success.Success.frame.Finish.activation, frames)}
          && A.correct program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table heap before.Registers.heap_limit
            (Heap.Closure (id, activation.Frame.env)) (A.Allocated success.Success.allocation)
          && Image.related out.body.New.state.X.memory success.Success.allocation.A.heap
          && success.Success.frame.Finish.activation === {activation with Frame.pc = next; accumulator = V.Closure_pointer before.Registers.heap}
          && Codec.decode next_signature next success.Success.frame.Finish.cells === Some (success.Success.frame.Finish.activation, padding)
          && Heap.length success.Success.frame.Finish.cells === Heap.length cells
          && Bytes.drop out.body.New.state.X.memory before.Registers.frame === Some success.Success.frame.Finish.bytes
          && Wire.decode_cells (D.S (Heap.length success.Success.frame.Finish.cells)) success.Success.frame.Finish.bytes ===
            Some (Heap.Cell (V.Word (Header.number pc), success.Success.frame.Finish.cells), success.Success.tail)
          && P.equal_prefix before.Registers.heap memory success.Success.allocated
          && P.equal_prefix before.Registers.frame memory out.body.New.state.X.memory
          && Bytes.drop success.Success.allocated frame_stop === Some success.Success.tail && Bytes.drop out.body.New.state.X.memory frame_stop === Some success.Success.tail
          && Hmc_linear_bounds.covers success.Success.allocated frame_stop && Hmc_linear_bounds.covers out.body.New.state.X.memory frame_stop
          && V.length out.body.New.state.X.memory === V.length memory)
      && (if out.registers.Registers.status = 2 then match out.failed_guard with
        | Guard.Absent -> false
        | Guard.Present guard -> Guard.reaches Failed.Heap module_
          (Dispatch.loop (Registers.globals before) memory (C.Succ host_capacity)) guard
        else true)
      && Calls.run out.fuel module_ (Dispatch.loop (Registers.globals before) memory (C.Succ host_capacity)) ===
        (if out.registers.Registers.status = 0 then Calls.Running (Dispatch.loop (Registers.globals out.registers) out.body.New.state.X.memory (C.Succ host_capacity))
         else Calls.Finished {GE.globals = Registers.globals out.registers; execution = {X.memory = out.body.New.state.X.memory;
           machine = {E.locals = S.Empty; stack = S.Push (S.I32 out.registers.Registers.status, S.Empty)}}})} @ immutable =
  fun lowered module_ table_base stack_base program globals stack_limit heap signature next_signature activation frames id type_ typing next
      cells padding old_pc pc capacity max_code plan before memory frame_stop bytes tail host_capacity function_index premise ->
    let config = Runtime.config table_base stack_base in
    let state = {X.memory; machine = {E.locals = Registers.locals before; stack = S.Empty}} in
    ghost_ (Runtime.config_def table_base stack_base; Registers.local_values before; Registers.matches_def (Registers.locals before) before;
      New.failure_def (); L.can_set_def (Registers.locals before) 11 (S.I32 2); S.same_type_def (S.I32 before.Registers.status) (S.I32 2));
    let body = New.correct lowered config.Assembly.locals table_base stack_base (Round.labels config) program globals stack_limit
      program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table heap signature next_signature activation frames id type_ typing next
      cells padding old_pc pc capacity max_code plan state before.Registers.frame frame_stop before.Registers.heap before.Registers.heap_limit 0 1 2 bytes tail () in
    ghost_ (fragment_def plan pc;
      L.other_local (Registers.locals before) 11 (S.I32 (New.failure ())) body.New.prepared.X.machine.E.locals 4 ();
      L.other_local (Registers.locals before) 11 (S.I32 (New.failure ())) body.New.prepared.X.machine.E.locals 12 ();
      L.other_local (Registers.locals before) 11 (S.I32 (New.failure ())) body.New.prepared.X.machine.E.locals 13 ();
      match body.New.source.Guarded.success with
      | None -> ()
      | Some success ->
        L.other_local body.New.prepared.X.machine.E.locals 1 (S.I32 (Heap.used success.Success.allocation.A.heap)) body.New.source.Guarded.state.X.machine.E.locals 4 ();
        L.other_local body.New.source.Guarded.state.X.machine.E.locals 11 (S.I32 (Status.zero ())) body.New.state.X.machine.E.locals 4 ();
        L.other_local body.New.prepared.X.machine.E.locals 1 (S.I32 (Heap.used success.Success.allocation.A.heap)) body.New.source.Guarded.state.X.machine.E.locals 12 ();
        L.other_local body.New.source.Guarded.state.X.machine.E.locals 11 (S.I32 (Status.zero ())) body.New.state.X.machine.E.locals 12 ();
        L.other_local body.New.prepared.X.machine.E.locals 1 (S.I32 (Heap.used success.Success.allocation.A.heap)) body.New.source.Guarded.state.X.machine.E.locals 13 ();
        L.other_local body.New.source.Guarded.state.X.machine.E.locals 11 (S.I32 (Status.zero ())) body.New.state.X.machine.E.locals 13 ());
    let registers = Step.complete lowered (fragment plan pc) module_ table_base stack_base before memory bytes cells tail host_capacity old_pc function_index body.New.fuel body.New.state () in
    ghost_ (Registers.exports_def body.New.state.X.machine.E.locals registers; Status.zero_def (); New.failure_def ());
    let failed_guard = ghost_ (match body.New.source.Guarded.success with
      | Some _ -> Guard.Absent
      | None ->
        let local = Guard_blocks.closure lowered {Hmc_wasm_closure_lower.object_ = plan; pc}
          table_base stack_base before memory () in
        Guard.Present (Guard.from_body Failed.Heap lowered (fragment plan pc) module_ table_base stack_base
          before memory bytes cells tail host_capacity old_pc function_index local ())) in
    {failed_guard; body; registers; fuel = Hmc_wasm_program_cost.dispatch (Round.cost config body.New.fuel) registers.Registers.status}

let (framed @ total) : (lowered : Lower.program) @ immutable -> (module_ : Func.module_) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (program : Program.program) @ immutable ->
    (globals : Machine.globals) @ immutable -> (stack_limit : D.index) @ immutable -> (heap : Heap.heap) @ immutable ->
    (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (id : D.index) @ immutable -> (type_ : D.mono) @ immutable -> (typing : D.typing) @ immutable -> (next : D.index) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (old_pc : W.limb) -> (pc : W.limb) ->
    (capacity : Hmc_wasm_relayout.count) -> (max_code : W.limb) -> (plan : Write.fragment) @ immutable ->
    (before : Registers.registers) @ immutable -> (memory : B.bytes) @ immutable -> (frame_stop : B.u32) ->
    (bytes : B.bytes) @ immutable -> (tail : B.bytes) @ immutable -> (host_capacity : C.count) @ immutable -> (function_index : B.u32) ->
    (abstract : Hmc_cfg_semantics.state) @ immutable -> (runtime : Hmc_runtime_closures.table) @ immutable ->
    (table_count : Hmc_runtime_descriptor_table.count) -> (cell_count : B.u32) ->
    {u : unit | Hmc_wasm_program_resources.valid program globals lowered.Lower.width stack_base frame_stop abstract heap activation frames before memory
      && Hmc_wasm_program_descriptors.valid program before memory runtime table_base table_count
      && Hmc_wasm_program_frame.valid signature activation before memory frame_stop old_pc cells padding bytes tail cell_count
      && Program.lookup program.Program.code activation.Frame.pc === Some (Program.Keep (G.Load (G.Closure id, type_, typing, next)))
      && Write.matches signature.G.locals id capacity max_code plan
      && next_signature.G.locals === signature.G.locals && next_signature.G.temporaries === signature.G.temporaries
      && Index.represents next pc && Index.represents (Heap.length cells) capacity
      && frame_stop = before.Registers.frame + 16 + 16 * capacity
      && frame_stop <= before.Registers.heap
      && Func.signature module_.Func.signatures (Dispatch.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table old_pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered (fragment plan pc) (Runtime.config table_base stack_base))} ->
    {out : result | Hmc_wasm_program_descriptors.valid program out.registers out.body.New.state.X.memory runtime table_base table_count
      && out.registers.Registers.frame = before.Registers.frame && out.registers.Registers.heap_limit = before.Registers.heap_limit
      && out.registers.Registers.stack_limit = before.Registers.stack_limit && out.registers.Registers.top = before.Registers.top
      && out.registers.Registers.tag === before.Registers.tag && out.registers.Registers.payload === before.Registers.payload
      && Machine.step program globals before.Registers.heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)} === out.body.New.source.Guarded.source
      && (match out.body.New.source.Guarded.success with
        | None ->
          Hmc_wasm_program_resources.valid program globals lowered.Lower.width stack_base frame_stop abstract heap activation frames out.registers out.body.New.state.X.memory
          && Hmc_wasm_program_frame.valid signature activation out.registers out.body.New.state.X.memory frame_stop old_pc cells padding bytes tail cell_count
          && out.body.New.source.Guarded.source === Machine.Exhausted Machine.Heap
          && out.registers.Registers.status = 2 && out.registers.Registers.heap = before.Registers.heap
          && out.body.New.state.X.memory === memory && before.Registers.heap + plan.Write.bytes > before.Registers.heap_limit
        | Some success ->
          Hmc_wasm_program_resources.valid program globals lowered.Lower.width stack_base frame_stop (Hmc_tail_semantics.step program abstract)
            success.Success.allocation.A.heap success.Success.frame.Finish.activation frames out.registers out.body.New.state.X.memory
          && Hmc_wasm_program_frame.valid next_signature success.Success.frame.Finish.activation out.registers out.body.New.state.X.memory frame_stop pc
            success.Success.frame.Finish.cells padding success.Success.frame.Finish.bytes success.Success.tail cell_count
          && out.registers.Registers.status = 0 && out.registers.Registers.heap = Heap.used success.Success.allocation.A.heap
          && out.body.New.source.Guarded.source === Machine.Advanced {Machine.heap = success.Success.allocation.A.heap; state = State.Running (success.Success.frame.Finish.activation, frames)}
          && A.correct program.Program.origin.Hmc_cfg_program.origin.Hmc_closure_program.table heap before.Registers.heap_limit
            (Heap.Closure (id, activation.Frame.env)) (A.Allocated success.Success.allocation)
          && Image.related out.body.New.state.X.memory success.Success.allocation.A.heap
          && success.Success.frame.Finish.activation === {activation with Frame.pc = next; accumulator = V.Closure_pointer before.Registers.heap}
          && Codec.decode next_signature next success.Success.frame.Finish.cells === Some (success.Success.frame.Finish.activation, padding)
          && Heap.length success.Success.frame.Finish.cells === Heap.length cells
          && Bytes.drop out.body.New.state.X.memory before.Registers.frame === Some success.Success.frame.Finish.bytes
          && Wire.decode_cells (D.S (Heap.length success.Success.frame.Finish.cells)) success.Success.frame.Finish.bytes ===
            Some (Heap.Cell (V.Word (Header.number pc), success.Success.frame.Finish.cells), success.Success.tail)
          && P.equal_prefix before.Registers.heap memory success.Success.allocated
          && P.equal_prefix before.Registers.frame memory out.body.New.state.X.memory
          && Bytes.drop success.Success.allocated frame_stop === Some success.Success.tail && Bytes.drop out.body.New.state.X.memory frame_stop === Some success.Success.tail
          && Hmc_linear_bounds.covers success.Success.allocated frame_stop && Hmc_linear_bounds.covers out.body.New.state.X.memory frame_stop
          && V.length out.body.New.state.X.memory === V.length memory)
      && (if out.registers.Registers.status = 2 then match out.failed_guard with
        | Guard.Absent -> false
        | Guard.Present guard -> Guard.reaches Failed.Heap module_
          (Dispatch.loop (Registers.globals before) memory (C.Succ host_capacity)) guard
        else true)
      && Calls.run out.fuel module_ (Dispatch.loop (Registers.globals before) memory (C.Succ host_capacity)) ===
        (if out.registers.Registers.status = 0 then Calls.Running (Dispatch.loop (Registers.globals out.registers) out.body.New.state.X.memory (C.Succ host_capacity))
         else Calls.Finished {GE.globals = Registers.globals out.registers; execution = {X.memory = out.body.New.state.X.memory;
           machine = {E.locals = S.Empty; stack = S.Push (S.I32 out.registers.Registers.status, S.Empty)}}})} @ immutable =
  fun lowered module_ table_base stack_base program globals stack_limit heap signature next_signature activation frames id type_ typing next
      cells padding old_pc pc capacity max_code plan before memory frame_stop bytes tail host_capacity function_index
      abstract runtime table_count cell_count premise ->
    ghost_ (Hmc_wasm_program_frame.valid_def signature activation before memory frame_stop old_pc cells padding bytes tail cell_count;
      Hmc_wasm_program_resources.valid_def program globals lowered.Lower.width stack_base frame_stop abstract heap activation frames before memory;
      Hmc_heap_invariant.valid_def program globals before.Registers.heap_limit {Machine.heap; state = State.Running (activation, frames)} abstract;
      Hmc_heap_invariant.request_valid program heap (State.Running (activation, frames)) abstract ();
      Hmc_heap_step.request_def program (State.Running (activation, frames));
      Hmc_heap_allocating.request_def (G.Load (G.Closure id, type_, typing, next)) activation;
      Hmc_wasm_heap_suffix.objects_above heap frame_stop ());
    let out = correct lowered module_ table_base stack_base program globals stack_limit heap signature next_signature activation frames id type_ typing next
      cells padding old_pc pc capacity max_code plan before memory frame_stop bytes tail host_capacity function_index () in
    ghost_ (match out.body.New.source.Guarded.success with
      | None ->
        Hmc_wasm_program_resources.valid_def program globals lowered.Lower.width stack_base frame_stop abstract heap activation frames out.registers out.body.New.state.X.memory;
        Hmc_wasm_program_descriptors.valid_def program before memory runtime table_base table_count;
        Hmc_wasm_program_descriptors.valid_def program out.registers out.body.New.state.X.memory runtime table_base table_count;
        Hmc_wasm_program_frame.valid_def signature activation out.registers out.body.New.state.X.memory frame_stop old_pc cells padding bytes tail cell_count
      | Some success ->
        Hmc_wasm_program_resources.allocate program globals lowered.Lower.width stack_base frame_stop abstract heap success.Success.allocation (Heap.Closure (id, activation.Frame.env))
          activation success.Success.frame.Finish.activation frames stack_limit before out.registers memory success.Success.allocated out.body.New.state.X.memory ();
        Hmc_wasm_program_descriptors.preserve program before out.registers memory out.body.New.state.X.memory runtime table_base table_count ();
        Hmc_wasm_program_frame.valid_def next_signature success.Success.frame.Finish.activation out.registers out.body.New.state.X.memory frame_stop pc
          success.Success.frame.Finish.cells padding success.Success.frame.Finish.bytes success.Success.tail cell_count);
    out
