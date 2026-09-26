module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module V = Hmc_tagged_cell
module Program = Hmc_tail_ir
module Machine = Hmc_heap_machine
module Index = Hmc_u32_index
module Wire = Hmc_heap_wire
module Bytes = Hmc_linear_bytes
module Cells = Hmc_wasm_call_save_memory
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module GE = Wasm_global_execution
module L = Wasm_locals
module C = Wasm_code
module I = Wasm_instruction
module T = Wasm_control
module Func = Wasm_functions
module Calls = Wasm_calls
module Lower = Hmc_wasm_program_lower
module Block = Hmc_wasm_program_block
module Runtime = Hmc_wasm_program_runtime
module Assembly = Hmc_wasm_program_functions
module Registers = Hmc_wasm_program_registers
module Step = Hmc_wasm_program_register_step
module Dispatch = Hmc_wasm_program_dispatch
module Round = Hmc_wasm_program_roundtrip
module Root = Hmc_wasm_program_root
module Read = Wasm_frame_snapshot
module Reads = Hmc_wasm_root_return
module Load = Wasm_memory_lowering
module LP = Wasm_local_preservation
module M = Wasm_memory
module Stack = Hmc_memory_stack
type result = {registers : Registers.registers; fuel : {n : C.count | not (n === C.Zero)}}
let (correct @ total) : (lowered : Lower.program) @ immutable -> (module_ : Func.module_) @ immutable ->
    (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : H.heap) @ immutable ->
    (activation : F.activation) @ immutable -> (stack_capacity : D.index) @ immutable ->
    (before : Registers.registers) @ immutable -> (memory : B.bytes) @ immutable -> (bytes : B.bytes) @ immutable ->
    (suffix : B.bytes) @ immutable -> (pc : B.u32) -> (rest : H.cells) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (host_capacity : C.count) @ immutable -> (function_index : B.u32) ->
    {u : unit | Program.lookup program.Program.code activation.F.pc === Some (Program.Keep G.Return)
      && activation.F.temporaries === F.Empty && Index.represents activation.F.pc pc
      && before.Registers.frame <= 4294967248 && before.Registers.top = stack_base && lowered.Lower.width > 0
      && Bytes.drop memory before.Registers.frame === Some bytes
      && Wire.decode_cells (H.length (Cells.cells pc activation.F.current activation.F.accumulator rest)) bytes ===
        Some (Cells.cells pc activation.F.current activation.F.accumulator rest, suffix)
      && Func.signature module_.Func.signatures (Dispatch.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered Block.Return (Runtime.config table_base stack_base))} ->
    {out : result | out.registers.Registers.status = 1 && out.registers.Registers.tag === V.tag activation.F.accumulator
      && out.registers.Registers.payload === V.payload activation.F.accumulator
      && Machine.step program globals before.Registers.heap_limit stack_capacity {Machine.heap; state = Q.Running (activation, Q.Halt)} ===
        Machine.Advanced {Machine.heap; state = Q.Done activation.F.accumulator}
      && Calls.run out.fuel module_ (Dispatch.loop (Registers.globals before) memory (C.Succ host_capacity)) ===
        Calls.Finished {GE.globals = Registers.globals out.registers; execution = {X.memory;
          machine = {E.locals = S.Empty; stack = S.Push (S.I32 (Root.finished ()), S.Empty)}}}} @ immutable =
  fun lowered module_ program globals heap activation stack_capacity before memory bytes suffix pc rest table_base stack_base host_capacity function_index premise ->
    let config = Runtime.config table_base stack_base in
    let state = {X.memory; machine = {E.locals = Registers.locals before; stack = S.Empty}} in
    ghost_ (Runtime.config_def table_base stack_base; Registers.local_values before; Registers.matches_def (Registers.locals before) before;
      Hmc_wasm_program_status.zero_def ();
      L.can_set_def (Registers.locals before) 11 (S.I32 0); S.same_type_def (S.I32 before.Registers.status) (S.I32 0);
      L.can_set_def (Registers.locals before) 12 (S.I64 (V.tag activation.F.accumulator));
      S.same_type_def (S.I64 before.Registers.tag) (S.I64 (V.tag activation.F.accumulator));
      L.can_set_def (Registers.locals before) 13 (S.I64 (V.payload activation.F.accumulator));
      S.same_type_def (S.I64 before.Registers.payload) (S.I64 (V.payload activation.F.accumulator));
      Stack.related_def program.Program.origin.Hmc_cfg_program.blocks lowered.Lower.width memory stack_base stack_base Q.Halt;
      Reads.reads_def 12 13; Read.emit_def (Reads.reads 12 13) 0;
      Read.emit_def (Read.Read (40, 13, Read.End)) 0; Read.emit_def Read.End 0;
      Load.read_code_def M.W64 32 0 12; Load.load_instruction_def M.W64 32;
      LP.preserves_def (C.Next (I.Local_get 0, C.Next (I.I64_load (3, 32), C.Next (I.Local_set 12, C.Empty)))) 11; LP.instruction_preserves_def (I.Local_get 0) 11;
      LP.preserves_def (C.Next (I.I64_load (3, 32), C.Next (I.Local_set 12, C.Empty))) 11; LP.instruction_preserves_def (I.I64_load (3, 32)) 11;
      LP.preserves_def (C.Next (I.Local_set 12, C.Empty)) 11; LP.instruction_preserves_def (I.Local_set 12) 11;
      LP.preserves_def C.Empty 11;
      Load.read_code_def M.W64 40 0 13; Load.load_instruction_def M.W64 40;
      LP.preserves_def (C.Next (I.Local_get 0, C.Next (I.I64_load (3, 40), C.Next (I.Local_set 13, C.Empty)))) 11; LP.instruction_preserves_def (I.Local_get 0) 11;
      LP.preserves_def (C.Next (I.I64_load (3, 40), C.Next (I.Local_set 13, C.Empty))) 11; LP.instruction_preserves_def (I.I64_load (3, 40)) 11;
      LP.preserves_def (C.Next (I.Local_set 13, C.Empty)) 11; LP.instruction_preserves_def (I.Local_set 13) 11;
      LP.preserves_def C.Empty 11;
      LP.append (Load.read_code M.W64 40 0 13) C.Empty 11 ();
      LP.append (Load.read_code M.W64 32 0 12) (Read.emit (Read.Read (40, 13, Read.End)) 0) 11 ());
    let body = Root.correct lowered config.Assembly.locals table_base program globals heap before.Registers.heap_limit stack_capacity
      activation pc rest state before.Registers.frame bytes suffix 0 12 13 4 stack_base lowered.Lower.width (Round.labels config) () in
    let contents = H.Cell (activation.F.current, H.Cell (activation.F.accumulator, rest)) in
    ghost_ (Cells.cells_def pc activation.F.current activation.F.accumulator rest;
      H.length_def (Cells.cells pc activation.F.current activation.F.accumulator rest));
    let registers = Step.complete lowered Block.Return module_ table_base stack_base before memory bytes contents suffix host_capacity pc function_index body.Root.fuel body.Root.state () in
    ghost_ (Registers.exports_def body.Root.state.X.machine.E.locals registers; Root.finished_def ());
    {registers; fuel = Hmc_wasm_program_cost.dispatch (Round.cost config body.Root.fuel) registers.Registers.status}
