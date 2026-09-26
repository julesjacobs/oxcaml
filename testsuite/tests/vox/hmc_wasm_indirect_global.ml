module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Machine = Hmc_heap_machine
module Program = Hmc_tail_ir
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Lower = Hmc_wasm_global_lower
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module Bytes = Hmc_linear_bytes
module Source = Hmc_wasm_global_invariant
module C = Wasm_code
module T = Wasm_control
module P = Wasm_instance_control
module GE = Wasm_global_execution
module Func = Wasm_functions
module Calls = Wasm_calls
module Registers = Wasm_global_registers
module Wrapper = Wasm_register_block
module Lift = Wasm_control_lift
module Continue = Wasm_control_branch_continue
module Indirect = Wasm_indirect_block
type result = {source : Source.result; fuel : C.count}
let[@def] (emit @ total) (loads : Registers.plan @ immutable) (fragment : Lower.fragment @ immutable) (base_local : B.u32) =
  Wrapper.emit loads (Lift.embed (Lower.emit fragment base_local) T.Empty) Registers.End T.Empty
let (correct @ total) : (loads : Registers.plan) @ immutable -> (module_ : Func.module_) @ immutable -> (target : Func.function_) @ immutable ->
    (call : Calls.configuration) @ immutable -> (type_index : B.u32) -> (slot : B.u32) -> (function_index : B.u32) ->
    (continuation : T.code) @ immutable -> (caller_stack : S.stack) @ immutable -> (capacity : C.count) @ immutable -> (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap : Heap.heap) @ immutable -> (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable ->
    (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (index : D.index) @ immutable -> (ty : D.mono) @ immutable -> (derivation : D.typing) @ immutable -> (next : D.index) @ immutable ->
    (fragment : Lower.fragment) @ immutable -> (old_pc : W.limb) -> (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | call.Calls.current.P.body.T.code === T.Instruction (Wasm_instruction.Call_indirect type_index, continuation)
      && call.Calls.current.P.body.T.state.X.machine.E.stack === S.Push (S.I32 slot, caller_stack)
      && call.Calls.capacity === C.Succ capacity
      && Func.signature module_.Func.signatures type_index === Some Func.Void
      && Func.element module_.Func.table slot === Some function_index && Func.lookup module_.Func.functions function_index === Some target
      && target.Func.result === Func.Void && target.Func.code === emit loads fragment base_local
      && GE.run (Registers.load_code loads)
        {GE.globals = call.Calls.current.P.globals; execution = {X.memory = call.Calls.current.P.body.T.state.X.memory;
          machine = {E.locals = Func.zero_locals target.Func.locals; stack = S.Empty}}} ===
        GE.Done {GE.globals = call.Calls.current.P.globals; execution = state}
      && state.X.machine.E.stack === S.Empty
      && Lower.corresponds globals (G.Load (G.Global index, ty, derivation, next)) fragment
      && Program.lookup program.Program.code activation.Frame.pc === Some (Program.Keep (G.Load (G.Global index, ty, derivation, next)))
      && Hmc_u32_index.represents activation.Frame.pc old_pc
      && next_signature.G.locals === signature.G.locals && next_signature.G.temporaries === signature.G.temporaries
      && Codec.decode signature activation.Frame.pc cells === Some (activation, padding)
      && base <= 4294967248 && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : result | Calls.run out.fuel module_ call === Calls.Running {call with Calls.current =
        {P.globals = call.Calls.current.P.globals; body = {T.code = continuation; labels = call.Calls.current.P.body.T.labels;
          state = {X.memory = out.source.Source.memory; machine = {E.locals = call.Calls.current.P.body.T.state.X.machine.E.locals; stack = caller_stack}}}}}
      && X.run (Lower.emit fragment base_local) state === X.Done {X.memory = out.source.Source.memory; machine = state.X.machine}
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)}
        === Machine.Advanced {Machine.heap; state = State.Running (out.source.Source.activation, frames)}
      && out.source.Source.activation === {activation with Frame.pc = next; accumulator = fragment.Lower.value}
      && Codec.decode next_signature next out.source.Source.cells === Some (out.source.Source.activation, padding)
      && Heap.length out.source.Source.cells === Heap.length cells && Bytes.drop out.source.Source.memory base === Some out.source.Source.bytes
      && Wire.decode_cells (D.S (Heap.length out.source.Source.cells)) out.source.Source.bytes === Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), out.source.Source.cells), tail)} @ immutable =
  fun loads module_ target call type_index slot function_index continuation caller_stack capacity program globals heap heap_limit stack_limit signature next_signature activation frames index ty derivation next fragment old_pc cells padding       state base_local base before_frame tail premise ->
    let source = Source.correct program globals heap heap_limit stack_limit signature next_signature activation frames index ty derivation next fragment old_pc cells padding       state base_local base before_frame tail () in
    let code = Lower.emit fragment base_local in
    let body = Lift.embed code T.Empty in
    let after = {X.memory = source.Source.memory; machine = state.X.machine} in
    let imported = {GE.globals = call.Calls.current.P.globals; execution = state} in
    let exported = {GE.globals = call.Calls.current.P.globals; execution = after} in
    let initial = {GE.globals = call.Calls.current.P.globals; execution = {X.memory = call.Calls.current.P.body.T.state.X.memory;
      machine = {E.locals = Func.zero_locals target.Func.locals; stack = S.Empty}}} in
    let block_fuel = Wrapper.cost loads (C.length code) Registers.End in
    ghost_ (Wasm_control_success.straight code state after ();
      Lift.correct code T.Empty (Continue.labels (Wrapper.epilogue Registers.End T.Empty) T.No_labels) state after ();
      Registers.store_code_def Registers.End; GE.run_def C.Empty exported;
      Wrapper.correct loads body Registers.End T.Empty T.No_labels initial imported after exported (C.length code) ();
      emit_def loads fragment base_local; Indirect.entry_def target call.Calls.current;
      Indirect.correct module_ type_index slot function_index target continuation call caller_stack capacity exported block_fuel ());
    {source; fuel = Indirect.cost block_fuel}
