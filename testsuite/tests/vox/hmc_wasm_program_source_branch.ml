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
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module C = Wasm_code
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
module Codec = Hmc_pointer_frame_codec
module Header = Hmc_wasm_header_update
module Branch = Hmc_wasm_program_branch
module Source = Hmc_wasm_branch_invariant
module Status = Hmc_wasm_program_status
module Structured = Hmc_wasm_structured_block
module Straight = Hmc_wasm_block_lower
module Simple = Hmc_wasm_simple_lower
let[@def] (fragment @ total) (yes_pc : B.u32) (no_pc : B.u32) =
  Block.Structured (Structured.Straight (Straight.Simple (Simple.Branch (yes_pc, no_pc))))
type result = {source : Source.result; registers : Registers.registers; fuel : {n : C.count | not (n === C.Zero)}}
let (correct @ total) : (lowered : Lower.program) @ immutable -> (module_ : Func.module_) @ immutable ->
    (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : H.heap) @ immutable ->
    (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (stack_capacity : D.index) @ immutable ->
    (yes : D.index) @ immutable -> (no : D.index) @ immutable -> (yes_pc : B.u32) -> (no_pc : B.u32) -> (condition : bool) -> (next : D.index) @ immutable ->
    (pc : B.u32) -> (old_pc : B.u32) -> (cells : H.cells) @ immutable -> (padding : H.cells) @ immutable ->
    (before : Registers.registers) @ immutable -> (memory : B.bytes) @ immutable -> (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (host_capacity : C.count) @ immutable -> (function_index : B.u32) ->
    {u : unit | activation.F.accumulator === V.Boolean condition
      && next === (if condition then yes else no) && pc = (if condition then yes_pc else no_pc)
      && Index.represents yes yes_pc && Index.represents no no_pc && Index.represents next pc
      && Program.lookup program.Program.code activation.F.pc === Some (Program.Keep (G.Branch (yes, no)))
      && Index.represents activation.F.pc old_pc
      && next_signature.G.locals === signature.G.locals && next_signature.G.temporaries === signature.G.temporaries
      && Codec.decode signature activation.F.pc cells === Some (activation, padding)
      && before.Registers.frame <= 4294967247 && Bytes.drop memory before.Registers.frame === Some bytes
      && Wire.decode_cells (D.S (H.length cells)) bytes === Some (H.Cell (V.Word (Header.number old_pc), cells), suffix)
      && Func.signature module_.Func.signatures (Dispatch.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table old_pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered (fragment yes_pc no_pc) (Runtime.config table_base stack_base))} ->
    {out : result | Hmc_linear_preservation.equal_prefix before.Registers.frame memory out.source.Source.memory
      && out.registers === {before with Registers.status = Status.zero ()}
      && Machine.step program globals before.Registers.heap_limit stack_capacity {Machine.heap; state = Q.Running (activation, frames)} ===
        Machine.Advanced {Machine.heap; state = Q.Running (out.source.Source.activation, frames)}
      && out.source.Source.activation === {activation with F.pc = next}
      && Codec.decode next_signature next out.source.Source.cells === Some (out.source.Source.activation, padding)
      && H.length out.source.Source.cells === H.length cells
      && Bytes.drop out.source.Source.memory before.Registers.frame === Some out.source.Source.bytes
      && Wire.decode_cells (D.S (H.length out.source.Source.cells)) out.source.Source.bytes ===
        Some (H.Cell (V.Word (Header.number pc), out.source.Source.cells), suffix)
      && Calls.run out.fuel module_ (Dispatch.loop (Registers.globals before) memory (C.Succ host_capacity)) ===
        Calls.Running (Dispatch.loop (Registers.globals out.registers) out.source.Source.memory (C.Succ host_capacity))} @ immutable =
  fun lowered module_ program globals heap signature next_signature activation frames stack_capacity yes no yes_pc no_pc condition next pc old_pc cells padding
      before memory bytes suffix table_base stack_base host_capacity function_index premise ->
    let config = Runtime.config table_base stack_base in
    let state = {X.memory; machine = {E.locals = Registers.locals before; stack = S.Empty}} in
    ghost_ (Runtime.config_def table_base stack_base; Registers.local_values before; Registers.matches_def (Registers.locals before) before;
      Branch.failure_def (); L.can_set_def (Registers.locals before) 11 (S.I32 2); S.same_type_def (S.I32 before.Registers.status) (S.I32 2));
    let body = Branch.correct lowered config.Assembly.locals table_base stack_base (Round.labels config) program globals heap before.Registers.heap_limit stack_capacity
      signature next_signature activation frames yes no yes_pc no_pc condition next pc old_pc cells padding state 0 before.Registers.frame bytes suffix () in
    ghost_ (fragment_def yes_pc no_pc);
    let registers = Step.complete lowered (fragment yes_pc no_pc) module_ table_base stack_base before memory bytes cells suffix host_capacity old_pc function_index body.Branch.fuel body.Branch.state () in
    ghost_ (Status.zero_def (); Registers.exports_def body.Branch.state.X.machine.E.locals registers;
      L.other_local (Registers.locals before) 11 (S.I32 (Status.zero ())) body.Branch.state.X.machine.E.locals 1 ();
      L.other_local (Registers.locals before) 11 (S.I32 (Status.zero ())) body.Branch.state.X.machine.E.locals 4 ();
      L.other_local (Registers.locals before) 11 (S.I32 (Status.zero ())) body.Branch.state.X.machine.E.locals 12 ();
      L.other_local (Registers.locals before) 11 (S.I32 (Status.zero ())) body.Branch.state.X.machine.E.locals 13 ());
    {source = body.Branch.source; registers; fuel = Hmc_wasm_program_cost.dispatch (Round.cost config body.Branch.fuel) registers.Registers.status}

let (framed @ total) : (lowered : Lower.program) @ immutable -> (module_ : Func.module_) @ immutable ->
    (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable -> (heap : H.heap) @ immutable ->
    (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (stack_capacity : D.index) @ immutable ->
    (yes : D.index) @ immutable -> (no : D.index) @ immutable -> (yes_pc : B.u32) -> (no_pc : B.u32) -> (condition : bool) -> (next : D.index) @ immutable ->
    (pc : B.u32) -> (old_pc : B.u32) -> (cells : H.cells) @ immutable -> (padding : H.cells) @ immutable ->
    (before : Registers.registers) @ immutable -> (memory : B.bytes) @ immutable -> (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (host_capacity : C.count) @ immutable -> (function_index : B.u32) -> (frame_end : B.u32) -> (cell_count : B.u32) -> (abstract : Hmc_cfg_semantics.state) @ immutable ->
    (runtime : Hmc_runtime_closures.table) @ immutable -> (table_count : Hmc_runtime_descriptor_table.count) ->
    {u : unit | Hmc_wasm_program_descriptors.valid program before memory runtime table_base table_count
      && Hmc_wasm_program_resources.valid program globals lowered.Lower.width stack_base frame_end abstract heap activation frames before memory
      && Hmc_wasm_program_frame.valid signature activation before memory frame_end old_pc cells padding bytes suffix cell_count
      && activation.F.accumulator === V.Boolean condition
      && next === (if condition then yes else no) && pc = (if condition then yes_pc else no_pc)
      && Index.represents yes yes_pc && Index.represents no no_pc && Index.represents next pc
      && Program.lookup program.Program.code activation.F.pc === Some (Program.Keep (G.Branch (yes, no)))
      && next_signature.G.locals === signature.G.locals && next_signature.G.temporaries === signature.G.temporaries
      && before.Registers.frame <= 4294967247
      && Func.signature module_.Func.signatures (Dispatch.void_signature ()) === Some Func.Void
      && Func.element module_.Func.table old_pc === Some function_index
      && Func.lookup module_.Func.functions function_index === Some (Assembly.function_ lowered (fragment yes_pc no_pc) (Runtime.config table_base stack_base))} ->
    {out : result | Hmc_wasm_program_descriptors.valid program out.registers out.source.Source.memory runtime table_base table_count
      && Hmc_wasm_program_resources.valid program globals lowered.Lower.width stack_base frame_end
        (Hmc_tail_semantics.step program abstract) heap out.source.Source.activation frames out.registers out.source.Source.memory
      && V.length out.source.Source.memory === V.length memory
      && Bytes.drop out.source.Source.memory frame_end === Bytes.drop memory frame_end
      && Hmc_linear_bounds.covers out.source.Source.memory frame_end
      && Hmc_linear_preservation.equal_prefix before.Registers.frame memory out.source.Source.memory
      && out.registers === {before with Registers.status = Status.zero ()}
      && Machine.step program globals before.Registers.heap_limit stack_capacity {Machine.heap; state = Q.Running (activation, frames)} ===
        Machine.Advanced {Machine.heap; state = Q.Running (out.source.Source.activation, frames)}
      && out.source.Source.activation === {activation with F.pc = next}
      && Hmc_wasm_program_frame.valid next_signature out.source.Source.activation out.registers out.source.Source.memory frame_end pc
        out.source.Source.cells padding out.source.Source.bytes suffix cell_count
      && H.length out.source.Source.cells === H.length cells
      && Calls.run out.fuel module_ (Dispatch.loop (Registers.globals before) memory (C.Succ host_capacity)) ===
        Calls.Running (Dispatch.loop (Registers.globals out.registers) out.source.Source.memory (C.Succ host_capacity))} @ immutable =
  fun lowered module_ program globals heap signature next_signature activation frames stack_capacity yes no yes_pc no_pc condition next pc old_pc cells padding
      before memory bytes suffix table_base stack_base host_capacity function_index frame_end cell_count abstract runtime table_count premise ->
    ghost_ (Hmc_wasm_program_frame.valid_def signature activation before memory frame_end old_pc cells padding bytes suffix cell_count);
    let out = correct lowered module_ program globals heap signature next_signature activation frames stack_capacity yes no yes_pc no_pc condition next pc old_pc cells padding
      before memory bytes suffix table_base stack_base host_capacity function_index () in
    ghost_ (
      Hmc_wasm_program_descriptors.preserve program before out.registers memory out.source.Source.memory runtime table_base table_count ();
      let old_cells = H.Cell (V.Word (Header.number old_pc), cells) in
      let new_cells = H.Cell (V.Word (Header.number pc), out.source.Source.cells) in
      H.length_def old_cells; H.length_def new_cells;
      Hmc_wasm_frame_preservation.correct memory out.source.Source.memory before.Registers.frame frame_end bytes out.source.Source.bytes old_cells new_cells suffix cell_count ();
      Hmc_wasm_program_resources.preserve program globals lowered.Lower.width stack_base frame_end abstract heap activation out.source.Source.activation frames
        stack_capacity before out.registers memory out.source.Source.memory ();
      Hmc_wasm_program_frame.valid_def next_signature out.source.Source.activation out.registers out.source.Source.memory frame_end pc
        out.source.Source.cells padding out.source.Source.bytes suffix cell_count);
    out
