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
module Lower = Hmc_wasm_pc_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module Bytes = Hmc_linear_bytes
module Source = Hmc_wasm_jump_invariant
module Status = Hmc_wasm_program_status
module Emit = Hmc_wasm_program_emit
module Structured = Hmc_wasm_structured_block
module Straight = Hmc_wasm_block_lower
module Block = Hmc_wasm_program_block
module T = Wasm_control
module C = Wasm_code
module L = Wasm_locals
module Lift = Wasm_control_lift
let[@def] (failure @ total) (unit : unit) : B.u32 = 2
type result = {source : Source.result; state : X.state; fuel : C.count}
let (correct @ total) : (lowered : Hmc_wasm_program_lower.program) @ immutable -> (locals : Hmc_wasm_program_emit.locals) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (outer : Wasm_control.labels) @ immutable -> (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap : Heap.heap) @ immutable -> (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable ->
    (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (next : D.index) @ immutable ->
    (pc : W.limb) -> (old_pc : W.limb) -> (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | base_local = locals.Hmc_wasm_program_emit.structured.Hmc_wasm_structured_block.frame
      && locals.Hmc_wasm_program_emit.status <> base_local && state.X.machine.E.stack === S.Empty
      && Wasm_locals.can_set state.X.machine.E.locals locals.Hmc_wasm_program_emit.status (S.I32 (failure ()))
      && Hmc_u32_index.represents next pc
      && Program.lookup program.Program.code activation.Frame.pc === Some (Program.Keep (G.Jump next))
      && Hmc_u32_index.represents activation.Frame.pc old_pc
      && next_signature.G.locals === signature.G.locals && next_signature.G.temporaries === signature.G.temporaries
      && Codec.decode signature activation.Frame.pc cells === Some (activation, padding)
      && base <= 4294967248 && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : result | Hmc_linear_preservation.equal_prefix base state.X.memory out.source.Source.memory
      && L.replaced state.X.machine.E.locals locals.Hmc_wasm_program_emit.status (S.I32 (Status.zero ())) out.state.X.machine.E.locals
      && out.state.X.memory === out.source.Source.memory && out.state.X.machine.E.stack === S.Empty
      && Wasm_locals.get out.state.X.machine.E.locals locals.Hmc_wasm_program_emit.status === Some (S.I32 (Status.zero ()))
      && Wasm_control.run out.fuel {Wasm_control.code = Hmc_wasm_program_emit.emit lowered
          (Hmc_wasm_program_block.Structured (Hmc_wasm_structured_block.Straight (Hmc_wasm_block_lower.Simple (Hmc_wasm_simple_lower.Jump pc)))) locals table_base stack_base;
          labels = outer; state} === Wasm_control.Running {Wasm_control.code = Wasm_control.Empty; labels = outer; state = out.state}
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)}
        === Machine.Advanced {Machine.heap; state = State.Running (out.source.Source.activation, frames)}
      && out.source.Source.activation === {activation with Frame.pc = next}
      && Codec.decode next_signature next out.source.Source.cells === Some (out.source.Source.activation, padding)
      && Heap.length out.source.Source.cells === Heap.length cells && Bytes.drop out.source.Source.memory base === Some out.source.Source.bytes
      && Wire.decode_cells (D.S (Heap.length out.source.Source.cells)) out.source.Source.bytes === Some (Heap.Cell (V.Word (Header.number pc), out.source.Source.cells), tail)} @ immutable =
  fun lowered locals table_base stack_base outer program globals heap heap_limit stack_limit signature next_signature activation frames next pc old_pc cells padding state base_local base before_frame tail premise ->
    let local = locals.Emit.status in
    let code = Lower.emit pc base_local in
    let body = Lift.embed code T.Empty in
    let prepared = Status.prepare local (failure ()) body outer state () in
    ghost_ (L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals base_local ());
    let source = Source.correct program globals heap heap_limit stack_limit signature next_signature activation frames next pc old_pc cells padding
      prepared base_local base before_frame tail () in
    let after_body = {X.memory = source.Source.memory; machine = prepared.X.machine} in
    ghost_ (L.can_set_def after_body.X.machine.E.locals local (S.I32 (Status.zero ()));
      S.same_type_def (S.I32 (failure ())) (S.I32 (Status.zero ())));
    let after = Status.straight local (failure ()) code outer state prepared after_body () in
    ghost_ (Wasm_local_replace_compose.compose state.X.machine.E.locals prepared.X.machine.E.locals after.X.machine.E.locals local (S.I32 (failure ())) (S.I32 (Status.zero ())) ();
      failure_def (); Emit.emit_def lowered (Block.Structured (Structured.Straight (Straight.Simple (Hmc_wasm_simple_lower.Jump pc)))) locals table_base stack_base;
      Structured.emit_def (Structured.Straight (Straight.Simple (Hmc_wasm_simple_lower.Jump pc))) locals.Emit.structured 2;
      Hmc_wasm_simple_lower.emit_def (Hmc_wasm_simple_lower.Jump pc) base_local;
      Straight.emit_def (Straight.Simple (Hmc_wasm_simple_lower.Jump pc)) base_local);
    {source; state = after; fuel = Status.cost (C.length code)}
