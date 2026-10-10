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
module Model = Hmc_frame_primitive_model
module Wire = Hmc_heap_wire
module Index = Hmc_u32_index
module Lower = Hmc_wasm_primitive_lower
module Relayout = Hmc_wasm_relayout
module Geometry = Hmc_wasm_relayout_geometry
module Header = Hmc_wasm_header_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module Bytes = Hmc_linear_bytes
module Source = Hmc_wasm_primitive_invariant
module Status = Hmc_wasm_program_status
module Emit = Hmc_wasm_program_emit
module Structured = Hmc_wasm_structured_block
module Straight = Hmc_wasm_block_lower
module Block = Hmc_wasm_program_block
module Machine = Hmc_heap_machine
module Program = Hmc_tail_ir
module T = Wasm_control
module C = Wasm_code
module L = Wasm_locals
module Lift = Wasm_control_lift
let[@def] (failure @ total) (unit : unit) : B.u32 = 2
type result = {source : Source.result; state : X.state; fuel : C.count}
let (correct @ total) : (lowered : Hmc_wasm_program_lower.program) @ immutable -> (locals : Emit.locals) @ immutable ->
    (table_base : B.u32) -> (stack_base : B.u32) -> (outer : T.labels) @ immutable -> (program : Program.program) @ immutable ->
    (globals : Machine.globals) @ immutable -> (heap : Heap.heap) @ immutable -> (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next_activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (operation : D.word_operation) @ immutable -> (next : D.index) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable -> (fragment : Lower.fragment) @ immutable -> (capacity : Relayout.count) -> (max_pc : W.limb) ->
    (old_pc : W.limb) -> (left : W.t) @ immutable -> (right : W.t) @ immutable -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable ->
    (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | base_local = locals.Emit.structured.Structured.frame && locals.Emit.status <> base_local
      && state.X.machine.E.stack === S.Empty && L.can_set state.X.machine.E.locals locals.Emit.status (S.I32 (failure ()))
      && Program.lookup program.Program.code activation.Frame.pc === Some (Program.Keep (G.Primitive (operation, next)))
      && Lower.matches signature (G.Primitive (operation, next)) capacity max_pc fragment
      && signature.G.temporaries === G.Value (context, D.Word64, schema)
      && activation.Frame.accumulator === V.Word right
      && (match activation.Frame.temporaries with Frame.Value (V.Word w, _, _) -> w === left | _ -> false)
      && Index.represents activation.Frame.pc old_pc && Index.represents (Heap.length cells) capacity
      && Model.successor signature operation === Some next_signature
      && Simple.step (G.Primitive (operation, next)) (State.Running (activation, frames)) === State.Running (next_activation, frames)
      && Codec.decode signature activation.Frame.pc cells === Some (activation, old_padding)
      && base + 16 + 16 * capacity <= 4294967296
      && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : result | Hmc_linear_preservation.equal_prefix base state.X.memory out.source.Source.memory
      && L.replaced state.X.machine.E.locals locals.Emit.status (S.I32 (Status.zero ())) out.state.X.machine.E.locals
      && out.state.X.memory === out.source.Source.memory && out.state.X.machine.E.stack === S.Empty
      && L.get out.state.X.machine.E.locals locals.Emit.status === Some (S.I32 (Status.zero ()))
      && T.run out.fuel {T.code = Emit.emit lowered (Block.Structured (Structured.Straight (Straight.Primitive fragment))) locals table_base stack_base;
        labels = outer; state} === T.Running {T.code = T.Empty; labels = outer; state = out.state}
      && Machine.step program globals heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)}
        === Machine.Advanced {Machine.heap; state = State.Running (next_activation, frames)}
      && Codec.decode next_signature next_activation.Frame.pc out.source.Source.cells === Some (next_activation, out.source.Source.padding)
      && Heap.length out.source.Source.cells === Heap.length cells && Bytes.drop out.source.Source.memory base === Some out.source.Source.bytes
      && Wire.decode_cells (D.S (Heap.length out.source.Source.cells)) out.source.Source.bytes === Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), out.source.Source.cells), tail)} @ immutable =
  fun lowered locals table_base stack_base outer program globals heap heap_limit stack_limit signature next_signature activation next_activation frames operation next context schema fragment capacity max_pc old_pc left right cells old_padding state base_local base before_frame tail premise ->
    let local = locals.Emit.status in
    let code = Lower.emit fragment base_local in
    let body = Lift.embed code T.Empty in
    let prepared = Status.prepare local (failure ()) body outer state () in
    ghost_ (L.other_local state.X.machine.E.locals local (S.I32 (failure ())) prepared.X.machine.E.locals base_local ());
    let source = Source.correct signature next_signature activation next_activation frames operation next context schema fragment capacity max_pc old_pc left right cells old_padding prepared base_local base before_frame tail () in
    let after_body = {X.memory = source.Source.memory; machine = prepared.X.machine} in
    ghost_ (L.can_set_def after_body.X.machine.E.locals local (S.I32 (Status.zero ()));
      S.same_type_def (S.I32 (failure ())) (S.I32 (Status.zero ())));
    let after = Status.straight local (failure ()) code outer state prepared after_body () in
    ghost_ (Wasm_local_replace_compose.compose state.X.machine.E.locals prepared.X.machine.E.locals after.X.machine.E.locals local (S.I32 (failure ())) (S.I32 (Status.zero ())) ();
      failure_def (); Emit.emit_def lowered (Block.Structured (Structured.Straight (Straight.Primitive fragment))) locals table_base stack_base;
      Structured.emit_def (Structured.Straight (Straight.Primitive fragment)) locals.Emit.structured 2;
      Straight.emit_def (Straight.Primitive fragment) base_local;
      Machine.step_def program globals heap_limit stack_limit {Machine.heap; state = State.Running (activation, frames)});
    {source; state = after; fuel = Status.cost (C.length code)}
