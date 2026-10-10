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
module Table = Hmc_wasm_table_lower
module Block = Hmc_wasm_block_lower
module Loop = Hmc_wasm_dispatch_loop
module Control = Wasm_control
type result = Hmc_wasm_primitive_invariant.result
let (correct @ total) : (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next_activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (operation : D.word_operation) @ immutable -> (next : D.index) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable ->
    (table : Table.table) @ immutable -> (outer : Control.labels) @ immutable -> (fragment : Lower.fragment) @ immutable -> (capacity : Relayout.count) -> (max_pc : W.limb) ->
    (old_pc : W.limb) -> (left : W.t) @ immutable -> (right : W.t) @ immutable -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable ->
    (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Lower.matches signature (G.Primitive (operation, next)) capacity max_pc fragment
      && signature.G.temporaries === G.Value (context, D.Word64, schema)
      && activation.Frame.accumulator === V.Word right
      && (match activation.Frame.temporaries with Frame.Value (V.Word word, _, _) -> word === left | _ -> false)
      && Table.lookup table old_pc === Some (Block.Primitive fragment) && state.X.machine.E.stack === S.Empty
      && Index.represents activation.Frame.pc old_pc && Index.represents (Heap.length cells) capacity
      && Model.successor signature operation === Some next_signature
      && Simple.step (G.Primitive (operation, next)) (State.Running (activation, frames)) === State.Running (next_activation, frames)
      && Codec.decode signature activation.Frame.pc cells === Some (activation, old_padding)
      && base + 16 + 16 * capacity <= 4294967296
      && Wasm_locals.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Bytes.drop state.X.memory base === Some before_frame
      && Wire.decode_cells (D.S (Heap.length cells)) before_frame === Some (Heap.Cell (V.Word (Header.number old_pc), cells), tail)} ->
    {out : result | Index.represents next_activation.Frame.pc fragment.Lower.pc
      && Wasm_memory.load out.memory base (Hmc_wasm_pc_update.offset ()) Wasm_memory.W64 === Some (S.I64 (Header.number fragment.Lower.pc))
      && Control.run (Loop.cost table old_pc base_local (Block.Primitive fragment)) (Loop.configuration table base_local outer state)
        === Control.Running (Loop.configuration table base_local outer {X.memory = out.memory; machine = state.X.machine})
      && X.run (Lower.emit fragment base_local) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && Codec.decode next_signature next_activation.Frame.pc out.cells === Some (next_activation, out.padding)
      && Heap.length out.cells === Heap.length cells && Bytes.drop out.memory base === Some out.bytes
      && Wire.decode_cells (D.S (Heap.length out.cells)) out.bytes === Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), out.cells), tail)} @ immutable =
  fun signature next_signature activation next_activation frames operation next context schema table outer fragment capacity max_pc old_pc left right cells old_padding state base_local base before_frame tail premise ->
    ghost_ (Lower.matches_def signature (G.Primitive (operation, next)) capacity max_pc fragment;
      Simple.step_def (G.Primitive (operation, next)) (State.Running (activation, frames)));
    let out = Hmc_wasm_primitive_invariant.correct signature next_signature activation next_activation frames operation next context schema fragment capacity max_pc old_pc left right
      cells old_padding state base_local base before_frame tail () in
    ghost_ (Hmc_wasm_frame_pc.correct out.memory base out.bytes fragment.Lower.pc out.cells tail ();
      Hmc_wasm_frame_pc.correct state.X.memory base before_frame old_pc cells tail ();
      Block.emit_def (Block.Primitive fragment) base_local;
      Loop.correct table old_pc base_local base (Block.Primitive fragment) outer state {X.memory = out.memory; machine = state.X.machine} ());
    out
