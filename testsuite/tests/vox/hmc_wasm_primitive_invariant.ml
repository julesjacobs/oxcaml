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
type result = {memory : B.bytes; cells : Heap.cells; padding : Heap.cells; bytes : B.bytes}
let (correct @ total) : (signature : G.signature) @ immutable -> (next_signature : G.signature) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next_activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (operation : D.word_operation) @ immutable -> (next : D.index) @ immutable ->
    (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable -> (fragment : Lower.fragment) @ immutable -> (capacity : Relayout.count) -> (max_pc : W.limb) ->
    (old_pc : W.limb) -> (left : W.t) @ immutable -> (right : W.t) @ immutable -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable ->
    (state : X.state) @ immutable -> (base_local : B.u32) -> (base : B.u32) -> (before_frame : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Lower.matches signature (G.Primitive (operation, next)) capacity max_pc fragment
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
    {out : result | Hmc_linear_preservation.equal_prefix base state.X.memory out.memory
      && X.run (Lower.emit fragment base_local) state === X.Done {X.memory = out.memory; machine = state.X.machine}
      && Codec.decode next_signature next_activation.Frame.pc out.cells === Some (next_activation, out.padding)
      && Heap.length out.cells === Heap.length cells && Bytes.drop out.memory base === Some out.bytes
      && Wire.decode_cells (D.S (Heap.length out.cells)) out.bytes === Some (Heap.Cell (V.Word (Header.number fragment.Lower.pc), out.cells), tail)} @ immutable =
  fun signature next_signature activation next_activation frames operation next context schema fragment capacity max_pc old_pc left right cells old_padding state base_local base before_frame tail premise ->
    ghost_ (Lower.matches_def signature (G.Primitive (operation, next)) capacity max_pc fragment);
    let view = Hmc_frame_slices.decode signature activation cells old_padding () in
    ghost_ (Codec.decode_temporaries_def signature.G.temporaries view.Hmc_frame_slices.temporaries);
    match view.Hmc_frame_slices.temporaries with
    | Heap.Cell (V.Word _, saved_start) ->
      (match Codec.decode_environment context saved_start with
      | None -> unreachable_ ()
      | Some (saved_env, more) -> (match Codec.decode_temporaries schema more with
        | None -> unreachable_ ()
        | Some (remaining, _) ->
          ghost_ (Codec.decode_temporaries_def (G.Environment (context, schema)) saved_start);
          let saved = Hmc_frame_slices.saved context schema saved_start (Frame.Environment (saved_env, remaining)) old_padding () in
          let env_count = Hmc_wasm_schema_counts.encode (Codec.locals_size signature.G.locals) capacity () in
          let saved_count = Hmc_wasm_schema_counts.encode (Codec.locals_size context) capacity () in
          let old_count = Hmc_wasm_schema_counts.encode (Codec.temporaries_size schema) capacity () in
          ghost_ (Heap.length_def cells; Heap.length_def (Heap.Cell (activation.Frame.accumulator, view.Hmc_frame_slices.body));
            Hmc_wasm_primitive_read.correct state.X.memory base before_frame old_pc activation.Frame.current right left
              view.Hmc_frame_slices.body saved_start tail (Codec.locals_size signature.G.locals) env_count fragment.Lower.left_offset ());
          let values = Seg.append saved.Hmc_frame_slices.saved saved.Hmc_frame_slices.older in
          let target = D.S (D.S (Heap.length values)) in
          ghost_ (Hmc_frame_relayout_patch.replacement_def (G.Restore next) saved.Hmc_frame_slices.saved activation.Frame.accumulator saved.Hmc_frame_slices.older;
            Hmc_wasm_replacement_size.correct (G.Restore next) saved.Hmc_frame_slices.saved saved.Hmc_frame_slices.older activation.Frame.accumulator values
              saved_count old_count fragment.Lower.required ();
            Hmc_cell_capacity.numeric target (Heap.length cells) fragment.Lower.required capacity ());
          let cut = Hmc_cell_capacity.split target cells () in
          let padding = cut.Hmc_cell_capacity.suffix in
          let after_body = Seg.append values padding in
          let value = Simple.primitive operation left right in
          let after_cells = Heap.Cell (activation.Frame.current, Heap.Cell (value, after_body)) in
          let full = Heap.Cell (V.Word (Header.number fragment.Lower.pc), after_cells) in
          let after_frame = Wire.encode_cells full tail in
          let after = Wasm_memory_splice.replace state.X.memory base before_frame after_frame () in
          ghost_ (
            Seg.drop_def target cells;
            Seg.drop_def (D.S (Heap.length values)) (Heap.Cell (activation.Frame.accumulator, view.Hmc_frame_slices.body));
            Heap.length_def cells; Heap.length_def (Heap.Cell (activation.Frame.accumulator, view.Hmc_frame_slices.body));
            Heap.length_def full; Heap.length_def after_cells; Heap.length_def (Heap.Cell (value, after_body));
            Hmc_wasm_primitive_step.correct signature next_signature activation next_activation frames operation next context schema fragment capacity max_pc
              env_count saved_count old_count old_pc left right view.Hmc_frame_slices.body saved_start
              saved.Hmc_frame_slices.saved saved.Hmc_frame_slices.more saved.Hmc_frame_slices.older old_padding padding
              after_body state after base_local base before_frame after_frame tail ();
            Hmc_wasm_range_four.length_append values padding;
            D.add_def target (Heap.length padding); D.add_def (D.S (Heap.length values)) (Heap.length padding));
          {memory = after; cells = after_cells; padding; bytes = after_frame}))
    | _ -> unreachable_ ()
