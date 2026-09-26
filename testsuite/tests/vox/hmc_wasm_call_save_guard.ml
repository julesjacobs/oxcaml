module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module V = Hmc_tagged_cell
module R = Hmc_wasm_relayout
module Index = Hmc_u32_index
module Codec = Hmc_pointer_frame_codec
module Save = Hmc_wasm_call_save
module Cells = Hmc_wasm_call_save_memory
module Padded = Hmc_wasm_call_save_padded
module Finish = Hmc_wasm_frame_pad_finish
module Pad = Hmc_wasm_frame_padding
module Slice = Hmc_frame_call_slices
module Reads = Hmc_wasm_call_save_reads
module Write = Wasm_mixed_write
module Seg = Hmc_frame_segments
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module P = Hmc_linear_preservation
module Source = Hmc_wasm_call_save_source
module K = Hmc_closure_ir
module Image = Hmc_heap_image
module Runtime = Hmc_runtime_closures
module Table = Hmc_runtime_descriptor_table
module Transport = Hmc_wasm_frame_transport
module Regions = Hmc_wasm_call_save_regions
module Saved = Hmc_memory_saved_frame
module Saved_frame = Hmc_wasm_saved_frame
module Cap = Hmc_frame_capacity
module Loaded = Hmc_wasm_call_save_loaded
module Push = Hmc_wasm_stack_push
module Stack = Hmc_memory_stack
module Q = Hmc_heap_state
module Extent = Hmc_heap_extent
module Body = Hmc_wasm_call_save_push
module Guard = Hmc_wasm_stack_guard
module Capacity = Hmc_memory_stack_capacity
module Exit = Hmc_wasm_allocation_exit
module Continue = Wasm_control_branch_continue
module T = Wasm_control
let[@def] (body @ total) (fragment : Save.fragment @ immutable) (padding : Write.writes @ immutable)
    (source_local : B.u32) (top_local : B.u32) (width : B.u32) =
  Push.emit (Padded.emit fragment padding source_local top_local) width top_local
let[@def] (emit @ total) (fragment : Save.fragment @ immutable) (padding : Write.writes @ immutable)
    (source_local : B.u32) (top_local : B.u32) (width : B.u32) (limit_local : B.u32) (depth : B.u32) (tail : T.code @ immutable) =
  Exit.emit width top_local limit_local (body fragment padding source_local top_local width) depth tail
let[@def] (cost @ total) (fragment : Save.fragment @ immutable) (padding : Write.writes @ immutable)
    (source_local : B.u32) (top_local : B.u32) (width : B.u32) (limit_local : B.u32) (top : B.u32) (limit : B.u32) =
  Exit.cost width top_local limit_local (body fragment padding source_local top_local width) top limit
let (correct @ total) : (frames : Q.frames) @ immutable -> (stack_base : B.u32) -> (width : B.u32) -> (stack_capacity : D.index) @ immutable -> (stack_limit : B.u32) -> (limit_local : B.u32) -> (depth : B.u32) -> (tail : T.code) @ immutable -> (outer : T.labels) @ immutable -> (blocks : G.table) @ immutable -> (block : G.block) @ immutable -> (stored_capacity : R.count) -> (table : K.table) @ immutable -> (heap : H.heap) @ immutable ->
    (runtime : Runtime.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    (frame_count : R.count) -> (frame_stop : B.u32) -> (signature : G.signature) @ immutable -> (activation : F.activation) @ immutable ->
    (cells : H.cells) @ immutable -> (old_padding : H.cells) @ immutable -> (context : D.context) @ immutable ->
    (ty : D.mono) @ immutable -> (schema : G.temporaries) @ immutable -> (next : D.index) @ immutable ->
    (env_count : R.count) -> (count : R.count) -> (old_pc : B.u32) -> (fragment : Save.fragment) @ immutable -> (capacity : R.count) ->
    (padding : Write.writes) @ immutable -> (padding_count : R.count) -> (padding_length : D.index) @ immutable ->
    (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (limit : B.u32) ->
    (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable -> (source_local : B.u32) -> (base_local : B.u32) ->
    {u : unit | width > 0 && stack_base <= base && stack_limit <= limit && width = 48 + 16 * count + 16 * padding_count
      && Extent.span (Saved.slots blocks) (Stack.zero ()) width
      && Capacity.region width stack_capacity stack_base stack_limit
      && state.X.machine.E.stack === S.Empty && L.get state.X.machine.E.locals limit_local === Some (S.I32 stack_limit) && Stack.related blocks width state.X.memory stack_base base frames
      && G.lookup blocks next === Some block && block.G.signature.G.locals === context && block.G.signature.G.temporaries === schema
      && Index.represents (Cap.capacity blocks) stored_capacity && stored_capacity = 2 + count + padding_count
      && H.valid table heap && Image.related state.X.memory heap && (H.used heap <= base || Hmc_heap_image_suffix.above heap stack_limit)
      && Table.related runtime state.X.memory table_base table_count && table_base + 32 * table_count <= base
      && Index.represents (H.length cells) frame_count && frame_stop = source + 16 + 16 * frame_count && frame_stop <= base
      && signature.G.temporaries === G.Value (context, ty, schema)
      && Codec.decode signature activation.F.pc cells === Some (activation, old_padding)
      && Index.represents activation.F.pc old_pc && Save.matches signature next capacity fragment
      && Index.represents (Codec.locals_size signature.G.locals) env_count
      && Index.represents (D.add (Codec.locals_size context) (Codec.temporaries_size schema)) count
      && 3 + env_count + count <= 268435452 && source + 64 + 16 * env_count + 16 * count <= 4294967296
      && Index.represents padding_length padding_count && 3 + count + padding_count <= 268435452
      && Pad.matches padding (3 + count) padding_length
      && Bounds.covers state.X.memory limit && Bytes.drop state.X.memory source === Some bytes
      && Wire.decode_cells (D.S (H.length cells)) bytes === Some (H.Cell (V.Word (Header.number old_pc), cells), suffix)
      && L.get state.X.machine.E.locals source_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)} ->
    {result : Body.result option | match result with
      | None -> base + width > stack_limit && not (D.present stack_capacity (Q.depth frames))
        && T.run (cost fragment padding source_local base_local width limit_local base stack_limit)
          {T.code = emit fragment padding source_local base_local width limit_local depth tail; labels = outer; state}
          === T.branch depth (Continue.labels tail outer) state
      | Some out -> base + width <= stack_limit && D.present stack_capacity (Q.depth frames)
        && T.run (cost fragment padding source_local base_local width limit_local base stack_limit)
          {T.code = emit fragment padding source_local base_local width limit_local depth tail; labels = outer; state}
          === T.Running {T.code = tail; labels = outer; state = {X.memory = out.Body.save.Loaded.saved.Source.written.Finish.memory;
            machine = {E.locals = out.Body.locals; stack = S.Empty}}}
        &&  Stack.related blocks width out.save.Loaded.saved.Source.written.Finish.memory stack_base (S.add32 base width)
        (Q.Frame (out.save.Loaded.saved.Source.view.Slice.saved, frames))
      && L.replaced state.X.machine.E.locals base_local (S.I32 (S.add32 base width)) out.locals
      && L.get out.locals base_local === Some (S.I32 (S.add32 base width))
      && Saved.load blocks out.save.Loaded.saved.Source.written.Finish.memory base === Some out.save.Loaded.saved.Source.view.Slice.saved
      && H.length out.save.Loaded.saved.Source.view.Slice.remaining === D.add (Codec.locals_size context) (Codec.temporaries_size schema)
      && Codec.decode (Hmc_frame_call_save.signature context schema) next
        (H.Cell (activation.F.current, H.Cell (activation.F.accumulator, out.save.Loaded.saved.Source.view.Slice.remaining))) === Some (out.save.Loaded.saved.Source.view.Slice.saved, H.Empty)
      && X.run (Push.emit (Padded.emit fragment padding source_local base_local) width base_local) state === X.Done {X.memory = out.save.Loaded.saved.Source.written.Finish.memory; machine = {E.locals = out.locals; stack = state.X.machine.E.stack}}
      && activation.F.temporaries === F.Value (out.save.Loaded.saved.Source.view.Slice.closure, out.save.Loaded.saved.Source.view.Slice.saved.F.env, out.save.Loaded.saved.Source.view.Slice.saved.F.temporaries)
      && out.save.Loaded.saved.Source.view.Slice.saved.F.pc === next && out.save.Loaded.saved.Source.view.Slice.saved.F.current === activation.F.current && out.save.Loaded.saved.Source.view.Slice.saved.F.accumulator === activation.F.accumulator
      && Bytes.drop out.save.Loaded.saved.Source.written.Finish.memory base === Some out.save.Loaded.saved.Source.written.Finish.bytes
      && Wire.decode_cells (H.length (Seg.append (Cells.cells fragment.Save.pc activation.F.current activation.F.accumulator out.save.Loaded.saved.Source.view.Slice.remaining) (Pad.cells padding_length))) out.save.Loaded.saved.Source.written.Finish.bytes ===
        Some (Seg.append (Cells.cells fragment.Save.pc activation.F.current activation.F.accumulator out.save.Loaded.saved.Source.view.Slice.remaining) (Pad.cells padding_length), out.save.Loaded.saved.Source.written.Finish.suffix)
      && Codec.decode (Hmc_frame_call_save.signature context schema) next
        (Seg.append (H.Cell (activation.F.current, H.Cell (activation.F.accumulator, out.save.Loaded.saved.Source.view.Slice.remaining))) (Pad.cells padding_length)) === Some (out.save.Loaded.saved.Source.view.Slice.saved, Pad.cells padding_length)
      && P.equal_prefix base state.X.memory out.save.Loaded.saved.Source.written.Finish.memory
      && Image.related out.save.Loaded.saved.Source.written.Finish.memory heap
      && Table.related runtime out.save.Loaded.saved.Source.written.Finish.memory table_base table_count
      && Bytes.drop out.save.Loaded.saved.Source.written.Finish.memory source === Some out.save.Loaded.active.Transport.bytes
      && Wire.decode_cells (D.S (H.length cells)) out.save.Loaded.active.Transport.bytes === Some (H.Cell (V.Word (Header.number old_pc), cells), out.save.Loaded.active.Transport.tail)
      && V.length out.save.Loaded.saved.Source.written.Finish.memory === V.length state.X.memory && Bounds.covers out.save.Loaded.saved.Source.written.Finish.memory limit} @ immutable =
  fun frames stack_base width stack_capacity stack_limit limit_local depth tail outer blocks block stored_capacity table heap runtime table_base table_count frame_count frame_stop signature activation cells old_padding context ty schema next env_count count old_pc fragment capacity padding padding_count padding_length state source base limit bytes suffix source_local base_local premise ->
    ghost_ (Guard.fits blocks width stack_base base stack_limit stack_capacity frames state.X.memory ();
      body_def fragment padding source_local base_local width;
      emit_def fragment padding source_local base_local width limit_local depth tail;
      cost_def fragment padding source_local base_local width limit_local base stack_limit);
    if base + width > stack_limit then (
      ghost_ (Guard.correct blocks width stack_base base stack_limit stack_capacity frames (body fragment padding source_local base_local width)
        base_local limit_local depth tail outer state state ());
      None)
    else (
      let stack_stop : B.u32 = base + width in
      ghost_ (Hmc_wasm_call_save_memory.extent_def count; Hmc_wasm_padding_memory.offset_def padding_count;
        S.add32_def (48 + 16 * count) (16 * padding_count); S.add32_def base width;
        if H.used heap <= base then () else Hmc_wasm_call_save_regions.above_lower heap stack_limit stack_stop ();
        Stack.zero_def (); Capacity.translate (Saved.slots blocks) 0 width base stack_stop ());
      let out = Body.correct frames stack_base width stack_stop blocks block stored_capacity table heap runtime table_base table_count frame_count frame_stop signature activation cells old_padding context ty schema next env_count count old_pc fragment capacity
        padding padding_count padding_length state source base limit bytes suffix source_local base_local () in
      let after = {X.memory = out.Body.save.Loaded.saved.Source.written.Finish.memory; machine = {E.locals = out.Body.locals; stack = S.Empty}} in
      ghost_ (S.add32_def base width;
        Guard.correct blocks width stack_base base stack_limit stack_capacity frames (body fragment padding source_local base_local width)
          base_local limit_local depth tail outer state after ());
      Some out)
