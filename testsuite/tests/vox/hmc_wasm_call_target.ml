module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Index = Hmc_u32_index
module Slots = Hmc_wasm_simple_lower
module Capture = Hmc_wasm_cons_capture
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Operands = Hmc_wasm_call_operands
module Code = Hmc_wasm_closure_code
module Image = Hmc_heap_image
let[@def] (emit @ total) (env_count : Slots.slot) (slots : Capture.slots @ immutable) (frame_local : B.u32) (object_local : B.u32) (code_local : B.u32) =
  E.append (Operands.emit env_count slots frame_local object_local) (Code.emit object_local code_local)
type result = {code : W.limb; locals : S.stack}
let (correct @ total) : (table : Hmc_closure_ir.table) @ immutable -> (heap : Heap.heap) @ immutable ->
    (id : D.index) @ immutable -> (captures : Heap.cells) @ immutable -> (count : Hmc_wasm_relayout.count) ->
    (signature : G.signature) @ immutable -> (activation : Frame.activation) @ immutable ->
    (context : D.context) @ immutable -> (ty : D.mono) @ immutable -> (schema : G.temporaries) @ immutable ->
    (closure : B.u32) -> (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    (old_pc : W.limb) -> (env_count : Slots.slot) -> (state : X.state) @ immutable -> (base : B.u32) ->
    (frame_local : B.u32) -> (object_local : B.u32) -> (code_local : B.u32) -> (slots : Capture.slots) @ immutable ->
    (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    {u : unit | Heap.valid table heap && Image.related state.X.memory heap
      && Index.represents (Heap.length captures) count
      && Hmc_heap_preservation.lookup_object heap closure === Some (Heap.Closure (id, captures))
      && code_local <> frame_local
      && (match L.get state.X.machine.E.locals code_local with Some (S.I32 _) -> true | _ -> false)
      && Codec.decode signature activation.Frame.pc cells === Some (activation, padding)
      && signature.G.temporaries === G.Value (context, ty, schema)
      && (match activation.Frame.temporaries with Frame.Value (value, _, _) -> value === V.Closure_pointer closure | _ -> false)
      && Index.represents activation.Frame.pc old_pc && Index.represents (Codec.locals_size signature.G.locals) env_count
      && base + 48 + 16 * env_count <= 4294967280
      && Bytes.drop state.X.memory base === Some bytes
      && Wire.decode_cells (D.S (Heap.length cells)) bytes === Some (Heap.Cell (V.Word (Header.number old_pc), cells), suffix)
      && object_local <> frame_local && object_local <> code_local
      && Capture.distinct slots && Capture.separate slots frame_local && Capture.separate slots object_local && Capture.separate slots code_local
      && (match L.get state.X.machine.E.locals object_local with Some (S.I32 _) -> true | _ -> false)
      && Capture.writable slots state.X.machine.E.locals && L.get state.X.machine.E.locals frame_local === Some (S.I32 base)} ->
    {out : result | Index.represents id out.code
      && X.run (emit env_count slots frame_local object_local code_local) state
        === X.Done {X.memory = state.X.memory; machine = {E.locals = out.locals; stack = state.X.machine.E.stack}}
      && L.get out.locals slots.Capture.head_tag === Some (S.I64 (V.tag (V.Closure_pointer closure))) && L.get out.locals slots.Capture.head_payload === Some (S.I64 (V.payload (V.Closure_pointer closure)))
      && L.get out.locals slots.Capture.tail_tag === Some (S.I64 (V.tag activation.Frame.accumulator))
      && L.get out.locals slots.Capture.tail_payload === Some (S.I64 (V.payload activation.Frame.accumulator))
      && L.get out.locals frame_local === Some (S.I32 base)
      && L.get out.locals object_local === Some (S.I32 closure) && L.get out.locals code_local === Some (S.I32 out.code)} @ immutable =
  fun table heap id captures count signature activation context ty schema closure cells padding old_pc env_count state base frame_local object_local code_local slots bytes suffix premise ->
    let operands = Operands.correct signature activation context ty schema closure cells padding old_pc env_count state base
      frame_local object_local code_local slots bytes suffix () in
    let middle = {X.memory = state.X.memory; machine = {E.locals = operands; stack = state.X.machine.E.stack}} in
    let target = Code.correct table heap middle closure id captures count object_local code_local () in
    ghost_ (Capture.separate_def slots code_local;
      L.other_local operands code_local (S.I32 target.Code.code) target.Code.locals slots.Capture.head_tag ();
      L.other_local operands code_local (S.I32 target.Code.code) target.Code.locals slots.Capture.head_payload ();
      L.other_local operands code_local (S.I32 target.Code.code) target.Code.locals slots.Capture.tail_tag ();
      L.other_local operands code_local (S.I32 target.Code.code) target.Code.locals slots.Capture.tail_payload ();
      L.other_local operands code_local (S.I32 target.Code.code) target.Code.locals frame_local ();
      L.other_local operands code_local (S.I32 target.Code.code) target.Code.locals object_local ();
      emit_def env_count slots frame_local object_local code_local;
      X.append_correct (Operands.emit env_count slots frame_local object_local) (Code.emit object_local code_local) state);
    {code = target.Code.code; locals = target.Code.locals}
