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
module Finish = Hmc_wasm_frame_pad_finish
module Pad = Hmc_wasm_frame_padding
module Slice = Hmc_frame_call_slices
module Write = Wasm_mixed_write
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module Source = Hmc_wasm_call_save_source
module K = Hmc_closure_ir
module Image = Hmc_heap_image
module Runtime = Hmc_runtime_closures
module Table = Hmc_runtime_descriptor_table
module Transport = Hmc_wasm_frame_transport
module Saved = Hmc_memory_saved_frame
module Cap = Hmc_frame_capacity
module Loaded = Hmc_wasm_call_save_loaded
module Stack = Hmc_memory_stack
module Q = Hmc_heap_state
module Extent = Hmc_heap_extent
module Body = Hmc_wasm_call_save_push
module Capacity = Hmc_memory_stack_capacity
module Continue = Wasm_control_branch_continue
module T = Wasm_control
module C = Hmc_cfg_program
module I = Hmc_tail_ir
module Model = Hmc_frame_call_entry
module Frame = Hmc_wasm_dynamic_call_frame
module Layout = Hmc_wasm_call_header_layout
module Above = Hmc_heap_image_suffix
module Machine = Hmc_heap_machine
module Plans = Hmc_wasm_call_plan_table
module Select = Hmc_wasm_call_plan_select
module Entry = Hmc_wasm_dynamic_call_entry
module Code = Wasm_code
module Fuel = Wasm_control_compose
module Slots = Hmc_wasm_descriptor_load
module Separate = Hmc_wasm_selected_call_entry
module Enter = Hmc_wasm_call_plan_enter
module Dispatch = Hmc_wasm_call_dispatch
module Capture = Hmc_wasm_cons_capture
module Call_Loaded = Hmc_wasm_loaded_call
module Guarded = Hmc_wasm_call_save_guard
module Stack_entry = Hmc_wasm_loaded_call_stack
module Transition = Hmc_heap_call_transition
module Call = Hmc_wasm_ordinary_call
module Labels = Wasm_empty_labels
let[@def] (emit @ total) (fragment : Save.fragment @ immutable) (padding : Write.writes @ immutable)
    (source_local : B.u32) (base_local : B.u32) (width : B.u32) (limit_local : B.u32) (depth : B.u32)
    (env_count : R.count) (capture : Capture.slots @ immutable) (plans : Plans.table @ immutable)
    (table_base : B.u32) (code_local : B.u32) (address_local : B.u32) (slots : Slots.slots @ immutable) (object_local : B.u32) (tail : T.code @ immutable) =
  T.Block (Call.emit fragment padding source_local base_local width limit_local depth env_count capture plans table_base code_local address_local slots object_local, tail)
type result = {call : Call.result; fuel : Code.count}
let (correct @ total) : (frames : Q.frames) @ immutable ->
    (stack_base : B.u32) ->
    (width : B.u32) ->
    (stack_capacity : D.index) @ immutable ->
    (stack_limit : B.u32) ->
    (limit_local : B.u32) ->
    (depth : B.u32) ->
    (outer : T.labels) @ immutable -> (tail : T.code) @ immutable ->
    (blocks : G.table) @ immutable ->
    (block : G.block) @ immutable ->
    (stored_capacity : R.count) ->
    (table : K.table) @ immutable ->
    (heap : H.heap) @ immutable ->
    (runtime : Runtime.table) @ immutable ->
    (table_base : B.u32) ->
    (table_count : Table.count) ->
    (frame_count : R.count) ->
    (frame_stop : B.u32) ->
    (signature : G.signature) @ immutable ->
    (activation : F.activation) @ immutable ->
    (cells : H.cells) @ immutable ->
    (old_padding : H.cells) @ immutable ->
    (context : D.context) @ immutable ->
    (ty : D.mono) @ immutable ->
    (schema : G.temporaries) @ immutable ->
    (next : D.index) @ immutable ->
    (env_count : R.count) ->
    (count : R.count) ->
    (old_pc : B.u32) ->
    (fragment : Save.fragment) @ immutable ->
    (capacity : R.count) ->
    (padding : Write.writes) @ immutable ->
    (padding_count : R.count) ->
    (padding_length : D.index) @ immutable ->
    (state : X.state) @ immutable ->
    (source : B.u32) ->
    (base : B.u32) ->
    (limit : B.u32) ->
    (bytes : B.bytes) @ immutable ->
    (suffix : B.bytes) @ immutable ->
    (source_local : B.u32) ->
    (base_local : B.u32) ->
    (program : I.program) @ immutable ->
    (entry : K.entry) @ immutable ->
    (function_ : C.function_entry) @ immutable ->
    (id : D.index) @ immutable ->
    (closure : B.u32) ->
    (captures : H.cells) @ immutable ->
    (capture : Capture.slots) @ immutable ->
    (capture_count : Hmc_wasm_relayout.count) ->
    (plans : Plans.table) @ immutable ->
    (code_local : B.u32) ->
    (call_capacity : Hmc_wasm_relayout.count) ->
    (address_local : B.u32) ->
    (slots : Slots.slots) @ immutable ->
    (callee_stop : B.u32) ->
    (object_local : B.u32) ->
    (globals : Machine.globals) @ immutable ->
    {u : unit | blocks === program.I.origin.C.blocks && table === program.I.origin.C.origin.Hmc_closure_program.table
      && I.lookup program.I.code activation.F.pc === Some (I.Keep (G.Call next))
      && base_local <> source_local
      && width > 0 && stack_base <= base && stack_limit <= limit && width = 48 + 16 * count + 16 * padding_count
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
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && Call_Loaded.separate capture object_local code_local base_local
      && Separate.separate slots address_local base_local
      && callee_stop <= stack_base && Stack.related program.I.origin.C.blocks width state.X.memory stack_base base frames
      && Runtime.related program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions runtime
      && Table.related runtime state.X.memory table_base table_count && table_base + 32 * table_count <= source
      && (match activation.F.temporaries with F.Value (value, _, _) -> value === V.Closure_pointer closure | _ -> false)
      && Index.represents activation.F.pc old_pc && Index.represents (Codec.locals_size signature.G.locals) env_count
      && source + 48 + 16 * env_count <= 4294967280
      && Bytes.drop state.X.memory source === Some bytes
      && object_local <> source_local && object_local <> code_local && code_local <> source_local
      && Capture.distinct capture && Capture.separate capture source_local && Capture.separate capture object_local && Capture.separate capture code_local
      && Capture.writable capture state.X.machine.E.locals
      && Call_Loaded.separate capture object_local code_local address_local && Call_Loaded.separate capture object_local code_local slots.Slots.start
      && Call_Loaded.separate capture object_local code_local slots.Slots.captures && Call_Loaded.separate capture object_local code_local slots.Slots.recursive
      && (match L.get state.X.machine.E.locals address_local with Some (S.I32 _) -> true | _ -> false)
      && Slots.distinct slots address_local && Slots.writable slots state.X.machine.E.locals
      && Separate.separate slots address_local code_local && Separate.separate slots address_local object_local
      && Separate.separate slots address_local source_local && Separate.separate slots address_local capture.Capture.tail_tag
      && Separate.separate slots address_local capture.Capture.tail_payload
      && H.valid program.I.origin.C.origin.Hmc_closure_program.table heap && Image.related state.X.memory heap && Above.above heap callee_stop
      && Hmc_heap_preservation.lookup_object heap closure === Some (H.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some function_
      && Codec.environment entry.K.captured captures && Index.represents (H.length captures) capture_count
      && Plans.related program.I.origin.C.origin.Hmc_closure_program.table call_capacity plans
      && (match L.get state.X.machine.E.locals code_local with Some (S.I32 _) -> true | _ -> false)
      && state.X.machine.E.stack === S.Empty
      && callee_stop = source + Layout.width entry.K.recursive + 16 * capture_count && callee_stop <= limit
      && Hmc_linear_bounds.covers state.X.memory limit
      && (match L.get state.X.machine.E.locals object_local with Some (S.I32 _) -> true | _ -> false)} ->
    {result : result option | match result with
      | None -> Machine.step program globals limit stack_capacity {Machine.heap; state = Q.Running (activation, frames)} === Machine.Exhausted Machine.Stack
        && T.run (Code.Succ (Guarded.cost fragment padding source_local base_local width limit_local base stack_limit))
          {T.code = emit fragment padding source_local base_local width limit_local depth env_count capture plans table_base code_local address_local slots object_local tail; labels = outer; state}
          === T.branch depth (Continue.labels (Call_Loaded.emit env_count capture plans table_base code_local address_local slots object_local source_local) (Continue.labels tail outer)) state
      | Some out -> Wasm_empty_labels.related out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.selected.Select.cleanup
          (Continue.labels tail outer) out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.selected.Select.selected.T.labels
        && Machine.step program globals limit stack_capacity {Machine.heap; state = Q.Running (activation, frames)} ===
          Machine.Advanced {Machine.heap; state = Q.Running (out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.entered, Q.Frame (out.call.saved.Body.save.Loaded.saved.Source.view.Slice.saved, frames))}
        && Stack.related blocks width out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory stack_base (S.add32 base width) (Q.Frame (out.call.saved.Body.save.Loaded.saved.Source.view.Slice.saved, frames))
        && L.get out.call.callee.Call_Loaded.entry.Dispatch.locals base_local === Some (S.I32 (S.add32 base width))
        && Image.related out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory heap
        && Table.related runtime out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory table_base table_count
        && Bounds.covers out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory limit
        && V.length out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory === V.length state.X.memory
        && Index.represents function_.C.start out.call.callee.Call_Loaded.pc
        && Codec.decode (Model.signature entry) function_.C.start out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.cells ===
          Some (out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.entered, H.Empty)
        && Bytes.drop out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory source ===
          Some out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.bytes
        && Wire.decode_cells (H.length (H.Cell (V.Word (Header.number out.call.callee.Call_Loaded.pc), out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.cells)))
          out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.bytes ===
          Some (H.Cell (V.Word (Header.number out.call.callee.Call_Loaded.pc), out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.cells),
            out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.suffix)
        && T.run out.fuel {T.code = emit fragment padding source_local base_local width limit_local depth env_count capture plans table_base code_local address_local slots object_local tail; labels = outer; state} ===
          T.Running {T.code = tail; labels = outer;
            state = {X.memory = out.call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory; machine = {E.locals = out.call.callee.Call_Loaded.entry.Dispatch.locals; stack = S.Empty}}}} @ immutable =
  fun frames stack_base width stack_capacity stack_limit limit_local depth outer tail blocks block stored_capacity table heap runtime table_base table_count frame_count frame_stop signature activation cells old_padding context ty schema next env_count count old_pc fragment capacity padding padding_count padding_length state source base limit bytes suffix source_local base_local program entry function_ id closure captures capture capture_count plans code_local call_capacity address_local slots callee_stop object_local globals premise ->
    let labels = Continue.labels tail outer in
    let body = Call.emit fragment padding source_local base_local width limit_local depth env_count capture plans table_base code_local address_local slots object_local in
    let start = {T.code = emit fragment padding source_local base_local width limit_local depth env_count capture plans table_base code_local address_local slots object_local tail; labels = outer; state} in
    ghost_ (emit_def fragment padding source_local base_local width limit_local depth env_count capture plans table_base code_local address_local slots object_local tail;
      Continue.labels_def tail outer; T.step_def start; T.enter_def body tail None start; T.stack_def state S.Empty);
    match Call.correct frames stack_base width stack_capacity stack_limit limit_local depth labels blocks block stored_capacity table heap runtime table_base table_count frame_count frame_stop signature activation cells old_padding context ty schema next env_count count old_pc fragment capacity padding padding_count padding_length state source base limit bytes suffix source_local base_local program entry function_ id closure captures capture capture_count plans code_local call_capacity address_local slots callee_stop object_local globals () with
    | None ->
      ghost_ (T.run_def (Code.Succ (Guarded.cost fragment padding source_local base_local width limit_local base stack_limit)) start);
      None
    | Some call ->
      let finished = {X.memory = call.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory;
        machine = {E.locals = call.Call.callee.Call_Loaded.entry.Dispatch.locals; stack = S.Empty}} in
      let selected_labels = call.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.selected.Select.selected.T.labels in
      let cleanup = call.Call.callee.Call_Loaded.entry.Dispatch.call.Enter.selected.Select.cleanup in
      let finish_cost = Fuel.add cleanup (Code.Succ Code.Zero) in
      let total = Fuel.add call.Call.fuel finish_cost in
      ghost_ (Labels.correct cleanup labels selected_labels finished ();
        Fuel.correct cleanup (Code.Succ Code.Zero) {T.code = T.Empty; labels = selected_labels; state = finished};
        T.run_def (Code.Succ Code.Zero) {T.code = T.Empty; labels; state = finished};
        T.step_def {T.code = T.Empty; labels; state = finished}; T.stack_def finished S.Empty;
        T.run_def Code.Zero {T.code = tail; labels = outer; state = finished};
        Fuel.correct call.Call.fuel finish_cost {T.code = body; labels; state};
        T.run_def (Code.Succ total) start);
      Some {call; fuel = Code.Succ total}
