module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module I = Hmc_tail_ir
module F = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Model = Hmc_frame_call_entry
module Decode = Hmc_frame_call_decode
module Copy = Hmc_wasm_call_captures
module Frame = Hmc_wasm_dynamic_call_frame
module Layout = Hmc_wasm_call_header_layout
module Read = Hmc_wasm_closure_read
module Image = Hmc_heap_image
module Above = Hmc_heap_image_suffix
module Machine = Hmc_heap_machine
module Index = Hmc_u32_index
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Plans = Hmc_wasm_call_plan_table
module Select = Hmc_wasm_call_plan_select
module Local_select = Wasm_local_select
module Entry = Hmc_wasm_dynamic_call_entry
module T = Wasm_control
module Code = Wasm_code
module Lift = Wasm_control_lift
module Fuel = Wasm_control_compose
module Descriptor = Hmc_runtime_closures
module Table = Hmc_runtime_descriptor_table
module Slots = Hmc_wasm_descriptor_load
module Descriptor_select = Hmc_wasm_descriptor_select
module Preserve = Hmc_wasm_descriptor_preserve
module Separate = Hmc_wasm_selected_call_entry
module Enter = Hmc_wasm_call_plan_enter
module Dispatch = Hmc_wasm_call_dispatch
module Target = Hmc_wasm_call_target
module Target_preserve = Hmc_wasm_call_target_preserve
module Bound = Hmc_runtime_descriptor_bound
module Capture = Hmc_wasm_cons_capture
module Source = Hmc_heap_frame
module G = Hmc_cfg_ir
module Position = Hmc_wasm_simple_lower
let[@def] (separate @ total) (capture : Capture.slots @ immutable) (object_local : B.u32) (code_local : B.u32) (local : B.u32) =
  Capture.separate capture local && object_local <> local && code_local <> local
let[@def] (emit @ total) (env_count : Position.slot) (capture : Capture.slots @ immutable)
    (plans : Plans.table @ immutable) (table_base : B.u32) (code_local : B.u32) (address_local : B.u32)
    (slots : Slots.slots @ immutable) (object_local : B.u32) (frame_local : B.u32) =
  Lift.embed (Target.emit env_count capture frame_local object_local code_local)
    (Dispatch.emit plans table_base code_local address_local slots object_local frame_local capture.Capture.tail_tag capture.Capture.tail_payload)
type result = {entry : Dispatch.result; fuel : Code.count; pc : W.limb}
let (correct_preserving @ total) : (keep : B.u32) -> (program : I.program) @ immutable -> (heap : Heap.heap) @ immutable ->
    (entry : K.entry) @ immutable -> (function_ : C.function_entry) @ immutable -> (id : D.index) @ immutable ->
    (closure : B.u32) -> (captures : Heap.cells) @ immutable -> (signature : G.signature) @ immutable -> (activation : Source.activation) @ immutable ->
    (context : D.context) @ immutable -> (ty : D.mono) @ immutable -> (schema : G.temporaries) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (old_pc : W.limb) -> (env_count : Position.slot) ->
    (capture : Capture.slots) @ immutable -> (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    (count : Hmc_wasm_relayout.count) -> (plans : Plans.table) @ immutable -> (code_local : B.u32) -> (labels : T.labels) @ immutable -> (capacity : Hmc_wasm_relayout.count) -> (runtime : Descriptor.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    (address_local : B.u32) -> (slots : Slots.slots) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (stop : B.u32) -> (limit : B.u32) ->
    (object_local : B.u32) -> (frame_local : B.u32) ->
    {u : unit | Separate.separate slots address_local keep
      && separate capture object_local code_local keep
      && Descriptor.related program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions runtime
      && Table.related runtime state.X.memory table_base table_count && table_base + 32 * table_count <= base
      && Codec.decode signature activation.Source.pc cells === Some (activation, padding)
      && signature.G.temporaries === G.Value (context, ty, schema)
      && (match activation.Source.temporaries with Source.Value (value, _, _) -> value === V.Closure_pointer closure | _ -> false)
      && Index.represents activation.Source.pc old_pc && Index.represents (Codec.locals_size signature.G.locals) env_count
      && base + 48 + 16 * env_count <= 4294967280
      && Bytes.drop state.X.memory base === Some bytes
      && Wire.decode_cells (D.S (Heap.length cells)) bytes === Some (Heap.Cell (V.Word (Header.number old_pc), cells), suffix)
      && object_local <> frame_local && object_local <> code_local && code_local <> frame_local
      && Capture.distinct capture && Capture.separate capture frame_local && Capture.separate capture object_local && Capture.separate capture code_local
      && Capture.writable capture state.X.machine.E.locals
      && separate capture object_local code_local address_local && separate capture object_local code_local slots.Slots.start
      && separate capture object_local code_local slots.Slots.captures && separate capture object_local code_local slots.Slots.recursive
      && (match L.get state.X.machine.E.locals address_local with Some (S.I32 _) -> true | _ -> false)
      && Slots.distinct slots address_local && Slots.writable slots state.X.machine.E.locals
      && Separate.separate slots address_local code_local && Separate.separate slots address_local object_local
      && Separate.separate slots address_local frame_local && Separate.separate slots address_local capture.Capture.tail_tag
      && Separate.separate slots address_local capture.Capture.tail_payload
      && Heap.valid program.I.origin.C.origin.Hmc_closure_program.table heap && Image.related state.X.memory heap && Above.above heap stop
      && Hmc_heap_preservation.lookup_object heap closure === Some (Heap.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some function_
      && Codec.environment entry.K.captured captures && Index.represents (Heap.length captures) count
      && Plans.related program.I.origin.C.origin.Hmc_closure_program.table capacity plans
      && (match L.get state.X.machine.E.locals code_local with Some (S.I32 _) -> true | _ -> false)
      && state.X.machine.E.stack === S.Empty
      && stop = base + Layout.width entry.K.recursive + 16 * count && stop <= limit
      && Hmc_linear_bounds.covers state.X.memory limit
      && (match L.get state.X.machine.E.locals object_local with Some (S.I32 _) -> true | _ -> false)
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 base)
} ->
    {out : result | Wasm_empty_labels.related out.entry.Dispatch.call.Enter.selected.Select.cleanup labels out.entry.Dispatch.call.Enter.selected.Select.selected.T.labels
      && L.get out.entry.Dispatch.locals keep === L.get state.X.machine.E.locals keep
      && Index.represents function_.C.start out.pc
      && Machine.invoke program heap (V.Closure_pointer closure) activation.Source.accumulator === Some out.entry.Dispatch.call.Enter.entry.Entry.entered
      && Table.related runtime out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory table_base table_count
      && Hmc_linear_preservation.equal_prefix base state.X.memory out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory
      && Image.related out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory heap && Hmc_linear_bounds.covers out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory limit
      && V.length out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory === V.length state.X.memory
      && out.entry.Dispatch.call.Enter.entry.Entry.entered === Model.activation entry function_.C.start (V.Closure_pointer closure) activation.Source.accumulator captures
      && Codec.decode (Model.signature entry) function_.C.start out.entry.Dispatch.call.Enter.entry.Entry.cells === Some (out.entry.Dispatch.call.Enter.entry.Entry.entered, Heap.Empty)
      && Bytes.drop out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory base === Some out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.bytes
      && Wire.decode_cells (Heap.length (Heap.Cell (V.Word (Header.number out.pc), out.entry.Dispatch.call.Enter.entry.Entry.cells)))
        out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.bytes ===
        Some (Heap.Cell (V.Word (Header.number out.pc), out.entry.Dispatch.call.Enter.entry.Entry.cells), out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.suffix)
      && Bytes.drop out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory stop === Bytes.drop state.X.memory stop
      && T.run out.fuel
        {T.code = emit env_count capture plans table_base code_local address_local slots object_local frame_local; labels; state}
        === T.Running {T.code = T.Empty; labels = out.entry.Dispatch.call.Enter.selected.Select.selected.T.labels;
          state = {X.memory = out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory; machine = {E.locals = out.entry.Dispatch.locals; stack = S.Empty}}}} @ immutable =
  fun keep program heap entry function_ id closure captures signature activation context ty schema cells padding old_pc env_count capture bytes suffix count plans code_local labels capacity runtime table_base table_count address_local slots state base stop limit object_local frame_local premise ->
    let target = Target.correct program.I.origin.C.origin.Hmc_closure_program.table heap id captures count signature activation context ty schema closure cells padding old_pc env_count
      state base frame_local object_local code_local capture bytes suffix () in
    let middle = {X.memory = state.X.memory; machine = {E.locals = target.Target.locals; stack = S.Empty}} in
    ghost_ (separate_def capture object_local code_local keep;
      Target_preserve.correct env_count capture frame_local object_local code_local state middle keep ();
      Descriptor.lookup_correct program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions runtime id target.Target.code ());
    match Descriptor.lookup runtime target.Target.code with
    | None -> unreachable_ ()
    | Some descriptor ->
      ghost_ (Descriptor.describes_def descriptor entry function_;
        Bound.correct runtime state.X.memory table_base table_count target.Target.code descriptor ();
        separate_def capture object_local code_local address_local; separate_def capture object_local code_local slots.Slots.start;
        separate_def capture object_local code_local slots.Slots.captures; separate_def capture object_local code_local slots.Slots.recursive;
        Target_preserve.correct env_count capture frame_local object_local code_local state middle address_local ();
        Target_preserve.correct env_count capture frame_local object_local code_local state middle slots.Slots.start ();
        Target_preserve.correct env_count capture frame_local object_local code_local state middle slots.Slots.captures ();
        Target_preserve.correct env_count capture frame_local object_local code_local state middle slots.Slots.recursive ();
        Slots.writable_def slots state.X.machine.E.locals; Slots.writable_def slots target.Target.locals;
        Slots.limb_slot_def state.X.machine.E.locals slots.Slots.start; Slots.limb_slot_def target.Target.locals slots.Slots.start;
        Slots.limb_slot_def state.X.machine.E.locals slots.Slots.captures; Slots.limb_slot_def target.Target.locals slots.Slots.captures;
        Slots.limb_slot_def state.X.machine.E.locals slots.Slots.recursive; Slots.limb_slot_def target.Target.locals slots.Slots.recursive);
      let entered = Dispatch.correct_preserving keep program heap entry function_ id closure captures activation.Source.accumulator count plans target.Target.code code_local labels capacity
        runtime table_base table_count address_local slots descriptor middle base stop limit object_local frame_local capture.Capture.tail_tag capture.Capture.tail_payload () in
      let setup = Target.emit env_count capture frame_local object_local code_local in
      let fuel = Fuel.add (Code.length setup) entered.Dispatch.fuel in
      ghost_ (Wasm_control_success.straight setup state middle ();
        Lift.correct setup (Dispatch.emit plans table_base code_local address_local slots object_local frame_local capture.Capture.tail_tag capture.Capture.tail_payload) labels state middle ();
        Fuel.correct (Code.length setup) entered.Dispatch.fuel {T.code = emit env_count capture plans table_base code_local address_local slots object_local frame_local; labels; state};
        emit_def env_count capture plans table_base code_local address_local slots object_local frame_local);
      {entry = entered; fuel; pc = descriptor.Descriptor.start}

let (correct @ total) : (program : I.program) @ immutable -> (heap : Heap.heap) @ immutable ->
    (entry : K.entry) @ immutable -> (function_ : C.function_entry) @ immutable -> (id : D.index) @ immutable ->
    (closure : B.u32) -> (captures : Heap.cells) @ immutable -> (signature : G.signature) @ immutable -> (activation : Source.activation) @ immutable ->
    (context : D.context) @ immutable -> (ty : D.mono) @ immutable -> (schema : G.temporaries) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable -> (old_pc : W.limb) -> (env_count : Position.slot) ->
    (capture : Capture.slots) @ immutable -> (bytes : B.bytes) @ immutable -> (suffix : B.bytes) @ immutable ->
    (count : Hmc_wasm_relayout.count) -> (plans : Plans.table) @ immutable -> (code_local : B.u32) -> (labels : T.labels) @ immutable -> (capacity : Hmc_wasm_relayout.count) -> (runtime : Descriptor.table) @ immutable -> (table_base : B.u32) -> (table_count : Table.count) ->
    (address_local : B.u32) -> (slots : Slots.slots) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (stop : B.u32) -> (limit : B.u32) ->
    (object_local : B.u32) -> (frame_local : B.u32) ->
    {u : unit | Descriptor.related program.I.origin.C.origin.Hmc_closure_program.table program.I.origin.C.functions runtime
      && Table.related runtime state.X.memory table_base table_count && table_base + 32 * table_count <= base
      && Codec.decode signature activation.Source.pc cells === Some (activation, padding)
      && signature.G.temporaries === G.Value (context, ty, schema)
      && (match activation.Source.temporaries with Source.Value (value, _, _) -> value === V.Closure_pointer closure | _ -> false)
      && Index.represents activation.Source.pc old_pc && Index.represents (Codec.locals_size signature.G.locals) env_count
      && base + 48 + 16 * env_count <= 4294967280
      && Bytes.drop state.X.memory base === Some bytes
      && Wire.decode_cells (D.S (Heap.length cells)) bytes === Some (Heap.Cell (V.Word (Header.number old_pc), cells), suffix)
      && object_local <> frame_local && object_local <> code_local && code_local <> frame_local
      && Capture.distinct capture && Capture.separate capture frame_local && Capture.separate capture object_local && Capture.separate capture code_local
      && Capture.writable capture state.X.machine.E.locals
      && separate capture object_local code_local address_local && separate capture object_local code_local slots.Slots.start
      && separate capture object_local code_local slots.Slots.captures && separate capture object_local code_local slots.Slots.recursive
      && (match L.get state.X.machine.E.locals address_local with Some (S.I32 _) -> true | _ -> false)
      && Slots.distinct slots address_local && Slots.writable slots state.X.machine.E.locals
      && Separate.separate slots address_local code_local && Separate.separate slots address_local object_local
      && Separate.separate slots address_local frame_local && Separate.separate slots address_local capture.Capture.tail_tag
      && Separate.separate slots address_local capture.Capture.tail_payload
      && Heap.valid program.I.origin.C.origin.Hmc_closure_program.table heap && Image.related state.X.memory heap && Above.above heap stop
      && Hmc_heap_preservation.lookup_object heap closure === Some (Heap.Closure (id, captures))
      && K.lookup program.I.origin.C.origin.Hmc_closure_program.table id === Some entry
      && C.lookup program.I.origin.C.functions id === Some function_
      && Codec.environment entry.K.captured captures && Index.represents (Heap.length captures) count
      && Plans.related program.I.origin.C.origin.Hmc_closure_program.table capacity plans
      && (match L.get state.X.machine.E.locals code_local with Some (S.I32 _) -> true | _ -> false)
      && state.X.machine.E.stack === S.Empty
      && stop = base + Layout.width entry.K.recursive + 16 * count && stop <= limit
      && Hmc_linear_bounds.covers state.X.memory limit
      && (match L.get state.X.machine.E.locals object_local with Some (S.I32 _) -> true | _ -> false)
      && L.get state.X.machine.E.locals frame_local === Some (S.I32 base)
} ->
    {out : result | Wasm_empty_labels.related out.entry.Dispatch.call.Enter.selected.Select.cleanup labels out.entry.Dispatch.call.Enter.selected.Select.selected.T.labels
      && Index.represents function_.C.start out.pc
      && Machine.invoke program heap (V.Closure_pointer closure) activation.Source.accumulator === Some out.entry.Dispatch.call.Enter.entry.Entry.entered
      && Table.related runtime out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory table_base table_count
      && Hmc_linear_preservation.equal_prefix base state.X.memory out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory
      && Image.related out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory heap && Hmc_linear_bounds.covers out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory limit
      && V.length out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory === V.length state.X.memory
      && out.entry.Dispatch.call.Enter.entry.Entry.entered === Model.activation entry function_.C.start (V.Closure_pointer closure) activation.Source.accumulator captures
      && Codec.decode (Model.signature entry) function_.C.start out.entry.Dispatch.call.Enter.entry.Entry.cells === Some (out.entry.Dispatch.call.Enter.entry.Entry.entered, Heap.Empty)
      && Bytes.drop out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory base === Some out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.bytes
      && Wire.decode_cells (Heap.length (Heap.Cell (V.Word (Header.number out.pc), out.entry.Dispatch.call.Enter.entry.Entry.cells)))
        out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.bytes ===
        Some (Heap.Cell (V.Word (Header.number out.pc), out.entry.Dispatch.call.Enter.entry.Entry.cells), out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.suffix)
      && Bytes.drop out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory stop === Bytes.drop state.X.memory stop
      && T.run out.fuel
        {T.code = emit env_count capture plans table_base code_local address_local slots object_local frame_local; labels; state}
        === T.Running {T.code = T.Empty; labels = out.entry.Dispatch.call.Enter.selected.Select.selected.T.labels;
          state = {X.memory = out.entry.Dispatch.call.Enter.entry.Entry.frame.Frame.memory; machine = {E.locals = out.entry.Dispatch.locals; stack = S.Empty}}}} @ immutable =
  fun program heap entry function_ id closure captures signature activation context ty schema cells padding old_pc env_count capture bytes suffix count plans code_local labels capacity runtime table_base table_count address_local slots state base stop limit object_local frame_local premise ->
    ghost_ (separate_def capture object_local code_local frame_local);
    correct_preserving frame_local program heap entry function_ id closure captures signature activation context ty schema cells padding old_pc env_count capture bytes suffix count plans code_local labels capacity runtime table_base table_count address_local slots state base stop limit object_local frame_local ()
