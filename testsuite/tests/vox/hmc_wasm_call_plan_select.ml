module B = Wasm_u32
module D = Hm_declarative
module K = Hmc_closure_ir
module R = Hmc_wasm_relayout
module Copy = Hmc_wasm_call_captures
module Frame = Hmc_wasm_dynamic_call_frame
module Plans = Hmc_wasm_call_plan_table
module Select = Wasm_local_select
module Index = Hmc_u32_index
module Lift = Wasm_control_lift
module T = Wasm_control
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
let[@def] (emit @ total) (plans : Plans.table @ immutable) (code_local : B.u32) (pc_local : B.u32) (object_local : B.u32)
    (frame_local : B.u32) (argument_tag : B.u32) (argument_payload : B.u32) =
  Select.emit (Plans.prepare plans pc_local object_local frame_local argument_tag argument_payload) code_local
module Resume = Wasm_local_select_resume
module Labels = Wasm_empty_labels
type result = {fragment : Copy.fragment; selected : T.configuration; cleanup : Wasm_code.count}
let (correct @ total) : (source : K.table) @ immutable -> (capacity : R.count) -> (plans : Plans.table) @ immutable ->
    (id : D.index) @ immutable -> (code : B.u32) -> (entry : K.entry) @ immutable ->
    (code_local : B.u32) -> (pc_local : B.u32) -> (object_local : B.u32) -> (frame_local : B.u32) ->
    (argument_tag : B.u32) -> (argument_payload : B.u32) -> (state : X.state) @ immutable -> (labels : T.labels) @ immutable ->
    {u : unit | Plans.related source capacity plans && Index.represents id code && K.lookup source id === Some entry
      && L.get state.X.machine.E.locals code_local === Some (S.I32 code) && state.X.machine.E.stack === S.Empty} ->
    {out : result | Labels.related out.cleanup labels out.selected.T.labels
      && Copy.matches entry capacity out.fragment
      && out.selected.T.code === Lift.embed (Frame.emit out.fragment pc_local object_local frame_local argument_tag argument_payload) T.Empty
      && out.selected.T.state === state
      && T.run (Select.cost (Plans.prepare plans pc_local object_local frame_local argument_tag argument_payload) code code_local)
        {T.code = emit plans code_local pc_local object_local frame_local argument_tag argument_payload; labels; state} === T.Running out.selected} @ immutable =
  fun source capacity plans id code entry code_local pc_local object_local frame_local argument_tag argument_payload state labels premise ->
    ghost_ (Plans.lookup_correct source capacity plans id code ());
    match Plans.lookup plans code with
    | None -> unreachable_ ()
    | Some fragment ->
      let table = Plans.prepare plans pc_local object_local frame_local argument_tag argument_payload in
      let selected = Select.selection table code code_local labels state in
      ghost_ (Plans.prepared_lookup plans code pc_local object_local frame_local argument_tag argument_payload;
        Select.correct table code code_local labels state ();
        Select.selected_code table code code_local labels state;
        emit_def plans code_local pc_local object_local frame_local argument_tag argument_payload);
      ghost_ (Resume.correct table code code_local labels state);
      {fragment; selected; cleanup = Resume.cost table code}
let (reject @ total) : (plans : Plans.table) @ immutable -> (code : B.u32) ->
    (code_local : B.u32) -> (pc_local : B.u32) -> (object_local : B.u32) -> (frame_local : B.u32) ->
    (argument_tag : B.u32) -> (argument_payload : B.u32) -> (state : X.state) @ immutable -> (labels : T.labels) @ immutable ->
    {u : unit | Plans.lookup plans code === None && L.get state.X.machine.E.locals code_local === Some (S.I32 code)
      && state.X.machine.E.stack === S.Empty} ->
    {u : unit | T.run (Wasm_control_compose.add (Select.cost (Plans.prepare plans pc_local object_local frame_local argument_tag argument_payload) code code_local)
        (Wasm_code.Succ Wasm_code.Zero))
      {T.code = emit plans code_local pc_local object_local frame_local argument_tag argument_payload; labels; state} === T.Trap} @ ghost =
  fun plans code code_local pc_local object_local frame_local argument_tag argument_payload state labels premise -> ghost_ (
    Plans.prepared_lookup plans code pc_local object_local frame_local argument_tag argument_payload;
    Select.reject (Plans.prepare plans pc_local object_local frame_local argument_tag argument_payload) code code_local labels state ();
    emit_def plans code_local pc_local object_local frame_local argument_tag argument_payload)
