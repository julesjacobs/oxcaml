module B = Wasm_u32
module V = Wasm_static_types
module Q = Wasm_static_sequence
module Check = Wasm_static_control
module T = Wasm_control
module I = Wasm_instruction
module Data = Hmc_wasm_static_call_data
module Plans = Hmc_wasm_call_plan_table
module Table = Hmc_wasm_dispatch_code
module Select = Wasm_local_select
module Slots = Hmc_wasm_descriptor_load
module Capture = Hmc_wasm_cons_capture
let[@def] (after @ total) (plans : Plans.table @ immutable) (state : V.state @ immutable) =
  match plans with Plans.Empty -> V.dead () | Plans.Add _ -> state
let (finished @ total) : (plans : Plans.table) @ immutable ->
    {u : unit | V.finish Wasm_functions.Void (after plans (V.initial ()))} @ ghost =
  fun plans -> ghost_ (
    after_def plans (V.initial ()); V.initial_def (); V.dead_def ();
    V.consume_result_def Wasm_functions.Void (after plans (V.initial ()));
    V.finish_def Wasm_functions.Void (after plans (V.initial ())))
let rec (select @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable -> (plans : Plans.table) @ immutable ->
    (code : B.u32) -> (pc : B.u32) -> (object_ : B.u32) -> (frame : B.u32) -> (tag : B.u32) -> (payload : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals code === Some V.I32 && V.local context.V.locals pc === Some V.I32
      && V.local context.V.locals object_ === Some V.I32 && V.local context.V.locals frame === Some V.I32
      && V.local context.V.locals tag === Some V.I64 && V.local context.V.locals payload === Some V.I64} ->
    {u : unit | Check.check context labels (Select.emit (Plans.prepare plans pc object_ frame tag payload) code) state === Some (after plans state)} @ ghost =
  fun context labels plans code pc object_ frame tag payload state premise -> ghost_ (
    after_def plans state; Plans.prepare_def plans pc object_ frame tag payload;
    Select.emit_def (Plans.prepare plans pc object_ frame tag payload) code;
    match plans with
    | Plans.Empty ->
      Select.trap_def (); V.plain_def I.Unreachable state; V.instruction_def context (I.Plain I.Unreachable) state;
      Check.check_def context labels T.Empty (V.dead ());
      Check.check_def context labels (Select.trap ()) state
    | Plans.Add (number, fragment, rest) ->
      let empty = V.initial () in
      let yes_code = Hmc_wasm_dynamic_call_frame.emit fragment pc object_ frame tag payload in
      let yes = Wasm_control_lift.embed yes_code T.Empty in
      let no = Select.emit (Plans.prepare rest pc object_ frame tag payload) code in
      Data.frame context fragment pc object_ frame tag payload empty ();
      Check.check_def context (Check.Label labels) T.Empty empty;
      Q.embed_checked context (Check.Label labels) yes_code T.Empty empty empty ();
      select context (Check.Label labels) rest code pc object_ frame tag payload empty ();
      finished rest;
      V.initial_def (); V.consume_result_def Wasm_functions.Void empty; V.finish_def Wasm_functions.Void empty;
      Hmc_wasm_static_return.probe context number code state ();
      Wasm_static_steps.take_push V.I32 state;
      Check.check_def context labels T.Empty state;
      Check.check_def context labels (T.If (yes, no, T.Empty)) (V.push V.I32 state);
      Q.embed_checked context labels (Wasm_local_probe.emit number code) (T.If (yes, no, T.Empty)) state (V.push V.I32 state) ())
let (dispatch @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable -> (plans : Plans.table) @ immutable ->
    (base : B.u32) -> (code : B.u32) -> (address : B.u32) -> (slots : Slots.slots) @ immutable ->
    (object_ : B.u32) -> (frame : B.u32) -> (tag : B.u32) -> (payload : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals code === Some V.I32 && V.local context.V.locals address === Some V.I32
      && Data.descriptor_typed context slots && V.local context.V.locals object_ === Some V.I32
      && V.local context.V.locals frame === Some V.I32 && V.local context.V.locals tag === Some V.I64 && V.local context.V.locals payload === Some V.I64} ->
    {u : unit | Check.check context labels (Hmc_wasm_call_dispatch.emit plans base code address slots object_ frame tag payload) state === Some (after plans state)} @ ghost =
  fun context labels plans base code address slots object_ frame tag payload state premise -> ghost_ (
    Data.descriptor context base code address slots state ();
    Data.descriptor_typed_def context slots;
    select context labels plans code slots.Slots.start object_ frame tag payload state ();
    Hmc_wasm_call_plan_select.emit_def plans code slots.Slots.start object_ frame tag payload;
    Q.embed_checked context labels (Hmc_wasm_descriptor_select.emit base code address slots)
      (Hmc_wasm_call_plan_select.emit plans code slots.Slots.start object_ frame tag payload) state state ();
    Hmc_wasm_call_dispatch.emit_def plans base code address slots object_ frame tag payload)
let (loaded @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (environment : Hmc_wasm_simple_lower.slot) -> (capture : Capture.slots) @ immutable -> (plans : Plans.table) @ immutable ->
    (base : B.u32) -> (code : B.u32) -> (address : B.u32) -> (slots : Slots.slots) @ immutable ->
    (object_ : B.u32) -> (frame : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals code === Some V.I32 && V.local context.V.locals address === Some V.I32
      && Data.descriptor_typed context slots && V.local context.V.locals object_ === Some V.I32
      && V.local context.V.locals frame === Some V.I32 && Hmc_wasm_static_cons.slots_typed context capture} ->
    {u : unit | Check.check context labels (Hmc_wasm_loaded_call.emit environment capture plans base code address slots object_ frame) state === Some (after plans state)} @ ghost =
  fun context labels environment capture plans base code address slots object_ frame state premise -> ghost_ (
    Data.target context environment capture frame object_ code state ();
    Hmc_wasm_static_cons.slots_typed_def context capture;
    dispatch context labels plans base code address slots object_ frame capture.Capture.tail_tag capture.Capture.tail_payload state ();
    Q.embed_checked context labels (Hmc_wasm_call_target.emit environment capture frame object_ code)
      (Hmc_wasm_call_dispatch.emit plans base code address slots object_ frame capture.Capture.tail_tag capture.Capture.tail_payload) state state ();
    Hmc_wasm_loaded_call.emit_def environment capture plans base code address slots object_ frame)
module Save = Hmc_wasm_call_save
module Guard = Hmc_wasm_call_save_guard
module Mixed = Wasm_mixed_write
module Memory = Wasm_static_memory
let (save @ total) : (context : V.context) @ immutable -> (fragment : Save.fragment) @ immutable -> (padding : Mixed.writes) @ immutable ->
    (frame : B.u32) -> (top : B.u32) -> (width : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals frame === Some V.I32 && V.local context.V.locals top === Some V.I32 && Memory.mixed_sources context padding} ->
    {u : unit | Q.check context (Guard.body fragment padding frame top width) state === Some state} @ ghost =
  fun context fragment padding frame top width state premise -> ghost_ (
    Wasm_static_copy.cross context fragment.Save.copies frame top state ();
    Hmc_wasm_static_simple.pc context fragment.Save.pc top state ();
    Q.append context (Wasm_cross_copy.emit fragment.Save.copies frame top) (Hmc_wasm_pc_update.emit fragment.Save.pc top) state;
    Save.emit_def fragment frame top;
    Memory.mixed context padding top state ();
    Q.append context (Save.emit fragment frame top) (Mixed.emit padding top) state;
    Hmc_wasm_call_save_padded.emit_def fragment padding frame top;
    Hmc_wasm_static_objects.advance context width top state ();
    Q.append context (Hmc_wasm_call_save_padded.emit fragment padding frame top) (Hmc_wasm_allocation_advance.emit width top) state;
    Hmc_wasm_stack_push.emit_def (Hmc_wasm_call_save_padded.emit fragment padding frame top) width top;
    Guard.body_def fragment padding frame top width)
let (ordinary @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (fragment : Save.fragment) @ immutable -> (padding : Mixed.writes) @ immutable ->
    (frame : B.u32) -> (top : B.u32) -> (width : B.u32) -> (limit : B.u32) -> (depth : B.u32) ->
    (environment : Hmc_wasm_relayout.count) -> (capture : Capture.slots) @ immutable -> (plans : Plans.table) @ immutable ->
    (base : B.u32) -> (code : B.u32) -> (address : B.u32) -> (slots : Slots.slots) @ immutable -> (object_ : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals code === Some V.I32 && V.local context.V.locals address === Some V.I32
      && Data.descriptor_typed context slots && V.local context.V.locals object_ === Some V.I32
      && V.local context.V.locals frame === Some V.I32 && Hmc_wasm_static_cons.slots_typed context capture
      && V.local context.V.locals top === Some V.I32 && V.local context.V.locals limit === Some V.I32
      && Memory.mixed_sources context padding && Check.label (Check.Label labels) depth === Some Wasm_functions.Void} ->
    {u : unit | Check.check context labels (Hmc_wasm_ordinary_call.emit fragment padding frame top width limit depth environment capture plans base code address slots object_) state === Some (after plans state)} @ ghost =
  fun context labels fragment padding frame top width limit depth environment capture plans base code address slots object_ state premise -> ghost_ (
    save context fragment padding frame top width (V.initial ()) ();
    let tail = Hmc_wasm_loaded_call.emit environment capture plans base code address slots object_ frame in
    loaded context labels environment capture plans base code address slots object_ frame state ();
    Hmc_wasm_static_allocation.exit context labels width top limit (Guard.body fragment padding frame top width) depth tail state ();
    Guard.emit_def fragment padding frame top width limit depth tail;
    Hmc_wasm_ordinary_call.emit_def fragment padding frame top width limit depth environment capture plans base code address slots object_)
let rec (padding @ total) : (context : V.context) @ immutable -> (writes : Mixed.writes) @ immutable -> (position : int) -> (count : Hm_declarative.index) @ immutable ->
    {u : unit | Hmc_wasm_frame_padding.matches writes position count} ->
    {u : unit | Memory.mixed_sources context writes} @ ghost =
  fun context writes position count premise -> ghost_ (
    Hmc_wasm_frame_padding.matches_def writes position count;
    Memory.mixed_sources_def context writes;
    match count, writes with
    | Hm_declarative.Z, Mixed.End -> ()
    | Hm_declarative.S rest, Mixed.Write (_, Mixed.Constant a, Mixed.Write (offset, Mixed.Constant b, tail)) ->
      Memory.mixed_source_def context (Mixed.Constant a);
      Memory.mixed_sources_def context (Mixed.Write (offset, Mixed.Constant b, tail));
      Memory.mixed_source_def context (Mixed.Constant b);
      padding context tail (position + 1) rest ()
    | _ -> ())
