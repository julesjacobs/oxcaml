module B = Wasm_u32
module C = Wasm_code
module E = Wasm_execution
module V = Wasm_static_types
module Q = Wasm_static_sequence
module Memory = Wasm_static_memory
module Read = Wasm_frame_snapshot
module Frame = Wasm_frame_write
module Capture = Hmc_wasm_cons_capture
module Write = Hmc_wasm_cons_write
module Allocate = Hmc_wasm_cons_allocate
module Result = Hmc_wasm_cons_result
module Finish = Hmc_wasm_cons_finish
module Success = Hmc_wasm_cons_success
module Pop = Hmc_wasm_value_pop
module Objects = Hmc_wasm_static_objects
let[@def] rec (destinations @ total) (context : V.context @ immutable) (reads : Read.reads @ immutable) = match reads with
  | Read.End -> true
  | Read.Read (_, destination, rest) -> (match V.local context.V.locals destination with Some V.I64 -> true | _ -> false) && destinations context rest
let rec (snapshot @ total) : (context : V.context) @ immutable -> (reads : Read.reads) @ immutable -> (base : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32 && destinations context reads} ->
    {u : unit | Q.check context (Read.emit reads base) state === Some state} @ ghost =
  fun context reads base state premise -> ghost_ (
    destinations_def context reads; Read.emit_def reads base;
    match reads with
    | Read.End -> Q.check_def context C.Empty state
    | Read.Read (offset, destination, rest) ->
      Memory.ty_def Wasm_memory.W64; Memory.read context Wasm_memory.W64 offset base destination state ();
      snapshot context rest base state ();
      Q.append context (Wasm_memory_lowering.read_code Wasm_memory.W64 offset base destination) (Read.emit rest base) state)
let[@def] (slots_typed @ total) (context : V.context @ immutable) (slots : Capture.slots @ immutable) = ghost_ (
  V.local context.V.locals slots.Capture.head_tag === Some V.I64 && V.local context.V.locals slots.Capture.head_payload === Some V.I64
  && V.local context.V.locals slots.Capture.tail_tag === Some V.I64 && V.local context.V.locals slots.Capture.tail_payload === Some V.I64)
let (capture @ total) : (context : V.context) @ immutable -> (head_tag : B.u32) -> (head_payload : B.u32) ->
    (slots : Capture.slots) @ immutable -> (base : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32 && slots_typed context slots} ->
    {u : unit | Q.check context (Capture.emit head_tag head_payload slots base) state === Some state} @ ghost =
  fun context head_tag head_payload slots base state premise -> ghost_ (
    slots_typed_def context slots; Capture.reads_def head_tag head_payload slots;
    destinations_def context Read.End;
    destinations_def context (Read.Read (40, slots.Capture.tail_payload, Read.End));
    destinations_def context (Read.Read (32, slots.Capture.tail_tag, Read.Read (40, slots.Capture.tail_payload, Read.End)));
    destinations_def context (Read.Read (head_payload, slots.Capture.head_payload, Read.Read (32, slots.Capture.tail_tag, Read.Read (40, slots.Capture.tail_payload, Read.End))));
    destinations_def context (Capture.reads head_tag head_payload slots);
    snapshot context (Capture.reads head_tag head_payload slots) base state ();
    Capture.emit_def head_tag head_payload slots base)
let (write @ total) : (context : V.context) @ immutable -> (slots : Capture.slots) @ immutable -> (heap : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals heap === Some V.I32 && slots_typed context slots} ->
    {u : unit | Q.check context (Write.emit heap slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload) state === Some state} @ ghost =
  fun context slots heap state premise -> ghost_ (
    slots_typed_def context slots;
    Write.writes_def slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload;
    Memory.frame_sources_def context Frame.End;
    Memory.frame_sources_def context (Frame.Write (24, slots.Capture.tail_payload, Frame.End));
    Memory.frame_sources_def context (Frame.Write (16, slots.Capture.tail_tag, Frame.Write (24, slots.Capture.tail_payload, Frame.End)));
    Memory.frame_sources_def context (Frame.Write (8, slots.Capture.head_payload, Frame.Write (16, slots.Capture.tail_tag, Frame.Write (24, slots.Capture.tail_payload, Frame.End))));
    Memory.frame_sources_def context (Write.writes slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload);
    Memory.frame context (Write.writes slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload) heap state ();
    Write.emit_def heap slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload)
let (allocate @ total) : (context : V.context) @ immutable -> (slots : Capture.slots) @ immutable -> (heap : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals heap === Some V.I32 && slots_typed context slots} ->
    {u : unit | Q.check context (Allocate.emit heap slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload) state === Some state} @ ghost =
  fun context slots heap state premise -> ghost_ (
    write context slots heap state (); Objects.advance context (Wasm_four_words.width ()) heap state ();
    Q.append context (Write.emit heap slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload)
      (Hmc_wasm_allocation_advance.emit (Wasm_four_words.width ()) heap) state;
    Allocate.emit_def heap slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload)
let (result @ total) : (context : V.context) @ immutable -> (frame : B.u32) -> (heap : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals frame === Some V.I32 && V.local context.V.locals heap === Some V.I32} ->
    {u : unit | Q.check context (Result.emit frame heap) state === Some state} @ ghost =
  fun context frame heap state premise -> ghost_ (
    Result.payload_def frame heap; Hmc_wasm_closure_result.payload_def 32 frame heap;
    Objects.payload context 32 frame heap state ();
    Memory.immediate context (Result.tag_offset ()) frame (Result.tag ()) state ();
    Q.append context (Result.payload frame heap) (Wasm_immediate_write.emit (Result.tag_offset ()) frame (Result.tag ())) state;
    Result.emit_def frame heap)
let (finish @ total) : (context : V.context) @ immutable -> (fragment : Pop.fragment) @ immutable ->
    (frame : B.u32) -> (heap : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals frame === Some V.I32 && V.local context.V.locals heap === Some V.I32} ->
    {u : unit | Q.check context (Finish.emit fragment frame heap) state === Some state} @ ghost =
  fun context fragment frame heap state premise -> ghost_ (
    result context frame heap state (); Hmc_wasm_static_simple.relayout context (Finish.moves fragment) frame state ();
    Q.append context (Result.emit frame heap) (Hmc_wasm_relayout.emit (Finish.moves fragment) frame) state;
    Finish.emit_def fragment frame heap)
let (success @ total) : (context : V.context) @ immutable -> (fragment : Pop.fragment) @ immutable -> (slots : Capture.slots) @ immutable ->
    (frame : B.u32) -> (heap : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals frame === Some V.I32 && V.local context.V.locals heap === Some V.I32 && slots_typed context slots} ->
    {u : unit | Q.check context (Success.emit fragment frame heap slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload) state === Some state} @ ghost =
  fun context fragment slots frame heap state premise -> ghost_ (
    allocate context slots heap state (); finish context fragment frame heap state ();
    Q.append context (Allocate.emit heap slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload)
      (Finish.emit fragment frame heap) state;
    Success.emit_def fragment frame heap slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload)
module Structured = Hmc_wasm_structured_block
module Check = Wasm_static_control
let (cons @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (fragment : Pop.fragment) @ immutable -> (locals : Structured.locals) @ immutable -> (depth : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals locals.Structured.frame === Some V.I32
      && V.local context.V.locals locals.Structured.heap === Some V.I32 && V.local context.V.locals locals.Structured.limit === Some V.I32
      && slots_typed context locals.Structured.scratch && Check.label (Check.Label labels) depth === Some Wasm_functions.Void} ->
    {u : unit | Check.check context labels (Structured.emit (Structured.Cons fragment) locals depth) state === Some state} @ ghost =
  fun context labels fragment locals depth state premise -> ghost_ (
    let slots = locals.Structured.scratch in
    let body = Success.emit fragment locals.Structured.frame locals.Structured.heap slots.Capture.head_tag slots.Capture.head_payload slots.Capture.tail_tag slots.Capture.tail_payload in
    let capture_code = Capture.emit fragment.Pop.head_tag fragment.Pop.head_payload slots locals.Structured.frame in
    let tail = Hmc_wasm_allocation_exit.emit (Wasm_four_words.width ()) locals.Structured.heap locals.Structured.limit body depth Wasm_control.Empty in
    success context fragment slots locals.Structured.frame locals.Structured.heap (V.initial ()) ();
    Hmc_wasm_static_allocation.exit context labels (Wasm_four_words.width ()) locals.Structured.heap locals.Structured.limit body depth Wasm_control.Empty state ();
    Check.check_def context labels Wasm_control.Empty state;
    capture context fragment.Pop.head_tag fragment.Pop.head_payload slots locals.Structured.frame state ();
    Q.embed_checked context labels capture_code tail state state ();
    Structured.emit_def (Structured.Cons fragment) locals depth)
