module B = Wasm_u32
module V = Wasm_static_types
module Check = Wasm_static_control
module T = Wasm_control
module Structured = Hmc_wasm_structured_block
module Capture = Hmc_wasm_cons_capture
module List_capture = Hmc_wasm_list_capture
module List = Hmc_wasm_static_list
module Cons = Hmc_wasm_static_cons
let[@def] (locals_typed @ total) (context : V.context @ immutable) (locals : Structured.locals @ immutable) = ghost_ (
  V.local context.V.locals locals.Structured.frame === Some V.I32
  && V.local context.V.locals locals.Structured.heap === Some V.I32
  && V.local context.V.locals locals.Structured.limit === Some V.I32
  && V.local context.V.locals locals.Structured.object_ === Some V.I32
  && Cons.slots_typed context locals.Structured.scratch)
let (list @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (fragment : Hmc_wasm_list_lower.fragment) @ immutable -> (locals : Structured.locals) @ immutable -> (depth : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | locals_typed context locals} ->
    {u : unit | Check.check context labels (Structured.emit (Structured.List_branch fragment) locals depth) state === Some state} @ ghost =
  fun context labels fragment locals depth state premise -> ghost_ (
    locals_typed_def context locals;
    let slots = locals.Structured.scratch in
    let scratch = {List_capture.head_tag = slots.Capture.head_tag; head_payload = slots.Capture.head_payload;
      tail_tag = slots.Capture.tail_tag; tail_payload = slots.Capture.tail_payload} in
    Cons.slots_typed_def context slots; List.slots_typed_def context scratch;
    List.conditional context labels fragment.Hmc_wasm_list_lower.full fragment.Hmc_wasm_list_lower.empty_pc
      locals.Structured.frame locals.Structured.object_ scratch T.Empty state ();
    Check.check_def context labels T.Empty state;
    Structured.emit_def (Structured.List_branch fragment) locals depth)
let (block @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (fragment : Structured.fragment) @ immutable -> (locals : Structured.locals) @ immutable -> (depth : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | locals_typed context locals && Check.label (Check.Label labels) depth === Some Wasm_functions.Void} ->
    {u : unit | Check.check context labels (Structured.emit fragment locals depth) state === Some state} @ ghost =
  fun context labels fragment locals depth state premise -> ghost_ (
    locals_typed_def context locals;
    match fragment with
    | Structured.Straight code ->
      Hmc_wasm_static_block.embedded context labels code locals.Structured.frame T.Empty state ();
      Check.check_def context labels T.Empty state; Structured.emit_def fragment locals depth
    | Structured.Closure closure -> Hmc_wasm_static_objects.closure context labels closure locals depth state ()
    | Structured.Cons cons -> Cons.cons context labels cons locals depth state ()
    | Structured.List_branch branch -> list context labels branch locals depth state ())
