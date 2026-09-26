module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module E = Wasm_execution
module V = Wasm_static_types
module Q = Wasm_static_sequence
module Step = Wasm_static_steps
module Memory = Wasm_static_memory
module Advance = Hmc_wasm_allocation_advance
module Result = Hmc_wasm_closure_result
module Write = Hmc_wasm_closure_write
module Allocate = Hmc_wasm_closure_allocate
module Finish = Hmc_wasm_closure_finish
module Success = Hmc_wasm_closure_success
let (advance @ total) : (context : V.context) @ immutable -> (bytes : B.u32) -> (cursor : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals cursor === Some V.I32} ->
    {u : unit | Q.check context (Advance.emit bytes cursor) state === Some state} @ ghost =
  fun context bytes cursor state premise -> ghost_ (
    Advance.emit_def bytes cursor;
    let a = V.push V.I32 state in let b = V.push V.I32 a in
    let c3 = C.Next (I.Local_set cursor, C.Empty) in
    let c2 = C.Next (I.Plain I.I32_add, c3) in
    let c1 = C.Next (I.I32_const bytes, c2) in
    Step.local_get context cursor V.I32 state (); V.instruction_def context (I.I32_const bytes) a;
    Step.take_push V.I32 a; Step.unary_push V.I32 V.I32 state;
    V.binary_def V.I32 V.I32 b; V.plain_def I.I32_add b; V.instruction_def context (I.Plain I.I32_add) b;
    Step.local_set context cursor V.I32 state ();
    Q.check_def context C.Empty state; Q.check_def context c3 a; Q.check_def context c2 b;
    Q.check_def context c1 a; Q.check_def context (Advance.emit bytes cursor) state)
let (payload @ total) : (context : V.context) @ immutable -> (bytes : B.u32) -> (frame : B.u32) -> (heap : B.u32) ->
    (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals frame === Some V.I32 && V.local context.V.locals heap === Some V.I32} ->
    {u : unit | Q.check context (Result.payload bytes frame heap) state === Some state} @ ghost =
  fun context bytes frame heap state premise -> ghost_ (
    Result.payload_def bytes frame heap;
    let a = V.push V.I32 state in let b = V.push V.I32 a in let c = V.push V.I32 b in let d = V.push V.I64 a in
    let c5 = C.Next (I.I64_store (3, 40), C.Empty) in
    let c4 = C.Next (I.Plain I.I64_extend_i32_u, c5) in
    let c3 = C.Next (I.Plain I.I32_sub, c4) in
    let c2 = C.Next (I.I32_const bytes, c3) in
    let c1 = C.Next (I.Local_get heap, c2) in
    Step.local_get context frame V.I32 state (); Step.local_get context heap V.I32 a ();
    V.instruction_def context (I.I32_const bytes) b;
    Step.take_push V.I32 b; Step.unary_push V.I32 V.I32 a;
    V.binary_def V.I32 V.I32 c; V.plain_def I.I32_sub c; V.instruction_def context (I.Plain I.I32_sub) c;
    Step.extend32 context a; Step.store64 context 3 40 state;
    Q.check_def context C.Empty state; Q.check_def context c5 d; Q.check_def context c4 b;
    Q.check_def context c3 c; Q.check_def context c2 b; Q.check_def context c1 a;
    Q.check_def context (Result.payload bytes frame heap) state)
let (result @ total) : (context : V.context) @ immutable -> (bytes : B.u32) -> (frame : B.u32) -> (heap : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals frame === Some V.I32 && V.local context.V.locals heap === Some V.I32} ->
    {u : unit | Q.check context (Result.emit bytes frame heap) state === Some state} @ ghost =
  fun context bytes frame heap state premise -> ghost_ (
    payload context bytes frame heap state ();
    Memory.immediate context (Result.tag_offset ()) frame (Result.tag ()) state ();
    Q.append context (Result.payload bytes frame heap) (Wasm_immediate_write.emit (Result.tag_offset ()) frame (Result.tag ())) state;
    Result.emit_def bytes frame heap)
let (write @ total) : (context : V.context) @ immutable -> (fragment : Write.fragment) @ immutable -> (frame : B.u32) -> (heap : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals frame === Some V.I32 && V.local context.V.locals heap === Some V.I32} ->
    {u : unit | Q.check context (Write.emit fragment frame heap) state === Some state} @ ghost =
  fun context fragment frame heap state premise -> ghost_ (
    Wasm_static_copy.cross context fragment.Write.copies frame heap state ();
    Memory.immediate context (Write.zero ()) heap (Hmc_wasm_header_words.tag ()) state ();
    Hmc_wasm_static_simple.pc context fragment.Write.code heap state ();
    Q.append context (Wasm_immediate_write.emit (Write.zero ()) heap (Hmc_wasm_header_words.tag ())) (Hmc_wasm_pc_update.emit fragment.Write.code heap) state;
    Q.append context (Wasm_cross_copy.emit fragment.Write.copies frame heap)
      (E.append (Wasm_immediate_write.emit (Write.zero ()) heap (Hmc_wasm_header_words.tag ())) (Hmc_wasm_pc_update.emit fragment.Write.code heap)) state;
    Write.emit_def fragment frame heap)
let (allocate @ total) : (context : V.context) @ immutable -> (fragment : Write.fragment) @ immutable -> (frame : B.u32) -> (heap : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals frame === Some V.I32 && V.local context.V.locals heap === Some V.I32} ->
    {u : unit | Q.check context (Allocate.emit fragment frame heap) state === Some state} @ ghost =
  fun context fragment frame heap state premise -> ghost_ (
    write context fragment frame heap state (); advance context fragment.Write.bytes heap state ();
    Q.append context (Write.emit fragment frame heap) (Advance.emit fragment.Write.bytes heap) state;
    Allocate.emit_def fragment frame heap)
let (finish @ total) : (context : V.context) @ immutable -> (bytes : B.u32) -> (pc : W.limb) -> (frame : B.u32) -> (heap : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals frame === Some V.I32 && V.local context.V.locals heap === Some V.I32} ->
    {u : unit | Q.check context (Finish.emit bytes pc frame heap) state === Some state} @ ghost =
  fun context bytes pc frame heap state premise -> ghost_ (
    result context bytes frame heap state (); Hmc_wasm_static_simple.pc context pc frame state ();
    Q.append context (Result.emit bytes frame heap) (Hmc_wasm_pc_update.emit pc frame) state;
    Finish.emit_def bytes pc frame heap)
let (success @ total) : (context : V.context) @ immutable -> (fragment : Write.fragment) @ immutable -> (pc : W.limb) -> (frame : B.u32) -> (heap : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals frame === Some V.I32 && V.local context.V.locals heap === Some V.I32} ->
    {u : unit | Q.check context (Success.emit fragment pc frame heap) state === Some state} @ ghost =
  fun context fragment pc frame heap state premise -> ghost_ (
    allocate context fragment frame heap state (); finish context fragment.Write.bytes pc frame heap state ();
    Q.append context (Allocate.emit fragment frame heap) (Finish.emit fragment.Write.bytes pc frame heap) state;
    Success.emit_def fragment pc frame heap)
module Structured = Hmc_wasm_structured_block
module Closure = Hmc_wasm_closure_lower
module Check = Wasm_static_control
let (closure @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (fragment : Closure.fragment) @ immutable -> (locals : Structured.locals) @ immutable -> (depth : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals locals.Structured.frame === Some V.I32
      && V.local context.V.locals locals.Structured.heap === Some V.I32
      && V.local context.V.locals locals.Structured.limit === Some V.I32
      && Check.label (Check.Label labels) depth === Some Wasm_functions.Void} ->
    {u : unit | Check.check context labels (Structured.emit (Structured.Closure fragment) locals depth) state === Some state} @ ghost =
  fun context labels fragment locals depth state premise -> ghost_ (
    success context fragment.Closure.object_ fragment.Closure.pc locals.Structured.frame locals.Structured.heap (V.initial ()) ();
    Hmc_wasm_static_allocation.exit context labels fragment.Closure.object_.Write.bytes locals.Structured.heap locals.Structured.limit
      (Success.emit fragment.Closure.object_ fragment.Closure.pc locals.Structured.frame locals.Structured.heap) depth Wasm_control.Empty state ();
    Check.check_def context labels Wasm_control.Empty state;
    Structured.emit_def (Structured.Closure fragment) locals depth)
