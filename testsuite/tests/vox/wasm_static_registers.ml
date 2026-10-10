module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module T = Wasm_control
module F = Wasm_functions
module G = Wasm_globals
module V = Wasm_static_types
module Step = Wasm_static_steps
module Q = Wasm_static_sequence
module Check = Wasm_static_control
module Registers = Wasm_global_registers
module Wrapper = Wasm_register_block
module Lift = Wasm_control_lift
let[@def] (binding @ total) (context : V.context @ immutable) (global : B.u32) (local : B.u32) =
  match V.global context.V.globals global, V.local context.V.locals local with
  | Some V.I32, Some V.I32 | Some V.I64, Some V.I64 -> true | _ -> false
let[@def] rec (bindings @ total) (context : V.context @ immutable) (plan : Registers.plan @ immutable) =
  match plan with Registers.End -> true | Registers.Binding (global, local, rest) -> binding context global local && bindings context rest
let[@def] rec (writable @ total) (context : V.context @ immutable) (plan : Registers.plan @ immutable) =
  match plan with Registers.End -> true | Registers.Binding (global, _, rest) -> G.writable context.V.globals.G.permissions global && writable context rest
let rec (load @ total) : (context : V.context) @ immutable -> (plan : Registers.plan) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | bindings context plan} ->
    {u : unit | Q.check context (Registers.load_code plan) state === Some state} @ ghost =
  fun context plan state premise -> ghost_ (
    bindings_def context plan; Registers.load_code_def plan;
    match plan with Registers.End -> Q.check_def context C.Empty state
    | Registers.Binding (global, local, rest) ->
      binding_def context global local;
      match V.global context.V.globals global with
      | None -> ()
      | Some ty ->
        Step.global_get context global ty state (); Step.local_set context local ty state ();
        load context rest state ();
        Q.check_def context (C.Next (I.Local_set local, Registers.load_code rest)) (V.push ty state);
        Q.check_def context (Registers.load_code plan) state)
let rec (store @ total) : (context : V.context) @ immutable -> (plan : Registers.plan) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | bindings context plan && writable context plan} ->
    {u : unit | Q.check context (Registers.store_code plan) state === Some state} @ ghost =
  fun context plan state premise -> ghost_ (
    bindings_def context plan; writable_def context plan; Registers.store_code_def plan;
    match plan with Registers.End -> Q.check_def context C.Empty state
    | Registers.Binding (global, local, rest) ->
      binding_def context global local;
      match V.global context.V.globals global with
      | None -> ()
      | Some ty ->
        Step.local_get context local ty state (); Step.global_set context global ty state ();
        store context rest state ();
        Q.check_def context (C.Next (I.Global_set global, Registers.store_code rest)) (V.push ty state);
        Q.check_def context (Registers.store_code plan) state)
let (wrapper @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (loads : Registers.plan) @ immutable -> (body : T.code) @ immutable -> (stores : Registers.plan) @ immutable ->
    (tail : T.code) @ immutable -> (state : V.state) @ immutable -> (ended : V.state) @ immutable ->
    {u : unit | bindings context loads && bindings context stores && writable context stores
      && Check.check context (Check.Label labels) body (V.initial ()) === Some ended && V.finish F.Void ended} ->
    {u : unit | Check.check context labels (Wrapper.emit loads body stores tail) state === Check.check context labels tail state} @ ghost =
  fun context labels loads body stores tail state ended premise -> ghost_ (
    load context loads state (); store context stores state ();
    Registers.load_straight loads; Registers.store_straight stores;
    Wrapper.emit_def loads body stores tail; Wrapper.epilogue_def stores tail;
    Q.embed context labels (Registers.store_code stores) tail state ();
    Check.check_def context labels (T.Block (body, Wrapper.epilogue stores tail)) state;
    Q.embed context labels (Registers.load_code loads) (T.Block (body, Wrapper.epilogue stores tail)) state ())
