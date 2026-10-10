module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module E = Wasm_execution
module Plan = Wasm_parallel_copy
module Copy = Wasm_cross_copy
module V = Wasm_static_types
module Step = Wasm_static_steps
module Q = Wasm_static_sequence
let rec (cross @ total) : (context : V.context) @ immutable -> (plan : Plan.plan) @ immutable ->
    (source_local : B.u32) -> (target_local : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals source_local === Some V.I32 && V.local context.V.locals target_local === Some V.I32} ->
    {u : unit | Q.check context (Copy.emit plan source_local target_local) state === Some state} @ ghost =
  fun context plan source_local target_local state premise -> ghost_ (
    Copy.emit_def plan source_local target_local;
    match plan with
    | Plan.End -> Q.check_def context C.Empty state
    | Plan.Copy (source, destination, rest) ->
      let address = V.push V.I32 state in
      let reading = V.push V.I32 address in
      let captured = V.push V.I64 address in
      let write = C.Next (I.I64_store (3, destination), C.Empty) in
      let tail = E.append (Copy.emit rest source_local target_local) write in
      Step.local_get context target_local V.I32 state ();
      Step.local_get context source_local V.I32 address ();
      Step.load64 context 3 source address;
      cross context rest source_local target_local captured ();
      Step.store64 context 3 destination state;
      Q.check_def context write captured; Q.check_def context C.Empty state;
      Q.append context (Copy.emit rest source_local target_local) write captured;
      Q.check_def context (C.Next (I.I64_load (3, source), tail)) reading;
      Q.check_def context (C.Next (I.Local_get source_local, C.Next (I.I64_load (3, source), tail))) address;
      Q.check_def context (Copy.emit plan source_local target_local) state)
let rec (same_base @ total) : (plan : Plan.plan) @ immutable -> (base : B.u32) ->
    {u : unit | Plan.emit plan base === Copy.emit plan base base} @ ghost = fun plan base -> ghost_ (
  Plan.emit_def plan base; Copy.emit_def plan base base;
  match plan with Plan.End -> () | Plan.Copy (_, _, rest) -> same_base rest base)
let (parallel @ total) : (context : V.context) @ immutable -> (plan : Plan.plan) @ immutable ->
    (base : B.u32) -> (state : V.state) @ immutable -> {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (Plan.emit plan base) state === Some state} @ ghost =
  fun context plan base state premise -> ghost_ (same_base plan base; cross context plan base base state ())
