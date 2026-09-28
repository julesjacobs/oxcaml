module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module T = Wasm_control
module V = Wasm_static_types
module Q = Wasm_static_sequence
module Step = Wasm_static_steps
module Check = Wasm_static_control
module Guard = Hmc_wasm_allocation_guard
module Select = Hmc_wasm_allocation_select
let (guard @ total) : (context : V.context) @ immutable -> (bytes : B.u32) -> (cursor : B.u32) -> (limit : B.u32) ->
    (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals cursor === Some V.I32 && V.local context.V.locals limit === Some V.I32} ->
    {u : unit | Q.check context (Guard.emit bytes cursor limit) state === Some (V.push V.I32 state)} @ ghost =
  fun context bytes cursor limit state premise -> ghost_ (
    Guard.emit_def bytes cursor limit;
    let a = V.push V.I32 state in let b = V.push V.I64 state in
    let c = V.push V.I32 b in let d = V.push V.I64 b in let e = V.push V.I64 d in
    let c7 = C.Next (I.Plain I.I32_eqz, C.Empty) in
    let c6 = C.Next (I.Plain I.I64_lt_u, c7) in
    let c5 = C.Next (I.Plain I.I64_add, c6) in
    let c4 = C.Next (I.I64_const (Hmc_wasm_header_update.number bytes), c5) in
    let c3 = C.Next (I.Plain I.I64_extend_i32_u, c4) in
    let c2 = C.Next (I.Local_get cursor, c3) in
    let c1 = C.Next (I.Plain I.I64_extend_i32_u, c2) in
    Step.local_get context limit V.I32 state (); Step.extend32 context state;
    Step.local_get context cursor V.I32 b (); Step.extend32 context b;
    Step.constant64 context (Hmc_wasm_header_update.number bytes) d;
    Step.take_push V.I64 d; Step.unary_push V.I64 V.I64 b;
    V.binary_def V.I64 V.I64 e; V.plain_def I.I64_add e; V.instruction_def context (I.Plain I.I64_add) e;
    Step.take_push V.I64 b; Step.unary_push V.I64 V.I32 state;
    V.binary_def V.I64 V.I32 d; V.plain_def I.I64_lt_u d; V.instruction_def context (I.Plain I.I64_lt_u) d;
    Step.unary_push V.I32 V.I32 state; V.plain_def I.I32_eqz a; V.instruction_def context (I.Plain I.I32_eqz) a;
    Q.check_def context C.Empty a; Q.check_def context c7 a; Q.check_def context c6 d;
    Q.check_def context c5 e; Q.check_def context c4 d; Q.check_def context c3 c;
    Q.check_def context c2 b; Q.check_def context c1 a; Q.check_def context (Guard.emit bytes cursor limit) state)
let (select @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (bytes : B.u32) -> (cursor : B.u32) -> (limit : B.u32) -> (success : T.code) @ immutable ->
    (exhausted : T.code) @ immutable -> (tail : T.code) @ immutable -> (state : V.state) @ immutable ->
    (success_state : V.state) @ immutable -> (exhausted_state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals cursor === Some V.I32 && V.local context.V.locals limit === Some V.I32
      && Check.check context (Check.Label labels) success (V.initial ()) === Some success_state
      && Check.check context (Check.Label labels) exhausted (V.initial ()) === Some exhausted_state
      && V.finish Wasm_functions.Void success_state && V.finish Wasm_functions.Void exhausted_state} ->
    {u : unit | Check.check context labels (Select.emit bytes cursor limit success exhausted tail) state === Check.check context labels tail state} @ ghost =
  fun context labels bytes cursor limit success exhausted tail state success_state exhausted_state premise -> ghost_ (
    guard context bytes cursor limit state ();
    Step.take_push V.I32 state;
    Check.check_def context labels (T.If (success, exhausted, tail)) (V.push V.I32 state);
    Q.embed_checked context labels (Guard.emit bytes cursor limit) (T.If (success, exhausted, tail)) state (V.push V.I32 state) ();
    Select.emit_def bytes cursor limit success exhausted tail)
let (exit @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (bytes : B.u32) -> (cursor : B.u32) -> (limit : B.u32) -> (success : C.t) @ immutable ->
    (depth : B.u32) -> (tail : T.code) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals cursor === Some V.I32 && V.local context.V.locals limit === Some V.I32
      && Q.check context success (V.initial ()) === Some (V.initial ())
      && Check.label (Check.Label labels) depth === Some Wasm_functions.Void} ->
    {u : unit | Check.check context labels (Hmc_wasm_allocation_exit.emit bytes cursor limit success depth tail) state ===
      Check.check context labels tail state} @ ghost =
  fun context labels bytes cursor limit success depth tail state premise -> ghost_ (
    let empty = V.initial () in let dead = V.dead () in
    let escape = Hmc_wasm_allocation_exit.escape depth in
    Q.embed_checked context (Check.Label labels) success T.Empty empty empty ();
    Check.check_def context (Check.Label labels) T.Empty empty;
    V.initial_def (); V.dead_def ();
    V.consume_result_def Wasm_functions.Void empty; V.finish_def Wasm_functions.Void empty;
    V.consume_result_def Wasm_functions.Void dead; V.finish_def Wasm_functions.Void dead;
    Hmc_wasm_allocation_exit.escape_def depth;
    Check.branch_def (Check.Label labels) depth empty;
    Check.check_def context (Check.Label labels) escape empty;
    Check.check_def context (Check.Label labels) T.Empty dead;
    select context labels bytes cursor limit (Wasm_control_lift.embed success T.Empty) escape tail state empty dead ();
    Hmc_wasm_allocation_exit.emit_def bytes cursor limit success depth tail)
