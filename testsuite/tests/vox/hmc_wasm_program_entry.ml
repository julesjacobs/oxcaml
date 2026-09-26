module B = Wasm_u32
module C = Wasm_code
module S = Wasm_scalar
module T = Wasm_control
module P = Wasm_instance_control
module F = Wasm_functions
module M = Wasm_calls
module G = Wasm_globals
module Runtime = Hmc_wasm_program_runtime
module Dispatch = Hmc_wasm_program_dispatch
let (start @ total) : (module_ : F.module_) @ immutable -> (entry : B.u32) -> (globals : G.t) @ immutable ->
    (memory : B.bytes) @ immutable -> (capacity : C.count) @ immutable ->
    {u : unit | F.lookup module_.F.functions entry === Some (Runtime.dispatcher ())} ->
    {out : M.configuration | M.start module_ entry memory globals capacity === M.Running out
      && M.run (C.Succ C.Zero) module_ out === M.Running (Dispatch.loop globals memory capacity)} @ immutable =
  fun module_ entry globals memory capacity premise ->
    let code = T.Loop (Runtime.dispatch_body (), Dispatch.exit ()) in
    let out = Dispatch.point code T.No_labels globals memory S.Empty capacity in
    ghost_ (
      Runtime.dispatcher_def (); Dispatch.exit_def ();
      Dispatch.point_def code T.No_labels globals memory S.Empty capacity;
      M.start_def module_ entry memory globals capacity; F.zero_locals_def F.No_locals;
      Dispatch.loop_def globals memory capacity; Dispatch.labels_def ();
      Dispatch.point_def (Runtime.dispatch_body ()) (Dispatch.labels ()) globals memory S.Empty capacity;
      P.step_def out.M.current; T.step_def out.M.current.P.body;
      T.enter_def (Runtime.dispatch_body ()) (Dispatch.exit ()) (Some (Runtime.dispatch_body ())) out.M.current.P.body;
      T.stack_def out.M.current.P.body.T.state S.Empty;
      Wasm_calls_body.step module_ out (Dispatch.loop globals memory capacity).M.current ();
      M.run_def (C.Succ C.Zero) module_ out;
      M.run_def C.Zero module_ (Dispatch.loop globals memory capacity));
    out
