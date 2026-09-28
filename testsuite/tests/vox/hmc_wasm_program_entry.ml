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
(* Starting the exported dispatcher; its prologue ([Dispatch.prologue]) then
   reaches the dispatcher loop. *)
let (start @ total) : (module_ : F.module_) @ immutable -> (entry : B.u32) -> (globals : G.t) @ immutable ->
    (memory : B.bytes) @ immutable -> (capacity : C.count) @ immutable ->
    {u : unit | F.lookup module_.F.functions entry === Some (Runtime.dispatcher ())} ->
    {out : M.configuration | M.start module_ entry memory globals capacity === M.Running out
      && out === Dispatch.point (Runtime.dispatcher ()).F.code T.No_labels globals memory S.Empty capacity} @ immutable =
  fun module_ entry globals memory capacity premise ->
    let out = Dispatch.point (Runtime.dispatcher ()).F.code T.No_labels globals memory S.Empty capacity in
    ghost_ (
      Runtime.dispatcher_def ();
      Dispatch.point_def (Runtime.dispatcher ()).F.code T.No_labels globals memory S.Empty capacity;
      M.start_def module_ entry memory globals capacity; F.zero_locals_def F.No_locals);
    out
