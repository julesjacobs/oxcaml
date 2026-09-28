module D = Hm_declarative
module W = Hmc_word64
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module B = Hmc_heap_step
module H = Hmc_heap_invariant
module P = Hmc_heap_preservation
module E = Hmc_heap_extent
module Math = Hmc_heap_extent_math
module N = Hmc_heap_demand
module R = Hmc_heap_runs
module Cap = Hmc_frame_capacity
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module S = Hmc_cfg_semantics
module C = Hmc_cfg_program
module O = Hmc_closure_program

let (available @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (configuration : X.configuration) @ immutable ->
    (abstract : S.state) @ immutable ->
    {u : unit | H.valid program globals heap_limit configuration abstract
      && E.fits (N.cells program abstract) (M.used configuration.X.heap) heap_limit
      && Cap.le (N.stack program abstract) stack_limit} ->
    {u : unit | match X.step program globals heap_limit stack_limit configuration with X.Advanced _ -> true | X.Exhausted _ -> false} @ ghost =
  fun program globals heap_limit stack_limit configuration abstract premise -> ghost_ (
    H.step program globals heap_limit stack_limit configuration abstract ();
    H.valid_def program globals heap_limit configuration abstract;
    N.request program configuration.X.heap configuration.X.state abstract ();
    match X.step program globals heap_limit stack_limit configuration with
    | X.Advanced _ -> ()
    | X.Exhausted reason ->
      B.exhausted_def program heap_limit stack_limit configuration reason;
      N.stack_def program abstract;
      Q.decode_def configuration.X.heap configuration.X.state;
      match configuration.X.state with
      | Q.Running (a, frames) ->
        F.decode_def configuration.X.heap a;
        (match Q.decode_frames configuration.X.heap frames with None -> () | Some source_frames ->
          P.extends_def configuration.X.heap configuration.X.heap;
          Q.frames_preserve program.I.origin.C.origin.O.table configuration.X.heap configuration.X.heap frames source_frames ();
          if reason === X.Stack then N.present (S.depth source_frames) stack_limit () else ())
      | _ -> ())
let rec (sufficient @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (fuel : D.index) @ immutable ->
    (configuration : X.configuration) @ immutable -> (abstract : S.state) @ immutable ->
    {u : unit | H.valid program globals heap_limit configuration abstract
      && E.fits (N.heap_plan program fuel abstract) (M.used configuration.X.heap) heap_limit
      && Cap.le (N.stack_plan program fuel abstract) stack_limit} ->
    {out : X.configuration | R.run program globals heap_limit stack_limit fuel configuration === R.Finished out
      && H.valid program globals heap_limit out (U.advance program fuel abstract)} @ immutable =
  fun program globals heap_limit stack_limit fuel configuration abstract premise ->
    ghost_ (R.run_def program globals heap_limit stack_limit fuel configuration; U.advance_def program fuel abstract;
      N.heap_plan_def program fuel abstract; N.stack_plan_def program fuel abstract);
    match fuel with
    | D.Z -> configuration
    | D.S rest ->
      ghost_ (Cap.max_bounds (N.stack program abstract) (N.stack_plan program rest (U.step program abstract));
        Cap.transitive (N.stack program abstract) (N.stack_plan program fuel abstract) stack_limit ();
        Cap.transitive (N.stack_plan program rest (U.step program abstract)) (N.stack_plan program fuel abstract) stack_limit ();
        Math.prefix (N.cells program abstract) (N.heap_plan program rest (U.step program abstract)) (M.used configuration.X.heap) heap_limit ();
        available program globals heap_limit stack_limit configuration abstract ();
        H.step program globals heap_limit stack_limit configuration abstract ();
        H.valid_def program globals heap_limit configuration abstract;
        Hmc_heap_resource_step.extent program globals heap_limit stack_limit configuration abstract ());
      match X.step program globals heap_limit stack_limit configuration with
      | X.Exhausted _ -> unreachable_ ()
      | X.Advanced next ->
        ghost_ (Math.consume (N.cells program abstract) (N.heap_plan program rest (U.step program abstract))
          (M.used configuration.X.heap) (M.used next.X.heap) heap_limit ());
        sufficient program globals heap_limit stack_limit rest next (U.step program abstract) ()
