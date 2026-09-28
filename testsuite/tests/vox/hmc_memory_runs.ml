module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module V = Hmc_tagged_cell
module M = Hmc_heap_objects
module X = Hmc_heap_machine
module I = Hmc_tail_ir
module C = Hmc_cfg_program
module P = Hmc_closure_program
module K = Hmc_closure_ir
module S = Hmc_cfg_semantics
module U = Hmc_tail_semantics
module H = Hmc_heap_invariant
module Model = Hmc_heap_runs
module Machine = Hmc_memory_machine
module Bounds = Hmc_linear_bounds
module Index = Hmc_u32_index

type result = Finished of Machine.configuration | Blocked of Machine.configuration * X.exhaustion * D.index [@@inductive]
let[@def] (related @ total) (limit : W.limb) (before : B.bytes @ immutable) (out : result @ immutable)
    (model : Model.result @ immutable) = ghost_ (match out, model with
  | Finished out, Model.Finished expected -> Machine.related out expected && Bounds.covers out.Machine.memory limit
      && V.length out.Machine.memory === V.length before
  | Blocked (out, reason, steps), Model.Blocked (expected, expected_reason, expected_steps) ->
      Machine.related out expected && reason === expected_reason && steps === expected_steps
      && Bounds.covers out.Machine.memory limit && V.length out.Machine.memory === V.length before
  | _ -> false)
let rec (run @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (capacity : W.limb) -> (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (fuel : D.index) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (model : X.configuration) @ ghost -> (abstract : S.state) @ ghost ->
    {u : unit | Index.fits (K.size program.I.origin.C.origin.P.table) capacity
      && H.valid program globals heap_limit model abstract && Machine.related configuration model
      && Bounds.covers configuration.Machine.memory heap_limit} ->
    {out : result | related heap_limit configuration.Machine.memory out
      (Model.run program globals heap_limit stack_limit fuel model)} @ immutable =
  fun program globals capacity heap_limit stack_limit fuel configuration model abstract premise ->
  ghost_ (Machine.related_def configuration model; H.valid_def program globals heap_limit model abstract;
    Model.run_def program globals heap_limit stack_limit fuel model);
  let out = match fuel with
  | D.Z -> Finished configuration
  | D.S rest ->
    ghost_ (H.request_valid program model.X.heap model.X.state abstract ();
      H.step program globals heap_limit stack_limit model abstract ());
    let step = Machine.step program globals capacity heap_limit stack_limit configuration (ghost_ model.X.heap) abstract () in
    ghost_ (Machine.result_related_def heap_limit configuration.Machine.memory step
      (X.step program globals heap_limit stack_limit model));
    (match step with
    | Machine.Exhausted reason -> Blocked (configuration, reason, D.Z)
    | Machine.Advanced next ->
      let next_model = ghost_ (match X.step program globals heap_limit stack_limit model with
        | X.Advanced next -> next | X.Exhausted _ -> unreachable_ ()) in
      let out = run program globals capacity heap_limit stack_limit rest next next_model (ghost_ (U.step program abstract)) () in
      ghost_ (related_def heap_limit next.Machine.memory out (Model.run program globals heap_limit stack_limit rest next_model));
      match out with Finished out -> Finished out | Blocked (out, reason, steps) -> Blocked (out, reason, D.S steps))
  in
  ghost_ (related_def heap_limit configuration.Machine.memory out (Model.run program globals heap_limit stack_limit fuel model));
  out
