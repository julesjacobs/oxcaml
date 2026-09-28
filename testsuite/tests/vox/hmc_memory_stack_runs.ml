module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module V = Hmc_tagged_cell
module G = Hmc_cfg_ir
module K = Hmc_closure_ir
module X = Hmc_heap_machine
module I = Hmc_tail_ir
module C = Hmc_cfg_program
module P = Hmc_closure_program
module S = Hmc_cfg_semantics
module U = Hmc_tail_semantics
module H = Hmc_heap_invariant
module Model = Hmc_heap_runs
module Machine = Hmc_memory_stack_machine
module Stack = Hmc_memory_stack
module Saved = Hmc_memory_saved_frame
module Capacity = Hmc_memory_stack_capacity
module Bounds = Hmc_linear_bounds
module Index = Hmc_u32_index
module E = Hmc_heap_extent

type result = Finished of Machine.configuration | Blocked of Machine.configuration * X.exhaustion * D.index [@@inductive]
let[@def] (related @ total) (blocks : G.table @ immutable) (width : W.limb) (base : W.limb)
    (heap_limit : W.limb) (stack_limit : W.limb) (before : B.bytes @ immutable) (out : result @ immutable)
    (model : Model.result @ immutable) = ghost_ (match out, model with
  | Finished out, Model.Finished expected -> Machine.related blocks width base out expected
      && Bounds.covers out.Machine.memory heap_limit && Bounds.covers out.Machine.memory stack_limit
      && base <= out.Machine.top && out.Machine.top <= stack_limit && V.length out.Machine.memory === V.length before
  | Blocked (out, reason, steps), Model.Blocked (expected, expected_reason, expected_steps) ->
      Machine.related blocks width base out expected && reason === expected_reason && steps === expected_steps
      && Bounds.covers out.Machine.memory heap_limit && Bounds.covers out.Machine.memory stack_limit
      && base <= out.Machine.top && out.Machine.top <= stack_limit && V.length out.Machine.memory === V.length before
  | _ -> false)
let rec (run @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (code_capacity : W.limb) -> (width : W.limb) -> (heap_limit : W.limb) -> (base : W.limb) -> (stack_limit : W.limb) ->
    (fuel : D.index) @ immutable -> (configuration : Machine.configuration) @ immutable ->
    (model : X.configuration) @ ghost -> (abstract : S.state) @ ghost -> (frame_limit : D.index) @ ghost ->
    {u : unit | H.valid program globals heap_limit model abstract && Machine.related program.I.origin.C.blocks width base configuration model
      && Index.fits (K.size program.I.origin.C.origin.P.table) code_capacity && Index.fits (G.size program.I.origin.C.blocks) code_capacity
      && width > 0 && E.span (Saved.slots program.I.origin.C.blocks) (Stack.zero ()) width
      && Capacity.region width frame_limit base stack_limit && heap_limit <= base && base <= configuration.Machine.top && configuration.Machine.top <= stack_limit
      && Bounds.covers configuration.Machine.memory heap_limit && Bounds.covers configuration.Machine.memory stack_limit} ->
    {out : result | related program.I.origin.C.blocks width base heap_limit stack_limit configuration.Machine.memory out
      (Model.run program globals heap_limit frame_limit fuel model)} @ immutable =
  fun program globals code_capacity width heap_limit base stack_limit fuel configuration model abstract frame_limit premise ->
  ghost_ (Model.run_def program globals heap_limit frame_limit fuel model);
  let out = match fuel with
  | D.Z -> Finished configuration
  | D.S rest ->
    ghost_ (H.step program globals heap_limit frame_limit model abstract ());
    let step = Machine.step program globals code_capacity width heap_limit base stack_limit configuration model abstract frame_limit () in
    ghost_ (Machine.result_related_def program.I.origin.C.blocks width base heap_limit stack_limit configuration.Machine.memory step
      (X.step program globals heap_limit frame_limit model));
    (match step with
    | Machine.Exhausted reason -> Blocked (configuration, reason, D.Z)
    | Machine.Advanced next ->
      let next_model = ghost_ (match X.step program globals heap_limit frame_limit model with
        | X.Advanced next -> next | X.Exhausted _ -> unreachable_ ()) in
      let out = run program globals code_capacity width heap_limit base stack_limit rest next next_model (ghost_ (U.step program abstract)) frame_limit () in
      ghost_ (related_def program.I.origin.C.blocks width base heap_limit stack_limit next.Machine.memory out
        (Model.run program globals heap_limit frame_limit rest next_model));
      match out with Finished out -> Finished out | Blocked (out, reason, steps) -> Blocked (out, reason, D.S steps))
  in
  ghost_ (related_def program.I.origin.C.blocks width base heap_limit stack_limit configuration.Machine.memory out
    (Model.run program globals heap_limit frame_limit fuel model));
  out
