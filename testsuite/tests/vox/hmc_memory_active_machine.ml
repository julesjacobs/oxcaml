module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module V = Hmc_tagged_cell
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module G = Hmc_cfg_ir
module K = Hmc_closure_ir
module I = Hmc_tail_ir
module C = Hmc_cfg_program
module P = Hmc_closure_program
module S = Hmc_cfg_semantics
module U = Hmc_tail_semantics
module H = Hmc_heap_invariant
module Index = Hmc_u32_index
module Bounds = Hmc_linear_bounds
module E = Hmc_heap_extent
module Image = Hmc_heap_image
module Stack = Hmc_memory_stack
module Saved = Hmc_memory_saved_frame
module Capacity = Hmc_memory_stack_capacity
module Inner = Hmc_memory_stack_machine

type status = Running | Done of V.value | Stuck [@@inductive]
type configuration = {memory : B.bytes; frontier : W.limb; top : W.limb; status : status}
type result = Advanced of configuration | Exhausted of X.exhaustion [@@inductive]
let[@def] (related @ total) (blocks : G.table @ immutable) (width : W.limb) (base : W.limb) (active : W.limb)
    (concrete : configuration @ immutable) (model : X.configuration @ immutable) = ghost_ (
  concrete.frontier = M.used model.X.heap && Image.related concrete.memory model.X.heap
  && (match concrete.status, model.X.state with
    | Done a, Q.Done b -> a === b
    | Stuck, Q.Stuck -> true
    | Running, Q.Running (a, frames) -> Saved.load blocks concrete.memory active === Some a
        && Stack.related blocks width concrete.memory base concrete.top frames
    | _ -> false))
let[@def] (result_related @ total) (blocks : G.table @ immutable) (width : W.limb) (base : W.limb) (active : W.limb)
    (heap_limit : W.limb) (stack_limit : W.limb) (memory_limit : W.limb) (before : B.bytes @ immutable)
    (out : result @ immutable) (model : X.result @ immutable) = ghost_ (match out, model with
  | Exhausted reason, X.Exhausted expected -> reason === expected
  | Advanced out, X.Advanced expected -> related blocks width base active out expected
      && Bounds.covers out.memory heap_limit && Bounds.covers out.memory stack_limit && Bounds.covers out.memory memory_limit
      && base <= out.top && out.top <= stack_limit && V.length out.memory === V.length before
  | _ -> false)
let (step @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable -> (code_capacity : W.limb) ->
    (width : W.limb) -> (heap_limit : W.limb) -> (base : W.limb) -> (stack_limit : W.limb) ->
    (active : W.limb) -> (active_end : W.limb) -> (memory_limit : W.limb) ->
    (configuration : configuration) @ immutable -> (model : X.configuration) @ ghost -> (abstract : S.state) @ ghost -> (frame_limit : D.index) @ ghost ->
    {u : unit | H.valid program globals heap_limit model abstract && related program.I.origin.C.blocks width base active configuration model
      && Index.fits (K.size program.I.origin.C.origin.P.table) code_capacity && Index.fits (G.size program.I.origin.C.blocks) code_capacity
      && width > 0 && E.span (Saved.slots program.I.origin.C.blocks) (Stack.zero ()) width
      && Capacity.region width frame_limit base stack_limit && heap_limit <= base && base <= configuration.top && configuration.top <= stack_limit
      && stack_limit <= active && E.span (Saved.slots program.I.origin.C.blocks) active active_end && active_end <= memory_limit
      && Bounds.covers configuration.memory heap_limit && Bounds.covers configuration.memory stack_limit && Bounds.covers configuration.memory memory_limit} ->
    {out : result | result_related program.I.origin.C.blocks width base active heap_limit stack_limit memory_limit configuration.memory out
      (X.step program globals heap_limit frame_limit model)} @ immutable =
  fun program globals code_capacity width heap_limit base stack_limit active active_end memory_limit configuration model abstract frame_limit premise ->
  let blocks = program.I.origin.C.blocks in
  ghost_ (related_def blocks width base active configuration model);
  let out = match configuration.status with
  | Done _ | Stuck -> ghost_ (X.step_def program globals heap_limit frame_limit model); Advanced configuration
  | Running ->
    match Saved.load blocks configuration.memory active with
    | None -> unreachable_ ()
    | Some a ->
      let inner = {Inner.memory = configuration.memory; frontier = configuration.frontier; top = configuration.top; state = Inner.Running a} in
      ghost_ (Inner.related_def blocks width base inner model; H.step program globals heap_limit frame_limit model abstract ());
      let result = Inner.step program globals code_capacity width heap_limit base stack_limit inner model abstract frame_limit () in
      ghost_ (Inner.result_related_def blocks width base heap_limit stack_limit configuration.memory result
        (X.step program globals heap_limit frame_limit model));
      match result with
      | Inner.Exhausted reason -> Exhausted reason
      | Inner.Advanced next ->
        let next_model = ghost_ (match X.step program globals heap_limit frame_limit model with X.Advanced next -> next | X.Exhausted _ -> unreachable_ ()) in
        ghost_ (Inner.related_def blocks width base next next_model;
          Bounds.same_length configuration.memory next.Inner.memory memory_limit ());
        let memory, status = match next.Inner.state with
        | Inner.Done value -> next.Inner.memory, Done value
        | Inner.Stuck -> next.Inner.memory, Stuck
        | Inner.Running a ->
          ghost_ (Hmc_memory_current_shape.current program globals heap_limit next_model (U.step program abstract) ();
            H.valid_def program globals heap_limit next_model (U.step program abstract));
          let memory = Saved.store blocks code_capacity next.Inner.memory memory_limit active active_end a () in
          ghost_ (Image.preserve program.I.origin.C.origin.P.table next.Inner.memory memory next_model.X.heap active ();
            Bounds.same_length next.Inner.memory memory heap_limit (); Bounds.same_length next.Inner.memory memory stack_limit ();
            match next_model.X.state with Q.Running (_, frames) -> Stack.preserve blocks width next.Inner.memory memory base next.Inner.top frames active () | _ -> ());
          memory, Running
        in
        let out = {memory; frontier = next.Inner.frontier; top = next.Inner.top; status} in
        ghost_ (related_def blocks width base active out next_model);
        Advanced out
  in
  ghost_ (result_related_def blocks width base active heap_limit stack_limit memory_limit configuration.memory out
    (X.step program globals heap_limit frame_limit model));
  out
