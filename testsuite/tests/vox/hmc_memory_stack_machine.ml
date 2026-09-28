module D = Hm_declarative
module B = Wasm_u32
module W = Hmc_word64
module V = Hmc_tagged_cell
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module C = Hmc_cfg_program
module P = Hmc_closure_program
module S = Hmc_cfg_semantics
module R = Hmc_closure_semantics
module Image = Hmc_heap_image
module Ops = Hmc_memory_operations
module Invoke = Hmc_memory_invoke

module A = Hmc_memory_allocate
module Index = Hmc_u32_index
module Bounds = Hmc_linear_bounds
module Request = Hmc_heap_step
module Allocating = Hmc_heap_allocating
module E = Hmc_heap_extent
module Model_alloc = Hmc_heap_allocate
module K = Hmc_closure_ir

module Stack = Hmc_memory_stack
module Capacity = Hmc_memory_stack_capacity
module H = Hmc_heap_invariant
module Saved = Hmc_memory_saved_frame
module L = Hmc_linear_bytes
module Basic = Hmc_heap_simple

type state = Running of F.activation | Done of V.value | Stuck [@@inductive]
type configuration = {memory : B.bytes; frontier : W.limb; top : W.limb; state : state}
type result = Advanced of configuration | Exhausted of X.exhaustion [@@inductive]
let[@def] (related @ total) (blocks : G.table @ immutable) (width : W.limb) (base : W.limb)
    (concrete : configuration @ immutable) (model : X.configuration @ immutable) = ghost_ (
  concrete.frontier = M.used model.X.heap && Image.related concrete.memory model.X.heap
  && (match concrete.state, model.X.state with
    | Done a, Q.Done b -> a === b
    | Stuck, Q.Stuck -> true
    | Running a, Q.Running (b, frames) -> a === b && Stack.related blocks width concrete.memory base concrete.top frames
    | _ -> false))
let[@def] (result_related @ total) (blocks : G.table @ immutable) (width : W.limb) (base : W.limb)
    (heap_limit : W.limb) (stack_limit : W.limb) (before : B.bytes @ immutable) (out : result @ immutable) (model : X.result @ immutable) = ghost_ (
  match out, model with
  | Exhausted reason, X.Exhausted expected -> reason === expected
  | Advanced out, X.Advanced expected -> related blocks width base out expected
      && Bounds.covers out.memory heap_limit && Bounds.covers out.memory stack_limit
      && base <= out.top && out.top <= stack_limit && V.length out.memory === V.length before
  | _ -> false)
let (step @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (code_capacity : W.limb) -> (width : W.limb) -> (heap_limit : W.limb) -> (base : W.limb) -> (stack_limit : W.limb) ->
    (configuration : configuration) @ immutable -> (model : X.configuration) @ ghost -> (abstract : S.state) @ ghost ->
    (frame_limit : D.index) @ ghost ->
    {u : unit | H.valid program globals heap_limit model abstract
      && related program.I.origin.C.blocks width base configuration model
      && Index.fits (K.size program.I.origin.C.origin.P.table) code_capacity && Index.fits (G.size program.I.origin.C.blocks) code_capacity
      && width > 0 && E.span (Saved.slots program.I.origin.C.blocks) (Stack.zero ()) width
      && Capacity.region width frame_limit base stack_limit && heap_limit <= base && base <= configuration.top && configuration.top <= stack_limit
      && Bounds.covers configuration.memory heap_limit && Bounds.covers configuration.memory stack_limit} ->
    {out : result | result_related program.I.origin.C.blocks width base heap_limit stack_limit configuration.memory out
      (X.step program globals heap_limit frame_limit model)} @ immutable =
  fun program globals code_capacity width heap_limit base stack_limit configuration model abstract frame_limit premise ->
  let blocks = program.I.origin.C.blocks in
  let memory = configuration.memory in
  let heap = ghost_ model.X.heap in
  let frames = ghost_ (match model.X.state with Q.Running (_, frames) -> frames | _ -> Q.Halt) in
  ghost_ (related_def blocks width base configuration model; H.valid_def program globals heap_limit model abstract;
    H.request_valid program model.X.heap model.X.state abstract ();
    Request.request_def program model.X.state;
    Hmc_memory_read_step.correct program globals heap_limit frame_limit memory model abstract ();
    Hmc_memory_read_step.step_def program globals heap_limit frame_limit memory model;
    X.step_def program globals heap_limit frame_limit model);
  let (allocate @ total) : (a : F.activation) @ immutable -> (next : D.index) @ immutable ->
      (env : M.cells) @ immutable -> (temporaries : F.temporaries) @ immutable -> (object_ : M.object_) @ immutable ->
      {u : unit | M.object_valid program.I.origin.C.origin.P.table (M.view heap) object_
        && Stack.related blocks width memory base configuration.top frames} ->
      {out : result | result_related blocks width base heap_limit stack_limit memory out
        (X.allocation_result a frames next env temporaries (X.allocate heap heap_limit object_))} @ immutable =
    fun a next env temporaries object_ premise ->
    let allocation = A.allocate program.I.origin.C.origin.P.table code_capacity memory heap configuration.frontier heap_limit object_ () in
    ghost_ (Hmc_memory_machine.allocation_matches program.I.origin.C.origin.P.table memory heap heap_limit object_ allocation ());
    let out = match allocation with
    | A.Exhausted -> Exhausted X.Heap
    | A.Allocated allocation ->
      ghost_ (Stack.preserve_suffix blocks width memory allocation.A.memory base configuration.top frames heap_limit ();
        Bounds.same_length memory allocation.A.memory stack_limit ());
      Advanced {memory = allocation.A.memory; frontier = allocation.A.frontier; top = configuration.top;
        state = Running {a with F.pc = next; env; temporaries; accumulator = allocation.A.reference}} in
    ghost_ (X.allocation_result_def a frames next env temporaries (X.allocate heap heap_limit object_);
      result_related_def blocks width base heap_limit stack_limit memory out
        (X.allocation_result a frames next env temporaries (X.allocate heap heap_limit object_));
      match out, X.allocation_result a frames next env temporaries (X.allocate heap heap_limit object_) with
      | Advanced out, X.Advanced expected -> related_def blocks width base out expected | _ -> ());
    out
  in
  let out = match configuration.state with
  | Done _ | Stuck -> Advanced configuration
  | Running a ->
    ghost_ (Q.decode_def heap model.X.state; F.decode_def heap a; F.decode_temporaries_def heap a.F.temporaries);
    (match I.lookup program.I.code a.F.pc with
    | Some I.Tail_call -> (match a.F.temporaries with
      | F.Value (closure, _, _) -> (match Invoke.invoke program memory closure a.F.accumulator with
        | None -> Advanced {configuration with state = Stuck}
        | Some entered -> Advanced {configuration with state = Running entered})
      | _ -> Advanced {configuration with state = Stuck})
    | Some (I.Keep (G.Call next)) -> (match a.F.temporaries with
      | F.Value (closure, env, rest) ->
        ghost_ (match M.decode heap closure with None -> () | Some value -> Invoke.correct program memory heap closure a.F.accumulator value ());
        (match Invoke.invoke program memory closure a.F.accumulator with
        | None -> Advanced {configuration with state = Stuck}
        | Some entered ->
          let saved = {a with F.pc = next; env; temporaries = rest} in
          ghost_ (Hmc_memory_call_shape.saved program globals heap_limit heap a frames abstract next closure env rest entered ();
            Capacity.remaining blocks width memory base configuration.top stack_limit frame_limit frames ());
          (match Stack.push blocks code_capacity width memory base configuration.top stack_limit frames saved () with
          | None -> Exhausted X.Stack
          | Some pushed ->
            ghost_ (Image.preserve program.I.origin.C.origin.P.table memory pushed.Stack.memory heap configuration.top ();
              Bounds.same_length memory pushed.Stack.memory heap_limit ());
            Advanced {configuration with memory = pushed.Stack.memory; top = pushed.Stack.top; state = Running entered}))
      | _ -> Advanced {configuration with state = Stuck})
    | Some (I.Keep G.Return) ->
      ghost_ (Basic.step_def G.Return model.X.state);
      (match a.F.temporaries with
      | F.Empty ->
        ghost_ (Stack.pop_correct blocks width memory base configuration.top frames ();
          Stack.related_def blocks width memory base configuration.top frames; Stack.previous_def width configuration.top);
        (match Stack.pop blocks width memory base configuration.top with
        | Stack.Empty -> Advanced {configuration with state = Done a.F.accumulator}
        | Stack.Invalid -> unreachable_ ()
        | Stack.Popped (saved, top) -> Advanced {configuration with top; state = Running {saved with F.accumulator = a.F.accumulator}})
      | _ -> Advanced {configuration with state = Stuck})
    | Some (I.Keep (G.List_branch (empty, full))) -> (match a.F.accumulator with
      | V.Nil -> Advanced {configuration with state = Running {a with F.pc = empty}}
      | V.Cons_pointer _ -> (match Ops.read_cons memory a.F.accumulator with
        | None -> Advanced {configuration with state = Stuck}
        | Some pair -> Advanced {configuration with state = Running {a with F.pc = full;
          env = M.Cell (pair.Ops.head, M.Cell (pair.Ops.tail, a.F.env)); temporaries = F.Environment (a.F.env, a.F.temporaries)}})
      | _ -> Advanced {configuration with state = Stuck})
    | Some (I.Keep (G.Load (G.Closure id, ty, derivation, next))) ->
      ghost_ (Allocating.request_def (G.Load (G.Closure id, ty, derivation, next)) a);
      allocate a next a.F.env a.F.temporaries (M.Closure (id, a.F.env)) ()
    | Some (I.Keep (G.Cons next)) -> ghost_ (Allocating.request_def (G.Cons next) a); (match a.F.temporaries with
      | F.Value (head, env, rest) -> allocate a next env rest (M.Cons (head, a.F.accumulator)) ()
      | _ -> Advanced {configuration with state = Stuck})
    | Some (I.Keep (G.Load (G.Global id, _, _, next))) ->
      (match X.global globals id with None -> Advanced {configuration with state = Stuck}
      | Some accumulator -> Advanced {configuration with state = Running {a with F.pc = next; accumulator}})
    | Some (I.Keep op) ->
      ghost_ (Basic.step_def op model.X.state; Basic.step_def op (Q.Running (a, Q.Halt)));
      (match Basic.step op (Q.Running (a, Q.Halt)) with
      | Q.Running (a, _) -> Advanced {configuration with state = Running a}
      | Q.Done value -> Advanced {configuration with state = Done value}
      | Q.Stuck -> Advanced {configuration with state = Stuck})
    | None -> Advanced {configuration with state = Stuck})
  in
  ghost_ (result_related_def blocks width base heap_limit stack_limit memory out (X.step program globals heap_limit frame_limit model);
    match out, X.step program globals heap_limit frame_limit model with
    | Advanced out, X.Advanced expected -> related_def blocks width base out expected | _ -> ());
  out
