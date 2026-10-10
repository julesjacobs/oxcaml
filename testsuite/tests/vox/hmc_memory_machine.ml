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

type configuration = {memory : B.bytes; frontier : W.limb; state : Q.state}
type result = Advanced of configuration | Exhausted of X.exhaustion [@@inductive]
let[@def] (related @ total) (concrete : configuration @ immutable) (model : X.configuration @ immutable) = ghost_ (
  concrete.frontier = M.used model.X.heap && concrete.state === model.X.state && Image.related concrete.memory model.X.heap)
let[@def] (result_related @ total) (limit : W.limb) (before : B.bytes @ immutable)
    (out : result @ immutable) (model : X.result @ immutable) = ghost_ (match out, model with
  | Exhausted reason, X.Exhausted expected -> reason === expected
  | Advanced out, X.Advanced expected -> related out expected && Bounds.covers out.memory limit
      && V.length out.memory === V.length before
      && Hmc_linear_bytes.drop before limit === Hmc_linear_bytes.drop out.memory limit
  | _ -> false)
let (allocation_matches @ total) : (table : K.table) @ immutable -> (memory : B.bytes) @ immutable ->
    (heap : M.heap) @ immutable -> (limit : W.limb) -> (object_ : M.object_) @ immutable -> (out : A.result) @ immutable ->
    {u : unit | A.correct table memory heap limit object_ out && M.valid table heap && M.used heap <= limit
      && M.object_valid table (M.view heap) object_} ->
    {u : unit | match out, X.allocate heap limit object_ with
      | A.Exhausted, Model_alloc.Exhausted -> true
      | A.Allocated out, Model_alloc.Allocated expected -> out.reference === expected.Model_alloc.reference
        && out.frontier = M.used expected.Model_alloc.heap && Image.related out.memory expected.Model_alloc.heap
        && Bounds.covers out.memory limit && V.length out.memory === V.length memory
        && Hmc_linear_bytes.drop memory limit === Hmc_linear_bytes.drop out.memory limit
      | _ -> false} @ ghost = fun table memory heap limit object_ out premise -> ghost_ (
  A.correct_def table memory heap limit object_ out;
  Hmc_heap_machine_proofs.allocate table heap limit object_ ();
  Model_alloc.correct_def table heap limit object_ (X.allocate heap limit object_);
  (match out with
  | A.Exhausted -> ()
  | A.Allocated out ->
    let next = M.Allocate ({M.address = M.used heap; stop = out.A.frontier; object_}, heap) in
    Model_alloc.correct_def table heap limit object_ (Model_alloc.Allocated {Model_alloc.heap = next; reference = out.A.reference});
    M.used_def next; E.sufficient (M.slots object_) (M.used heap) out.A.frontier limit ());
  (match X.allocate heap limit object_ with
  | Model_alloc.Exhausted -> ()
  | Model_alloc.Allocated expected -> E.sufficient (M.slots object_) (M.used heap) (M.used expected.Model_alloc.heap) limit ());
  match out, X.allocate heap limit object_ with
  | A.Allocated out, Model_alloc.Allocated expected ->
    let next = M.Allocate ({M.address = M.used heap; stop = out.A.frontier; object_}, heap) in
    Model_alloc.correct_def table heap limit object_ (Model_alloc.Allocated {Model_alloc.heap = next; reference = out.A.reference});
    M.used_def next; M.used_def expected.Model_alloc.heap;
    E.unique (M.slots object_) (M.used heap) out.A.frontier (M.used expected.Model_alloc.heap) ()
  | _ -> ())
let (step @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (capacity : W.limb) -> (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable ->
    (configuration : configuration) @ immutable -> (heap : M.heap) @ ghost -> (abstract : S.state) @ ghost ->
    {u : unit | Index.fits (K.size program.I.origin.C.origin.P.table) capacity
      && M.valid program.I.origin.C.origin.P.table heap && Image.related configuration.memory heap
      && configuration.frontier = M.used heap && configuration.frontier <= heap_limit
      && Bounds.covers configuration.memory heap_limit && Q.decode heap configuration.state === Some abstract
      && (match Request.request program configuration.state with None -> true | Some object_ ->
        M.object_valid program.I.origin.C.origin.P.table (M.view heap) object_)} ->
    {out : result | result_related heap_limit configuration.memory out
      (X.step program globals heap_limit stack_limit {X.heap; state = configuration.state})} @ immutable =
  fun program globals capacity heap_limit stack_limit configuration heap abstract premise ->
  let memory = configuration.memory in
  let model = ghost_ ({X.heap; state = configuration.state}) in
  ghost_ (Hmc_memory_read_step.correct program globals heap_limit stack_limit memory model abstract ();
    Hmc_memory_read_step.step_def program globals heap_limit stack_limit memory model;
    X.step_def program globals heap_limit stack_limit model;
    Request.request_def program configuration.state);
  let (allocate @ total) : (a : F.activation) @ immutable -> (frames : Q.frames) @ immutable ->
      (next : D.index) @ immutable -> (env : M.cells) @ immutable -> (temporaries : F.temporaries) @ immutable ->
      (object_ : M.object_) @ immutable ->
      {u : unit | M.object_valid program.I.origin.C.origin.P.table (M.view heap) object_} ->
      {out : result | result_related heap_limit memory out
        (X.allocation_result a frames next env temporaries (X.allocate heap heap_limit object_))} @ immutable =
    fun a frames next env temporaries object_ premise ->
    let out = A.allocate program.I.origin.C.origin.P.table capacity memory heap configuration.frontier heap_limit object_ () in
    ghost_ (allocation_matches program.I.origin.C.origin.P.table memory heap heap_limit object_ out ());
    let result = match out with
    | A.Exhausted -> Exhausted X.Heap
    | A.Allocated out -> Advanced {memory = out.A.memory; frontier = out.A.frontier;
        state = Q.Running ({a with F.pc = next; env; temporaries; accumulator = out.A.reference}, frames)} in
    ghost_ (X.allocation_result_def a frames next env temporaries (X.allocate heap heap_limit object_);
      result_related_def heap_limit memory result (X.allocation_result a frames next env temporaries (X.allocate heap heap_limit object_));
      match result, X.allocation_result a frames next env temporaries (X.allocate heap heap_limit object_) with
      | Advanced out, X.Advanced expected -> related_def out expected | _ -> ());
    result
  in
  let out =
  match configuration.state with
  | Q.Done _ | Q.Stuck -> Advanced configuration
  | Q.Running (a, frames) ->
    (match I.lookup program.I.code a.F.pc with
    | Some I.Tail_call -> (match a.F.temporaries with
      | F.Value (closure, _, _) -> (match Invoke.invoke program memory closure a.F.accumulator with
        | None -> Advanced {memory = configuration.memory; frontier = configuration.frontier; state = Q.Stuck}
        | Some entered -> Advanced {memory = configuration.memory; frontier = configuration.frontier; state = Q.Running (entered, frames)})
      | _ -> Advanced {memory = configuration.memory; frontier = configuration.frontier; state = Q.Stuck})
    | Some (I.Keep (G.Call next)) -> (match a.F.temporaries with
      | F.Value (closure, env, rest) -> (match Invoke.invoke program memory closure a.F.accumulator with
        | None -> Advanced {memory = configuration.memory; frontier = configuration.frontier; state = Q.Stuck}
        | Some entered -> if D.present stack_limit (Q.depth frames) then
          Advanced {memory = configuration.memory; frontier = configuration.frontier; state = Q.Running (entered, Q.Frame ({a with F.pc = next; env; temporaries = rest}, frames))}
          else Exhausted X.Stack)
      | _ -> Advanced {memory = configuration.memory; frontier = configuration.frontier; state = Q.Stuck})
    | Some (I.Keep (G.List_branch (empty, full))) -> (match a.F.accumulator with
      | V.Nil -> Advanced {memory = configuration.memory; frontier = configuration.frontier; state = Q.Running ({a with F.pc = empty}, frames)}
      | V.Cons_pointer _ -> (match Ops.read_cons memory a.F.accumulator with
        | None -> Advanced {memory = configuration.memory; frontier = configuration.frontier; state = Q.Stuck}
        | Some pair -> Advanced {memory = configuration.memory; frontier = configuration.frontier; state = Q.Running ({a with F.pc = full;
          env = M.Cell (pair.Ops.head, M.Cell (pair.Ops.tail, a.F.env)); temporaries = F.Environment (a.F.env, a.F.temporaries)}, frames)})
      | _ -> Advanced {memory = configuration.memory; frontier = configuration.frontier; state = Q.Stuck})
    | Some (I.Keep (G.Load (G.Closure id, ty, derivation, next))) ->
      ghost_ (Allocating.request_def (G.Load (G.Closure id, ty, derivation, next)) a);
      allocate a frames next a.F.env a.F.temporaries (M.Closure (id, a.F.env)) ()
    | Some (I.Keep (G.Cons next)) -> ghost_ (Allocating.request_def (G.Cons next) a); (match a.F.temporaries with
      | F.Value (head, env, rest) -> allocate a frames next env rest (M.Cons (head, a.F.accumulator)) ()
      | _ -> Advanced {configuration with state = Q.Stuck})
    | Some (I.Keep (G.Load (G.Global id, _, _, next))) ->
      (match X.global globals id with
      | None -> Advanced {configuration with state = Q.Stuck}
      | Some accumulator -> Advanced {configuration with state = Q.Running ({a with F.pc = next; accumulator}, frames)})
    | Some (I.Keep op) -> Advanced {configuration with state = Hmc_heap_simple.step op configuration.state}
    | None -> Advanced {configuration with state = Q.Stuck}) in
  ghost_ (result_related_def heap_limit memory out (X.step program globals heap_limit stack_limit model);
    match out, X.step program globals heap_limit stack_limit model with
    | Advanced out, X.Advanced expected -> related_def out expected | _ -> ());
  out
