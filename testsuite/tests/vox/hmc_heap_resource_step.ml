module D = Hm_declarative
module W = Hmc_word64
module M = Hmc_heap_objects
module A = Hmc_heap_allocate
module X = Hmc_heap_machine
module Q = Hmc_heap_state
module F = Hmc_heap_frame
module S = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module B = Hmc_heap_step
module E = Hmc_heap_extent
module N = Hmc_heap_demand

let (allocation @ total) : (heap : M.heap) @ immutable -> (limit : W.limb) -> (object_ : M.object_) @ immutable ->
    {u : unit | match X.allocate heap limit object_ with A.Exhausted -> true
      | A.Allocated out -> E.span (M.slots object_) (M.used heap) (M.used out.A.heap)} @ ghost = fun heap limit object_ -> ghost_ (
  X.allocate_def heap limit object_;
  match E.reserve (M.slots object_) (M.used heap) limit with
  | None -> () | Some stop -> M.used_def (M.Allocate ({M.address = M.used heap; stop; object_}, heap)))
let (extent @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (configuration : X.configuration) @ immutable ->
    (abstract : S.state) @ immutable -> {u : unit | Q.decode configuration.X.heap configuration.X.state === Some abstract} ->
    {u : unit | match X.step program globals heap_limit stack_limit configuration with X.Exhausted _ -> true
      | X.Advanced next -> E.span (N.cells program abstract) (M.used configuration.X.heap) (M.used next.X.heap)} @ ghost =
  fun program globals heap_limit stack_limit configuration abstract premise -> ghost_ (
    let heap = configuration.X.heap in
    N.request program heap configuration.X.state abstract ();
    B.request_def program configuration.X.state;
    X.step_def program globals heap_limit stack_limit configuration;
    E.span_def D.Z (M.used heap) (M.used heap);
    (match configuration.X.state with
    | Q.Done _ | Q.Stuck -> ()
    | Q.Running (a, frames) ->
      (match I.lookup program.I.code a.F.pc with
      | Some (I.Keep op) ->
        Hmc_heap_allocating.request_def op a;
        (match op with
        | G.Load (G.Closure id, _, _, next) ->
          allocation heap heap_limit (M.Closure (id, a.F.env));
          X.allocation_result_def a frames next a.F.env a.F.temporaries (X.allocate heap heap_limit (M.Closure (id, a.F.env)))
        | G.Cons next -> (match a.F.temporaries with
          | F.Value (head, env, rest) ->
            allocation heap heap_limit (M.Cons (head, a.F.accumulator));
            X.allocation_result_def a frames next env rest (X.allocate heap heap_limit (M.Cons (head, a.F.accumulator)))
          | _ -> ())
        | _ -> ())
      | _ -> ())))
