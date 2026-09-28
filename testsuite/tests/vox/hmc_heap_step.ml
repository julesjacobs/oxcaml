module D = Hm_declarative
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module A = Hmc_heap_allocating
module P = Hmc_heap_preservation
module J = Hmc_heap_globals
module N = Hmc_heap_control
module S = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module C = Hmc_cfg_program
module O = Hmc_closure_program
module E = Hmc_heap_extent

let[@def] (request @ total) (program : I.program @ immutable) (state : Q.state @ immutable) = match state with
  | Q.Running (a, _) -> (match I.lookup program.I.code a.F.pc with Some (I.Keep op) -> A.request op a | _ -> None)
  | _ -> None
let[@def] (ready @ total) (program : I.program @ immutable) (globals : X.globals @ immutable)
    (limit : Hmc_word64.limb) (configuration : X.configuration @ immutable) = ghost_ (
  M.valid program.I.origin.C.origin.O.table configuration.X.heap && M.used configuration.X.heap <= limit
  && J.related configuration.X.heap program.I.origin.C.origin.O.globals globals
  && (match request program configuration.X.state with None -> true | Some object_ ->
    M.object_valid program.I.origin.C.origin.O.table (M.view configuration.X.heap) object_))
let[@def] (exhausted @ total) (program : I.program @ immutable) (heap_limit : Hmc_word64.limb)
    (stack_limit : D.index @ immutable) (configuration : X.configuration @ immutable) (reason : X.exhaustion @ immutable) = ghost_ (
  match reason with
  | X.Heap -> (match request program configuration.X.state with None -> false | Some object_ ->
    not (E.fits (M.slots object_) (M.used configuration.X.heap) heap_limit))
  | X.Stack -> (match configuration.X.state with
    | Q.Running (a, frames) -> not (D.present stack_limit (Q.depth frames))
      && (match I.lookup program.I.code a.F.pc with Some (I.Keep (G.Call _)) -> true | _ -> false)
    | _ -> false))
let (step @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (heap_limit : Hmc_word64.limb) -> (stack_limit : D.index) @ immutable -> (configuration : X.configuration) @ immutable ->
    (abstract : S.state) @ immutable ->
    {u : unit | ready program globals heap_limit configuration && Q.decode configuration.X.heap configuration.X.state === Some abstract} ->
    {u : unit | match X.step program globals heap_limit stack_limit configuration with
      | X.Exhausted reason -> exhausted program heap_limit stack_limit configuration reason
      | X.Advanced next -> M.valid program.I.origin.C.origin.O.table next.X.heap
        && M.used next.X.heap <= heap_limit && P.extends next.X.heap configuration.X.heap
        && J.related next.X.heap program.I.origin.C.origin.O.globals globals
        && Q.decode next.X.heap next.X.state === Some (U.step program abstract)} @ ghost =
  fun program globals heap_limit stack_limit configuration abstract premise -> ghost_ (
    let heap = configuration.X.heap in
    ready_def program globals heap_limit configuration; request_def program configuration.X.state;
    P.extends_def heap heap;
    (match configuration.X.state with
    | Q.Done _ | Q.Stuck -> N.step program globals heap_limit stack_limit configuration abstract ()
    | Q.Running (a, frames) ->
      (match I.lookup program.I.code a.F.pc with
      | None -> N.step program globals heap_limit stack_limit configuration abstract ()
      | Some I.Tail_call -> N.supports_def I.Tail_call; N.step program globals heap_limit stack_limit configuration abstract ()
      | Some (I.Keep op) ->
        A.request_def op a; N.supports_def (I.Keep op);
        (match op with
        | G.Load (G.Global index, ty, derivation, next) -> J.step program globals heap_limit stack_limit heap a frames abstract index ty derivation next ()
        | G.Load (G.Closure _, _, _, _) | G.Cons _ ->
          (match A.request op a with
          | Some object_ -> A.step program globals heap_limit stack_limit heap a frames abstract op object_ ()
          | None ->
            X.step_def program globals heap_limit stack_limit configuration; Q.decode_def heap configuration.X.state;
            F.decode_def heap a; F.decode_temporaries_def heap a.F.temporaries; U.step_def program abstract; Q.decode_def heap Q.Stuck)
        | _ -> N.step program globals heap_limit stack_limit configuration abstract ())));
    match X.step program globals heap_limit stack_limit configuration with
    | X.Exhausted reason -> exhausted_def program heap_limit stack_limit configuration reason
    | X.Advanced next -> J.preserve program.I.origin.C.origin.O.table next.X.heap heap program.I.origin.C.origin.O.globals globals ())
