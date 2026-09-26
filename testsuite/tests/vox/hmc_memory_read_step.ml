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

let[@def] (step @ total) (program : I.program @ immutable) (globals : X.globals @ immutable)
    (heap_limit : W.limb) (stack_limit : D.index @ immutable) (memory : B.bytes @ immutable) (configuration : X.configuration @ immutable) =
  let heap = configuration.X.heap in
  match configuration.X.state with
  | Q.Done _ | Q.Stuck -> X.step program globals heap_limit stack_limit configuration
  | Q.Running (a, frames) ->
    (match I.lookup program.I.code a.F.pc with
    | Some I.Tail_call -> (match a.F.temporaries with
      | F.Value (closure, _, _) -> (match Invoke.invoke program memory closure a.F.accumulator with
        | None -> X.Advanced {X.heap; state = Q.Stuck}
        | Some entered -> X.Advanced {X.heap; state = Q.Running (entered, frames)})
      | _ -> X.Advanced {X.heap; state = Q.Stuck})
    | Some (I.Keep (G.Call next)) -> (match a.F.temporaries with
      | F.Value (closure, env, rest) -> (match Invoke.invoke program memory closure a.F.accumulator with
        | None -> X.Advanced {X.heap; state = Q.Stuck}
        | Some entered -> if D.present stack_limit (Q.depth frames) then
          X.Advanced {X.heap; state = Q.Running (entered, Q.Frame ({a with F.pc = next; env; temporaries = rest}, frames))}
          else X.Exhausted X.Stack)
      | _ -> X.Advanced {X.heap; state = Q.Stuck})
    | Some (I.Keep (G.List_branch (empty, full))) -> (match a.F.accumulator with
      | V.Nil -> X.Advanced {X.heap; state = Q.Running ({a with F.pc = empty}, frames)}
      | V.Cons_pointer _ -> (match Ops.read_cons memory a.F.accumulator with
        | None -> X.Advanced {X.heap; state = Q.Stuck}
        | Some pair -> X.Advanced {X.heap; state = Q.Running ({a with F.pc = full;
          env = M.Cell (pair.Ops.head, M.Cell (pair.Ops.tail, a.F.env)); temporaries = F.Environment (a.F.env, a.F.temporaries)}, frames)})
      | _ -> X.Advanced {X.heap; state = Q.Stuck})
    | _ -> X.step program globals heap_limit stack_limit configuration)
let (correct @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (memory : B.bytes) @ immutable ->
    (configuration : X.configuration) @ immutable -> (abstract : S.state) @ immutable ->
    {u : unit | M.valid program.I.origin.C.origin.P.table configuration.X.heap && Image.related memory configuration.X.heap
      && Q.decode configuration.X.heap configuration.X.state === Some abstract} ->
    {u : unit | step program globals heap_limit stack_limit memory configuration === X.step program globals heap_limit stack_limit configuration} @ ghost =
  fun program globals heap_limit stack_limit memory configuration abstract premise -> ghost_ (
    let heap = configuration.X.heap in
    step_def program globals heap_limit stack_limit memory configuration;
    X.step_def program globals heap_limit stack_limit configuration;
    Q.decode_def heap configuration.X.state;
    match configuration.X.state with
    | Q.Done _ | Q.Stuck -> ()
    | Q.Running (a, _) ->
      F.decode_def heap a; F.decode_temporaries_def heap a.F.temporaries;
      (match I.lookup program.I.code a.F.pc with
      | Some I.Tail_call | Some (I.Keep (G.Call _)) ->
        (match a.F.temporaries with
        | F.Value (closure, _, _) -> (match M.decode heap closure with
          | Some value -> Invoke.correct program memory heap closure a.F.accumulator value () | None -> ())
        | _ -> ())
      | Some (I.Keep (G.List_branch _)) ->
        M.decode_def heap a.F.accumulator; M.decode_value_def (M.view heap) a.F.accumulator;
        (match a.F.accumulator, M.decode heap a.F.accumulator with
        | V.Cons_pointer _, Some (R.V.Cons (head, tail)) -> Ops.cons_correct program.I.origin.C.origin.P.table heap memory a.F.accumulator head tail ()
        | _ -> ())
      | _ -> ()))
