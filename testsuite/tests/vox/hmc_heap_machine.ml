module D = Hm_declarative
module W = Hmc_word64
module V = Hmc_tagged_cell
module M = Hmc_heap_objects
module E = Hmc_heap_extent
module A = Hmc_heap_allocate
module P = Hmc_heap_preservation
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module B = Hmc_heap_simple
module G = Hmc_cfg_ir
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module I = Hmc_tail_ir

type globals = Empty_globals | Global of D.index * V.value * globals [@@inductive]
let[@def] rec (global @ total) (globals : globals @ immutable) (index : D.index @ immutable) = match globals with
  | Empty_globals -> None
  | Global (id, value, rest) -> if Hm_elaboration_check.index_equal id index then Some value else global rest index
let[@def] (allocate @ total) (heap : M.heap @ immutable) (limit : W.limb) (object_ : M.object_ @ immutable) =
  match E.reserve (M.slots object_) (M.used heap) limit with
  | None -> A.Exhausted
  | Some stop -> A.Allocated {A.heap = M.Allocate ({M.address = M.used heap; stop; object_}, heap);
    reference = M.reference object_ (M.used heap)}
type configuration = {heap : M.heap; state : Q.state}
type exhaustion = Heap | Stack [@@inductive]
type result = Advanced of configuration | Exhausted of exhaustion [@@inductive]
let[@def] (allocation_result @ total) (a : F.activation @ immutable) (frames : Q.frames @ immutable)
    (next : D.index @ immutable) (env : M.cells @ immutable) (temporaries : F.temporaries @ immutable)
    (allocation : A.result @ immutable) = match allocation with
  | A.Exhausted -> Exhausted Heap
  | A.Allocated allocation -> Advanced {heap = allocation.A.heap;
    state = Q.Running ({a with F.pc = next; env; temporaries; accumulator = allocation.A.reference}, frames)}
let[@def] (invoke @ total) (program : I.program @ immutable) (heap : M.heap @ immutable)
    (closure : V.value @ immutable) (argument : V.value @ immutable) = match closure with
  | V.Closure_pointer address -> (match P.lookup_object heap address with
    | Some (M.Closure (id, captures)) ->
      (match K.lookup program.I.origin.C.origin.Hmc_closure_program.table id, C.lookup program.I.origin.C.functions id with
      | Some entry, Some code ->
        let captures = if entry.K.recursive then M.Cell (closure, captures) else captures in
        Some {F.pc = code.C.start; env = M.Cell (argument, captures); accumulator = V.Nil; temporaries = F.Empty; current = closure}
      | _ -> None)
    | _ -> None)
  | _ -> None
let[@def] (step @ total) (program : I.program @ immutable) (globals : globals @ immutable)
    (heap_limit : W.limb) (stack_limit : D.index @ immutable) (configuration : configuration @ immutable) =
  let heap = configuration.heap in
  match configuration.state with
  | Q.Done _ | Q.Stuck -> Advanced configuration
  | Q.Running (a, frames) -> (match I.lookup program.I.code a.F.pc with
    | None -> Advanced {heap; state = Q.Stuck}
    | Some I.Tail_call -> (match a.F.temporaries with
      | F.Value (closure, _, _) -> (match invoke program heap closure a.F.accumulator with
        | None -> Advanced {heap; state = Q.Stuck}
        | Some entered -> Advanced {heap; state = Q.Running (entered, frames)})
      | _ -> Advanced {heap; state = Q.Stuck})
    | Some (I.Keep op) -> match op with
      | G.Load (G.Global id, _, _, next) -> (match global globals id with
        | None -> Advanced {heap; state = Q.Stuck}
        | Some accumulator -> Advanced {heap; state = Q.Running ({a with F.pc = next; accumulator}, frames)})
      | G.Load (G.Closure id, _, _, next) ->
        allocation_result a frames next a.F.env a.F.temporaries (allocate heap heap_limit (M.Closure (id, a.F.env)))
      | G.Cons next -> (match a.F.temporaries with
        | F.Value (head, env, rest) -> allocation_result a frames next env rest (allocate heap heap_limit (M.Cons (head, a.F.accumulator)))
        | _ -> Advanced {heap; state = Q.Stuck})
      | G.Call next -> (match a.F.temporaries with
        | F.Value (closure, env, rest) -> (match invoke program heap closure a.F.accumulator with
          | None -> Advanced {heap; state = Q.Stuck}
          | Some entered -> if D.present stack_limit (Q.depth frames) then
            Advanced {heap; state = Q.Running (entered, Q.Frame ({a with F.pc = next; env; temporaries = rest}, frames))}
            else Exhausted Stack)
        | _ -> Advanced {heap; state = Q.Stuck})
      | G.List_branch (empty, full) -> (match a.F.accumulator with
        | V.Nil -> Advanced {heap; state = Q.Running ({a with F.pc = empty}, frames)}
        | V.Cons_pointer address -> (match P.lookup_object heap address with
          | Some (M.Cons (head, tail)) -> Advanced {heap; state = Q.Running ({a with F.pc = full;
            env = M.Cell (head, M.Cell (tail, a.F.env)); temporaries = F.Environment (a.F.env, a.F.temporaries)}, frames)}
          | _ -> Advanced {heap; state = Q.Stuck})
        | _ -> Advanced {heap; state = Q.Stuck})
      | _ -> Advanced {heap; state = B.step op configuration.state})
