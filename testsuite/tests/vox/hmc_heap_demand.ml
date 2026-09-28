module D = Hm_declarative
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module B = Hmc_heap_step
module A = Hmc_heap_allocating
module Cap = Hmc_frame_capacity

let[@def] rec (environment_size @ total) (env : R.V.value @ immutable) = match env with
  | R.V.Bind (_, rest) -> D.S (environment_size rest) | _ -> D.Z
let rec (environment @ total) : (heap : M.heap) @ immutable -> (cells : M.cells) @ immutable -> (env : R.V.value) @ immutable ->
    {u : unit | M.decode_environment (M.view heap) cells === Some env} ->
    {u : unit | M.length cells === environment_size env} @ ghost = fun heap cells env premise -> ghost_ (
  M.decode_environment_def (M.view heap) cells; M.length_def cells; environment_size_def env;
  match cells with M.Empty -> () | M.Cell (_, rest) ->
    match M.decode_environment (M.view heap) rest with None -> () | Some tail -> environment heap rest tail ())
let[@def] (cells @ total) (program : I.program @ immutable) (state : S.state @ immutable) = match state with
  | S.Running (a, _) -> (match I.lookup program.I.code a.S.pc with
    | Some (I.Keep (G.Load (G.Closure _, _, _, _))) -> D.S (environment_size a.S.env)
    | Some (I.Keep (G.Cons _)) -> (match a.S.temporaries with S.Value _ -> D.S (D.S D.Z) | _ -> D.Z)
    | _ -> D.Z)
  | _ -> D.Z
let (request @ total) : (program : I.program) @ immutable -> (heap : M.heap) @ immutable ->
    (state : Q.state) @ immutable -> (abstract : S.state) @ immutable ->
    {u : unit | Q.decode heap state === Some abstract} ->
    {u : unit | cells program abstract === (match B.request program state with None -> D.Z | Some object_ -> M.slots object_)} @ ghost =
  fun program heap state abstract premise -> ghost_ (
    Q.decode_def heap state; B.request_def program state; cells_def program abstract;
    match state with
    | Q.Done _ | Q.Stuck -> ()
    | Q.Running (a, _) ->
      F.decode_def heap a; F.decode_temporaries_def heap a.F.temporaries;
      (match M.decode_environment (M.view heap) a.F.env with None -> () | Some env -> environment heap a.F.env env ());
      (match I.lookup program.I.code a.F.pc with Some (I.Keep op) -> A.request_def op a | _ -> ());
      match B.request program state with None -> () | Some object_ -> M.slots_def object_)
let[@def] (stack @ total) (program : I.program @ immutable) (state : S.state @ immutable) = match state with
  | S.Running (a, frames) -> (match I.lookup program.I.code a.S.pc with
    | Some (I.Keep (G.Call _)) -> D.S (S.depth frames) | _ -> S.depth frames)
  | _ -> D.Z
let[@def] rec (heap_plan @ total) (program : I.program @ immutable) (fuel : D.index @ immutable) (state : S.state @ immutable) = match fuel with
  | D.Z -> D.Z | D.S rest -> D.add (cells program state) (heap_plan program rest (U.step program state))
let[@def] rec (stack_plan @ total) (program : I.program @ immutable) (fuel : D.index @ immutable) (state : S.state @ immutable) = match fuel with
  | D.Z -> D.Z | D.S rest -> Cap.max (stack program state) (stack_plan program rest (U.step program state))
let rec (present @ total) : (depth : D.index) @ immutable -> (limit : D.index) @ immutable ->
    {u : unit | Cap.le (D.S depth) limit} -> {u : unit | D.present limit depth} @ ghost = fun depth limit premise -> ghost_ (
  Cap.le_def (D.S depth) limit; D.present_def limit depth;
  match depth, limit with D.S n, D.S m -> present n m () | _ -> ())
