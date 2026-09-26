module D = Hm_declarative
module M = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module L = Hmc_heap_machine_proofs
module A = Hmc_heap_allocate
module P = Hmc_heap_preservation
module S = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module C = Hmc_cfg_program
module O = Hmc_closure_program
module E = Hmc_heap_extent

let[@def] (request @ total) (op : G.instruction @ immutable) (a : F.activation @ immutable) = match op with
  | G.Load (G.Closure id, _, _, _) -> Some (M.Closure (id, a.F.env))
  | G.Cons _ -> (match a.F.temporaries with F.Value (head, _, _) -> Some (M.Cons (head, a.F.accumulator)) | _ -> None)
  | _ -> None
let (step @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (heap_limit : Hmc_word64.limb) -> (stack_limit : D.index) @ immutable -> (heap : M.heap) @ immutable ->
    (a : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (abstract : S.state) @ immutable ->
    (op : G.instruction) @ immutable -> (object_ : M.object_) @ immutable ->
    {u : unit | M.valid program.I.origin.C.origin.O.table heap && M.used heap <= heap_limit
      && Q.decode heap (Q.Running (a, frames)) === Some abstract && request op a === Some object_
      && I.lookup program.I.code a.F.pc === Some (I.Keep op)
      && M.object_valid program.I.origin.C.origin.O.table (M.view heap) object_} ->
    {u : unit | match X.step program globals heap_limit stack_limit {X.heap; state = Q.Running (a, frames)} with
      | X.Advanced next -> M.valid program.I.origin.C.origin.O.table next.X.heap && P.extends next.X.heap heap
        && M.used next.X.heap <= heap_limit && Q.decode next.X.heap next.X.state === Some (U.step program abstract)
      | X.Exhausted reason -> reason === X.Heap && not (E.fits (M.slots object_) (M.used heap) heap_limit)} @ ghost =
  fun program globals heap_limit stack_limit heap a frames abstract op object_ premise -> ghost_ (
    let configuration = {X.heap; state = Q.Running (a, frames)} in
    request_def op a; X.step_def program globals heap_limit stack_limit configuration;
    Q.decode_def heap configuration.X.state; F.decode_def heap a; F.decode_temporaries_def heap a.F.temporaries;
    U.step_def program abstract;
    (match op with G.Load (atom, _, _, _) ->
      (match M.decode_environment (M.view heap) a.F.env with None -> () | Some env -> S.load_def program.I.origin.C.origin.O.globals env atom)
    | _ -> ());
    L.allocate program.I.origin.C.origin.O.table heap heap_limit object_ ();
    let allocation = X.allocate heap heap_limit object_ in
    A.correct_def program.I.origin.C.origin.O.table heap heap_limit object_ allocation;
    (match allocation with
    | A.Exhausted -> ()
    | A.Allocated allocated ->
      (match F.decode heap a, Q.decode_frames heap frames with
      | Some source_a, Some source_frames ->
        F.preserve program.I.origin.C.origin.O.table allocated.A.heap heap a source_a ();
        Q.frames_preserve program.I.origin.C.origin.O.table allocated.A.heap heap frames source_frames ();
        F.decode_def allocated.A.heap a; F.decode_temporaries_def allocated.A.heap a.F.temporaries
      | _ -> ());
      M.decode_object_def (M.view heap) object_;
      (match object_ with M.Cons (head, tail) -> M.decode_def heap head; M.decode_def heap tail | _ -> ()));
    (match op with
    | G.Load (_, _, _, next) -> X.allocation_result_def a frames next a.F.env a.F.temporaries allocation
    | G.Cons next -> (match a.F.temporaries with F.Value (_, env, rest) -> X.allocation_result_def a frames next env rest allocation | _ -> ())
    | _ -> ());
    match X.step program globals heap_limit stack_limit configuration with
    | X.Exhausted _ -> ()
    | X.Advanced next -> Q.decode_def next.X.heap next.X.state;
      match next.X.state with Q.Running (b, _) -> F.decode_def next.X.heap b | _ -> ())
