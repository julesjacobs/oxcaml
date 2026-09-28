module D = Hm_declarative
module M = Hmc_heap_objects
module V = Hmc_tagged_cell
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module L = Hmc_heap_machine_proofs
module B = Hmc_heap_simple
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module C = Hmc_cfg_program
module O = Hmc_closure_program
module P = Hmc_heap_preservation

let[@def] (supports @ total) (instruction : I.instruction @ immutable) = match instruction with
  | I.Keep (G.Load (G.Global _, _, _, _)) | I.Keep (G.Load (G.Closure _, _, _, _)) | I.Keep (G.Cons _) -> false
  | _ -> true
let (step @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (heap_limit : Hmc_word64.limb) -> (stack_limit : D.index) @ immutable -> (configuration : X.configuration) @ immutable ->
    (abstract : S.state) @ immutable ->
    {u : unit | M.valid program.I.origin.C.origin.O.table configuration.X.heap
      && Q.decode configuration.X.heap configuration.X.state === Some abstract
      && (match configuration.X.state with Q.Running (a, _) ->
        (match I.lookup program.I.code a.F.pc with None -> true | Some op -> supports op) | _ -> true)} ->
    {u : unit | match X.step program globals heap_limit stack_limit configuration with
      | X.Advanced next -> next.X.heap === configuration.X.heap
        && Q.decode next.X.heap next.X.state === Some (U.step program abstract)
      | X.Exhausted reason -> reason === X.Stack && (match configuration.X.state with
        | Q.Running (a, frames) -> not (D.present stack_limit (Q.depth frames))
          && (match I.lookup program.I.code a.F.pc with Some (I.Keep (G.Call _)) -> true | _ -> false)
        | _ -> false)} @ ghost = fun program globals heap_limit stack_limit configuration abstract premise -> ghost_ (
  let heap = configuration.X.heap in
  X.step_def program globals heap_limit stack_limit configuration;
  Q.decode_def heap configuration.X.state; U.step_def program abstract;
  (match configuration.X.state with
  | Q.Done _ | Q.Stuck -> ()
  | Q.Running (a, frames) ->
    F.decode_def heap a; F.decode_temporaries_def heap a.F.temporaries;
    M.decode_def heap a.F.accumulator; M.decode_value_def (M.view heap) a.F.accumulator;
    (match I.lookup program.I.code a.F.pc with
    | None -> ()
    | Some instruction ->
      supports_def instruction;
      (match instruction with
      | I.Tail_call | I.Keep (G.Call _) ->
        (match a.F.temporaries with
        | F.Value (closure, _, _) ->
          (match M.decode heap closure, M.decode heap a.F.accumulator with
          | Some source_closure, Some argument ->
            L.invoke program heap closure a.F.accumulator source_closure argument ();
            L.abstract_invoke_def program source_closure argument
          | _ -> ())
        | _ -> ())
      | I.Keep (G.List_branch _) ->
        (match a.F.accumulator with
        | V.Cons_pointer address ->
          (match M.decode heap a.F.accumulator with None -> () | Some value ->
            let object_ = P.fetch program.I.origin.C.origin.O.table heap address value () in
            M.decode_object_def (M.view heap) object_;
            match object_ with
            | M.Cons (head, tail) ->
              M.decode_environment_def (M.view heap) (M.Cell (head, M.Cell (tail, a.F.env)));
              M.decode_environment_def (M.view heap) (M.Cell (tail, a.F.env))
            | _ -> ())
        | _ -> ())
      | I.Keep op -> B.supports_def op;
        Hmc_heap_simple_proofs.step program heap a frames abstract op ())));
  let result = X.step program globals heap_limit stack_limit configuration in
  match result with
  | X.Exhausted _ -> ()
  | X.Advanced next ->
    Q.decode_def next.X.heap next.X.state;
    match next.X.state with
    | Q.Running (a, frames) -> F.decode_def heap a; F.decode_temporaries_def heap a.F.temporaries;
      Q.decode_frames_def heap frames;
      (match frames with Q.Halt -> () | Q.Frame (saved, _) -> F.decode_def heap saved)
    | _ -> ())
