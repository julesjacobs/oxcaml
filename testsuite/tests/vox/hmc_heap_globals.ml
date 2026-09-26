module D = Hm_declarative
module M = Hmc_heap_objects
module P = Hmc_closure_program
module X = Hmc_heap_machine
module R = Hmc_closure_semantics
module K = Hmc_closure_ir
module E = Hmc_heap_preservation
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module S = Hmc_cfg_semantics
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module I = Hmc_tail_ir
module U = Hmc_tail_semantics

let[@def] rec (related @ total) (heap : M.heap @ immutable) (source : P.globals @ immutable) (target : X.globals @ immutable) = ghost_ (
  match source, target with
  | P.No_globals, X.Empty_globals -> true
  | P.Global (code, rest), X.Global (index, value, tail) -> index === P.size rest
    && M.decode heap value === Some (R.V.Closure (code, R.V.Empty)) && related heap rest tail
  | _ -> false)
let rec (preserve @ total) : (table : K.table) @ immutable -> (larger : M.heap) @ immutable -> (smaller : M.heap) @ immutable ->
    (source : P.globals) @ immutable -> (target : X.globals) @ immutable ->
    {u : unit | M.valid table larger && M.valid table smaller && E.extends larger smaller && related smaller source target} ->
    {u : unit | related larger source target} @ ghost = fun table larger smaller source target premise -> ghost_ (
  related_def smaller source target; related_def larger source target;
  match source, target with P.Global (code, rest), X.Global (_, value, tail) ->
    E.decode_preserve table larger smaller value (R.V.Closure (code, R.V.Empty)) (); preserve table larger smaller rest tail ()
  | _ -> ())
let rec (lookup @ total) : (heap : M.heap) @ immutable -> (source : P.globals) @ immutable -> (target : X.globals) @ immutable ->
    (index : D.index) @ immutable -> {u : unit | related heap source target} ->
    {u : unit | match X.global target index with None -> P.lookup source index === None
      | Some value -> (match P.lookup source index with None -> false | Some code -> M.decode heap value === Some (R.V.Closure (code, R.V.Empty)))} @ ghost =
  fun heap source target index premise -> ghost_ (
    related_def heap source target; X.global_def target index; P.lookup_def source index;
    match source, target with
    | P.Global (_, rest), X.Global (id, _, tail) ->
      let _ = Hm_elaboration_check.index_equal index (P.size rest) in
      if Hm_elaboration_check.index_equal id index then () else lookup heap rest tail index ()
    | _ -> ())
let (step @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (heap_limit : Hmc_word64.limb) -> (stack_limit : D.index) @ immutable -> (heap : M.heap) @ immutable ->
    (a : F.activation) @ immutable -> (frames : Q.frames) @ immutable -> (abstract : S.state) @ immutable ->
    (index : D.index) @ immutable -> (ty : D.mono) @ immutable -> (derivation : D.typing) @ immutable -> (next : D.index) @ immutable ->
    {u : unit | related heap program.I.origin.C.origin.P.globals globals && Q.decode heap (Q.Running (a, frames)) === Some abstract
      && I.lookup program.I.code a.F.pc === Some (I.Keep (G.Load (G.Global index, ty, derivation, next)))} ->
    {u : unit | match X.step program globals heap_limit stack_limit {X.heap; state = Q.Running (a, frames)} with
      | X.Exhausted _ -> false
      | X.Advanced out -> out.X.heap === heap && Q.decode heap out.X.state === Some (U.step program abstract)} @ ghost =
  fun program globals heap_limit stack_limit heap a frames abstract index ty derivation next premise -> ghost_ (
    let configuration = {X.heap; state = Q.Running (a, frames)} in
    X.step_def program globals heap_limit stack_limit configuration;
    Q.decode_def heap configuration.X.state; F.decode_def heap a; U.step_def program abstract;
    lookup heap program.I.origin.C.origin.P.globals globals index ();
    (match M.decode_environment (M.view heap) a.F.env with None -> () | Some env ->
      S.load_def program.I.origin.C.origin.P.globals env (G.Global index));
    match X.step program globals heap_limit stack_limit configuration with
    | X.Exhausted _ -> ()
    | X.Advanced out -> Q.decode_def heap out.X.state;
      match out.X.state with Q.Running (b, _) -> F.decode_def heap b | _ -> ())
