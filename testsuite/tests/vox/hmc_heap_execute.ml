module D = Hm_declarative
module W = Hmc_word64
module M = Hmc_heap_objects
module V = Hmc_tagged_cell
module Q = Hmc_heap_state
module X = Hmc_heap_machine
module A = Hmc_heap_initialize
module E = Hmc_heap_runs
module H = Hmc_heap_invariant
module B = Hmc_heap_step
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module S = Hmc_cfg_semantics
module R = Hmc_closure_semantics

type result = Initialization_exhausted of M.heap * D.index | Execution of X.globals * E.result [@@inductive]
let[@def] (correct @ total) (program : I.program @ immutable) (base : W.limb) (heap_limit : W.limb)
    (stack_limit : D.index @ immutable) (input : W.t @ immutable) (fuel : D.index @ immutable) (out : result @ immutable) = ghost_ (
  match out with
  | Initialization_exhausted (heap, code) -> A.correct program base heap_limit input (A.Heap_exhausted (heap, code))
  | Execution (globals, E.Finished configuration) -> H.valid program globals heap_limit configuration (U.advance program fuel (U.initial program input))
  | Execution (globals, E.Blocked (configuration, reason, steps)) ->
    H.valid program globals heap_limit configuration (U.advance program steps (U.initial program input))
    && B.exhausted program heap_limit stack_limit configuration reason && D.present fuel steps)
let (execute @ total) : (program : I.program) @ immutable -> (base : W.limb) -> (heap_limit : W.limb) ->
    (stack_limit : D.index) @ immutable -> (input : W.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | base <= heap_limit} ->
    {out : result | correct program base heap_limit stack_limit input fuel out} @ immutable =
  fun program base heap_limit stack_limit input fuel premise ->
    let initial = A.initialize program base heap_limit input () in
    ghost_ (A.correct_def program base heap_limit input initial);
    match initial with
    | A.Heap_exhausted (heap, code) ->
      let out = Initialization_exhausted (heap, code) in
      ghost_ (correct_def program base heap_limit stack_limit input fuel out); out
    | A.Initialized start ->
      ghost_ (E.correct program start.A.globals heap_limit stack_limit fuel start.A.configuration (U.initial program input) ());
      let out = Execution (start.A.globals, E.run program start.A.globals heap_limit stack_limit fuel start.A.configuration) in
      ghost_ (correct_def program base heap_limit stack_limit input fuel out); out
let (word_agreement @ total) : (heap : M.heap) @ immutable -> (state : Q.state) @ immutable -> (word : W.t) @ immutable ->
    {u : unit | (Q.decode heap state === Some (S.Done (R.V.Word word))) = (state === Q.Done (V.Word word))} @ ghost =
  fun heap state word -> ghost_ (
    Q.decode_def heap state;
    match state with Q.Done value -> M.decode_def heap value; M.decode_value_def (M.view heap) value | _ -> ())
let[@def] (returned @ total) (out : result @ immutable) (word : W.t @ immutable) = ghost_ (match out with
  | Execution (_, E.Finished configuration) -> configuration.X.state === Q.Done (V.Word word) | _ -> false)
let[@def] (exhausted @ total) (out : result @ immutable) = match out with
  | Initialization_exhausted _ | Execution (_, E.Blocked _) -> true | Execution (_, E.Finished _) -> false
