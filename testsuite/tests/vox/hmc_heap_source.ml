module D = Hm_declarative
module W = Hmc_word64
module M = Hmc_monomorphic
module F = Hmc_source_semantics
module V = Hm_interpreter_typing
module Q = Hmc_monomorphic_simulation
module C = Hmc_cfg_program
module O = Hmc_closure_program
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module T = Hmc_tail_simulation
module X = Hmc_heap_machine
module S = Hmc_heap_state
module H = Hmc_heap_invariant
module E = Hmc_heap_runs
module R = Hmc_heap_execute

let (reflection @ total) : (program : I.program) @ immutable -> (definitions : {d : M.definitions | M.origins d}) @ immutable ->
    (base : W.limb) -> (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (input : W.t) @ immutable ->
    (fuel : D.index) @ immutable -> (out : R.result) @ immutable -> (word : W.t) @ immutable ->
    {u : unit | program.I.origin.C.origin.O.origin.M.definitions === definitions
      && R.correct program base heap_limit stack_limit input fuel out && R.returned out word} ->
    {n : D.index | F.advance n (Q.source_start program.I.origin.C.origin.O.origin input) === F.Done (V.Word word)} @ immutable =
  fun program definitions base heap_limit stack_limit input fuel out word premise ->
    ghost_ (R.correct_def program base heap_limit stack_limit input fuel out; R.returned_def out word);
    match out with
    | R.Execution (globals, E.Finished configuration) ->
      ghost_ (H.valid_def program globals heap_limit configuration (U.advance program fuel (U.initial program input));
        R.word_agreement configuration.X.heap configuration.X.state word);
      T.source_reflection program definitions input word fuel ()
    | _ -> unreachable_ ()
type execution = {steps : D.index; result : R.result}
let (preservation @ total) : (program : I.program) @ immutable -> (definitions : {d : M.definitions | M.origins d}) @ immutable ->
    (base : W.limb) -> (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (input : W.t) @ immutable ->
    (word : W.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | base <= heap_limit && program.I.origin.C.origin.O.origin.M.definitions === definitions
      && F.advance fuel (Q.source_start program.I.origin.C.origin.O.origin input) === F.Done (V.Word word)} ->
    {out : execution | R.correct program base heap_limit stack_limit input out.steps out.result
      && (R.returned out.result word || R.exhausted out.result)} @ immutable =
  fun program definitions base heap_limit stack_limit input word fuel premise ->
    let steps = T.source_preservation program definitions input word fuel () in
    let out = R.execute program base heap_limit stack_limit input steps () in
    ghost_ (R.correct_def program base heap_limit stack_limit input steps out; R.returned_def out word; R.exhausted_def out;
      match out with
      | R.Execution (globals, E.Finished configuration) ->
        H.valid_def program globals heap_limit configuration (U.advance program steps (U.initial program input));
        R.word_agreement configuration.X.heap configuration.X.state word
      | _ -> ()); {steps; result = out}
let (safe @ total) : (program : I.program) @ immutable -> (definitions : {d : M.definitions | M.origins d}) @ immutable ->
    (base : W.limb) -> (heap_limit : W.limb) -> (stack_limit : D.index) @ immutable -> (input : W.t) @ immutable ->
    (fuel : D.index) @ immutable -> (out : R.result) @ immutable ->
    {u : unit | program.I.origin.C.origin.O.origin.M.definitions === definitions && R.correct program base heap_limit stack_limit input fuel out} ->
    {u : unit | match out with R.Initialization_exhausted _ -> true
      | R.Execution (_, E.Finished configuration) | R.Execution (_, E.Blocked (configuration, _, _)) -> not (configuration.X.state === S.Stuck)} @ ghost =
  fun program definitions base heap_limit stack_limit input fuel out premise -> ghost_ (
    R.correct_def program base heap_limit stack_limit input fuel out;
    match out with
    | R.Initialization_exhausted _ -> ()
    | R.Execution (globals, E.Finished configuration) ->
      H.valid_def program globals heap_limit configuration (U.advance program fuel (U.initial program input));
      T.safe program definitions input fuel (); S.decode_def configuration.X.heap configuration.X.state
    | R.Execution (globals, E.Blocked (configuration, _, steps)) ->
      H.valid_def program globals heap_limit configuration (U.advance program steps (U.initial program input));
      T.safe program definitions input steps (); S.decode_def configuration.X.heap configuration.X.state)
