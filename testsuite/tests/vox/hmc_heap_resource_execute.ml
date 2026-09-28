module D = Hm_declarative
module W = Hmc_word64
module M = Hmc_heap_objects
module X = Hmc_heap_machine
module A = Hmc_heap_initialize
module E = Hmc_heap_extent
module Math = Hmc_heap_extent_math
module N = Hmc_heap_demand
module H = Hmc_heap_invariant
module B = Hmc_heap_resources
module R = Hmc_heap_execute
module Runs = Hmc_heap_runs
module Cap = Hmc_frame_capacity
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module S = Hmc_cfg_semantics
module V = Hmc_closure_semantics

let[@def] (heap_cells @ total) (program : I.program @ immutable) (input : W.t @ immutable) (fuel : D.index @ immutable) =
  D.add (A.cells program) (N.heap_plan program fuel (U.initial program input))
let[@def] (stack_frames @ total) (program : I.program @ immutable) (input : W.t @ immutable) (fuel : D.index @ immutable) =
  N.stack_plan program fuel (U.initial program input)
let (execute @ total) : (program : I.program) @ immutable -> (base : W.limb) -> (heap_limit : W.limb) ->
    (stack_limit : D.index) @ immutable -> (input : W.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | base <= heap_limit && E.fits (heap_cells program input fuel) base heap_limit
      && Cap.le (stack_frames program input fuel) stack_limit} ->
    {out : R.result | R.correct program base heap_limit stack_limit input fuel out && not (R.exhausted out)} @ immutable =
  fun program base heap_limit stack_limit input fuel premise ->
    ghost_ (heap_cells_def program input fuel; stack_frames_def program input fuel;
      Math.prefix (A.cells program) (N.heap_plan program fuel (U.initial program input)) base heap_limit ());
    let start = A.sufficient program base heap_limit input () in
    ghost_ (A.correct_def program base heap_limit input (A.Initialized start);
      Math.consume (A.cells program) (N.heap_plan program fuel (U.initial program input)) base (M.used start.A.configuration.X.heap) heap_limit ());
    let final = B.sufficient program start.A.globals heap_limit stack_limit fuel start.A.configuration (U.initial program input) () in
    let out = R.Execution (start.A.globals, Runs.Finished final) in
    ghost_ (R.correct_def program base heap_limit stack_limit input fuel out; R.exhausted_def out); out
let (normal @ total) : (program : I.program) @ immutable -> (base : W.limb) -> (heap_limit : W.limb) ->
    (stack_limit : D.index) @ immutable -> (input : W.t) @ immutable -> (fuel : D.index) @ immutable -> (word : W.t) @ immutable ->
    {u : unit | base <= heap_limit && E.fits (heap_cells program input fuel) base heap_limit
      && Cap.le (stack_frames program input fuel) stack_limit
      && U.advance program fuel (U.initial program input) === S.Done (V.V.Word word)} ->
    {out : R.result | R.correct program base heap_limit stack_limit input fuel out && R.returned out word} @ immutable =
  fun program base heap_limit stack_limit input fuel word premise ->
    let out = execute program base heap_limit stack_limit input fuel () in
    ghost_ (R.correct_def program base heap_limit stack_limit input fuel out; R.exhausted_def out; R.returned_def out word;
      match out with R.Execution (globals, Runs.Finished configuration) ->
        H.valid_def program globals heap_limit configuration (U.advance program fuel (U.initial program input));
        R.word_agreement configuration.X.heap configuration.X.state word
      | _ -> ()); out
