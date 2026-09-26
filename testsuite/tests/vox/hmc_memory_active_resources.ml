module D = Hm_declarative
module W = Hmc_word64
module M = Hmc_heap_objects
module X = Hmc_heap_machine
module I = Hmc_tail_ir
module C = Hmc_cfg_program
module U = Hmc_tail_semantics
module S = Hmc_cfg_semantics
module H = Hmc_heap_invariant
module Demand = Hmc_heap_demand
module E = Hmc_heap_extent
module Cap = Hmc_frame_capacity
module Model = Hmc_heap_runs
module Machine = Hmc_memory_active_machine
module Runs = Hmc_memory_active_runs
module Source = Hmc_memory_active_source
let (sufficient @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (width : W.limb) -> (base : W.limb) -> (active : W.limb) -> (heap_limit : W.limb) -> (stack_limit : W.limb) -> (memory_limit : W.limb) ->
    (frame_limit : D.index) @ immutable -> (fuel : D.index) @ immutable -> (initial : X.configuration) @ immutable ->
    (abstract : S.state) @ immutable -> (before : Wasm_u32.bytes) @ immutable -> (out : Runs.result) @ immutable ->
    {u : unit | H.valid program globals heap_limit initial abstract
      && E.fits (Demand.heap_plan program fuel abstract) (M.used initial.X.heap) heap_limit
      && Cap.le (Demand.stack_plan program fuel abstract) frame_limit
      && Runs.related program.I.origin.C.blocks width base active heap_limit stack_limit memory_limit before out
        (Model.run program globals heap_limit frame_limit fuel initial)} ->
    {u : unit | match out with Runs.Finished _ -> true | Runs.Blocked _ -> false} @ ghost =
  fun program globals width base active heap_limit stack_limit memory_limit frame_limit fuel initial abstract before out premise -> ghost_ (
    let _ = Hmc_heap_resources.sufficient program globals heap_limit frame_limit fuel initial abstract () in
    Runs.related_def program.I.origin.C.blocks width base active heap_limit stack_limit memory_limit before out
      (Model.run program globals heap_limit frame_limit fuel initial))
let (normal @ total) : (program : I.program) @ immutable -> (globals : X.globals) @ immutable ->
    (width : W.limb) -> (base : W.limb) -> (active : W.limb) -> (heap_limit : W.limb) -> (stack_limit : W.limb) -> (memory_limit : W.limb) ->
    (frame_limit : D.index) @ immutable -> (fuel : D.index) @ immutable -> (initial : X.configuration) @ immutable ->
    (abstract : S.state) @ immutable -> (before : Wasm_u32.bytes) @ immutable -> (out : Runs.result) @ immutable -> (word : W.t) @ immutable ->
    {u : unit | H.valid program globals heap_limit initial abstract
      && E.fits (Demand.heap_plan program fuel abstract) (M.used initial.X.heap) heap_limit
      && Cap.le (Demand.stack_plan program fuel abstract) frame_limit
      && U.advance program fuel abstract === S.Done (Hmc_closure_semantics.V.Word word)
      && Runs.related program.I.origin.C.blocks width base active heap_limit stack_limit memory_limit before out
        (Model.run program globals heap_limit frame_limit fuel initial)} ->
    {u : unit | Source.returned out word} @ ghost =
  fun program globals width base active heap_limit stack_limit memory_limit frame_limit fuel initial abstract before out word premise -> ghost_ (
    let final = Hmc_heap_resources.sufficient program globals heap_limit frame_limit fuel initial abstract () in
    Runs.related_def program.I.origin.C.blocks width base active heap_limit stack_limit memory_limit before out
      (Model.run program globals heap_limit frame_limit fuel initial);
    H.valid_def program globals heap_limit final (U.advance program fuel abstract);
    Hmc_heap_execute.word_agreement final.X.heap final.X.state word;
    Source.returned_def out word;
    match out with Runs.Finished concrete -> Machine.related_def program.I.origin.C.blocks width base active concrete final | _ -> ())
