module D = Hm_declarative
module W = Hmc_word64
module V = Hmc_tagged_cell
module M = Hmc_monomorphic
module F = Hmc_source_semantics
module Values = Hm_interpreter_typing
module Q = Hmc_monomorphic_simulation
module C = Hmc_cfg_program
module P = Hmc_closure_program
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module T = Hmc_tail_simulation
module X = Hmc_heap_machine
module State = Hmc_heap_state
module H = Hmc_heap_invariant
module Model = Hmc_heap_runs
module Machine = Hmc_memory_stack_machine
module Runs = Hmc_memory_stack_runs
let[@def] (returned @ total) (out : Runs.result @ immutable) (word : W.t @ immutable) = ghost_ (
  match out with Runs.Finished configuration -> configuration.Machine.state === Machine.Done (V.Word word) | _ -> false)
let (reflection @ total) : (program : I.program) @ immutable -> (definitions : {d : M.definitions | M.origins d}) @ immutable ->
    (globals : X.globals) @ immutable -> (width : W.limb) -> (base : W.limb) -> (heap_limit : W.limb) -> (stack_limit : W.limb) ->
    (frame_limit : D.index) @ immutable -> (input : W.t) @ immutable -> (fuel : D.index) @ immutable ->
    (initial : X.configuration) @ ghost -> (before : Wasm_u32.bytes) @ ghost -> (out : Runs.result) @ immutable -> (word : W.t) @ immutable ->
    {u : unit | program.I.origin.C.origin.P.origin.M.definitions === definitions
      && H.valid program globals heap_limit initial (U.initial program input)
      && Runs.related program.I.origin.C.blocks width base heap_limit stack_limit before out
          (Model.run program globals heap_limit frame_limit fuel initial) && returned out word} ->
    {n : D.index | F.advance n (Q.source_start program.I.origin.C.origin.P.origin input) === F.Done (Values.Word word)} @ immutable =
  fun program definitions globals width base heap_limit stack_limit frame_limit input fuel initial before out word premise ->
    ghost_ (returned_def out word;
      Runs.related_def program.I.origin.C.blocks width base heap_limit stack_limit before out
        (Model.run program globals heap_limit frame_limit fuel initial);
      Model.correct program globals heap_limit frame_limit fuel initial (U.initial program input) ();
      match out, Model.run program globals heap_limit frame_limit fuel initial with
      | Runs.Finished concrete, Model.Finished model ->
        Machine.related_def program.I.origin.C.blocks width base concrete model;
        H.valid_def program globals heap_limit model (U.advance program fuel (U.initial program input));
        Hmc_heap_execute.word_agreement model.X.heap model.X.state word
      | _ -> ());
    T.source_reflection program definitions input word fuel ()
let (safe @ total) : (program : I.program) @ immutable -> (definitions : {d : M.definitions | M.origins d}) @ immutable ->
    (globals : X.globals) @ immutable -> (width : W.limb) -> (base : W.limb) -> (heap_limit : W.limb) -> (stack_limit : W.limb) ->
    (frame_limit : D.index) @ immutable -> (input : W.t) @ immutable -> (fuel : D.index) @ immutable ->
    (initial : X.configuration) @ immutable -> (before : Wasm_u32.bytes) @ immutable -> (out : Runs.result) @ immutable ->
    {u : unit | program.I.origin.C.origin.P.origin.M.definitions === definitions
      && H.valid program globals heap_limit initial (U.initial program input)
      && Runs.related program.I.origin.C.blocks width base heap_limit stack_limit before out
          (Model.run program globals heap_limit frame_limit fuel initial)} ->
    {u : unit | match out with Runs.Finished configuration | Runs.Blocked (configuration, _, _) ->
      not (configuration.Machine.state === Machine.Stuck)} @ ghost =
  fun program definitions globals width base heap_limit stack_limit frame_limit input fuel initial before out premise -> ghost_ (
    Runs.related_def program.I.origin.C.blocks width base heap_limit stack_limit before out
      (Model.run program globals heap_limit frame_limit fuel initial);
    Model.correct program globals heap_limit frame_limit fuel initial (U.initial program input) ();
    match out, Model.run program globals heap_limit frame_limit fuel initial with
    | Runs.Finished concrete, Model.Finished model ->
      Machine.related_def program.I.origin.C.blocks width base concrete model;
      H.valid_def program globals heap_limit model (U.advance program fuel (U.initial program input));
      T.safe program definitions input fuel (); State.decode_def model.X.heap model.X.state
    | Runs.Blocked (concrete, _, steps), Model.Blocked (model, _, _) ->
      Machine.related_def program.I.origin.C.blocks width base concrete model;
      H.valid_def program globals heap_limit model (U.advance program steps (U.initial program input));
      T.safe program definitions input steps (); State.decode_def model.X.heap model.X.state
    | _ -> ())
type execution = {steps : D.index; result : Runs.result}
let (preservation @ total) : (program : I.program) @ immutable -> (definitions : {d : M.definitions | M.origins d}) @ immutable ->
    (globals : X.globals) @ immutable -> (code_capacity : W.limb) -> (width : W.limb) -> (base : W.limb) ->
    (heap_limit : W.limb) -> (stack_limit : W.limb) -> (frame_limit : D.index) @ immutable ->
    (input : W.t) @ immutable -> (word : W.t) @ immutable -> (source_fuel : D.index) @ immutable ->
    (configuration : Machine.configuration) @ immutable -> (initial : X.configuration) @ ghost ->
    {u : unit | program.I.origin.C.origin.P.origin.M.definitions === definitions
      && F.advance source_fuel (Q.source_start program.I.origin.C.origin.P.origin input) === F.Done (Values.Word word)
      && H.valid program globals heap_limit initial (U.initial program input)
      && Machine.related program.I.origin.C.blocks width base configuration initial
      && Hmc_u32_index.fits (Hmc_closure_ir.size program.I.origin.C.origin.P.table) code_capacity
      && Hmc_u32_index.fits (Hmc_cfg_ir.size program.I.origin.C.blocks) code_capacity
      && width > 0 && Hmc_heap_extent.span (Hmc_memory_saved_frame.slots program.I.origin.C.blocks) (Hmc_memory_stack.zero ()) width
      && Hmc_memory_stack_capacity.region width frame_limit base stack_limit && heap_limit <= base
      && base <= configuration.Machine.top && configuration.Machine.top <= stack_limit
      && Hmc_linear_bounds.covers configuration.Machine.memory heap_limit && Hmc_linear_bounds.covers configuration.Machine.memory stack_limit} ->
    {out : execution | Runs.related program.I.origin.C.blocks width base heap_limit stack_limit configuration.Machine.memory out.result
        (Model.run program globals heap_limit frame_limit out.steps initial)
      && (returned out.result word || match out.result with Runs.Blocked _ -> true | Runs.Finished _ -> false)} @ immutable =
  fun program definitions globals code_capacity width base heap_limit stack_limit frame_limit input word source_fuel configuration initial premise ->
    let steps = T.source_preservation program definitions input word source_fuel () in
    let result = Runs.run program globals code_capacity width heap_limit base stack_limit steps configuration initial (ghost_ (U.initial program input)) frame_limit () in
    ghost_ (Runs.related_def program.I.origin.C.blocks width base heap_limit stack_limit configuration.Machine.memory result
        (Model.run program globals heap_limit frame_limit steps initial);
      returned_def result word; Model.correct program globals heap_limit frame_limit steps initial (U.initial program input) ();
      match result, Model.run program globals heap_limit frame_limit steps initial with
      | Runs.Finished concrete, Model.Finished model ->
        Machine.related_def program.I.origin.C.blocks width base concrete model;
        H.valid_def program globals heap_limit model (U.advance program steps (U.initial program input));
        Hmc_heap_execute.word_agreement model.X.heap model.X.state word
      | _ -> ());
    {steps; result}
