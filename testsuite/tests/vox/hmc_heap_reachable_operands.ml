module D = Hm_declarative
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module S = Hmc_cfg_semantics
module M = Hmc_monomorphic
module C = Hmc_cfg_program
module K = Hmc_closure_program
module Operands = Hmc_heap_operand_shapes
let rec (advance_step @ total) : (program : I.program) @ immutable -> (fuel : D.index) @ immutable ->
    (state : S.state) @ immutable ->
    {u : unit | U.advance program fuel (U.step program state) === U.step program (U.advance program fuel state)} @ ghost =
  fun program fuel state -> ghost_ (
    U.advance_def program fuel state; U.advance_def program fuel (U.step program state);
    match fuel with D.Z -> () | D.S rest -> advance_step program rest (U.step program state))
let (advance_next @ total) : (program : I.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    (fuel : D.index) @ immutable ->
    {u : unit | U.advance program (D.S fuel) (U.initial program input) === U.step program (U.advance program fuel (U.initial program input))} @ ghost =
  fun program input fuel -> ghost_ (
    U.advance_def program (D.S fuel) (U.initial program input);
    advance_step program fuel (U.initial program input))
let (progress @ total) : (program : I.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    (fuel : D.index) @ immutable -> (state : S.state) @ immutable ->
    {u : unit | state === U.advance program fuel (U.initial program input)} ->
    {u : unit | Operands.not_stuck state && Operands.not_stuck (U.step program state)} @ ghost =
  fun program input fuel state premise -> ghost_ (
    let source = program.I.origin.C.origin.K.origin in
    M.ready_def source;
    let definitions : {d : M.definitions | M.origins d} = refine_ source.M.definitions in
    Hmc_tail_simulation.safe program definitions input fuel ();
    Hmc_tail_simulation.safe program definitions input (D.S fuel) ();
    advance_next program input fuel;
    Operands.not_stuck_def state; Operands.not_stuck_def (U.step program state))
