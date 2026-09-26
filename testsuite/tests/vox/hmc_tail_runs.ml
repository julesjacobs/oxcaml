module D = Hm_declarative
module C = Hmc_cfg_program
module P = Hmc_closure_program
module R = Hmc_closure_semantics
module S = Hmc_cfg_semantics
module W = Hmc_cfg_states
module I = Hmc_tail_ir
module U = Hmc_tail_semantics

let rec (advance_add @ total) : (program : I.program) @ immutable -> (first : D.index) @ immutable ->
    (second : D.index) @ immutable -> (state : S.state) @ immutable ->
    {u : unit | U.advance program (D.add first second) state === U.advance program second (U.advance program first state)} @ ghost =
  fun program first second state -> ghost_ (
    D.add_def first second; U.advance_def program first state; U.advance_def program (D.add first second) state;
    match first with D.Z -> () | D.S n -> advance_add program n second (U.step program state))

type result = {state : W.state; source_fuel : D.index}
let rec (advance @ total) : (program : I.program) @ immutable -> (fuel : D.index) @ immutable ->
    (state : W.state) @ immutable -> {u : unit | W.valid program.I.origin.C.blocks state} ->
    {r : result | W.valid program.I.origin.C.blocks r.state
      && W.source r.state === R.advance program.I.origin.C.origin.P.table program.I.origin.C.origin.P.globals r.source_fuel (W.source state)
      && W.target r.state === U.advance program fuel (W.target state)} @ immutable =
  fun program fuel state premise ->
    ghost_ (U.advance_def program fuel (W.target state));
    match fuel with
    | D.Z ->
      ghost_ (R.advance_def program.I.origin.C.origin.P.table program.I.origin.C.origin.P.globals D.Z (W.source state));
      {state; source_fuel = D.Z}
    | D.S n ->
      let next = Hmc_tail_step.step program state () in
      let rest = advance program n next () in
      ghost_ (Hmc_cfg_step.source_advance_add program.I.origin (W.source_steps state) rest.source_fuel (W.source state));
      {state = rest.state; source_fuel = D.add (W.source_steps state) rest.source_fuel}
