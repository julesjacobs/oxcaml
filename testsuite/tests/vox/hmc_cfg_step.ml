module Height = Hmc_cfg_height
module D = Hm_declarative
module P = Hmc_closure_program
module R = Hmc_closure_semantics
module C = Hmc_cfg_program
module S = Hmc_cfg_semantics
module W = Hmc_cfg_states

let (step @ total) : (program : C.program) @ immutable -> (state : W.state) @ immutable ->
    {u : unit | W.valid program.C.blocks state} ->
    {out : W.state | Height.le (Height.state out) (D.S (Height.state state))
      && W.valid program.C.blocks out
      && W.source out === R.advance program.C.origin.P.table program.C.origin.P.globals (W.source_steps state) (W.source state)
      && W.target out === S.step program (W.target state)} @ immutable =
  fun program state premise ->
    ghost_ (Height.state_def state; Height.grow (Height.state state));
    match state with
    | W.Running (W.Evaluate (term, trace), k, acc) ->
      let out = Hmc_cfg_evaluate.step program term trace k acc () in
      ghost_ (W.source_steps_def state;
        R.advance_def program.C.origin.P.table program.C.origin.P.globals (D.S D.Z) (W.source state);
        R.advance_def program.C.origin.P.table program.C.origin.P.globals D.Z
          (R.step program.C.origin.P.table program.C.origin.P.globals (W.source state)));
      out
    | W.Running (W.Returning, k, acc) ->
      let out = Hmc_cfg_return.step program k acc () in
      ghost_ (Height.weaken (Height.state out) (Height.continuation k) ()); out
    | W.Done _ | W.Stuck ->
      ghost_ (W.source_def state; W.target_def state; W.source_steps_def state;
        S.step_def program (W.target state); R.step_def program.C.origin.P.table program.C.origin.P.globals (W.source state);
        R.advance_def program.C.origin.P.table program.C.origin.P.globals (D.S D.Z) (W.source state);
        R.advance_def program.C.origin.P.table program.C.origin.P.globals D.Z (W.source state));
      state

let rec (source_advance_add @ total) : (program : C.program) @ immutable -> (first : D.index) @ immutable ->
    (second : D.index) @ immutable -> (state : R.state) @ immutable ->
    {u : unit | R.advance program.C.origin.P.table program.C.origin.P.globals (D.add first second) state
      === R.advance program.C.origin.P.table program.C.origin.P.globals second
        (R.advance program.C.origin.P.table program.C.origin.P.globals first state)} @ ghost =
  fun program first second state -> ghost_ (
    D.add_def first second;
    R.advance_def program.C.origin.P.table program.C.origin.P.globals first state;
    R.advance_def program.C.origin.P.table program.C.origin.P.globals (D.add first second) state;
    match first with D.Z -> () | D.S n -> source_advance_add program n second
      (R.step program.C.origin.P.table program.C.origin.P.globals state))

type result = {state : W.state; source_fuel : D.index}
let rec (advance @ total) : (program : C.program) @ immutable -> (fuel : D.index) @ immutable ->
    (state : W.state) @ immutable -> {u : unit | W.valid program.C.blocks state} ->
    {r : result | W.valid program.C.blocks r.state
      && W.source r.state === R.advance program.C.origin.P.table program.C.origin.P.globals r.source_fuel (W.source state)
      && W.target r.state === S.advance program fuel (W.target state)} @ immutable =
  fun program fuel state premise ->
    ghost_ (S.advance_def program fuel (W.target state));
    match fuel with
    | D.Z ->
      ghost_ (R.advance_def program.C.origin.P.table program.C.origin.P.globals D.Z (W.source state));
      {state; source_fuel = D.Z}
    | D.S n ->
      let next = step program state () in
      let rest = advance program n next () in
      ghost_ (source_advance_add program (W.source_steps state) rest.source_fuel (W.source state));
      {state = rest.state; source_fuel = D.add (W.source_steps state) rest.source_fuel}
