module Height = Hmc_cfg_height
module D = Hm_declarative
module K = Hmc_closure_ir
module P = Hmc_closure_program
module R = Hmc_closure_semantics
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module O = Hmc_cfg_origin
module S = Hmc_cfg_semantics
module W = Hmc_cfg_states
module A = Hmc_cfg_start
module T = Hmc_tail_runs
module N = Hmc_tail_normalize
module B = Hmc_closure_simulation
module M = Hmc_monomorphic
module Q = Hmc_monomorphic_simulation
module F = Hmc_source_semantics
module V = Hm_interpreter_typing
module I = Hmc_tail_ir
module U = Hmc_tail_semantics

let (initial @ total) : (program : I.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    {state : W.state | Height.state state === D.Z && W.valid program.I.origin.C.blocks state && W.target state === U.initial program input
      && W.source state === R.advance program.I.origin.C.origin.P.table program.I.origin.C.origin.P.globals (A.startup_steps ())
        (B.target_start program.I.origin.C.origin input)} @ immutable = fun program input ->
  let state = Hmc_cfg_simulation.initial program.I.origin input in
  ghost_ (U.initial_def program input); state

let (word_agreement @ total) : (state : W.state) @ immutable -> (word : Hmc_word64.t) @ immutable ->
    {u : unit | (W.source state === R.Done (R.V.Word word)) = (W.target state === S.Done (R.V.Word word))} @ ghost =
  fun state word -> ghost_ (W.source_def state; W.target_def state)
let (stuck_agreement @ total) : (state : W.state) @ immutable ->
    {u : unit | (W.source state === R.Stuck) = (W.target state === S.Stuck)} @ ghost =
  fun state -> ghost_ (W.source_def state; W.target_def state)
let rec (source_done_stable @ total) : (program : I.program) @ immutable -> (fuel : D.index) @ immutable ->
    (value : R.V.value) @ immutable ->
    {u : unit | R.advance program.I.origin.C.origin.P.table program.I.origin.C.origin.P.globals fuel (R.Done value) === R.Done value} @ ghost =
  fun program fuel value -> ghost_ (
    R.advance_def program.I.origin.C.origin.P.table program.I.origin.C.origin.P.globals fuel (R.Done value);
    R.step_def program.I.origin.C.origin.P.table program.I.origin.C.origin.P.globals (R.Done value);
    match fuel with D.Z -> () | D.S n -> source_done_stable program n value)

let (reflection @ total) : (program : I.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    (output : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | U.advance program fuel (U.initial program input) === S.Done (R.V.Word output)} ->
    {n : D.index | R.advance program.I.origin.C.origin.P.table program.I.origin.C.origin.P.globals n
      (B.target_start program.I.origin.C.origin input) === R.Done (R.V.Word output)} @ immutable =
  fun program input output fuel premise ->
    let start = initial program input in
    let run = T.advance program fuel start () in
    ghost_ (word_agreement run.T.state output;
      Hmc_cfg_step.source_advance_add program.I.origin (A.startup_steps ()) run.T.source_fuel (B.target_start program.I.origin.C.origin input));
    D.add (A.startup_steps ()) run.T.source_fuel

let (preservation @ total) : (program : I.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    (output : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | R.advance program.I.origin.C.origin.P.table program.I.origin.C.origin.P.globals fuel
      (B.target_start program.I.origin.C.origin input) === R.Done (R.V.Word output)} ->
    {n : D.index | Height.le n (Height.twice fuel) && U.advance program n (U.initial program input) === S.Done (R.V.Word output)} @ immutable =
  fun program input output fuel premise ->
    let start = initial program input in
    ghost_ (Hmc_cfg_step.source_advance_add program.I.origin fuel (A.startup_steps ()) (B.target_start program.I.origin.C.origin input);
      source_done_stable program (A.startup_steps ()) (R.V.Word output);
      Q.add_commute fuel (A.startup_steps ());
      Hmc_cfg_step.source_advance_add program.I.origin (A.startup_steps ()) fuel (B.target_start program.I.origin.C.origin input));
    let run = N.advance program fuel start () in
    ghost_ (word_agreement run.N.state output;
      Height.add_zero (Height.twice fuel);
      Height.prefix_le run.N.fuel (Height.state run.N.state);
      Height.transitive run.N.fuel (D.add run.N.fuel (Height.state run.N.state)) (Height.twice fuel) ());
    run.N.fuel

let (safe @ total) : (program : I.program) @ immutable ->
    (definitions : {d : M.definitions | M.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | program.I.origin.C.origin.P.origin.M.definitions === definitions} ->
    {u : unit | not (U.advance program fuel (U.initial program input) === S.Stuck)} @ ghost =
  fun program definitions input fuel premise -> ghost_ (
    let start = initial program input in
    let run = T.advance program fuel start () in
    Hmc_cfg_step.source_advance_add program.I.origin (A.startup_steps ()) run.T.source_fuel (B.target_start program.I.origin.C.origin input);
    B.safe program.I.origin.C.origin definitions input (D.add (A.startup_steps ()) run.T.source_fuel) ();
    stuck_agreement run.T.state)

let (source_preservation @ total) : (program : I.program) @ immutable ->
    (definitions : {d : M.definitions | M.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (output : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | program.I.origin.C.origin.P.origin.M.definitions === definitions
      && F.advance fuel (Q.source_start program.I.origin.C.origin.P.origin input) === F.Done (V.Word output)} ->
    {n : D.index | Height.le n (Height.twice (D.S fuel)) && U.advance program n (U.initial program input) === S.Done (R.V.Word output)} @ immutable =
  fun program definitions input output fuel premise ->
    ghost_ (B.source_preservation program.I.origin.C.origin definitions input output fuel ());
    preservation program input output (D.S fuel) ()
let (source_reflection @ total) : (program : I.program) @ immutable ->
    (definitions : {d : M.definitions | M.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (output : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | program.I.origin.C.origin.P.origin.M.definitions === definitions
      && U.advance program fuel (U.initial program input) === S.Done (R.V.Word output)} ->
    {n : D.index | F.advance n (Q.source_start program.I.origin.C.origin.P.origin input) === F.Done (V.Word output)} @ immutable =
  fun program definitions input output fuel premise ->
    let n = reflection program input output fuel () in
    ghost_ (B.source_reflection program.I.origin.C.origin definitions input output n ());
    D.add (Q.source_offset program.I.origin.C.origin.P.origin) n
