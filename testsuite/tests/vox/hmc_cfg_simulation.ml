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
module T = Hmc_cfg_step
module N = Hmc_cfg_normalize
module B = Hmc_closure_simulation
module M = Hmc_monomorphic
module Q = Hmc_monomorphic_simulation
module F = Hmc_source_semantics
module V = Hm_interpreter_typing

let (initial @ total) : (program : C.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    {state : W.state | Height.state state === D.Z && W.valid program.C.blocks state && W.target state === S.initial program input
      && W.source state === R.advance program.C.origin.P.table program.C.origin.P.globals (A.startup_steps ())
        (B.target_start program.C.origin input)} @ immutable = fun program input ->
  let start = A.initial program input in
  let a = start.A.activation in
  let k = W.Halt (a.S.env, a.S.current, start.A.code.C.return_label) in
  let control = W.Evaluate (start.A.entry.K.body, start.A.code.C.trace) in
  let state = W.Running (control, k, a.S.accumulator) in
  ghost_ (Height.state_def state; Height.continuation_def k;
    C.function_valid_def program.C.blocks start.A.entry start.A.code;
    C.return_block_def start.A.entry;
    O.instruction_def program.C.blocks start.A.code.C.return_label G.Return;
    W.continuation_valid_def program.C.blocks k; W.resume_def k; W.environment_def k;
    W.current_def k; W.temporaries_def k; W.frames_def k; W.source_continuation_def k;
    W.valid_def program.C.blocks state; W.source_def state; W.target_def state;
    W.activation_def control k a.S.accumulator);
  state

let (word_agreement @ total) : (state : W.state) @ immutable -> (word : Hmc_word64.t) @ immutable ->
    {u : unit | (W.source state === R.Done (R.V.Word word)) = (W.target state === S.Done (R.V.Word word))} @ ghost =
  fun state word -> ghost_ (W.source_def state; W.target_def state)
let (stuck_agreement @ total) : (state : W.state) @ immutable ->
    {u : unit | (W.source state === R.Stuck) = (W.target state === S.Stuck)} @ ghost =
  fun state -> ghost_ (W.source_def state; W.target_def state)
let rec (source_done_stable @ total) : (program : C.program) @ immutable -> (fuel : D.index) @ immutable ->
    (value : R.V.value) @ immutable ->
    {u : unit | R.advance program.C.origin.P.table program.C.origin.P.globals fuel (R.Done value) === R.Done value} @ ghost =
  fun program fuel value -> ghost_ (
    R.advance_def program.C.origin.P.table program.C.origin.P.globals fuel (R.Done value);
    R.step_def program.C.origin.P.table program.C.origin.P.globals (R.Done value);
    match fuel with D.Z -> () | D.S n -> source_done_stable program n value)

let (reflection @ total) : (program : C.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    (output : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | S.advance program fuel (S.initial program input) === S.Done (R.V.Word output)} ->
    {n : D.index | R.advance program.C.origin.P.table program.C.origin.P.globals n
      (B.target_start program.C.origin input) === R.Done (R.V.Word output)} @ immutable =
  fun program input output fuel premise ->
    let start = initial program input in
    let run = T.advance program fuel start () in
    ghost_ (word_agreement run.T.state output;
      T.source_advance_add program (A.startup_steps ()) run.T.source_fuel (B.target_start program.C.origin input));
    D.add (A.startup_steps ()) run.T.source_fuel

let (preservation @ total) : (program : C.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    (output : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | R.advance program.C.origin.P.table program.C.origin.P.globals fuel
      (B.target_start program.C.origin input) === R.Done (R.V.Word output)} ->
    {n : D.index | S.advance program n (S.initial program input) === S.Done (R.V.Word output)} @ immutable =
  fun program input output fuel premise ->
    let start = initial program input in
    ghost_ (T.source_advance_add program fuel (A.startup_steps ()) (B.target_start program.C.origin input);
      source_done_stable program (A.startup_steps ()) (R.V.Word output);
      Q.add_commute fuel (A.startup_steps ());
      T.source_advance_add program (A.startup_steps ()) fuel (B.target_start program.C.origin input));
    let run = N.advance program fuel start () in
    ghost_ (word_agreement run.N.state output);
    run.N.fuel

let (safe @ total) : (program : C.program) @ immutable ->
    (definitions : {d : M.definitions | M.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | program.C.origin.P.origin.M.definitions === definitions} ->
    {u : unit | not (S.advance program fuel (S.initial program input) === S.Stuck)} @ ghost =
  fun program definitions input fuel premise -> ghost_ (
    let start = initial program input in
    let run = T.advance program fuel start () in
    T.source_advance_add program (A.startup_steps ()) run.T.source_fuel (B.target_start program.C.origin input);
    B.safe program.C.origin definitions input (D.add (A.startup_steps ()) run.T.source_fuel) ();
    stuck_agreement run.T.state)

let (source_preservation @ total) : (program : C.program) @ immutable ->
    (definitions : {d : M.definitions | M.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (output : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | program.C.origin.P.origin.M.definitions === definitions
      && F.advance fuel (Q.source_start program.C.origin.P.origin input) === F.Done (V.Word output)} ->
    {n : D.index | S.advance program n (S.initial program input) === S.Done (R.V.Word output)} @ immutable =
  fun program definitions input output fuel premise ->
    ghost_ (B.source_preservation program.C.origin definitions input output fuel ());
    preservation program input output (D.S fuel) ()
let (source_reflection @ total) : (program : C.program) @ immutable ->
    (definitions : {d : M.definitions | M.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (output : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | program.C.origin.P.origin.M.definitions === definitions
      && S.advance program fuel (S.initial program input) === S.Done (R.V.Word output)} ->
    {n : D.index | F.advance n (Q.source_start program.C.origin.P.origin input) === F.Done (V.Word output)} @ immutable =
  fun program definitions input output fuel premise ->
    let n = reflection program input output fuel () in
    ghost_ (B.source_reflection program.C.origin definitions input output n ());
    D.add (Q.source_offset program.C.origin.P.origin) n
