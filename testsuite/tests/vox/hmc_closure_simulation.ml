module D = Hm_declarative
module C = Hmc_monomorphic
module K = Hmc_closure_ir
module P = Hmc_closure_program
module Q = Hmc_monomorphic_semantics
module R = Hmc_closure_semantics
module W = Hmc_closure_values
module H = Hmc_closure_states
module S = Hmc_monomorphic_simulation
module F = Hmc_source_semantics
module V = Hm_interpreter_typing

let rec (advance @ total) : (p : P.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable -> (fuel : D.index) @ immutable ->
    (state : H.state) @ immutable ->
    {u : unit | p.P.origin.C.definitions === definitions && H.valid p.P.table state} ->
    {out : H.state | H.valid p.P.table out
      && H.source p.P.table out === Q.advance definitions fuel (H.source p.P.table state)
      && H.target out === R.advance p.P.table p.P.globals fuel (H.target state)} @ immutable =
  fun p definitions fuel state premise ->
    ghost_ (Q.advance_def definitions fuel (H.source p.P.table state);
      R.advance_def p.P.table p.P.globals fuel (H.target state));
    match fuel with D.Z -> state | D.S n ->
      let next = Hmc_closure_step.step p definitions state () in advance p definitions n next ()

let[@def] (target_start @ total) (p : P.program @ immutable) (input : Hmc_word64.t @ immutable) =
  R.initial (K.Apply (K.Closure p.P.entry, K.Word input))
let (initial @ total) : (p : P.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    {state : H.state | H.valid p.P.table state
      && H.source p.P.table state === S.target_start p.P.origin input
      && H.target state === target_start p input} @ immutable = fun p input ->
  let source = C.Apply (p.P.origin.C.entry, C.Word input) in
  let code = K.Apply (K.Closure p.P.entry, K.Word input) in
  let control = H.Evaluate (R.V.Empty, source, code) in
  let state = H.Running (control, H.Halt) in
  ghost_ (P.valid_def p;
    H.valid_def p.P.table state; H.control_valid_def p.P.table control; H.continuation_valid_def p.P.table H.Halt;
    W.valid_def p.P.table R.V.Empty; W.environment_def R.V.Empty;
    K.related_def p.P.table source code; K.related_def p.P.table (C.Word input) (K.Word input);
    H.source_def p.P.table state; H.target_def state;
    H.source_control_def p.P.table control; H.target_control_def control;
    H.source_continuation_def p.P.table H.Halt; H.target_continuation_def H.Halt;
    W.source_def p.P.table R.V.Empty;
    S.target_start_def p.P.origin input; target_start_def p input;
    Q.initial_def source; R.initial_def code);
  state
let (run @ total) : (p : P.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | p.P.origin.C.definitions === definitions} ->
    {out : H.state | H.valid p.P.table out
      && H.source p.P.table out === Q.advance definitions fuel (S.target_start p.P.origin input)
      && H.target out === R.advance p.P.table p.P.globals fuel (target_start p input)} @ immutable =
  fun p definitions input fuel premise ->
    let start = initial p input in advance p definitions fuel start ()
let (word_agreement @ total) : (table : K.table) @ immutable -> (state : H.state) @ immutable ->
    (word : Hmc_word64.t) @ immutable ->
    {u : unit | (H.source table state === Q.Done (Q.V.Word word)) = (H.target state === R.Done (R.V.Word word))} @ ghost =
  fun table state word -> ghost_ (
    H.source_def table state; H.target_def state;
    match state with H.Done v -> W.source_def table v | _ -> ())
let (normal_return @ total) : (p : P.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (output : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | p.P.origin.C.definitions === definitions} ->
    {u : unit | (Q.advance definitions fuel (S.target_start p.P.origin input) === Q.Done (Q.V.Word output))
      = (R.advance p.P.table p.P.globals fuel (target_start p input) === R.Done (R.V.Word output))} @ ghost =
  fun p definitions input output fuel premise -> ghost_ (
    let out = run p definitions input fuel () in word_agreement p.P.table out output)
let (safe @ total) : (p : P.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | p.P.origin.C.definitions === definitions} ->
    {u : unit | not (R.advance p.P.table p.P.globals fuel (target_start p input) === R.Stuck)} @ ghost =
  fun p definitions input fuel premise -> ghost_ (
    let out = run p definitions input fuel () in
    Hmc_monomorphic_safety.safe p.P.origin definitions input fuel ();
    H.source_def p.P.table out; H.target_def out)

let (source_preservation @ total) : (p : P.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (output : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | p.P.origin.C.definitions === definitions
      && F.advance fuel (S.source_start p.P.origin input) === F.Done (V.Word output)} ->
    {u : unit | R.advance p.P.table p.P.globals (D.S fuel) (target_start p input) === R.Done (R.V.Word output)} @ ghost =
  fun p definitions input output fuel premise -> ghost_ (
    S.normal_return_preservation p.P.origin definitions input output fuel ();
    normal_return p definitions input output (D.S fuel) ())
let (source_reflection @ total) : (p : P.program) @ immutable ->
    (definitions : {d : C.definitions | C.origins d}) @ immutable ->
    (input : Hmc_word64.t) @ immutable -> (output : Hmc_word64.t) @ immutable -> (fuel : D.index) @ immutable ->
    {u : unit | p.P.origin.C.definitions === definitions
      && R.advance p.P.table p.P.globals fuel (target_start p input) === R.Done (R.V.Word output)} ->
    {u : unit | F.advance (D.add (S.source_offset p.P.origin) fuel) (S.source_start p.P.origin input)
      === F.Done (V.Word output)} @ ghost = fun p definitions input output fuel premise -> ghost_ (
    normal_return p definitions input output fuel ();
    S.normal_return_reflection p.P.origin definitions input output fuel ())
