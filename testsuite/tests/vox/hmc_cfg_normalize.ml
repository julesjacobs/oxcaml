module Height = Hmc_cfg_height
module D = Hm_declarative
module P = Hmc_closure_program
module R = Hmc_closure_semantics
module C = Hmc_cfg_program
module S = Hmc_cfg_semantics
module W = Hmc_cfg_states
module E = Hmc_cfg_execution
module T = Hmc_cfg_step

let[@def] (administrative @ total) (k : W.continuation @ immutable) = match k with
  | W.Scope _ | W.Call_return _ -> true | _ -> false

type result = {continuation : W.continuation; fuel : D.index}
let rec (normalize @ total) : (program : C.program) @ immutable -> (k : W.continuation) @ immutable ->
    (acc : R.V.value) @ immutable -> {u : unit | W.continuation_valid program.C.blocks k} ->
    {r : result | W.continuation_valid program.C.blocks r.continuation && not (administrative r.continuation)
      && D.add r.fuel (Height.continuation r.continuation) === Height.continuation k
      && W.source (W.Running (W.Returning, r.continuation, acc)) === W.source (W.Running (W.Returning, k, acc))
      && W.target (W.Running (W.Returning, r.continuation, acc))
        === S.advance program r.fuel (W.target (W.Running (W.Returning, k, acc)))} @ immutable =
  fun program k acc premise ->
    ghost_ (Height.continuation_def k; administrative_def k; W.continuation_valid_def program.C.blocks k);
    match k with
    | W.Scope (_, _, rest) ->
      let r = normalize program rest acc () in
      let state = W.Running (W.Returning, k, acc) in
      let next = W.Running (W.Returning, rest, acc) in
      let a = W.activation W.Returning k acc in
      ghost_ (W.source_def state; W.source_def next;
        W.source_continuation_def k; W.target_def state; W.target_def next;
        W.activation_def W.Returning k acc; W.activation_def W.Returning rest acc;
        W.resume_def k; W.environment_def k; W.current_def k; W.temporaries_def k; W.frames_def k;
        E.restore program a (W.frames k) (W.resume rest) (W.environment rest) (W.temporaries rest) ();
        S.advance_def program (D.S r.fuel) (W.target state));
      ghost_ (D.add_def (D.S r.fuel) (Height.continuation r.continuation));
      {continuation = r.continuation; fuel = D.S r.fuel}
    | W.Call_return (_, _, _, argument, rest) ->
      let r = normalize program rest acc () in
      let state = W.Running (W.Returning, k, acc) in
      let next = W.Running (W.Returning, rest, acc) in
      let a = W.activation W.Returning k acc in
      ghost_ (W.source_def state; W.source_def next;
        W.source_continuation_def k; W.target_def state; W.target_def next;
        W.activation_def W.Returning k acc; W.activation_def W.Returning rest acc;
        W.resume_def k; W.environment_def k; W.current_def k; W.temporaries_def k; W.frames_def k;
        W.saved_def rest argument; E.return_frame program a (W.frames k) (W.saved rest argument) (W.frames rest) ();
        S.advance_def program (D.S r.fuel) (W.target state));
      ghost_ (D.add_def (D.S r.fuel) (Height.continuation r.continuation));
      {continuation = r.continuation; fuel = D.S r.fuel}
    | _ ->
      ghost_ (S.advance_def program D.Z (W.target (W.Running (W.Returning, k, acc))));
      ghost_ (D.add_def D.Z (Height.continuation k));
      {continuation = k; fuel = D.Z}

type execution = {state : W.state; fuel : D.index}
let (step @ total) : (program : C.program) @ immutable -> (state : W.state) @ immutable ->
    {u : unit | W.valid program.C.blocks state} ->
    {r : execution | Height.le (D.add r.fuel (Height.state r.state)) (D.S (D.S (Height.state state)))
      && W.valid program.C.blocks r.state
      && W.source r.state === R.step program.C.origin.P.table program.C.origin.P.globals (W.source state)
      && W.target r.state === S.advance program r.fuel (W.target state)} @ immutable =
  fun program state premise ->
  ghost_ (Height.state_def state);
  match state with
  | W.Running (W.Returning, k, acc) ->
    ghost_ (W.valid_def program.C.blocks state);
    let normalized = normalize program k acc () in
    let before = W.Running (W.Returning, normalized.continuation, acc) in
    ghost_ (W.valid_def program.C.blocks before; administrative_def normalized.continuation;
      W.source_steps_def before);
    let after = T.step program before () in
    ghost_ (Height.state_def before;
      Height.normalized_work normalized.fuel (Height.state state) (Height.state before) (Height.state after) ());
    ghost_ (R.advance_def program.C.origin.P.table program.C.origin.P.globals (D.S D.Z) (W.source before);
      R.advance_def program.C.origin.P.table program.C.origin.P.globals D.Z
        (R.step program.C.origin.P.table program.C.origin.P.globals (W.source before));
      S.advance_def program (D.S D.Z) (W.target before);
      S.advance_def program D.Z (S.step program (W.target before));
      E.advance_add program normalized.fuel (D.S D.Z) (W.target state));
    {state = after; fuel = D.add normalized.fuel (D.S D.Z)}
  | _ ->
    let after = T.step program state () in
    ghost_ (D.add_def (D.S D.Z) (Height.state after); D.add_def D.Z (Height.state after);
      Height.le_def (D.S (Height.state after)) (D.S (D.S (Height.state state))));
    ghost_ (W.source_steps_def state;
      R.advance_def program.C.origin.P.table program.C.origin.P.globals (D.S D.Z) (W.source state);
      R.advance_def program.C.origin.P.table program.C.origin.P.globals D.Z
        (R.step program.C.origin.P.table program.C.origin.P.globals (W.source state));
      S.advance_def program (D.S D.Z) (W.target state);
      S.advance_def program D.Z (S.step program (W.target state)));
    {state = after; fuel = D.S D.Z}

let rec (advance @ total) : (program : C.program) @ immutable -> (fuel : D.index) @ immutable ->
    (state : W.state) @ immutable -> {u : unit | W.valid program.C.blocks state} ->
    {r : execution | Height.le (D.add r.fuel (Height.state r.state))
        (D.add (Height.twice fuel) (Height.state state))
      && W.valid program.C.blocks r.state
      && W.source r.state === R.advance program.C.origin.P.table program.C.origin.P.globals fuel (W.source state)
      && W.target r.state === S.advance program r.fuel (W.target state)} @ immutable =
  fun program fuel state premise ->
    ghost_ (Height.twice_def fuel);
    ghost_ (R.advance_def program.C.origin.P.table program.C.origin.P.globals fuel (W.source state));
    match fuel with
    | D.Z -> ghost_ (D.add_def D.Z (Height.state state); Height.reflexive (Height.state state); S.advance_def program D.Z (W.target state)); {state; fuel = D.Z}
    | D.S n ->
      let next = step program state () in
      let rest = advance program n next.state () in
      ghost_ (Height.compose_work next.fuel rest.fuel (Height.state state) (Height.state next.state)
        (Height.state rest.state) (Height.twice n) ();
        D.add_def (D.S (D.S (Height.twice n))) (Height.state state);
        D.add_def (D.S (Height.twice n)) (Height.state state));
      ghost_ (E.advance_add program next.fuel rest.fuel (W.target state));
      {state = rest.state; fuel = D.add next.fuel rest.fuel}
