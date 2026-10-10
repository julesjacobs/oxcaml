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
module T = Hmc_tail_sites
module I = Hmc_tail_ir
module U = Hmc_tail_semantics
module F = Hmc_tail_continuation
module E = Hmc_tail_execution
module B = Hmc_cfg_step

let (tail_call @ total) : (p : I.program) @ immutable -> (left : R.V.value) @ immutable ->
    (label : D.index) @ immutable -> (rest : W.continuation) @ immutable -> (arg : R.V.value) @ immutable -> (exit : T.exit) @ immutable ->
    {u : unit | W.valid p.I.origin.C.blocks (W.Running (W.Returning, W.Right (W.Apply, left, label, rest), arg))
      && I.find p.I.sites label === Some exit} ->
    {out : W.state | Height.le (Height.state out) (D.S (Height.continuation rest))
      && W.valid p.I.origin.C.blocks out
      && W.source out === R.advance p.I.origin.C.origin.P.table p.I.origin.C.origin.P.globals
        (W.source_steps (W.Running (W.Returning, W.Right (W.Apply, left, label, rest), arg)))
        (W.source (W.Running (W.Returning, W.Right (W.Apply, left, label, rest), arg)))
      && W.target out === U.step p (W.target (W.Running (W.Returning, W.Right (W.Apply, left, label, rest), arg)))} @ immutable =
  fun p left label rest arg exit premise ->
    let k = W.Right (W.Apply, left, label, rest) in
    let state = W.Running (W.Returning, k, arg) in
    ghost_ (Height.state_def W.Stuck; Height.le_def D.Z (D.S (Height.continuation rest)); I.valid_def p; W.valid_def p.I.origin.C.blocks state; W.continuation_valid_def p.I.origin.C.blocks k;
      W.operation_def W.Apply (W.resume rest);
      I.find_valid p.I.origin.C.blocks p.I.sites label exit ();
      F.unique p.I.origin.C.blocks label (G.Call (W.resume rest)) (G.Call (T.entry exit)) ();
      F.tail_of_exit p.I.origin.C.blocks exit rest ();
      W.source_def state; W.target_def state; W.activation_def W.Returning k arg;
      W.resume_def k; W.environment_def k; W.current_def k; W.temporaries_def k; W.frames_def k;
      W.source_continuation_def k; W.source_steps_def state;
      O.instruction_def p.I.origin.C.blocks label (G.Call (W.resume rest));
      I.lookup_related p.I.origin.C.blocks p.I.code p.I.sites label ();
      I.select_def p.I.sites label (G.Call (W.resume rest));
      U.step_def p (W.target state);
      R.step_def p.I.origin.C.origin.P.table p.I.origin.C.origin.P.globals (W.source state);
      R.advance_def p.I.origin.C.origin.P.table p.I.origin.C.origin.P.globals (D.S D.Z) (W.source state);
      R.advance_def p.I.origin.C.origin.P.table p.I.origin.C.origin.P.globals D.Z
        (R.step p.I.origin.C.origin.P.table p.I.origin.C.origin.P.globals (W.source state)));
    match left with
    | R.V.Closure (id, captured) -> (match K.lookup p.I.origin.C.origin.P.table id with
      | None ->
        ghost_ (W.valid_def p.I.origin.C.blocks W.Stuck; W.source_def W.Stuck; W.target_def W.Stuck); W.Stuck
      | Some callee ->
        ghost_ (C.valid_def p.I.origin);
        let code = C.lookup_origin p.I.origin.C.blocks p.I.origin.C.origin.P.table p.I.origin.C.functions id callee () in
        let env = R.V.Bind (arg, if callee.K.recursive then R.V.Bind (left, captured) else captured) in
        ghost_ (C.function_valid_def p.I.origin.C.blocks callee code; C.return_block_def callee;
          O.instruction_def p.I.origin.C.blocks code.C.return_label G.Return);
        let next_k = F.retarget p.I.origin.C.blocks rest env left code.C.return_label () in
        let control = W.Evaluate (callee.K.body, code.C.trace) in
        let out = W.Running (control, next_k, R.V.Nil) in
        ghost_ (Height.state_def out; Height.weaken (Height.continuation next_k) (Height.continuation rest) (); W.valid_def p.I.origin.C.blocks out; W.source_def out; W.target_def out;
          W.activation_def control next_k R.V.Nil);
        out)
    | _ -> ghost_ (W.valid_def p.I.origin.C.blocks W.Stuck; W.source_def W.Stuck; W.target_def W.Stuck); W.Stuck

let (step @ total) : (p : I.program) @ immutable -> (state : W.state) @ immutable ->
    {u : unit | W.valid p.I.origin.C.blocks state} ->
    {out : W.state | Height.le (Height.state out) (D.S (Height.state state))
      && W.valid p.I.origin.C.blocks out
      && W.source out === R.advance p.I.origin.C.origin.P.table p.I.origin.C.origin.P.globals (W.source_steps state) (W.source state)
      && W.target out === U.step p (W.target state)} @ immutable = fun p state premise ->
  ghost_ (Height.state_def state);
  match state with
  | W.Running (W.Returning, W.Right (W.Apply, left, label, rest), arg) ->
    (match I.find p.I.sites label with
    | Some exit ->
      let out = tail_call p left label rest arg exit () in
      ghost_ (Height.continuation_def (W.Right (W.Apply, left, label, rest));
        Height.weaken (Height.state out) (D.S (Height.continuation rest)) ()); out
    | None ->
      ghost_ (W.target_def state; W.activation_def W.Returning (W.Right (W.Apply, left, label, rest)) arg;
        W.resume_def (W.Right (W.Apply, left, label, rest)); E.unselected_def p (W.target state);
        E.ordinary p (W.target state) ());
      B.step p.I.origin state ())
  | W.Running (W.Evaluate (term, trace), k, acc) ->
    ghost_ (E.evaluate p term trace k acc (); E.ordinary p (W.target state) ()); B.step p.I.origin state ()
  | W.Running (W.Returning, k, acc) ->
    ghost_ (E.call_def k; E.returning p k acc (); E.ordinary p (W.target state) ()); B.step p.I.origin state ()
  | W.Done _ | W.Stuck ->
    ghost_ (W.target_def state; E.unselected_def p (W.target state); E.ordinary p (W.target state) ()); B.step p.I.origin state ()
