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
module E = Hmc_cfg_execution

let (step @ total) : (program : C.program) @ immutable -> (k : W.continuation) @ immutable ->
    (accumulator : R.V.value) @ immutable ->
    {u : unit | W.valid program.C.blocks (W.Running (W.Returning, k, accumulator))} ->
    {out : W.state | Height.le (Height.state out) (Height.continuation k)
      && W.valid program.C.blocks out
      && W.source out === R.advance program.C.origin.P.table program.C.origin.P.globals
        (W.source_steps (W.Running (W.Returning, k, accumulator))) (W.source (W.Running (W.Returning, k, accumulator)))
      && W.target out === S.step program (W.target (W.Running (W.Returning, k, accumulator)))} @ immutable =
  fun program k accumulator premise ->
    let state = W.Running (W.Returning, k, accumulator) in
    let a = W.activation W.Returning k accumulator in
    ghost_ (Height.continuation_def k; Height.grow (Height.continuation k);
      Height.reflexive (D.S (Height.continuation k)); Height.reflexive (Height.continuation k);
      Height.state_def W.Stuck; Height.le_def D.Z (Height.continuation k);
      W.valid_def program.C.blocks state; W.source_def state; W.target_def state;
      W.activation_def W.Returning k accumulator; W.continuation_valid_def program.C.blocks k;
      W.resume_def k; W.environment_def k; W.current_def k; W.temporaries_def k; W.frames_def k;
      W.source_continuation_def k; W.source_steps_def state;
      R.advance_def program.C.origin.P.table program.C.origin.P.globals (W.source_steps state) (W.source state);
      R.advance_def program.C.origin.P.table program.C.origin.P.globals D.Z (W.source state);
      R.advance_def program.C.origin.P.table program.C.origin.P.globals D.Z
        (R.step program.C.origin.P.table program.C.origin.P.globals (W.source state));
      R.step_def program.C.origin.P.table program.C.origin.P.globals (W.source state));
    match k with
    | W.Halt _ ->
      let out = W.Done accumulator in
      ghost_ (E.return_halt program a (W.frames k) (); Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out);
      out
    | W.Left (op, term, trace, _, finish, rest) ->
      let next_k = W.Right (op, accumulator, finish, rest) in
      let next_control = W.Evaluate (term, trace) in
      let out = W.Running (next_control, next_k, accumulator) in
      ghost_ (E.save_value program a (W.frames k) (O.entry trace) (W.environment rest) (W.temporaries rest) ();
        Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
        W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
        W.source_continuation_def next_k;
        Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
        W.activation_def next_control next_k accumulator);
      out
    | W.Let_body (term, trace, _, restore, rest) ->
      let next_k = W.Scope (R.V.Bind (accumulator, W.environment rest), restore, rest) in
      let next_control = W.Evaluate (term, trace) in
      let out = W.Running (next_control, next_k, accumulator) in
      ghost_ (E.bind program a (W.frames k) (O.entry trace) (W.environment rest) (W.temporaries rest) ();
        Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
        W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
        W.source_continuation_def next_k;
        Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
        W.activation_def next_control next_k accumulator);
      out
    | W.Scope (_, _, rest) ->
      let next_k = rest in
      let next_control = W.Returning in
      let out = W.Running (next_control, next_k, accumulator) in
      ghost_ (E.restore program a (W.frames k) (W.resume rest) (W.environment rest) (W.temporaries rest) ();
        Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
        W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
        W.source_continuation_def next_k;
        Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
        W.activation_def next_control next_k accumulator);
      out
    | W.Call_return (_, _, _, argument, rest) ->
      let next_k = rest in
      let next_control = W.Returning in
      let out = W.Running (next_control, next_k, accumulator) in
      ghost_ (W.saved_def rest argument; E.return_frame program a (W.frames k) (W.saved rest argument) (W.frames rest) ();
        Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
        W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
        W.source_continuation_def next_k;
        Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
        W.activation_def next_control next_k accumulator);
      out
    | W.Right (op, left, _, rest) ->
      ghost_ (W.operation_def op (W.resume rest));
      (match op with
      | W.Cons ->
        let next_k = rest in
        let next_control = W.Returning in
        let out = W.Running (next_control, next_k, (R.V.Cons (left, accumulator))) in
        ghost_ (E.cons program a (W.frames k) (W.resume rest) left (W.environment rest) (W.temporaries rest) ();
          Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
          W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
          W.source_continuation_def next_k;
          Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
          W.activation_def next_control next_k (R.V.Cons (left, accumulator)));
        out
      | W.Primitive op -> (match left, accumulator with
        | R.V.Word l, R.V.Word r ->
          let next_k = rest in
          let next_control = W.Returning in
          let out = W.Running (next_control, next_k, (R.primitive op l r)) in
          ghost_ (E.primitive program a (W.frames k) op (W.resume rest) l r (W.environment rest) (W.temporaries rest) ();
            Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
            W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
            W.source_continuation_def next_k;
            Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
            W.activation_def next_control next_k (R.primitive op l r));
          out
        | _ ->
          ghost_ (O.instruction_def program.C.blocks a.S.pc (G.Primitive (op, W.resume rest)); S.step_def program (W.target state);
            W.valid_def program.C.blocks W.Stuck; W.source_def W.Stuck; W.target_def W.Stuck);
          W.Stuck)
      | W.Apply -> (match left with
        | R.V.Closure (id, captured) -> (match K.lookup program.C.origin.P.table id with
          | None ->
            ghost_ (O.instruction_def program.C.blocks a.S.pc (G.Call (W.resume rest)); S.step_def program (W.target state);
              W.valid_def program.C.blocks W.Stuck; W.source_def W.Stuck; W.target_def W.Stuck);
            W.Stuck
          | Some callee ->
            ghost_ (C.valid_def program);
            let code = C.lookup_origin program.C.blocks program.C.origin.P.table program.C.functions id callee () in
            let env = R.V.Bind (accumulator, if callee.K.recursive then R.V.Bind (left, captured) else captured) in
            let next_k = W.Call_return (env, left, code.C.return_label, accumulator, rest) in
            let next_control = W.Evaluate (callee.K.body, code.C.trace) in
            let out = W.Running (next_control, next_k, R.V.Nil) in
            ghost_ (C.function_valid_def program.C.blocks callee code; C.return_block_def callee;
              O.instruction_def program.C.blocks code.C.return_label G.Return;
              E.call program a (W.frames k) (W.resume rest) id captured (W.environment rest) (W.temporaries rest) callee code ();
              W.saved_def rest accumulator;
              Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
              W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
              W.source_continuation_def next_k;
              Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
              W.activation_def next_control next_k R.V.Nil);
            out)
        | _ ->
          ghost_ (O.instruction_def program.C.blocks a.S.pc (G.Call (W.resume rest)); S.step_def program (W.target state);
            W.valid_def program.C.blocks W.Stuck; W.source_def W.Stuck; W.target_def W.Stuck);
          W.Stuck))
    | W.Conditional (yes, no, yt, nt, _, rest) ->
      (match accumulator with
      | R.V.True ->
        let next_k = rest in
        let next_control = W.Evaluate (yes, yt) in
        let out = W.Running (next_control, next_k, accumulator) in
        ghost_ (E.branch_true program a (W.frames k) (O.entry yt) (O.entry nt) ();
          Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
          W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
          W.source_continuation_def next_k;
          Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
          W.activation_def next_control next_k accumulator);
        out
      | R.V.False ->
        let next_k = rest in
        let next_control = W.Evaluate (no, nt) in
        let out = W.Running (next_control, next_k, accumulator) in
        ghost_ (E.branch_false program a (W.frames k) (O.entry yt) (O.entry nt) ();
          Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
          W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
          W.source_continuation_def next_k;
          Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
          W.activation_def next_control next_k accumulator);
        out
      | _ ->
        ghost_ (O.instruction_def program.C.blocks a.S.pc (G.Branch (O.entry yt, O.entry nt)); S.step_def program (W.target state);
          W.valid_def program.C.blocks W.Stuck; W.source_def W.Stuck; W.target_def W.Stuck);
        W.Stuck)
    | W.List_cases (empty, full, et, ft, _, restore, rest) ->
      (match accumulator with
      | R.V.Nil ->
        let next_k = rest in
        let next_control = W.Evaluate (empty, et) in
        let out = W.Running (next_control, next_k, accumulator) in
        ghost_ (E.list_empty program a (W.frames k) (O.entry et) (O.entry ft) ();
          Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
          W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
          W.source_continuation_def next_k;
          Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
          W.activation_def next_control next_k accumulator);
        out
      | R.V.Cons (head, tail) ->
        let next_k = W.Scope (R.V.Bind (head, R.V.Bind (tail, W.environment rest)), restore, rest) in
        let next_control = W.Evaluate (full, ft) in
        let out = W.Running (next_control, next_k, accumulator) in
        ghost_ (E.list_full program a (W.frames k) (O.entry et) (O.entry ft) head tail ();
          Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
          W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
          W.source_continuation_def next_k;
          Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
          W.activation_def next_control next_k accumulator);
        out
      | _ ->
        ghost_ (O.instruction_def program.C.blocks a.S.pc (G.List_branch (O.entry et, O.entry ft)); S.step_def program (W.target state);
          W.valid_def program.C.blocks W.Stuck; W.source_def W.Stuck; W.target_def W.Stuck);
        W.Stuck)
