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

let (step @ total) : (program : C.program) @ immutable -> (term : K.term) @ immutable ->
    (trace : O.t) @ immutable -> (k : W.continuation) @ immutable -> (accumulator : R.V.value) @ immutable ->
    {u : unit | W.valid program.C.blocks (W.Running (W.Evaluate (term, trace), k, accumulator))} ->
    {out : W.state | Height.le (Height.state out) (D.S (Height.continuation k))
      && W.valid program.C.blocks out
      && W.source out === R.step program.C.origin.P.table program.C.origin.P.globals
        (W.source (W.Running (W.Evaluate (term, trace), k, accumulator)))
      && W.target out === S.step program (W.target (W.Running (W.Evaluate (term, trace), k, accumulator)))} @ immutable =
  fun program term trace k accumulator premise ->
    let control = W.Evaluate (term, trace) in
    let state = W.Running (control, k, accumulator) in
    let a = W.activation control k accumulator in
    ghost_ (Height.continuation_def k; Height.grow (Height.continuation k);
      Height.reflexive (D.S (Height.continuation k)); Height.reflexive (Height.continuation k);
      Height.state_def W.Stuck; Height.le_def D.Z (D.S (Height.continuation k));
      W.valid_def program.C.blocks state; W.source_def state; W.target_def state;
      W.activation_def control k accumulator; O.entry_def trace;
      O.generated_def program.C.blocks term (W.resume k) trace;
      R.step_def program.C.origin.P.table program.C.origin.P.globals (W.source state));
    match term, trace with
    | _, O.Leaf (_, atom, ty, derivation) ->
      ghost_ (G.term_def atom; E.leaf_agreement program (W.environment k) atom (W.source_continuation k));
      (match S.load program.C.origin.P.globals (W.environment k) atom with
      | None ->
        ghost_ (O.instruction_def program.C.blocks a.S.pc (G.Load (atom, ty, derivation, W.resume k));
          S.step_def program (W.target state); W.valid_def program.C.blocks W.Stuck;
          W.source_def W.Stuck; W.target_def W.Stuck);
        W.Stuck
      | Some value ->
        let out = W.Running (W.Returning, k, value) in
        ghost_ (E.load program a (W.frames k) atom ty derivation (W.resume k) value ();
          Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
          W.activation_def W.Returning k value);
        out)
    | K.Apply (left, right), O.Binary (_, save, finish, l, r) ->
      let next_k = W.Left (W.Apply, right, r, save, finish, k) in
      let next_control = W.Evaluate (left, l) in
      let out = W.Running (next_control, next_k, accumulator) in
      ghost_ (E.save_environment program a (W.frames k) (O.entry l) ();
        W.operation_def W.Apply (W.resume k); O.binary_operation_def term (W.resume k);
        Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
        W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
        W.source_continuation_def next_k;
        Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
        W.activation_def next_control next_k accumulator);
      out
    | K.Cons (left, right), O.Binary (_, save, finish, l, r) ->
      let next_k = W.Left (W.Cons, right, r, save, finish, k) in
      let next_control = W.Evaluate (left, l) in
      let out = W.Running (next_control, next_k, accumulator) in
      ghost_ (E.save_environment program a (W.frames k) (O.entry l) ();
        W.operation_def W.Cons (W.resume k); O.binary_operation_def term (W.resume k);
        Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
        W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
        W.source_continuation_def next_k;
        Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
        W.activation_def next_control next_k accumulator);
      out
    | K.Primitive (op, left, right), O.Binary (_, save, finish, l, r) ->
      let next_k = W.Left (W.Primitive op, right, r, save, finish, k) in
      let next_control = W.Evaluate (left, l) in
      let out = W.Running (next_control, next_k, accumulator) in
      ghost_ (E.save_environment program a (W.frames k) (O.entry l) ();
        W.operation_def (W.Primitive op) (W.resume k); O.binary_operation_def term (W.resume k);
        Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
        W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
        W.source_continuation_def next_k;
        Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
        W.activation_def next_control next_k accumulator);
      out
    | K.Let (left, right), O.Binding (_, bind, restore, l, r) ->
      let next_k = W.Let_body (right, r, bind, restore, k) in
      let next_control = W.Evaluate (left, l) in
      let out = W.Running (next_control, next_k, accumulator) in
      ghost_ (E.save_environment program a (W.frames k) (O.entry l) ();

        Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
        W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
        W.source_continuation_def next_k;
        Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
        W.activation_def next_control next_k accumulator);
      out
    | K.If (condition, yes, no), O.Conditional (_, branch, c, yt, nt) ->
      let next_k = W.Conditional (yes, no, yt, nt, branch, k) in
      let next_control = W.Evaluate (condition, c) in
      let out = W.Running (next_control, next_k, accumulator) in
      ghost_ (E.jump program a (W.frames k) (O.entry c) ();

        Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
        W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
        W.source_continuation_def next_k;
        Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
        W.activation_def next_control next_k accumulator);
      out
    | K.CaseList (scrutinee, empty, full), O.Matching (_, branch, restore, st, et, ft) ->
      let next_k = W.List_cases (empty, full, et, ft, branch, restore, k) in
      let next_control = W.Evaluate (scrutinee, st) in
      let out = W.Running (next_control, next_k, accumulator) in
      ghost_ (E.jump program a (W.frames k) (O.entry st) ();

        Height.continuation_def next_k; Height.grow (Height.continuation next_k);
        Height.reflexive (Height.continuation next_k); W.continuation_valid_def program.C.blocks next_k; W.resume_def next_k;
        W.environment_def next_k; W.current_def next_k; W.temporaries_def next_k; W.frames_def next_k;
        W.source_continuation_def next_k;
        Height.state_def out; W.valid_def program.C.blocks out; W.source_def out; W.target_def out;
        W.activation_def next_control next_k accumulator);
      out
    | _ -> unreachable_ ()
