module D = Hm_declarative
module K = Hmc_closure_ir
module P = Hmc_closure_program
module R = Hmc_closure_semantics
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module O = Hmc_cfg_origin
module S = Hmc_cfg_semantics
module B = Hmc_closure_simulation
module M = Hmc_monomorphic
module T = Hmc_templates

let[@def] (environment @ total) (id : D.index @ immutable) (entry : K.entry @ immutable)
    (input : Hmc_word64.t @ immutable) =
  R.V.Bind (R.V.Word input, if entry.K.recursive then R.V.Bind (R.V.Closure (id, R.V.Empty), R.V.Empty) else R.V.Empty)
let[@def] (startup_steps @ total) (u : unit) = D.S (D.S (D.S (D.S (D.S D.Z))))

let (source_start @ total) : (p : P.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    (entry : K.entry) @ immutable -> {u : unit | K.lookup p.P.table p.P.entry === Some entry} ->
    {u : unit | R.advance p.P.table p.P.globals (startup_steps ()) (B.target_start p input)
      === R.Running (R.Evaluate (environment p.P.entry entry input, entry.K.body), R.Halt)} @ ghost =
  fun p input entry premise -> ghost_ (
    let start = B.target_start p input in
    let s1 = R.Running (R.Evaluate (R.V.Empty, K.Closure p.P.entry), R.Apply_function (R.V.Empty, K.Word input, R.Halt)) in
    let s2 = R.Running (R.Return (R.V.Closure (p.P.entry, R.V.Empty)), R.Apply_function (R.V.Empty, K.Word input, R.Halt)) in
    let s3 = R.Running (R.Evaluate (R.V.Empty, K.Word input), R.Apply_argument (R.V.Closure (p.P.entry, R.V.Empty), R.Halt)) in
    let s4 = R.Running (R.Return (R.V.Word input), R.Apply_argument (R.V.Closure (p.P.entry, R.V.Empty), R.Halt)) in
    startup_steps_def (); B.target_start_def p input; R.initial_def (K.Apply (K.Closure p.P.entry, K.Word input));
    environment_def p.P.entry entry input;
    R.step_def p.P.table p.P.globals start; R.step_def p.P.table p.P.globals s1;
    R.step_def p.P.table p.P.globals s2; R.step_def p.P.table p.P.globals s3;
    R.step_def p.P.table p.P.globals s4;
    R.advance_def p.P.table p.P.globals (startup_steps ()) start;
    R.advance_def p.P.table p.P.globals (D.S (D.S (D.S (D.S D.Z)))) s1;
    R.advance_def p.P.table p.P.globals (D.S (D.S (D.S D.Z))) s2;
    R.advance_def p.P.table p.P.globals (D.S (D.S D.Z)) s3;
    R.advance_def p.P.table p.P.globals (D.S D.Z) s4;
    R.advance_def p.P.table p.P.globals D.Z
      (R.Running (R.Evaluate (environment p.P.entry entry input, entry.K.body), R.Halt)))

type result = {entry : K.entry; code : C.function_entry; activation : S.activation}
let (initial @ total) : (p : C.program) @ immutable -> (input : Hmc_word64.t) @ immutable ->
    {r : result | K.lookup p.C.origin.P.table p.C.origin.P.entry === Some r.entry
      && C.lookup p.C.functions p.C.origin.P.entry === Some r.code
      && C.function_valid p.C.blocks r.entry r.code
      && S.initial p input === S.Running (r.activation, S.Halt)
      && r.activation.S.pc === O.entry r.code.C.trace
      && r.activation.S.env === environment p.C.origin.P.entry r.entry input
      && r.activation.S.temporaries === S.Empty
      && r.activation.S.current === R.V.Closure (p.C.origin.P.entry, R.V.Empty)
      && R.advance p.C.origin.P.table p.C.origin.P.globals (startup_steps ()) (B.target_start p.C.origin input)
        === R.Running (R.Evaluate (r.activation.S.env, r.entry.K.body), R.Halt)} @ immutable =
  fun p input ->
    let origin = p.C.origin in
    ghost_ (C.valid_def p; P.valid_def origin;
      K.typed_def (M.manifest origin.P.origin.M.definitions) origin.P.table D.Empty_context
        (K.Closure origin.P.entry) (D.Function (D.Word64, D.Word64)) origin.P.origin.M.source.T.derivation);
    match K.lookup origin.P.table origin.P.entry with
    | None -> unreachable_ ()
    | Some entry ->
      let code = C.lookup_origin p.C.blocks origin.P.table p.C.functions origin.P.entry entry () in
      let activation = {S.pc = code.C.start; env = environment origin.P.entry entry input;
        accumulator = R.V.Nil; temporaries = S.Empty; current = R.V.Closure (origin.P.entry, R.V.Empty)} in
      ghost_ (C.function_valid_def p.C.blocks entry code; S.initial_def p input;
        environment_def origin.P.entry entry input; source_start origin input entry ());
      {entry; code; activation}
