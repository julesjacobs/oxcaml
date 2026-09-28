module D = Hm_declarative
module K = Hmc_closure_ir
module P = Hmc_closure_program
module R = Hmc_closure_semantics
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module S = Hmc_cfg_semantics
module H = Hmc_frame_shape
module V = Hmc_frame_values

let (edge @ total) : (table : K.table) @ immutable -> (blocks : G.table) @ immutable ->
    (g : D.context) @ immutable -> (temporaries : G.temporaries) @ immutable -> (accumulator : D.mono option) @ immutable ->
    (a : S.activation) @ immutable ->
    {u : unit | G.accepts blocks a.S.pc g temporaries accumulator && H.locals table g a.S.env
      && H.temporaries table temporaries a.S.temporaries && H.value table a.S.accumulator && H.value table a.S.current} ->
    {u : unit | H.at_label table blocks a} @ ghost = fun table blocks g temporaries accumulator a premise -> ghost_ (
  G.accepts_def blocks a.S.pc g temporaries accumulator; H.at_label_def table blocks a;
  match G.lookup blocks a.S.pc with None -> () | Some block -> H.activation_def table block.G.signature a)
let (entry @ total) : (table : K.table) @ immutable -> (blocks : G.table) @ immutable -> (g : D.context) @ immutable ->
    (a : S.activation) @ immutable ->
    {u : unit | G.entry blocks a.S.pc g G.Empty_temporaries && a.S.temporaries === S.Empty
      && H.locals table g a.S.env && H.value table a.S.accumulator && H.value table a.S.current} ->
    {u : unit | H.at_label table blocks a} @ ghost = fun table blocks g a premise -> ghost_ (
  G.entry_def blocks a.S.pc g G.Empty_temporaries; H.at_label_def table blocks a;
  H.temporaries_def table G.Empty_temporaries S.Empty;
  match G.lookup blocks a.S.pc with None -> () | Some block -> H.activation_def table block.G.signature a)
let (accumulator @ total) : (table : K.table) @ immutable -> (blocks : G.table) @ immutable ->
    (a : S.activation) @ immutable -> (v : R.V.value) @ immutable ->
    {u : unit | H.at_label table blocks a && H.value table v} ->
    {u : unit | H.at_label table blocks {a with S.accumulator = v}} @ ghost = fun table blocks a v premise -> ghost_ (
  H.at_label_def table blocks a; H.at_label_def table blocks {a with S.accumulator = v};
  match G.lookup blocks a.S.pc with None -> () | Some block ->
    H.activation_def table block.G.signature a; H.activation_def table block.G.signature {a with S.accumulator = v})
let (enter @ total) : (p : C.program) @ immutable -> (id : D.index) @ immutable -> (captured : R.V.value) @ immutable ->
    (arg : R.V.value) @ immutable -> (callee : K.entry) @ immutable -> (code : C.function_entry) @ immutable ->
    {u : unit | H.value p.C.origin.P.table (R.V.Closure (id, captured)) && H.value p.C.origin.P.table arg
      && K.lookup p.C.origin.P.table id === Some callee && C.function_valid p.C.blocks callee code} ->
    {a : S.activation | H.at_label p.C.origin.P.table p.C.blocks a
      && a === {S.pc = code.C.start;
        env = R.V.Bind (arg, if callee.K.recursive then R.V.Bind (R.V.Closure (id, captured), captured) else captured);
        accumulator = R.V.Nil; temporaries = S.Empty; current = R.V.Closure (id, captured)}} @ immutable =
  fun p id captured arg callee code premise ->
    let closure = R.V.Closure (id, captured) in
    let table = p.C.origin.P.table in
    ghost_ (H.value_def table closure; H.valid_def table closure; H.locals_def table callee.K.captured captured;
      C.function_valid_def p.C.blocks callee code; K.context_def callee);
    let env = if callee.K.recursive then R.V.Bind (closure, captured) else captured in
    ghost_ (if callee.K.recursive then V.binding table callee.K.captured
      (D.Forall (D.Z, D.Function (callee.K.argument, callee.K.result))) closure captured () else ());
    let a = {S.pc = code.C.start; env = R.V.Bind (arg, env); accumulator = R.V.Nil; temporaries = S.Empty; current = closure} in
    ghost_ (V.binding table (if callee.K.recursive then D.Binding (D.Forall (D.Z, D.Function (callee.K.argument, callee.K.result)), callee.K.captured)
      else callee.K.captured) (D.Forall (D.Z, callee.K.argument)) arg env ();
      H.value_def table R.V.Nil; H.valid_def table R.V.Nil; H.first_class_def R.V.Nil;
      entry table p.C.blocks (K.context callee) a ());
    a
