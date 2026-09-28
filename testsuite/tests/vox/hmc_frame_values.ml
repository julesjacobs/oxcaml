module D = Hm_declarative
module K = Hmc_closure_ir
module P = Hmc_closure_program
module M = Hmc_monomorphic
module A = Hmc_manifest
module R = Hmc_closure_semantics
module G = Hmc_cfg_ir
module H = Hmc_frame_shape
module B = Hmc_specialized_body
module I = Hmc_instance
module C = Hmc_cfg_program

let (binding @ total) : (table : K.table) @ immutable -> (context : D.context) @ immutable ->
    (scheme : D.scheme) @ immutable -> (head : R.V.value) @ immutable -> (tail : R.V.value) @ immutable ->
    {u : unit | H.value table head && H.locals table context tail} ->
    {u : unit | H.locals table (D.Binding (scheme, context)) (R.V.Bind (head, tail))} @ ghost =
  fun table context scheme head tail premise -> ghost_ (
    H.value_def table head; H.locals_def table context tail;
    H.locals_def table (D.Binding (scheme, context)) (R.V.Bind (head, tail));
    H.valid_def table (R.V.Bind (head, tail)); H.environment_def (D.Binding (scheme, context)) (R.V.Bind (head, tail)))
let rec (local @ total) : (table : K.table) @ immutable -> (env : R.V.value) @ immutable -> (index : D.index) @ immutable ->
    (v : R.V.value) @ immutable -> {u : unit | H.valid table env && R.lookup env index === Some v} ->
    {u : unit | H.value table v} @ ghost = fun table env index v premise -> ghost_ (
  H.valid_def table env; R.lookup_def env index; H.value_def table v;
  match env, index with R.V.Bind (_, tail), D.S i -> local table tail i v () | _ -> ())
let (closure @ total) : (interface : A.table) @ immutable -> (table : K.table) @ immutable -> (g : D.context) @ immutable ->
    (id : D.index) @ immutable -> (ty : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | K.typed interface table g (K.Closure id) ty d} ->
    {u : unit | match K.lookup table id with None -> false | Some entry -> entry.K.captured === g} @ ghost =
  fun interface table g id ty d premise -> ghost_ (K.typed_def interface table g (K.Closure id) ty d)
let rec (global @ total) : (interface : A.table) @ immutable -> (table : K.table) @ immutable ->
    (source : M.definitions) @ immutable -> (globals : P.globals) @ immutable -> (id : D.index) @ immutable ->
    (code : D.index) @ immutable -> {u : unit | P.mapped interface table source globals && P.lookup globals id === Some code} ->
    {u : unit | H.value table (R.V.Closure (code, R.V.Empty))} @ ghost = fun interface table source globals id code premise -> ghost_ (
  P.mapped_def interface table source globals; P.lookup_def globals id;
  match source, globals with
  | M.Definition (d, rest), P.Global (selected, tail) ->
    if Hm_elaboration_check.index_equal id (P.size tail) then (
      closure interface table D.Empty_context selected (Hmc_ground_type.mono d.M.body.B.origin.I.ty) d.M.body.B.derivation ();
      H.value_def table (R.V.Closure (code, R.V.Empty)); H.first_class_def (R.V.Closure (code, R.V.Empty));
      H.valid_def table (R.V.Closure (code, R.V.Empty)); H.valid_def table R.V.Empty; H.environment_def D.Empty_context R.V.Empty)
    else global interface table rest tail id code ()
  | _ -> ())
let (load @ total) : (program : C.program) @ immutable -> (g : D.context) @ immutable -> (env : R.V.value) @ immutable ->
    (atom : G.atom) @ immutable -> (ty : D.mono) @ immutable -> (d : D.typing) @ immutable -> (v : R.V.value) @ immutable ->
    {u : unit | H.locals program.C.origin.P.table g env
      && K.typed (M.manifest program.C.origin.P.origin.M.definitions) program.C.origin.P.table g (G.term atom) ty d
      && Hmc_cfg_semantics.load program.C.origin.P.globals env atom === Some v} ->
    {u : unit | H.value program.C.origin.P.table v} @ ghost = fun program g env atom ty d v premise -> ghost_ (
  let p = program.C.origin in
  H.locals_def p.P.table g env; G.term_def atom; Hmc_cfg_semantics.load_def p.P.globals env atom;
  H.value_def p.P.table v; H.first_class_def v; H.valid_def p.P.table v;
  match atom with
  | G.Local index -> local p.P.table env index v ()
  | G.Global id -> (match P.lookup p.P.globals id with None -> () | Some code ->
    P.valid_def p; global (M.manifest p.P.origin.M.definitions) p.P.table p.P.origin.M.definitions p.P.globals id code ())
  | G.Closure id -> closure (M.manifest p.P.origin.M.definitions) p.P.table g id ty d ()
  | _ -> ())
let (primitive @ total) : (table : K.table) @ immutable -> (op : D.word_operation) @ immutable ->
    (a : Hmc_word64.t) @ immutable -> (b : Hmc_word64.t) @ immutable ->
    {u : unit | H.value table (R.primitive op a b)} @ ghost = fun table op a b -> ghost_ (
  R.primitive_def op a b; H.value_def table (R.primitive op a b);
  H.first_class_def (R.primitive op a b); H.valid_def table (R.primitive op a b))
