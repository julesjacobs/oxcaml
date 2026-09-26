module D = Hm_declarative
module T = Hmc_templates
module I = Hmc_instance
module A = Hmc_ground_arguments
module G = Hmc_ground_annotations
module F = Hmc_admission

type tree = Empty | Reference of D.index * I.instance | Child of tree
  | Pair of tree * tree | Triple of tree * tree * tree [@@inductive]

let[@def] rec (context @ total) (locals : D.context @ immutable)
    (globals : D.context @ immutable) = match locals with
  | D.Empty_context -> globals
  | D.Binding (scheme, rest) -> D.Binding (scheme, context rest globals)

let[@def] rec (global_index @ total) (locals : D.context @ immutable)
    (index : D.index @ immutable) = match locals with
  | D.Empty_context -> Some index
  | D.Binding (_, rest) -> match index with D.Z -> None | D.S i -> global_index rest i

let rec (global_lookup @ total) : (locals : D.context) @ immutable -> (globals : D.context) @ immutable ->
    (index : D.index) @ immutable -> (global : D.index) @ immutable ->
    {u : unit | global_index locals index === Some global} ->
    {u : unit | D.lookup (context locals globals) index === D.lookup globals global} @ ghost =
  fun locals globals index global premise -> ghost_ (
    global_index_def locals index; context_def locals globals;
    match locals with D.Empty_context -> ()
    | D.Binding (_, rest) -> D.lookup_def (context locals globals) index;
      (match index with D.Z -> () | D.S i -> global_lookup rest globals i global ()))

let[@def] rec (below @ total) (catalog : T.catalog @ immutable) (tree : tree @ immutable) = ghost_ (
  match tree with
  | Empty -> true
  | Reference (_, instance) -> D.present (T.rank catalog) instance.I.key.A.owner
  | Child child -> below catalog child
  | Pair (a, b) -> below catalog a && below catalog b
  | Triple (a, b, c) -> below catalog a && below catalog b && below catalog c)

let[@def] rec (records @ total) (catalog : T.catalog @ immutable) (locals : D.context @ immutable)
    (term : D.term @ immutable) (d : D.typing @ immutable) (tree : tree @ immutable) = ghost_ (
  match term, d, tree with
  | D.Bound i, D.Variable _, Empty -> global_index locals i === None
  | D.Bound i, D.Variable args, Reference (j, instance) ->
    global_index locals i === Some j
    && T.selection catalog j === Some {T.definition = instance.I.definition; earlier = instance.I.earlier}
    && A.declarative instance.I.key.A.arguments === args
  | (D.Truth | D.False), D.Constant, Empty | D.Word _, D.Word_constant, Empty
  | D.Nil, D.Empty_list _, Empty -> true
  | D.Lambda body, D.Abstraction (a, db), Child child ->
    records catalog (D.Binding (D.Forall (D.Z, a), locals)) body db child
  | D.Recursive body, D.Recursion (a, b, db), Child child ->
    records catalog (D.Binding (D.Forall (D.Z, a),
      D.Binding (D.Forall (D.Z, D.Function (a, b)), locals))) body db child
  | D.Apply (a, b), D.Application (_, da, db), Pair (ra, rb)
  | D.Cons (a, b), D.List_cons (_, da, db), Pair (ra, rb)
  | D.Primitive (_, a, b), D.Word_primitive (da, db), Pair (ra, rb) ->
    records catalog locals a da ra && records catalog locals b db rb
  | D.If (a, b, c), D.Conditional (da, db, dc), Triple (ra, rb, rc) ->
    records catalog locals a da ra && records catalog locals b db rb && records catalog locals c dc rc
  | D.CaseList (s, a, b), D.List_case (element, ds, da, db), Triple (rs, ra, rb) ->
    records catalog locals s ds rs && records catalog locals a da ra
    && records catalog (D.Binding (D.Forall (D.Z, element),
      D.Binding (D.Forall (D.Z, D.List_type element), locals))) b db rb
  | D.Let (a, b), D.Let_binding (D.Forall (D.Z, ty), da, db), Pair (ra, rb) ->
    records catalog locals a da ra && records catalog (D.Binding (D.Forall (D.Z, ty), locals)) b db rb
  | _ -> false)

let rec (arguments_ground @ total) : (args : D.arguments) @ immutable ->
    {u : unit | G.arguments args} -> {u : unit | A.ground args} @ ghost = fun args premise -> ghost_ (
    G.arguments_def args; A.ground_def args;
    match args with D.No_arguments -> () | D.Argument (_, rest) -> arguments_ground rest ())

let rec (scan @ total) : (catalog : {c : T.catalog | T.valid c}) @ immutable ->
    (locals : D.context) @ immutable -> (term : D.term) @ immutable ->
    (ty : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | D.typed D.Z (context locals (T.context catalog)) term ty d
      && F.local term d && G.typing d} ->
    {r : tree | records catalog locals term d r && below catalog r} @ immutable =
  fun catalog locals term ty d premise ->
    ghost_ (D.typed_def D.Z (context locals (T.context catalog)) term ty d;
      F.local_def term d; G.typing_def d);
    let out = match term, d with
    | D.Bound i, D.Variable args ->
      (match global_index locals i with
      | None -> Empty
      | Some j ->
        ghost_ (global_lookup locals (T.context catalog) i j (); arguments_ground args ());
        match A.read args with
        | None -> unreachable_ ()
        | Some actuals ->
          ghost_ (A.represented actuals);
          match I.request catalog j actuals with
          | I.Unknown_template | I.Wrong_arity -> unreachable_ ()
          | I.Instance instance -> Reference (j, instance))
    | (D.Truth | D.False), D.Constant | D.Word _, D.Word_constant | D.Nil, D.Empty_list _ -> Empty
    | D.Lambda body, D.Abstraction (a, db) ->
      (match ty with D.Function (_, b) ->
        let next = D.Binding (D.Forall (D.Z, a), locals) in
        ghost_ (context_def next (T.context catalog));
        Child (scan catalog next body b db ())
      | _ -> unreachable_ ())
    | D.Recursive body, D.Recursion (a, b, db) ->
      let self = D.Binding (D.Forall (D.Z, D.Function (a, b)), locals) in
      let next = D.Binding (D.Forall (D.Z, a), self) in
      ghost_ (context_def self (T.context catalog); context_def next (T.context catalog));
      Child (scan catalog next body b db ())
    | D.Apply (a, b), D.Application (arg, da, db) ->
      Pair (scan catalog locals a (D.Function (arg, ty)) da (), scan catalog locals b arg db ())
    | D.Cons (a, b), D.List_cons (element, da, db) ->
      Pair (scan catalog locals a element da (), scan catalog locals b ty db ())
    | D.Primitive (_, a, b), D.Word_primitive (da, db) ->
      Pair (scan catalog locals a D.Word64 da (), scan catalog locals b D.Word64 db ())
    | D.If (a, b, c), D.Conditional (da, db, dc) ->
      Triple (scan catalog locals a D.Boolean da (), scan catalog locals b ty db (), scan catalog locals c ty dc ())
    | D.CaseList (s, a, b), D.List_case (element, ds, da, db) ->
      let tail = D.Binding (D.Forall (D.Z, D.List_type element), locals) in
      let next = D.Binding (D.Forall (D.Z, element), tail) in
      ghost_ (context_def tail (T.context catalog); context_def next (T.context catalog));
      Triple (scan catalog locals s (D.List_type element) ds (), scan catalog locals a ty da (),
        scan catalog next b ty db ())
    | D.Let (a, b), D.Let_binding (D.Forall (D.Z, arg), da, db) ->
      let next = D.Binding (D.Forall (D.Z, arg), locals) in
      ghost_ (D.add_def D.Z D.Z; context_def next (T.context catalog);
        let identity = {Hm_interpreter_substitution.front = D.No_arguments; tail = D.Z} in
        Hmc_parameter_closed.closed_context identity D.Z (context locals (T.context catalog)) ());
      Pair (scan catalog locals a arg da (), scan catalog next b ty db ())
    | _ -> unreachable_ () in
    ghost_ (records_def catalog locals term d out; below_def catalog out);
    out

let (body @ total) : (body : Hmc_specialized_body.t) @ immutable ->
    {r : tree | records body.Hmc_specialized_body.origin.I.earlier D.Empty_context
      body.Hmc_specialized_body.origin.I.definition.T.source body.Hmc_specialized_body.derivation r
      && below body.Hmc_specialized_body.origin.I.earlier r} @ immutable = fun body ->
  let origin = body.Hmc_specialized_body.origin in
  ghost_ (Hmc_specialized_body.valid_def body; I.valid_def origin;
    context_def D.Empty_context (T.context origin.I.earlier));
  let catalog : {c : T.catalog | T.valid c} = refine_ origin.I.earlier in
  scan catalog D.Empty_context origin.I.definition.T.source
    (Hmc_ground_type.mono origin.I.ty) body.Hmc_specialized_body.derivation ()

let (entry @ total) : (program : {p : T.program | T.ready p}) @ immutable ->
    {r : tree | records program.T.globals D.Empty_context program.T.entry program.T.derivation r
      && below program.T.globals r} @ immutable = fun program ->
  ghost_ (T.ready_def program; context_def D.Empty_context (T.context program.T.globals);
    G.typing_ground (T.context program.T.globals) program.T.entry
      (D.Function (D.Word64, D.Word64)) program.T.derivation ());
  let catalog : {c : T.catalog | T.valid c} = refine_ program.T.globals in
  scan catalog D.Empty_context program.T.entry (D.Function (D.Word64, D.Word64)) program.T.derivation ()
