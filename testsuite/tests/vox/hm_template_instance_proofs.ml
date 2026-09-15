open Copy_spec
open Level_spec
module D = Hm_declarative
module T = Hm_type_proofs
module A = Hm_abstraction
module F = Hm_freshness_proofs

let[@def] rec (arguments @ total) (names : A.names @ immutable)
    (choices : (node Pref.t @ immutable total -> ty @ immutable total) @ total) = ghost_ (
  match names with A.No_names -> D.No_arguments
  | A.Name (p, rest) -> D.Argument (T.embed (choices p), arguments rest choices))

let rec (arguments_length @ total) : (names : A.names) @ immutable ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    {u : unit | D.length (arguments names choices) === A.count names} @ ghost = fun names choices -> ghost_ (
    arguments_def names choices; A.count_def names;
    let args = arguments names choices in D.length_def args;
    (match names with A.No_names -> () | A.Name (_, rest) -> arguments_length rest choices; ());
    let u = () in refine_ u)

let rec (arguments_wf @ total) : (names : A.names) @ immutable ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    {u : unit | D.arguments_wf D.Z (arguments names choices)} @ ghost = fun names choices -> ghost_ (
    arguments_def names choices; let args = arguments names choices in
    let z = D.Z in D.arguments_wf_def z args;
    (match names with A.No_names -> () | A.Name (p, rest) ->
      let t = choices p in T.embed_wf z t; arguments_wf rest choices; ()); let u = () in refine_ u)

let rec (open_position @ total) : (names : A.names) @ immutable ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (p : node Pref.t) @ immutable -> (i : D.index) @ immutable ->
    {u : unit | A.position names p === Some i} ->
    {u : unit | D.open_index (arguments names choices) i === T.embed (choices p)} @ ghost =
  fun names choices p i premise -> ghost_ (
    let refine_ premise = premise in A.position_def names p; arguments_def names choices;
    let args = arguments names choices in D.open_index_def args i;
    let u = () in match names with A.No_names -> refine_ u | A.Name (q, rest) ->
      if p === q then refine_ u else match A.position rest p with
      | None -> refine_ u | Some j -> open_position rest choices p j (refine_ u); refine_ u)

let (open_parameter @ total) : (names : A.names) @ immutable ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (p : node Pref.t) @ immutable -> {u : unit | not (A.position names p === None)} ->
    {u : unit | D.open_type (arguments names choices)
      (A.abstract_type names D.Z (T.embed (Variable p))) === T.embed (choices p)} @ ghost =
  fun names choices p premise -> ghost_ (
    let refine_ premise = premise in let z = D.Z in let t = Variable p in T.embed_def t;
    let mono = T.embed t in A.abstract_type_def names z mono; A.abstract_free_def names z p;
    let body = A.abstract_type names z mono in let args = arguments names choices in
    D.open_type_def args body; let u = () in match A.position names p with None -> refine_ u
    | Some i -> D.add_def z i; open_position names choices p i (refine_ u); refine_ u)

let (open_boundary @ total) : (names : A.names) @ immutable ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (t : ty) @ immutable -> {u : unit | A.avoids names (T.embed t)} ->
    {u : unit | D.open_type (arguments names choices)
      (A.abstract_type names D.Z (T.embed t)) === T.embed t} @ ghost = fun names choices t premise -> ghost_ (
    let refine_ premise = premise in let z = D.Z in let mono = T.embed t in let u = () in
    Hm_abstraction_proofs.abstract_avoids names z mono (refine_ u);
    let k = A.count names in Hm_substitution_proofs.shift_embed z k t;
    let args = arguments names choices in Hm_substitution_proofs.open_embed args t; refine_ u)

let[@def] rec (parameters_in @ total) (names : A.names @ immutable) (schema : template @ immutable) = ghost_ (
  match schema with Boundary _ | Constant _ -> true
  | Parameter p -> not (A.position names p === None)
  | Indirect (_, child) -> parameters_in names child
  | Product (_, a, b) -> parameters_in names a && parameters_in names b)

let[@def] rec (boundaries_avoid @ total) (names : A.names @ immutable)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (schema : template @ immutable) = ghost_ (match schema with
  | Boundary p -> A.avoids names (T.embed (rho p))
  | Parameter _ | Constant _ -> true
  | Indirect (_, child) -> boundaries_avoid names rho child
  | Product (_, a, b) -> boundaries_avoid names rho a && boundaries_avoid names rho b)

let rec (open_template @ total) : (names : A.names) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schema : template) @ immutable ->
    {u : unit | parameters_in names schema && boundaries_avoid names rho schema} ->
    {u : unit | D.open_type (arguments names choices)
      (A.abstract_type names D.Z (T.embed (interpret rho F.variable_choice schema)))
      === T.embed (interpret rho choices schema)} @ ghost = fun names rho choices schema premise -> ghost_ (
    let refine_ premise = premise in parameters_in_def names schema; boundaries_avoid_def names rho schema;
    let variable_choice = F.variable_choice in interpret_def rho variable_choice schema; interpret_def rho choices schema;
    let u = () in match schema with
    | Boundary p -> let t = rho p in open_boundary names choices t (refine_ u); refine_ u
    | Parameter p -> F.variable_choice_def p; open_parameter names choices p (refine_ u); refine_ u
    | Constant _ -> let t = Boolean in let mono = T.embed t in T.embed_def t;
      A.avoids_def names mono; open_boundary names choices t (refine_ u); refine_ u
    | Indirect (_, child) -> open_template names rho choices child (refine_ u); refine_ u
    | Product (_, a, b) ->
      let old = interpret rho F.variable_choice schema in let next = interpret rho choices schema in
      T.embed_def old; T.embed_def next; let mono = T.embed old in let z = D.Z in
      A.abstract_type_def names z mono; let body = A.abstract_type names z mono in
      let args = arguments names choices in D.open_type_def args body;
      open_template names rho choices a (refine_ u); open_template names rho choices b (refine_ u); refine_ u)

let rec (parameters_subset @ total) : (schema : template) @ immutable -> (names : A.names) @ immutable ->
    (included : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position (F.template_names schema) p === None || not (A.position names p === None)})) @ total ->
    {u : unit | parameters_in names schema} @ ghost = fun schema names included -> ghost_ (
    F.template_names_def schema; parameters_in_def names schema; let u = () in match schema with
    | Boundary _ | Constant _ -> refine_ u
    | Parameter p -> included p; let ps = F.template_names schema in A.position_def ps p; refine_ u
    | Indirect (_, child) -> let refine_ u = parameters_subset child names (refine_ included) in refine_ u
    | Product (_, a, b) ->
      let left : ((p : node Pref.t) @ immutable ->
        {u : unit | A.position (F.template_names a) p === None || not (A.position names p === None)}) @ total = fun p ->
        included p; let aa = F.template_names a in let bb = F.template_names b in
        F.join_position aa bb p; let u = () in refine_ u in
      let right : ((p : node Pref.t) @ immutable ->
        {u : unit | A.position (F.template_names b) p === None || not (A.position names p === None)}) @ total = fun p ->
        included p; let aa = F.template_names a in let bb = F.template_names b in
        F.join_position aa bb p; let u = () in refine_ u in
      parameters_subset a names left; parameters_subset b names right; refine_ u)

let (scheme_instance @ total) :
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schema : template) @ immutable ->
    {u : unit | boundaries_avoid (F.template_names schema) rho schema} ->
    {args : D.arguments | D.arguments_wf D.Z args
      && D.length args === D.arity (F.template_scheme rho schema)
      && D.open_scheme (F.template_scheme rho schema) args === T.embed (interpret rho choices schema)} @ immutable ghost =
  fun rho choices schema premise -> ghost_ (
    let refine_ premise = premise in let names = F.template_names schema in
    let included : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position (F.template_names schema) p === None || not (A.position names p === None)}) @ total =
      fun _p -> let u = () in refine_ u in
    parameters_subset schema names included; let u = () in open_template names rho choices schema (refine_ u);
    F.template_scheme_def rho schema; let sigma = F.template_scheme rho schema in let args = arguments names choices in
    D.arity_def sigma; D.open_scheme_def sigma args; arguments_length names choices; arguments_wf names choices; refine_ args)

let rec (selected_generic @ total) : (h : Pref.heap) @ immutable -> (schema : template) @ immutable ->
    (p : node Pref.t) @ immutable -> {u : unit | template h schema} ->
    {u : unit | A.position (F.template_names schema) p === None || at_level h p === Generic} @ ghost =
  fun h schema p premise -> ghost_ (
    let refine_ premise = premise in template_def h schema; F.template_names_def schema;
    let names = F.template_names schema in let u = () in match schema with
    | Boundary _ | Constant _ -> A.position_def names p; refine_ u
    | Parameter q -> A.position_def names p;
      let empty = A.No_names in A.position_def empty p;
      let var : desc = Var in generic_desc_def h q var; at_level_def h q; refine_ u
    | Indirect (_, child) -> selected_generic h child p (refine_ u); refine_ u
    | Product (_, a, b) -> let aa = F.template_names a in let bb = F.template_names b in
      F.join_position aa bb p; selected_generic h a p (refine_ u); selected_generic h b p (refine_ u); refine_ u)

let rec (canonical_boundaries @ total) : (h : Pref.heap) @ immutable -> (cut : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | Level_spec.ordered h p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === p
        && (not (H.mem h p) || Level_finite_spec.finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === Level_finite_spec.readback t})) @ total ->
    (names : A.names) @ immutable ->
    (generic : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || at_level h p === Generic})) @ total ->
    (schema : template) @ immutable ->
    {u : unit | template h schema && Hm_environment_spec.boundary_bound h cut schema} ->
    {u : unit | boundaries_avoid names rho schema} @ ghost =
  fun h cut order trees rho values names generic schema premise -> ghost_ (
    let refine_ premise = premise in template_def h schema; Hm_environment_spec.boundary_bound_def h cut schema;
    boundaries_avoid_def names rho schema; let u = () in match schema with
    | Parameter _ | Constant _ -> refine_ u
    | Boundary p -> root_def schema; below_def h p cut; let refine_ tree = trees p in values p;
      let high : ((q : node Pref.t) @ immutable ->
        {u : unit | A.position names q === None || not (Level_spec.below h q cut)}) @ total = fun q ->
        generic q; Level_spec.below_def h q cut; let u = () in refine_ u in
      F.readback_avoids h cut order names high tree (refine_ u); refine_ u
    | Indirect (_, child) -> canonical_boundaries h cut order trees rho values names generic child (refine_ u); refine_ u
    | Product (_, a, b) -> canonical_boundaries h cut order trees rho values names generic a (refine_ u);
      canonical_boundaries h cut order trees rho values names generic b (refine_ u); refine_ u)

let (canonical_instance @ total) : (h : Pref.heap) @ immutable -> (cut : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | Level_spec.ordered h p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === p
        && (not (H.mem h p) || Level_finite_spec.finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === Level_finite_spec.readback t})) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schema : template) @ immutable ->
    {u : unit | template h schema && Hm_environment_spec.boundary_bound h cut schema} ->
    {args : D.arguments | D.arguments_wf D.Z args
      && D.length args === D.arity (F.template_scheme rho schema)
      && D.open_scheme (F.template_scheme rho schema) args === T.embed (interpret rho choices schema)} @ immutable ghost =
  fun h cut order trees rho values choices schema premise -> ghost_ (
    let refine_ premise = premise in let names = F.template_names schema in
    let generic : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || at_level h p === Generic}) @ total = fun p ->
        let u = () in let refine_ u = selected_generic h schema p (refine_ u) in refine_ u in
    let u = () in canonical_boundaries h cut order trees rho values names generic schema (refine_ u);
    let refine_ args = scheme_instance rho choices schema (refine_ u) in refine_ args)

let[@def] rec (body @ total) (names : A.names @ immutable)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (schema : template @ immutable) = ghost_ (match schema with
  | Boundary p -> T.embed (rho p)
  | Parameter p -> A.abstract_free names D.Z p
  | Constant _ -> D.Boolean
  | Indirect (_, child) -> body names rho child
  | Product (_, a, b) -> D.Function (body names rho a, body names rho b))

let[@def] (scheme @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (schema : template @ immutable) = ghost_ (
  let names = F.template_names schema in D.Forall (A.count names, body names rho schema))

let[@def] rec (context @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (schemas : Hm_environment_spec.templates @ immutable) = ghost_ (match schemas with
  | Hm_environment_spec.No_templates -> D.Empty_context
  | Hm_environment_spec.Template_binding (schema, rest) -> D.Binding (scheme rho schema, context rho rest))

let rec (abstract_body @ total) : (names : A.names) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schema : template) @ immutable -> {u : unit | boundaries_avoid names rho schema} ->
    {u : unit | A.abstract_type names D.Z (T.embed (interpret rho F.variable_choice schema)) === body names rho schema} @ ghost =
  fun names rho schema premise -> ghost_ (
    let refine_ premise = premise in boundaries_avoid_def names rho schema; body_def names rho schema;
    let identity = F.variable_choice in interpret_def rho identity schema;
    let t = interpret rho identity schema in T.embed_def t;
    let mono = T.embed t in let z = D.Z in A.abstract_type_def names z mono;
    let u = () in match schema with
    | Boundary p -> Hm_abstraction_proofs.abstract_avoids names z mono (refine_ u);
      let k = A.count names in Hm_substitution_proofs.shift_embed z k t; refine_ u
    | Parameter p -> F.variable_choice_def p; let var = Variable p in T.embed_def var;
      let free = D.Free p in A.abstract_type_def names z free; refine_ u
    | Constant _ -> refine_ u
    | Indirect (_, child) -> abstract_body names rho child (refine_ u); refine_ u
    | Product (_, a, b) -> abstract_body names rho a (refine_ u); abstract_body names rho b (refine_ u); refine_ u)

let (scheme_reification @ total) :
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schema : template) @ immutable -> {u : unit | boundaries_avoid (F.template_names schema) rho schema} ->
    {u : unit | F.template_scheme rho schema === scheme rho schema} @ ghost = fun rho schema premise -> ghost_ (
    let refine_ premise = premise in F.template_scheme_def rho schema; scheme_def rho schema;
    let names = F.template_names schema in let u = () in abstract_body names rho schema (refine_ u); refine_ u)

let rec (body_instance @ total) : (names : A.names) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schema : template) @ immutable -> {u : unit | parameters_in names schema} ->
    {u : unit | D.open_type (arguments names choices) (body names rho schema)
      === T.embed (interpret rho choices schema)} @ ghost = fun names rho choices schema premise -> ghost_ (
    let refine_ premise = premise in parameters_in_def names schema; body_def names rho schema;
    interpret_def rho choices schema; let t = interpret rho choices schema in T.embed_def t;
    let args = arguments names choices in let mono = body names rho schema in D.open_type_def args mono;
    let u = () in match schema with
    | Boundary p -> let v = rho p in Hm_substitution_proofs.open_embed args v; refine_ u
    | Parameter p -> let z = D.Z in A.abstract_free_def names z p;
      (match A.position names p with None -> refine_ u | Some i ->
        D.add_def z i; open_position names choices p i (refine_ u); refine_ u)
    | Constant _ -> refine_ u
    | Indirect (_, child) -> body_instance names rho choices child (refine_ u); refine_ u
    | Product (_, a, b) -> body_instance names rho choices a (refine_ u);
      body_instance names rho choices b (refine_ u); refine_ u)

let (direct_instance @ total) :
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schema : template) @ immutable ->
    {args : D.arguments | D.arguments_wf D.Z args
      && D.length args === D.arity (scheme rho schema)
      && D.open_scheme (scheme rho schema) args === T.embed (interpret rho choices schema)} @ immutable ghost =
  fun rho choices schema -> ghost_ (
    let names = F.template_names schema in
    let included : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position (F.template_names schema) p === None || not (A.position names p === None)}) @ total =
      fun _p -> let u = () in refine_ u in
    parameters_subset schema names included; let u = () in body_instance names rho choices schema (refine_ u);
    scheme_def rho schema; let sigma = scheme rho schema in let args = arguments names choices in
    D.arity_def sigma; D.open_scheme_def sigma args; arguments_length names choices; arguments_wf names choices; refine_ args)

let rec (body_wf @ total) : (names : A.names) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schema : template) @ immutable -> {u : unit | parameters_in names schema} ->
    {u : unit | D.mono_wf (A.count names) (body names rho schema)} @ ghost = fun names rho schema premise -> ghost_ (
    let refine_ premise = premise in parameters_in_def names schema; body_def names rho schema;
    let n = A.count names in let mono = body names rho schema in D.mono_wf_def n mono;
    let u = () in match schema with
    | Boundary p -> let t = rho p in T.embed_wf n t; refine_ u
    | Parameter p -> let z = D.Z in A.abstract_free_def names z p;
      (match A.position names p with None -> refine_ u | Some i ->
        D.add_def z i; Hm_abstraction_proofs.position_bound names p i (refine_ u); refine_ u)
    | Constant _ -> refine_ u
    | Indirect (_, child) -> body_wf names rho child (refine_ u); refine_ u
    | Product (_, a, b) -> body_wf names rho a (refine_ u); body_wf names rho b (refine_ u); refine_ u)

let (scheme_wf @ total) :
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schema : template) @ immutable -> {u : unit | D.scheme_wf D.Z (scheme rho schema)} @ ghost = fun rho schema -> ghost_ (
    let names = F.template_names schema in
    let included : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position (F.template_names schema) p === None || not (A.position names p === None)}) @ total =
      fun _p -> let u = () in refine_ u in
    parameters_subset schema names included; let u = () in body_wf names rho schema (refine_ u);
    scheme_def rho schema; let sigma = scheme rho schema in let z = D.Z in D.scheme_wf_def z sigma;
    let n = A.count names in Hm_abstraction_proofs.add_zero n; refine_ u)

let rec (context_wf @ total) :
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schemas : Hm_environment_spec.templates) @ immutable -> {u : unit | D.context_wf D.Z (context rho schemas)} @ ghost =
  fun rho schemas -> ghost_ (
    context_def rho schemas; let g = context rho schemas in let z = D.Z in D.context_wf_def z g;
    (match schemas with Hm_environment_spec.No_templates -> ()
    | Hm_environment_spec.Template_binding (schema, rest) -> scheme_wf rho schema; context_wf rho rest; ());
    let u = () in refine_ u)

let rec (lookup_context @ total) :
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schemas : Hm_environment_spec.templates) @ immutable -> (i : D.index) @ immutable ->
    (schema : template) @ immutable -> {u : unit | Hm_environment_spec.template_lookup schemas i === Some schema} ->
    {u : unit | D.lookup (context rho schemas) i === Some (scheme rho schema)} @ ghost =
  fun rho schemas i schema premise -> ghost_ (
    let refine_ premise = premise in context_def rho schemas; Hm_environment_spec.template_lookup_def schemas i;
    let g = context rho schemas in D.lookup_def g i; let u = () in match schemas with
    | Hm_environment_spec.No_templates -> refine_ u
    | Hm_environment_spec.Template_binding (_, rest) -> match i with D.Z -> refine_ u
      | D.S j -> lookup_context rho rest j schema (refine_ u); refine_ u)

let rec (body_model_transport @ total) : (names : A.names) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (schema : template) @ immutable ->
    (equal : ((p : node Pref.t) @ immutable ->
      {u : unit | not (Hm_environment_spec.boundary_member schema p) || rho p === tau p})) @ total ->
    {u : unit | body names rho schema === body names tau schema} @ ghost = fun names rho tau schema equal -> ghost_ (
    body_def names rho schema; body_def names tau schema; let u = () in match schema with
    | Parameter _ | Constant _ -> refine_ u
    | Boundary p -> equal p; Hm_environment_spec.boundary_member_def schema p; refine_ u
    | Indirect (_, child) ->
      let next : ((p : node Pref.t) @ immutable ->
        {u : unit | not (Hm_environment_spec.boundary_member child p) || rho p === tau p}) @ total = fun p ->
        equal p; Hm_environment_spec.boundary_member_def schema p; let u = () in refine_ u in
      body_model_transport names rho tau child next; refine_ u
    | Product (_, a, b) ->
      let left : ((p : node Pref.t) @ immutable ->
        {u : unit | not (Hm_environment_spec.boundary_member a p) || rho p === tau p}) @ total = fun p ->
        equal p; Hm_environment_spec.boundary_member_def schema p; let u = () in refine_ u in
      let right : ((p : node Pref.t) @ immutable ->
        {u : unit | not (Hm_environment_spec.boundary_member b p) || rho p === tau p}) @ total = fun p ->
        equal p; Hm_environment_spec.boundary_member_def schema p; let u = () in refine_ u in
      body_model_transport names rho tau a left; body_model_transport names rho tau b right; refine_ u)
