open Copy_spec
open Level_spec
open Level_finite_spec
module A = Hm_abstraction
module T = Hm_type_proofs
module D = Hm_declarative

let (below_child @ total) : (h : Pref.heap) @ immutable -> (cut : int) ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | ordered h p && below h p cut && Level_finite_spec.edge h p q} ->
    {u : unit | below h q cut} @ ghost = fun h cut p q premise -> ghost_ (
    let refine_ premise = premise in ordered_def h p; below_def h p cut;
    at_level_def h p; Level_finite_spec.edge_def h p q; Level_unifier_spec.observe_def h p;
    let u = () in match H.at h p with None -> refine_ u | Some v -> match v.level with
    | Generic -> refine_ u | Finite n -> children_below_def h v.desc n;
      (match v.desc with Var | Bool -> () | Link a -> below_def h a n; at_level_def h a; ()
      | Arrow (a, b) -> below_def h a n; below_def h b n; at_level_def h a; at_level_def h b; ());
      below_def h q cut; at_level_def h q; refine_ u)

let rec (readback_avoids @ total) : (h : Pref.heap) @ immutable -> (cut : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (names : A.names) @ immutable ->
    (high : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || not (below h p cut)})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree && below h (tree_root tree) cut} ->
    {u : unit | A.avoids names (T.embed (readback tree))} @ ghost =
  fun h cut order names high tree premise -> ghost_ (
    let refine_ premise = premise in finite_def h tree; tree_root_def tree; readback_def tree;
    let p = tree_root tree in order p;
    let t = readback tree in T.embed_def t; let mono = T.embed t in A.avoids_def names mono;
    let u = () in match tree with
    | Free p -> high p; refine_ u
    | Constant_tree _ -> refine_ u
    | Alias_tree (_, child) -> let q = tree_root child in Level_finite_spec.edge_def h p q;
      below_child h cut p q (refine_ u); readback_avoids h cut order names high child (refine_ u); refine_ u
    | Branch (_, a, b) -> let pa = tree_root a in let pb = tree_root b in
      Level_finite_spec.edge_def h p pa; Level_finite_spec.edge_def h p pb;
      below_child h cut p pa (refine_ u); below_child h cut p pb (refine_ u);
      readback_avoids h cut order names high a (refine_ u);
      readback_avoids h cut order names high b (refine_ u); refine_ u)

let[@def] rec (join @ total) (a : A.names @ immutable) (b : A.names @ immutable) =
  ghost_ (match a with A.No_names -> b | A.Name (p, rest) -> A.Name (p, join rest b))

let rec (join_position @ total) : (a : A.names) @ immutable -> (b : A.names) @ immutable ->
    (p : node Pref.t) @ immutable ->
    {u : unit | (A.position (join a b) p === None) ===
      (A.position a p === None && A.position b p === None)} @ ghost = fun a b p -> ghost_ (
    join_def a b; let both = join a b in A.position_def both p; A.position_def a p;
    let u = () in match a with A.No_names -> refine_ u | A.Name (q, rest) ->
      if p === q then refine_ u else (join_position rest b p; refine_ u))

let[@def] rec (generalized_names @ total) (h : Pref.heap @ immutable) (cut : int) (tree : tree @ immutable) =
  ghost_ (match tree with
  | Free p -> if below h p cut then A.No_names else A.Name (p, A.No_names)
  | Constant_tree _ -> A.No_names
  | Alias_tree (_, child) -> generalized_names h cut child
  | Branch (_, a, b) -> join (generalized_names h cut a) (generalized_names h cut b))

let rec (generalized_names_high @ total) : (h : Pref.heap) @ immutable -> (cut : int) ->
    (tree : tree) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | A.position (generalized_names h cut tree) p === None || not (below h p cut)} @ ghost =
  fun h cut tree p -> ghost_ (
    generalized_names_def h cut tree; let names = generalized_names h cut tree in
    let empty = A.No_names in A.position_def empty p;
    let u = () in match tree with
    | Free q -> if below h q cut then refine_ u else (A.position_def names p; refine_ u)
    | Constant_tree _ -> refine_ u
    | Alias_tree (_, child) -> generalized_names_high h cut child p; refine_ u
    | Branch (_, a, b) -> generalized_names_high h cut a p; generalized_names_high h cut b p;
      let na = generalized_names h cut a in let nb = generalized_names h cut b in
      join_position na nb p; refine_ u)

let (generalized_readback_avoids @ total) : (h : Pref.heap) @ immutable -> (cut : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (rhs : tree) @ immutable -> (boundary : tree) @ immutable ->
    {u : unit | finite h boundary && below h (tree_root boundary) cut} ->
    {u : unit | A.avoids (generalized_names h cut rhs) (T.embed (readback boundary))} @ ghost =
  fun h cut order rhs boundary premise -> ghost_ (
    let refine_ premise = premise in let names = generalized_names h cut rhs in
    let high : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || not (below h p cut)}) @ total = fun p ->
      let refine_ u = generalized_names_high h cut rhs p in refine_ u in
    let u = () in readback_avoids h cut order names high boundary (refine_ u); refine_ u)

let rec (generalized_names_below @ total) : (h : Pref.heap) @ immutable -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (tree : tree) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | finite h tree && below h (tree_root tree) depth} ->
    {u : unit | A.position (generalized_names h cut tree) p === None || below h p depth} @ ghost =
  fun h cut depth order tree p premise -> ghost_ (
    let refine_ premise = premise in finite_def h tree; tree_root_def tree;
    generalized_names_def h cut tree; let names = generalized_names h cut tree in
    let root = tree_root tree in order root;
    let empty = A.No_names in A.position_def empty p;
    let u = () in match tree with
    | Free q -> if below h q cut then refine_ u else (A.position_def names p; refine_ u)
    | Constant_tree _ -> refine_ u
    | Alias_tree (_, child) -> let q = tree_root child in Level_finite_spec.edge_def h root q;
      below_child h depth root q (refine_ u);
      generalized_names_below h cut depth order child p (refine_ u); refine_ u
    | Branch (_, a, b) -> let pa = tree_root a in let pb = tree_root b in
      Level_finite_spec.edge_def h root pa; Level_finite_spec.edge_def h root pb;
      below_child h depth root pa (refine_ u); below_child h depth root pb (refine_ u);
      generalized_names_below h cut depth order a p (refine_ u);
      generalized_names_below h cut depth order b p (refine_ u);
      let na = generalized_names h cut a in let nb = generalized_names h cut b in
      join_position na nb p; refine_ u)

let[@def] variable_choice : node Pref.t @ immutable total -> ty @ immutable total = fun p -> Variable p

let rec (template_avoids @ total) : (h : Pref.heap) @ immutable -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === readback t})) @ total ->
    (rhs : tree) @ immutable -> (schema : template) @ immutable ->
    {u : unit | finite h rhs && below h (tree_root rhs) depth && template h schema
      && Hm_environment_spec.boundary_bound h cut schema} ->
    {u : unit | A.avoids (generalized_names h cut rhs) (T.embed (interpret rho variable_choice schema))} @ ghost =
  fun h cut depth order trees rho values rhs schema premise -> ghost_ (
    let refine_ premise = premise in template_def h schema;
    Hm_environment_spec.boundary_bound_def h cut schema;
    interpret_def rho variable_choice schema;
    let names = generalized_names h cut rhs in let u = () in match schema with
    | Boundary p -> below_def h p cut; let refine_ tree = trees p in values p;
      generalized_readback_avoids h cut order rhs tree (refine_ u); refine_ u
    | Parameter p -> generalized_names_below h cut depth order rhs p (refine_ u);
      let var : desc = Var in generic_desc_def h p var; below_def h p depth; at_level_def h p;
      variable_choice_def p; let t = Variable p in T.embed_def t;
      let mono = D.Free p in A.avoids_def names mono; refine_ u
    | Constant _ -> let t = Boolean in T.embed_def t;
      let mono = D.Boolean in A.avoids_def names mono; refine_ u
    | Indirect (_, child) -> template_avoids h cut depth order trees rho values rhs child (refine_ u); refine_ u
    | Product (_, a, b) ->
      let ta = interpret rho variable_choice a in let tb = interpret rho variable_choice b in
      let t = Function (ta, tb) in T.embed_def t; let mono = T.embed t in A.avoids_def names mono;
      template_avoids h cut depth order trees rho values rhs a (refine_ u);
      template_avoids h cut depth order trees rho values rhs b (refine_ u); refine_ u)

let rec (abstract_avoids @ total) : (names : A.names) @ immutable -> (binders : A.names) @ immutable ->
    (cut : D.index) @ immutable -> (t : D.mono) @ immutable -> {u : unit | A.avoids names t} ->
    {u : unit | A.avoids names (A.abstract_type binders cut t)} @ ghost =
  fun names binders cut t premise -> ghost_ (
    let refine_ premise = premise in A.avoids_def names t; A.abstract_type_def binders cut t;
    let out = A.abstract_type binders cut t in A.avoids_def names out;
    let u = () in match t with
    | D.Parameter _ | D.Boolean -> refine_ u
    | D.Free p -> A.abstract_free_def binders cut p;
      (match A.position binders p with None -> refine_ u | Some _ ->
        A.avoids_def names out; refine_ u)
    | D.Function (a, b) -> abstract_avoids names binders cut a (refine_ u);
      abstract_avoids names binders cut b (refine_ u); refine_ u)

let[@def] rec (template_names @ total) (schema : template @ immutable) = ghost_ (match schema with
  | Boundary _ | Constant _ -> A.No_names
  | Parameter p -> A.Name (p, A.No_names)
  | Indirect (_, child) -> template_names child
  | Product (_, a, b) -> join (template_names a) (template_names b))

let[@def] (template_scheme @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (schema : template @ immutable) = ghost_ (
  let names = template_names schema in
  D.Forall (A.count names, A.abstract_type names D.Z (T.embed (interpret rho variable_choice schema))))

let[@def] rec (template_context @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (schemas : Hm_environment_spec.templates @ immutable) = ghost_ (match schemas with
  | Hm_environment_spec.No_templates -> D.Empty_context
  | Hm_environment_spec.Template_binding (schema, rest) -> D.Binding (template_scheme rho schema, template_context rho rest))

let rec (environment_avoids @ total) : (h : Pref.heap) @ immutable -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === readback t})) @ total ->
    (rhs : tree) @ immutable -> (env : Hm_environment_spec.env) @ immutable ->
    (schemas : Hm_environment_spec.templates) @ immutable ->
    {u : unit | finite h rhs && below h (tree_root rhs) depth && Hm_environment_spec.env_at h cut env schemas} ->
    {u : unit | A.context_avoids (generalized_names h cut rhs) (template_context rho schemas)} @ ghost =
  fun h cut depth order trees rho values rhs env schemas premise -> ghost_ (
    let refine_ premise = premise in Hm_environment_spec.env_at_def h cut env schemas;
    template_context_def rho schemas; let names = generalized_names h cut rhs in
    let context = template_context rho schemas in A.context_avoids_def names context;
    let u = () in match schemas with Hm_environment_spec.No_templates -> refine_ u
    | Hm_environment_spec.Template_binding (schema, rest) -> match env with Hm_environment_spec.Empty -> refine_ u
      | Hm_environment_spec.Bind (_, tail) ->
        template_avoids h cut depth order trees rho values rhs schema (refine_ u);
        let binders = template_names schema in let mono = T.embed (interpret rho variable_choice schema) in
        let z = D.Z in abstract_avoids names binders z mono (refine_ u);
        template_scheme_def rho schema; let scheme = template_scheme rho schema in A.scheme_avoids_def names scheme;
        environment_avoids h cut depth order trees rho values rhs tail rest (refine_ u); refine_ u)

let (generalize_readback_typing @ total) : (h : Pref.heap) @ immutable -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === readback t})) @ total ->
    (rhs : tree) @ immutable -> (env : Hm_environment_spec.env) @ immutable ->
    (schemas : Hm_environment_spec.templates) @ immutable ->
    (n : D.index) @ immutable -> (e : D.term) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | finite h rhs && below h (tree_root rhs) depth && Hm_environment_spec.env_at h cut env schemas
      && D.typed n (template_context rho schemas) e (T.embed (readback rhs)) d} ->
    {u : unit | let names = generalized_names h cut rhs in
      D.typed (D.add (A.count names) n) (D.weaken_context (A.count names) (template_context rho schemas))
        e (A.abstract_type names D.Z (T.embed (readback rhs))) (A.abstract_typing names D.Z d)} @ ghost =
  fun h cut depth order trees rho values rhs env schemas n e d premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    environment_avoids h cut depth order trees rho values rhs env schemas (refine_ u);
    let names = generalized_names h cut rhs in let context = template_context rho schemas in
    let mono = T.embed (readback rhs) in
    Hm_abstraction_proofs.generalize_typing names n context e mono d (refine_ u); refine_ u)
