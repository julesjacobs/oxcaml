open Copy_spec
open Level_spec
open Level_finite_spec
module A = Hm_abstraction
module T = Hm_type_proofs
module D = Hm_declarative

let (below_child @ total) : (h : node Pref.heap) @ immutable -> (cut : int) ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | ordered h p && below h p cut && Level_finite_spec.edge h p q} ->
    {u : unit | below h q cut} @ ghost = fun h cut p q premise -> ghost_ (
    ordered_def h p; below_def h p cut;
    at_level_def h p; Level_finite_spec.edge_def h p q; Level_unifier_spec.observe_def h p;
    match H.at h p with None -> () | Some v -> match v.level with
    | Generic -> () | Finite n -> children_below_def h v.desc n;
      (match v.desc with Var | Bool -> () | Link a -> below_def h a n; at_level_def h a; ()
      | Arrow (a, b) -> below_def h a n; below_def h b n; at_level_def h a; at_level_def h b; ());
      below_def h q cut; at_level_def h q; ())

let rec (readback_avoids @ total) : (h : node Pref.heap) @ immutable -> (cut : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (names : A.names) @ immutable ->
    (high : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || not (below h p cut)})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree && below h (tree_root tree) cut} ->
    {u : unit | A.avoids names (D.embed (readback tree))} @ ghost =
  fun h cut order names high tree premise -> ghost_ (
    finite_def h tree; tree_root_def tree; readback_def tree;
    let p = tree_root tree in order p;
    let t = readback tree in D.embed_def t; let mono = D.embed t in A.avoids_def names mono;
    match tree with
    | Free p -> high p; ()
    | Constant_tree _ -> ()
    | Alias_tree (_, child) -> let q = tree_root child in Level_finite_spec.edge_def h p q;
      below_child h cut p q (); readback_avoids h cut order names high child (); ()
    | Branch (_, a, b) -> let pa = tree_root a in let pb = tree_root b in
      Level_finite_spec.edge_def h p pa; Level_finite_spec.edge_def h p pb;
      below_child h cut p pa (); below_child h cut p pb ();
      readback_avoids h cut order names high a ();
      readback_avoids h cut order names high b (); ())

let[@def] rec (join @ total) (a : A.names @ immutable) (b : A.names @ immutable) =
  ghost_ (match a with A.No_names -> b | A.Name (p, rest) -> A.Name (p, join rest b))

let rec (join_position @ total) : (a : A.names) @ immutable -> (b : A.names) @ immutable ->
    (p : node Pref.t) @ immutable ->
    {u : unit | (A.position (join a b) p === None) ===
      (A.position a p === None && A.position b p === None)} @ ghost = fun a b p -> ghost_ (
    join_def a b; let both = join a b in A.position_def both p; A.position_def a p;
    match a with A.No_names -> () | A.Name (q, rest) ->
      if p === q then () else (join_position rest b p; ()))

let[@def] rec (generalized_names @ total) (h : node Pref.heap @ immutable) (cut : int) (tree : tree @ immutable) =
  ghost_ (match tree with
  | Free p -> if below h p cut then A.No_names else A.Name (p, A.No_names)
  | Constant_tree _ -> A.No_names
  | Alias_tree (_, child) -> generalized_names h cut child
  | Branch (_, a, b) -> join (generalized_names h cut a) (generalized_names h cut b))

let rec (generalized_names_high @ total) : (h : node Pref.heap) @ immutable -> (cut : int) ->
    (tree : tree) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | A.position (generalized_names h cut tree) p === None || not (below h p cut)} @ ghost =
  fun h cut tree p -> ghost_ (
    generalized_names_def h cut tree; let names = generalized_names h cut tree in
    let empty = A.No_names in A.position_def empty p;
    match tree with
    | Free q -> if below h q cut then () else (A.position_def names p; ())
    | Constant_tree _ -> ()
    | Alias_tree (_, child) -> generalized_names_high h cut child p; ()
    | Branch (_, a, b) -> generalized_names_high h cut a p; generalized_names_high h cut b p;
      let na = generalized_names h cut a in let nb = generalized_names h cut b in
      join_position na nb p; ())

let (generalized_readback_avoids @ total) : (h : node Pref.heap) @ immutable -> (cut : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (rhs : tree) @ immutable -> (boundary : tree) @ immutable ->
    {u : unit | finite h boundary && below h (tree_root boundary) cut} ->
    {u : unit | A.avoids (generalized_names h cut rhs) (D.embed (readback boundary))} @ ghost =
  fun h cut order rhs boundary premise -> ghost_ (
    let names = generalized_names h cut rhs in
    let high : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || not (below h p cut)}) @ total = fun p ->
      let () = generalized_names_high h cut rhs p in () in
    readback_avoids h cut order names high boundary (); ())

let rec (generalized_names_below @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (tree : tree) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | finite h tree && below h (tree_root tree) depth} ->
    {u : unit | A.position (generalized_names h cut tree) p === None || below h p depth} @ ghost =
  fun h cut depth order tree p premise -> ghost_ (
    finite_def h tree; tree_root_def tree;
    generalized_names_def h cut tree; let names = generalized_names h cut tree in
    let root = tree_root tree in order root;
    let empty = A.No_names in A.position_def empty p;
    match tree with
    | Free q -> if below h q cut then () else (A.position_def names p; ())
    | Constant_tree _ -> ()
    | Alias_tree (_, child) -> let q = tree_root child in Level_finite_spec.edge_def h root q;
      below_child h depth root q ();
      generalized_names_below h cut depth order child p (); ()
    | Branch (_, a, b) -> let pa = tree_root a in let pb = tree_root b in
      Level_finite_spec.edge_def h root pa; Level_finite_spec.edge_def h root pb;
      below_child h depth root pa (); below_child h depth root pb ();
      generalized_names_below h cut depth order a p ();
      generalized_names_below h cut depth order b p ();
      let na = generalized_names h cut a in let nb = generalized_names h cut b in
      join_position na nb p; ())

let[@def] variable_choice : node Pref.t @ immutable total -> ty @ immutable total = fun p -> Variable p

let rec (template_avoids @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === readback t})) @ total ->
    (rhs : tree) @ immutable -> (schema : template) @ immutable ->
    {u : unit | finite h rhs && below h (tree_root rhs) depth && template h schema
      && Hm_environment_spec.boundary_bound h cut schema} ->
    {u : unit | A.avoids (generalized_names h cut rhs) (D.embed (interpret rho variable_choice schema))} @ ghost =
  fun h cut depth order trees rho values rhs schema premise -> ghost_ (
    template_def h schema;
    Hm_environment_spec.boundary_bound_def h cut schema;
    interpret_def rho variable_choice schema;
    let names = generalized_names h cut rhs in match schema with
    | Boundary p -> below_def h p cut; let tree = trees p in values p;
      generalized_readback_avoids h cut order rhs tree (); ()
    | Parameter p -> generalized_names_below h cut depth order rhs p ();
      let var : desc = Var in generic_desc_def h p var; below_def h p depth; at_level_def h p;
      variable_choice_def p; let t = Variable p in D.embed_def t;
      let mono = D.Free p in A.avoids_def names mono; ()
    | Constant _ -> let t = Boolean in D.embed_def t;
      let mono = D.Boolean in A.avoids_def names mono; ()
    | Indirect (_, child) -> template_avoids h cut depth order trees rho values rhs child (); ()
    | Product (_, a, b) ->
      let ta = interpret rho variable_choice a in let tb = interpret rho variable_choice b in
      let t = Function (ta, tb) in D.embed_def t; let mono = D.embed t in A.avoids_def names mono;
      template_avoids h cut depth order trees rho values rhs a ();
      template_avoids h cut depth order trees rho values rhs b (); ())

let rec (abstract_avoids @ total) : (names : A.names) @ immutable -> (binders : A.names) @ immutable ->
    (cut : D.index) @ immutable -> (t : D.mono) @ immutable -> {u : unit | A.avoids names t} ->
    {u : unit | A.avoids names (A.abstract_type binders cut t)} @ ghost =
  fun names binders cut t premise -> ghost_ (
    A.avoids_def names t; A.abstract_type_def binders cut t;
    let out = A.abstract_type binders cut t in A.avoids_def names out;
    match t with
    | D.Parameter _ | D.Boolean -> ()
    | D.Free p -> A.abstract_free_def binders cut p;
      (match A.position binders p with None -> () | Some _ ->
        A.avoids_def names out; ())
    | D.Function (a, b) -> abstract_avoids names binders cut a ();
      abstract_avoids names binders cut b (); ())

let[@def] rec (template_names @ total) (schema : template @ immutable) = ghost_ (match schema with
  | Boundary _ | Constant _ -> A.No_names
  | Parameter p -> A.Name (p, A.No_names)
  | Indirect (_, child) -> template_names child
  | Product (_, a, b) -> join (template_names a) (template_names b))

let[@def] (template_scheme @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (schema : template @ immutable) = ghost_ (
  let names = template_names schema in
  D.Forall (A.count names, A.abstract_type names D.Z (D.embed (interpret rho variable_choice schema))))

let[@def] rec (template_context @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (schemas : Hm_environment_spec.templates @ immutable) = ghost_ (match schemas with
  | Hm_environment_spec.No_templates -> D.Empty_context
  | Hm_environment_spec.Template_binding (schema, rest) -> D.Binding (template_scheme rho schema, template_context rho rest))

let rec (environment_avoids @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (depth : int) ->
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
    Hm_environment_spec.env_at_def h cut env schemas;
    template_context_def rho schemas; let names = generalized_names h cut rhs in
    let context = template_context rho schemas in A.context_avoids_def names context;
    match schemas with Hm_environment_spec.No_templates -> ()
    | Hm_environment_spec.Template_binding (schema, rest) -> match env with Hm_environment_spec.Empty -> ()
      | Hm_environment_spec.Bind (_, tail) ->
        template_avoids h cut depth order trees rho values rhs schema ();
        let binders = template_names schema in let mono = D.embed (interpret rho variable_choice schema) in
        let z = D.Z in abstract_avoids names binders z mono ();
        template_scheme_def rho schema; let scheme = template_scheme rho schema in A.scheme_avoids_def names scheme;
        environment_avoids h cut depth order trees rho values rhs tail rest (); ())

let (generalize_readback_typing @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (depth : int) ->
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
      && D.typed n (template_context rho schemas) e (D.embed (readback rhs)) d} ->
    {u : unit | let names = generalized_names h cut rhs in
      D.typed (D.add (A.count names) n) (D.weaken_context (A.count names) (template_context rho schemas))
        e (A.abstract_type names D.Z (D.embed (readback rhs))) (A.abstract_typing names D.Z d)} @ ghost =
  fun h cut depth order trees rho values rhs env schemas n e d premise -> ghost_ (
    environment_avoids h cut depth order trees rho values rhs env schemas ();
    let names = generalized_names h cut rhs in let context = template_context rho schemas in
    let mono = D.embed (readback rhs) in
    Hm_abstraction_proofs.generalize_typing names n context e mono d (); ())
