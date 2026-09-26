module F = Hm_freshness_proofs
open Copy_spec
open Level_spec
open Level_finite_spec
module A = Hm_abstraction
module T = Hm_type_proofs
module D = Hm_declarative

let variable_choice = F.variable_choice
module E = Effective_level
let (below_child @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | E.valid_head h heads p && E.effective_ordered h heads p && E.effective_below h heads p cut && H.mem h q && E.valid_head h heads q && Level_finite_spec.edge h p q} ->
    {u : unit | E.effective_below h heads q cut} @ ghost = fun h heads cut p q premise -> ghost_ (
    E.effective_ordered_def h heads p;
    E.effective_below_def h heads p cut; E.effective_below_def h heads q cut;
    Level_finite_spec.edge_def h p q; Level_unifier_spec.observe_def h p;
    match H.at h p with None -> () | Some v ->
    match v.desc with
    | Var | Bool | Word -> ()
    | Link r -> E.link_level h heads p r (); ()
    | List a -> Level_unifier_spec.terminal_def h p;
      E.terminal_level h heads p (); at_level_def h p;
      (match v.level with Generic -> () | Finite n -> E.effective_below_def h heads a n; ()); ()
    | Arrow (a, b) -> Level_unifier_spec.terminal_def h p;
      E.terminal_level h heads p (); at_level_def h p;
      (match v.level with Generic -> () | Finite n ->
        E.effective_below_def h heads a n; E.effective_below_def h heads b n; ());
      ())

let rec (readback_avoids @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p && E.effective_ordered h heads p})) @ total ->
    (names : A.names) @ immutable ->
    (high : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || not (E.effective_below h heads p cut)})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree && E.effective_below h heads (tree_root tree) cut} ->
    {u : unit | A.avoids names (D.embed (readback tree))} @ ghost =
  fun h heads cut order names high tree premise -> ghost_ (
    finite_def h tree; tree_root_def tree; readback_def tree;
    let p = tree_root tree in order p;
    let t = readback tree in D.embed_def t; let mono = D.embed t in A.avoids_def names mono;
    match tree with
    | Free p -> high p; ()
    | Constant_tree _ | Word_tree _ -> ()
    | Alias_tree (_, child) | List_tree (_, child) -> let q = tree_root child in Level_finite_spec.edge_def h p q;
      finite_def h child; order q; below_child h heads cut p q (); readback_avoids h heads cut order names high child (); ()
    | Branch (_, a, b) -> let pa = tree_root a in let pb = tree_root b in
      Level_finite_spec.edge_def h p pa; Level_finite_spec.edge_def h p pb;
      finite_def h a; order pa; below_child h heads cut p pa (); finite_def h b; order pb; below_child h heads cut p pb ();
      readback_avoids h heads cut order names high a ();
      readback_avoids h heads cut order names high b (); ())

let[@def] rec (generalized_names @ total) (h : node Pref.heap @ immutable) (heads : E.heads @ total) (cut : int) (tree : tree @ immutable) =
  ghost_ (match tree with
  | Free p -> if E.effective_below h heads p cut then A.No_names else A.Name (p, A.No_names)
  | Constant_tree _ | Word_tree _ -> A.No_names
  | Alias_tree (_, child) | List_tree (_, child) -> generalized_names h heads cut child
  | Branch (_, a, b) -> F.join (generalized_names h heads cut a) (generalized_names h heads cut b))

let rec (generalized_names_high @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) ->
    (tree : tree) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | A.position (generalized_names h heads cut tree) p === None || not (E.effective_below h heads p cut)} @ ghost =
  fun h heads cut tree p -> ghost_ (
    generalized_names_def h heads cut tree; let names = generalized_names h heads cut tree in
    let empty = A.No_names in A.position_def empty p;
    match tree with
    | Free q -> if E.effective_below h heads q cut then () else (A.position_def names p; ())
    | Constant_tree _ | Word_tree _ -> ()
    | Alias_tree (_, child) | List_tree (_, child) -> generalized_names_high h heads cut child p; ()
    | Branch (_, a, b) -> generalized_names_high h heads cut a p; generalized_names_high h heads cut b p;
      let na = generalized_names h heads cut a in let nb = generalized_names h heads cut b in
      F.join_position na nb p; ())

let (generalized_readback_avoids @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p && E.effective_ordered h heads p})) @ total ->
    (rhs : tree) @ immutable -> (boundary : tree) @ immutable ->
    {u : unit | finite h boundary && E.effective_below h heads (tree_root boundary) cut} ->
    {u : unit | A.avoids (generalized_names h heads cut rhs) (D.embed (readback boundary))} @ ghost =
  fun h heads cut order rhs boundary premise -> ghost_ (
    let names = generalized_names h heads cut rhs in
    let high : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || not (E.effective_below h heads p cut)}) @ total = fun p ->
      let () = generalized_names_high h heads cut rhs p in () in
    readback_avoids h heads cut order names high boundary (); ())

let rec (generalized_names_below @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p && E.effective_ordered h heads p})) @ total ->
    (tree : tree) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | finite h tree && E.effective_below h heads (tree_root tree) depth} ->
    {u : unit | A.position (generalized_names h heads cut tree) p === None || E.effective_below h heads p depth} @ ghost =
  fun h heads cut depth order tree p premise -> ghost_ (
    finite_def h tree; tree_root_def tree;
    generalized_names_def h heads cut tree; let names = generalized_names h heads cut tree in
    let root = tree_root tree in order root;
    let empty = A.No_names in A.position_def empty p;
    match tree with
    | Free q -> if E.effective_below h heads q cut then () else (A.position_def names p; ())
    | Constant_tree _ | Word_tree _ -> ()
    | Alias_tree (_, child) | List_tree (_, child) -> let q = tree_root child in Level_finite_spec.edge_def h root q;
      finite_def h child; order q; below_child h heads depth root q ();
      generalized_names_below h heads cut depth order child p (); ()
    | Branch (_, a, b) -> let pa = tree_root a in let pb = tree_root b in
      Level_finite_spec.edge_def h root pa; Level_finite_spec.edge_def h root pb;
      finite_def h a; order pa; below_child h heads depth root pa (); finite_def h b; order pb; below_child h heads depth root pb ();
      generalized_names_below h heads cut depth order a p ();
      generalized_names_below h heads cut depth order b p ();
      let na = generalized_names h heads cut a in let nb = generalized_names h heads cut b in
      F.join_position na nb p; ())

let rec (template_avoids @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p && E.effective_ordered h heads p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === readback t})) @ total ->
    (rhs : tree) @ immutable -> (schema : template) @ immutable ->
    {u : unit | finite h rhs && E.effective_below h heads (tree_root rhs) depth && Effective_template.valid_template h heads schema
      && Effective_template.boundary_bound h heads cut schema} ->
    {u : unit | A.avoids (generalized_names h heads cut rhs) (D.embed (interpret rho variable_choice schema))} @ ghost =
  fun h heads cut depth order trees rho values rhs schema premise -> ghost_ (
    Effective_template.valid_template_def h heads schema;
    Effective_template.boundary_bound_def h heads cut schema;
    interpret_def rho variable_choice schema;
    let names = generalized_names h heads cut rhs in match schema with
    | Boundary p -> E.effective_below_def h heads p cut; let tree = trees p in values p;
      generalized_readback_avoids h heads cut order rhs tree (); ()
    | Parameter p -> generalized_names_below h heads cut depth order rhs p ();
      Effective_template.generic_def h heads p; E.effective_below_def h heads p depth; at_level_def h p;
      F.variable_choice_def p; let t = Variable p in D.embed_def t;
      let mono = D.Free p in A.avoids_def names mono; ()
    | Word_constant _ -> let t = Word64 in D.embed_def t;
      let mono = D.Word64 in A.avoids_def names mono; ()
    | List_template (_, child) ->
      let t = List_type (interpret rho variable_choice child) in D.embed_def t;
      let mono = D.embed t in A.avoids_def names mono;
      template_avoids h heads cut depth order trees rho values rhs child (); ()
    | Constant _ -> let t = Boolean in D.embed_def t;
      let mono = D.Boolean in A.avoids_def names mono; ()
    | Indirect (_, child) -> template_avoids h heads cut depth order trees rho values rhs child (); ()
    | Product (_, a, b) ->
      let ta = interpret rho variable_choice a in let tb = interpret rho variable_choice b in
      let t = Function (ta, tb) in D.embed_def t; let mono = D.embed t in A.avoids_def names mono;
      template_avoids h heads cut depth order trees rho values rhs a ();
      template_avoids h heads cut depth order trees rho values rhs b (); ())

let rec (environment_avoids @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p && E.effective_ordered h heads p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === readback t})) @ total ->
    (rhs : tree) @ immutable -> (env : Hm_environment_spec.env) @ immutable ->
    (schemas : Hm_environment_spec.templates) @ immutable ->
    {u : unit | finite h rhs && E.effective_below h heads (tree_root rhs) depth && Hm_effective_environment.effective_env h heads cut env schemas} ->
    {u : unit | A.context_avoids (generalized_names h heads cut rhs) (F.template_context rho schemas)} @ ghost =
  fun h heads cut depth order trees rho values rhs env schemas premise -> ghost_ (
    Hm_effective_environment.effective_env_def h heads cut env schemas;
    F.template_context_def rho schemas; let names = generalized_names h heads cut rhs in
    let context = F.template_context rho schemas in A.context_avoids_def names context;
    match schemas with Hm_environment_spec.No_templates -> ()
    | Hm_environment_spec.Template_binding (schema, rest) -> match env with Hm_environment_spec.Empty -> ()
      | Hm_environment_spec.Bind (_, tail) ->
        template_avoids h heads cut depth order trees rho values rhs schema ();
        let binders = F.template_names schema in let mono = D.embed (interpret rho variable_choice schema) in
        let z = D.Z in F.abstract_avoids names binders z mono ();
        F.template_scheme_def rho schema; let scheme = F.template_scheme rho schema in A.scheme_avoids_def names scheme;
        environment_avoids h heads cut depth order trees rho values rhs tail rest (); ())

let (generalize_readback_typing @ total) : (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p && E.effective_ordered h heads p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === readback t})) @ total ->
    (rhs : tree) @ immutable -> (env : Hm_environment_spec.env) @ immutable ->
    (schemas : Hm_environment_spec.templates) @ immutable ->
    (n : D.index) @ immutable -> (e : D.term) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | finite h rhs && E.effective_below h heads (tree_root rhs) depth && Hm_effective_environment.effective_env h heads cut env schemas
      && D.typed n (F.template_context rho schemas) e (D.embed (readback rhs)) d} ->
    {u : unit | let names = generalized_names h heads cut rhs in
      D.typed (D.add (A.count names) n) (D.weaken_context (A.count names) (F.template_context rho schemas))
        e (A.abstract_type names D.Z (D.embed (readback rhs))) (A.abstract_typing names D.Z d)} @ ghost =
  fun h heads cut depth order trees rho values rhs env schemas n e d premise -> ghost_ (
    environment_avoids h heads cut depth order trees rho values rhs env schemas ();
    let names = generalized_names h heads cut rhs in let context = F.template_context rho schemas in
    let mono = D.embed (readback rhs) in
    Hm_abstraction_proofs.generalize_typing names n context e mono d (); ())
