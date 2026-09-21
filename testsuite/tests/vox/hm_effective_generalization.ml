open Copy_spec
open Level_spec
open Level_finite_spec
module D = Hm_declarative
module T = Hm_type_proofs
module A = Hm_abstraction
module F = Hm_freshness_proofs
module EF = Hm_effective_freshness
module E = Effective_level
module P = Hm_template_instance_proofs
module G = Generalize_spec
module U = Forest_transport

let rec (low_names @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p && E.effective_ordered h heads p})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree && E.effective_below h heads (tree_root tree) cut} ->
    {u : unit | EF.generalized_names h heads cut tree === A.No_names} @ ghost = fun h heads cut order tree premise -> ghost_ (
    finite_def h tree; tree_root_def tree; EF.generalized_names_def h heads cut tree;
    let p = tree_root tree in order p; match tree with
    | Free _ | Constant_tree _ -> ()
    | Alias_tree (_, child) -> let q = tree_root child in edge_def h p q;
      finite_def h child; order q; EF.below_child h heads cut p q (); low_names h heads cut order child (); ()
    | Branch (_, a, b) -> let pa = tree_root a in let pb = tree_root b in edge_def h p pa; edge_def h p pb;
      finite_def h a; order pa; EF.below_child h heads cut p pa (); finite_def h b; order pb; EF.below_child h heads cut p pb ();
      low_names h heads cut order a (); low_names h heads cut order b ();
      let empty = A.No_names in F.join_def empty empty; ())

let rec (scheme_names @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p && E.effective_ordered h heads p})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree && E.effective_below h heads (tree_root tree) depth} ->
    {u : unit | F.template_names (Effective_template.scheme h heads cut tree) === EF.generalized_names h heads cut tree} @ ghost =
  fun h heads cut depth order tree premise -> ghost_ (
    Effective_template.scheme_def h heads cut tree;
    tree_root_def tree; finite_def h tree; let p = tree_root tree in order p;
    E.effective_below_def h heads p depth; E.effective_below_def h heads p cut; let level = E.level h heads p in G.close_level_def cut level;
    let schema = Effective_template.scheme h heads cut tree in F.template_names_def schema;
    if E.effective_below h heads p cut then (low_names h heads cut order tree (); ()) else (
      EF.generalized_names_def h heads cut tree; E.level_def h heads p; Level_unifier_spec.observe_def h p;
      match tree with
      | Free _ | Constant_tree _ -> ()
      | Alias_tree (_, child) -> let q = tree_root child in edge_def h p q;
        finite_def h child; order q; EF.below_child h heads depth p q (); scheme_names h heads cut depth order child (); ()
      | Branch (_, a, b) -> let pa = tree_root a in let pb = tree_root b in edge_def h p pa; edge_def h p pb;
        finite_def h a; order pa; EF.below_child h heads depth p pa (); finite_def h b; order pb; EF.below_child h heads depth p pb ();
        scheme_names h heads cut depth order a (); scheme_names h heads cut depth order b (); ()))

let (canonical_value @ total) : (h : Pref.heap) @ immutable ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let t = trees p in rho p === readback t})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree} ->
    {u : unit | rho (tree_root tree) === readback tree} @ ghost = fun h trees rho values tree premise -> ghost_ (
    finite_def h tree;
    let p = tree_root tree in let other = trees p in values p;
    Level_finite_proofs.finite_unique h tree other (); ())

let rec (scheme_readback @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let t = trees p in rho p === readback t})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree} ->
    {u : unit | interpret rho F.variable_choice (Effective_template.scheme h heads cut tree) === readback tree} @ ghost =
  fun h heads cut trees rho values tree premise -> ghost_ (
    Effective_template.scheme_def h heads cut tree;
    let p = tree_root tree in let level = E.level h heads p in
    let schema = Effective_template.scheme h heads cut tree in let identity = F.variable_choice in interpret_def rho identity schema;
    if not (G.close_level cut level === Generic) then (
      canonical_value h trees rho values tree (); ())
    else (finite_def h tree; tree_root_def tree; readback_def tree; Level_unifier_spec.observe_def h p;
      match tree with
      | Free q -> F.variable_choice_def q; ()
      | Constant_tree _ -> ()
      | Alias_tree (_, child) -> scheme_readback h heads cut trees rho values child (); ()
      | Branch (_, a, b) -> scheme_readback h heads cut trees rho values a ();
        scheme_readback h heads cut trees rho values b (); ()))

let rec (scheme_boundaries @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p && E.effective_ordered h heads p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let t = trees p in rho p === readback t})) @ total ->
    (names : A.names) @ immutable ->
    (high : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || not (E.effective_below h heads p cut)})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree && E.effective_below h heads (tree_root tree) depth} ->
    {u : unit | P.boundaries_avoid names rho (Effective_template.scheme h heads cut tree)} @ ghost =
  fun h heads cut depth order trees rho values names high tree premise -> ghost_ (
    Effective_template.scheme_def h heads cut tree;
    tree_root_def tree; finite_def h tree; let p = tree_root tree in order p;
    E.effective_below_def h heads p depth; E.effective_below_def h heads p cut; let level = E.level h heads p in G.close_level_def cut level;
    let schema = Effective_template.scheme h heads cut tree in P.boundaries_avoid_def names rho schema;
    if E.effective_below h heads p cut then (
      canonical_value h trees rho values tree ();
      EF.readback_avoids h heads cut order names high tree (); ())
    else (E.level_def h heads p; Level_unifier_spec.observe_def h p;
      match tree with
      | Free _ | Constant_tree _ -> ()
      | Alias_tree (_, child) -> let q = tree_root child in edge_def h p q;
        finite_def h child; order q; EF.below_child h heads depth p q ();
        scheme_boundaries h heads cut depth order trees rho values names high child (); ()
      | Branch (_, a, b) -> let pa = tree_root a in let pb = tree_root b in edge_def h p pa; edge_def h p pb;
        finite_def h a; order pa; EF.below_child h heads depth p pa (); finite_def h b; order pb; EF.below_child h heads depth p pb ();
        scheme_boundaries h heads cut depth order trees rho values names high a ();
        scheme_boundaries h heads cut depth order trees rho values names high b (); ()))

let (generalized_scheme @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p && E.effective_ordered h heads p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let t = trees p in rho p === readback t})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree && E.effective_below h heads (tree_root tree) depth} ->
    {u : unit | P.scheme rho (Effective_template.scheme h heads cut tree) ===
      D.Forall (A.count (EF.generalized_names h heads cut tree),
        A.abstract_type (EF.generalized_names h heads cut tree) D.Z (D.embed (readback tree)))} @ ghost =
  fun h heads cut depth order trees rho values tree premise -> ghost_ (
    let names = EF.generalized_names h heads cut tree in
    let high : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || not (E.effective_below h heads p cut)}) @ total = fun p ->
      let () = EF.generalized_names_high h heads cut tree p in () in
    scheme_names h heads cut depth order tree ();
    scheme_readback h heads cut trees rho values tree ();
    scheme_boundaries h heads cut depth order trees rho values names high tree ();
    let schema = Effective_template.scheme h heads cut tree in
    P.scheme_reification rho schema (); F.template_scheme_def rho schema; ())

let rec (selected_generic @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (schema : template) @ immutable ->
    (p : node Pref.t) @ immutable -> {u : unit | Effective_template.valid_template h heads schema} ->
    {u : unit | A.position (F.template_names schema) p === None || E.level h heads p === Generic} @ ghost =
  fun h heads schema p premise -> ghost_ (
    Effective_template.valid_template_def h heads schema; F.template_names_def schema;
    let names = F.template_names schema in match schema with
    | Boundary _ | Constant _ -> A.position_def names p; ()
    | Parameter q -> A.position_def names p;
      let empty = A.No_names in A.position_def empty p;
      Effective_template.generic_def h heads q; ()
    | Indirect (_, child) -> selected_generic h heads child p (); ()
    | Product (_, a, b) -> let aa = F.template_names a in let bb = F.template_names b in
      F.join_position aa bb p; selected_generic h heads a p (); selected_generic h heads b p (); ())

let rec (canonical_boundaries @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p && E.effective_ordered h heads p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === p
        && (not (H.mem h p) || Level_finite_spec.finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let t = trees p in rho p === Level_finite_spec.readback t})) @ total ->
    (names : A.names) @ immutable ->
    (generic : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || E.level h heads p === Generic})) @ total ->
    (schema : template) @ immutable ->
    {u : unit | Effective_template.valid_template h heads schema && Effective_template.boundary_bound h heads cut schema} ->
    {u : unit | P.boundaries_avoid names rho schema} @ ghost =
  fun h heads cut order trees rho values names generic schema premise -> ghost_ (
    Effective_template.valid_template_def h heads schema; Effective_template.boundary_bound_def h heads cut schema;
    P.boundaries_avoid_def names rho schema; match schema with
    | Parameter _ | Constant _ -> ()
    | Boundary p -> root_def schema; E.effective_below_def h heads p cut; let tree = trees p in values p;
      let high : ((q : node Pref.t) @ immutable ->
        {u : unit | A.position names q === None || not (E.effective_below h heads q cut)}) @ total = fun q ->
        generic q; E.effective_below_def h heads q cut; () in
      EF.readback_avoids h heads cut order names high tree (); ()
    | Indirect (_, child) -> canonical_boundaries h heads cut order trees rho values names generic child (); ()
    | Product (_, a, b) -> canonical_boundaries h heads cut order trees rho values names generic a ();
      canonical_boundaries h heads cut order trees rho values names generic b (); ())

let rec (canonical_context @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p && E.effective_ordered h heads p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let t = trees p in rho p === readback t})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (schemas : Hm_environment_spec.templates) @ immutable ->
    {u : unit | Hm_effective_environment.effective_env h heads cut env schemas} ->
    {u : unit | P.context rho schemas === F.template_context rho schemas} @ ghost =
  fun h heads cut order trees rho values env schemas premise -> ghost_ (
    Hm_effective_environment.effective_env_def h heads cut env schemas;
    P.context_def rho schemas; F.template_context_def rho schemas;
    match schemas with Hm_environment_spec.No_templates -> ()
    | Hm_environment_spec.Template_binding (schema, rest) -> match env with Hm_environment_spec.Empty -> ()
      | Hm_environment_spec.Bind (_, tail) ->
        let names = F.template_names schema in
        let generic : ((p : node Pref.t) @ immutable ->
          {u : unit | A.position names p === None || E.level h heads p === Generic}) @ total = fun p ->
          let () = selected_generic h heads schema p () in () in
        canonical_boundaries h heads cut order trees rho values names generic schema ();
        P.scheme_reification rho schema ();
        canonical_context h heads cut order trees rho values tail rest (); ())

let (generalize_typing @ total) : (h : Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads p && E.effective_ordered h heads p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let t = trees p in rho p === readback t})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (schemas : Hm_environment_spec.templates) @ immutable ->
    (tree : tree) @ immutable -> (e : D.term) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | finite h tree && E.effective_below h heads (tree_root tree) depth
      && Hm_effective_environment.effective_env h heads cut env schemas
      && D.typed D.Z (P.context rho schemas) e (D.embed (readback tree)) d} ->
    {d : D.typing | let names = F.template_names (Effective_template.scheme h heads cut tree) in
      D.typed (A.count names) (D.weaken_context (A.count names) (P.context rho schemas)) e
        (P.body names rho (Effective_template.scheme h heads cut tree)) d} @ immutable ghost =
  fun h heads cut depth order trees rho values env schemas tree e d premise -> ghost_ (
    canonical_context h heads cut order trees rho values env schemas ();
    let z = D.Z in EF.generalize_readback_typing h heads cut depth order trees rho values tree env schemas z e d ();
    generalized_scheme h heads cut depth order trees rho values tree ();
    scheme_names h heads cut depth order tree ();
    let schema = Effective_template.scheme h heads cut tree in P.scheme_def rho schema;
    let names = EF.generalized_names h heads cut tree in let k = A.count names in Hm_abstraction_proofs.add_zero k;
    let out = A.abstract_typing names z d in out)
