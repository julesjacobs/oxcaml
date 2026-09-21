open Copy_spec
open Level_spec
open Level_finite_spec
module D = Hm_declarative
module T = Hm_type_proofs
module A = Hm_abstraction
module F = Hm_freshness_proofs
module P = Hm_template_instance_proofs
module G = Generalize_spec
module U = Forest_transport

let rec (low_names @ total) : (h : node Pref.heap) @ immutable -> (cut : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree && below h (tree_root tree) cut} ->
    {u : unit | F.generalized_names h cut tree === A.No_names} @ ghost = fun h cut order tree premise -> ghost_ (
    let refine_ premise = premise in finite_def h tree; tree_root_def tree; F.generalized_names_def h cut tree;
    let p = tree_root tree in order p; let u = () in match tree with
    | Free _ | Constant_tree _ -> refine_ u
    | Alias_tree (_, child) -> let q = tree_root child in edge_def h p q;
      F.below_child h cut p q (refine_ u); low_names h cut order child (refine_ u); refine_ u
    | Branch (_, a, b) -> let pa = tree_root a in let pb = tree_root b in edge_def h p pa; edge_def h p pb;
      F.below_child h cut p pa (refine_ u); F.below_child h cut p pb (refine_ u);
      low_names h cut order a (refine_ u); low_names h cut order b (refine_ u);
      let empty = A.No_names in F.join_def empty empty; refine_ u)

let rec (scheme_names @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree && below h (tree_root tree) depth} ->
    {u : unit | F.template_names (G.scheme h cut (U.unfolding tree)) === F.generalized_names h cut tree} @ ghost =
  fun h cut depth order tree premise -> ghost_ (
    let refine_ premise = premise in U.unfolding_def tree; U.unfolding_root tree;
    let unfolded = U.unfolding tree in G.scheme_def h cut unfolded;
    tree_root_def tree; finite_def h tree; let p = tree_root tree in order p;
    below_def h p depth; below_def h p cut; let level = at_level h p in G.close_level_def cut level;
    let schema = G.scheme h cut unfolded in F.template_names_def schema;
    let u = () in if below h p cut then (low_names h cut order tree (refine_ u); refine_ u) else (
      F.generalized_names_def h cut tree; at_level_def h p; Level_unifier_spec.observe_def h p;
      match tree with
      | Free _ | Constant_tree _ -> refine_ u
      | Alias_tree (_, child) -> let q = tree_root child in edge_def h p q;
        F.below_child h depth p q (refine_ u); scheme_names h cut depth order child (refine_ u); refine_ u
      | Branch (_, a, b) -> let pa = tree_root a in let pb = tree_root b in edge_def h p pa; edge_def h p pb;
        F.below_child h depth p pa (refine_ u); F.below_child h depth p pb (refine_ u);
        scheme_names h cut depth order a (refine_ u); scheme_names h cut depth order b (refine_ u); refine_ u))

let (canonical_value @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === readback t})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree} ->
    {u : unit | rho (tree_root tree) === readback tree} @ ghost = fun h trees rho values tree premise -> ghost_ (
    let refine_ premise = premise in finite_def h tree;
    let p = tree_root tree in let refine_ other = trees p in values p;
    let u = () in Level_finite_proofs.finite_unique h tree other (refine_ u); refine_ u)

let rec (scheme_readback @ total) : (h : node Pref.heap) @ immutable -> (cut : int) ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === readback t})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree} ->
    {u : unit | interpret rho F.variable_choice (G.scheme h cut (U.unfolding tree)) === readback tree} @ ghost =
  fun h cut trees rho values tree premise -> ghost_ (
    let refine_ premise = premise in U.unfolding_def tree; U.unfolding_root tree;
    let unfolded = U.unfolding tree in G.scheme_def h cut unfolded;
    let p = tree_root tree in let level = at_level h p in
    let schema = G.scheme h cut unfolded in let identity = F.variable_choice in interpret_def rho identity schema;
    let u = () in if not (G.close_level cut level === Generic) then (
      canonical_value h trees rho values tree (refine_ u); refine_ u)
    else (finite_def h tree; tree_root_def tree; readback_def tree; Level_unifier_spec.observe_def h p;
      match tree with
      | Free q -> F.variable_choice_def q; refine_ u
      | Constant_tree _ -> refine_ u
      | Alias_tree (_, child) -> scheme_readback h cut trees rho values child (refine_ u); refine_ u
      | Branch (_, a, b) -> scheme_readback h cut trees rho values a (refine_ u);
        scheme_readback h cut trees rho values b (refine_ u); refine_ u))

let rec (scheme_boundaries @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === readback t})) @ total ->
    (names : A.names) @ immutable ->
    (high : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || not (below h p cut)})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree && below h (tree_root tree) depth} ->
    {u : unit | P.boundaries_avoid names rho (G.scheme h cut (U.unfolding tree))} @ ghost =
  fun h cut depth order trees rho values names high tree premise -> ghost_ (
    let refine_ premise = premise in U.unfolding_def tree; U.unfolding_root tree;
    let unfolded = U.unfolding tree in G.scheme_def h cut unfolded;
    tree_root_def tree; finite_def h tree; let p = tree_root tree in order p;
    below_def h p depth; below_def h p cut; let level = at_level h p in G.close_level_def cut level;
    let schema = G.scheme h cut unfolded in P.boundaries_avoid_def names rho schema;
    let u = () in if below h p cut then (
      canonical_value h trees rho values tree (refine_ u);
      F.readback_avoids h cut order names high tree (refine_ u); refine_ u)
    else (at_level_def h p; Level_unifier_spec.observe_def h p;
      match tree with
      | Free _ | Constant_tree _ -> refine_ u
      | Alias_tree (_, child) -> let q = tree_root child in edge_def h p q;
        F.below_child h depth p q (refine_ u);
        scheme_boundaries h cut depth order trees rho values names high child (refine_ u); refine_ u
      | Branch (_, a, b) -> let pa = tree_root a in let pb = tree_root b in edge_def h p pa; edge_def h p pb;
        F.below_child h depth p pa (refine_ u); F.below_child h depth p pb (refine_ u);
        scheme_boundaries h cut depth order trees rho values names high a (refine_ u);
        scheme_boundaries h cut depth order trees rho values names high b (refine_ u); refine_ u))

let (generalized_scheme @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === readback t})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree && below h (tree_root tree) depth} ->
    {u : unit | P.scheme rho (G.scheme h cut (U.unfolding tree)) ===
      D.Forall (A.count (F.generalized_names h cut tree),
        A.abstract_type (F.generalized_names h cut tree) D.Z (D.embed (readback tree)))} @ ghost =
  fun h cut depth order trees rho values tree premise -> ghost_ (
    let refine_ premise = premise in let names = F.generalized_names h cut tree in
    let high : ((p : node Pref.t) @ immutable ->
      {u : unit | A.position names p === None || not (below h p cut)}) @ total = fun p ->
      let refine_ u = F.generalized_names_high h cut tree p in refine_ u in
    let u = () in scheme_names h cut depth order tree (refine_ u);
    scheme_readback h cut trees rho values tree (refine_ u);
    scheme_boundaries h cut depth order trees rho values names high tree (refine_ u);
    let unfolded = U.unfolding tree in let schema = G.scheme h cut unfolded in
    P.scheme_reification rho schema (refine_ u); F.template_scheme_def rho schema; refine_ u)

let rec (canonical_context @ total) : (h : node Pref.heap) @ immutable -> (cut : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === readback t})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (schemas : Hm_environment_spec.templates) @ immutable ->
    {u : unit | Hm_environment_spec.env_at h cut env schemas} ->
    {u : unit | P.context rho schemas === F.template_context rho schemas} @ ghost =
  fun h cut order trees rho values env schemas premise -> ghost_ (
    let refine_ premise = premise in Hm_environment_spec.env_at_def h cut env schemas;
    P.context_def rho schemas; F.template_context_def rho schemas;
    let u = () in match schemas with Hm_environment_spec.No_templates -> refine_ u
    | Hm_environment_spec.Template_binding (schema, rest) -> match env with Hm_environment_spec.Empty -> refine_ u
      | Hm_environment_spec.Bind (_, tail) ->
        let names = F.template_names schema in
        let generic : ((p : node Pref.t) @ immutable ->
          {u : unit | A.position names p === None || at_level h p === Generic}) @ total = fun p ->
          let u = () in let refine_ u = P.selected_generic h schema p (refine_ u) in refine_ u in
        P.canonical_boundaries h cut order trees rho values names generic schema (refine_ u);
        P.scheme_reification rho schema (refine_ u);
        canonical_context h cut order trees rho values tail rest (refine_ u); refine_ u)

let (generalize_typing @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (depth : int) ->
    (order : ((p : node Pref.t) @ immutable -> {u : unit | ordered h p})) @ total ->
    (trees : ((p : node Pref.t) @ immutable ->
      {t : tree | tree_root t === p && (not (H.mem h p) || finite h t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((p : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees p in rho p === readback t})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (schemas : Hm_environment_spec.templates) @ immutable ->
    (tree : tree) @ immutable -> (e : D.term) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | finite h tree && below h (tree_root tree) depth
      && Hm_environment_spec.env_at h cut env schemas
      && D.typed D.Z (P.context rho schemas) e (D.embed (readback tree)) d} ->
    {d : D.typing | let names = F.template_names (G.scheme h cut (U.unfolding tree)) in
      D.typed (A.count names) (D.weaken_context (A.count names) (P.context rho schemas)) e
        (P.body names rho (G.scheme h cut (U.unfolding tree))) d} @ immutable ghost =
  fun h cut depth order trees rho values env schemas tree e d premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    canonical_context h cut order trees rho values env schemas (refine_ u);
    let z = D.Z in F.generalize_readback_typing h cut depth order trees rho values tree env schemas z e d (refine_ u);
    generalized_scheme h cut depth order trees rho values tree (refine_ u);
    scheme_names h cut depth order tree (refine_ u);
    let unfolded = U.unfolding tree in let schema = G.scheme h cut unfolded in P.scheme_def rho schema;
    let names = F.generalized_names h cut tree in let k = A.count names in Hm_abstraction_proofs.add_zero k;
    let out = A.abstract_typing names z d in refine_ out)
