open Copy_spec
open Level_spec
open Generalize_spec
open Provenance_spec
open Leaf_provenance_spec

let (child_below @ total) : (h : Pref.heap) @ immutable ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (cut : int) -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | below h p cut && edge h p q} -> {u : unit | below h q cut} @ ghost =
  fun h order cut p q premise -> ghost_ (
    let stop = Stop in let path = Step (q, stop) in
    reaches_def h p q path; reaches_def h q q stop;
    let u = () in Generalize_proofs.environment_bound h order cut p q path (u); u)

let rec (low_unfolded_agreement @ total) : (saved : Pref.heap) @ immutable ->
    (h : Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h eta x})) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below saved x cut) || rho x === eta x})) @ total ->
    (tree : bounded) @ immutable ->
    {u : unit | unfolded h tree && below h (bound_root tree) cut} ->
    {u : unit | rho (bound_root tree) === eta (bound_root tree)} @ ghost =
  fun saved h cut prior order rho rho_model eta eta_model equal tree premise -> ghost_ (
    unfolded_def h tree; bound_root_def tree;
    let p = bound_root tree in rho_model p; eta_model p;
    equation_def h rho p; equation_def h eta p; let u = () in
    match tree with
    | Tip _ -> low_var_def h p cut; Level_unifier_spec.observe_def h p;
      if low_var h p cut then (
        let origin = prior p in
        Relative_generalization.origin_agreement saved h cut rho rho_model eta eta_model equal p origin (u); u)
      else u
    | Through (_, child) -> let q = bound_root child in edge_def h p q;
      child_below h order cut p q (u);
      low_unfolded_agreement saved h cut prior order rho rho_model eta eta_model equal child (u); u
    | Fork (_, a, b) -> let q = bound_root a in let r = bound_root b in
      edge_def h p q; edge_def h p r; child_below h order cut p q (u); child_below h order cut p r (u);
      low_unfolded_agreement saved h cut prior order rho rho_model eta eta_model equal a (u);
      low_unfolded_agreement saved h cut prior order rho rho_model eta eta_model equal b (u); u)

let rec (relative_interpret @ total) : (saved : Pref.heap) @ immutable ->
    (h : Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h eta x})) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below saved x cut) || rho x === eta x})) @ total ->
    (tree : bounded) @ immutable -> {u : unit | unfolded h tree} ->
    {u : unit | interpret rho eta (scheme h cut tree) === eta (bound_root tree)} @ ghost =
  fun saved h cut prior order rho rho_model eta eta_model equal tree premise -> ghost_ (
    unfolded_def h tree; bound_root_def tree;
    let p = bound_root tree in let schema = scheme h cut tree in
    scheme_def h cut tree; interpret_def rho eta schema; at_level_def h p;
    let level = at_level h p in close_level_def cut level; let u = () in
    if not (close_level cut level === Generic) then (
      order p; ordered_def h p; below_def h p cut;
      low_unfolded_agreement saved h cut prior order rho rho_model eta eta_model equal tree (u); u)
    else (
      eta_model p; equation_def h eta p;
      match tree with Tip _ -> u
      | Through (_, child) -> relative_interpret saved h cut prior order rho rho_model eta eta_model equal child (u); u
      | Fork (_, a, b) -> relative_interpret saved h cut prior order rho rho_model eta eta_model equal a (u);
        relative_interpret saved h cut prior order rho rho_model eta eta_model equal b (u); u))
