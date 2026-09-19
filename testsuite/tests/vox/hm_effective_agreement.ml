open Copy_spec
open Level_spec
open Generalize_spec
open Provenance_spec
open Leaf_provenance_spec
open Level_finite_spec
module E = Effective_level
let rec (low_unfolded_agreement @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x && E.effective_ordered h heads x && (match E.level h heads x with Generic -> true | Finite n -> n >= 0)})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h eta x})) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below saved x cut) || rho x === eta x})) @ total ->
    (tree : tree) @ immutable ->
    {u : unit | finite h tree && E.effective_below h heads (tree_root tree) cut} ->
    {u : unit | rho (tree_root tree) === eta (tree_root tree)} @ ghost =
  fun saved h heads cut prior order rho rho_model eta eta_model equal tree premise -> ghost_ (
    finite_def h tree; tree_root_def tree;
    let p = tree_root tree in rho_model p; eta_model p;
    equation_def h rho p; equation_def h eta p; Level_unifier_spec.observe_def h p; match tree with
    | Free _ | Constant_tree _ -> low_var_def h p cut; Level_unifier_spec.observe_def h p;
      order p; Level_unifier_spec.terminal_def h p;
      E.terminal_level h heads p (); E.effective_below_def h heads p cut;
      below_def h p cut;
      if low_var h p cut then (
        let origin = prior p in
        Relative_generalization.origin_agreement saved h cut rho rho_model eta eta_model equal p origin (); ())
      else ()
    | Alias_tree (_, child) -> let q = tree_root child in Level_finite_spec.edge_def h p q;
      order p; order q; finite_def h child; Hm_effective_freshness.below_child h heads cut p q ();
      low_unfolded_agreement saved h heads cut prior order rho rho_model eta eta_model equal child (); ()
    | Branch (_, a, b) -> let q = tree_root a in let r = tree_root b in
      Level_finite_spec.edge_def h p q; Level_finite_spec.edge_def h p r; order p; order q; finite_def h a; Hm_effective_freshness.below_child h heads cut p q (); order r; finite_def h b; Hm_effective_freshness.below_child h heads cut p r ();
      low_unfolded_agreement saved h heads cut prior order rho rho_model eta eta_model equal a ();
      low_unfolded_agreement saved h heads cut prior order rho rho_model eta eta_model equal b (); ())

let rec (relative_interpret @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (heads : E.heads) @ total -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x && E.effective_ordered h heads x && (match E.level h heads x with Generic -> true | Finite n -> n >= 0)})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h eta x})) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below saved x cut) || rho x === eta x})) @ total ->
    (tree : tree) @ immutable -> {u : unit | finite h tree} ->
    {u : unit | interpret rho eta (Effective_template.scheme h heads cut tree) === eta (tree_root tree)} @ ghost =
  fun saved h heads cut prior order rho rho_model eta eta_model equal tree premise -> ghost_ (
    finite_def h tree; tree_root_def tree;
    let p = tree_root tree in let schema = Effective_template.scheme h heads cut tree in
    Effective_template.scheme_def h heads cut tree; interpret_def rho eta schema; E.level_def h heads p;
    let level = E.level h heads p in close_level_def cut level; if not (close_level cut level === Generic) then (
      order p; E.effective_ordered_def h heads p; E.effective_below_def h heads p cut;
      low_unfolded_agreement saved h heads cut prior order rho rho_model eta eta_model equal tree (); ())
    else (
      eta_model p; equation_def h eta p; Level_unifier_spec.observe_def h p;
      match tree with Free _ | Constant_tree _ -> ()
      | Alias_tree (_, child) -> relative_interpret saved h heads cut prior order rho rho_model eta eta_model equal child (); ()
      | Branch (_, a, b) -> relative_interpret saved h heads cut prior order rho rho_model eta eta_model equal a ();
        relative_interpret saved h heads cut prior order rho rho_model eta eta_model equal b (); ()))
