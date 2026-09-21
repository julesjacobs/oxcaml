open Copy_spec
open Level_spec
open Generalize_spec
open Generalize_proofs
open Generalize_scheme_proofs
open Provenance_spec
open Provenance_proofs

let rec (path_agreement @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h eta x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (path : path) @ immutable ->
    {u : unit | reaches h p q path && rho p === eta p} ->
    {u : unit | rho q === eta q} @ ghost = fun h rho rho_model eta eta_model p q path premise -> ghost_ (
    reaches_def h p q path;
    match path with Stop -> ()
    | Step (next, rest) -> rho_model p; eta_model p;
      equation_def h rho p; equation_def h eta p; edge_def h p next;
      path_agreement h rho rho_model eta eta_model next q rest ())

let (origin_agreement @ total) : (saved : Pref.heap) @ immutable ->
    (h : Pref.heap) @ immutable -> (cut : int) ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h eta x})) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below saved x cut) || rho x === eta x})) @ total ->
    (x : node Pref.t) @ immutable -> (origin : origin) @ immutable ->
    {u : unit | originates saved h cut x origin} ->
    {u : unit | rho x === eta x} @ ghost =
  fun saved h cut rho rho_model eta eta_model equal x origin premise -> ghost_ (
    originates_def saved h cut x origin;
    match origin with Origin (root, path) -> equal root;
      path_agreement h rho rho_model eta eta_model root x path ())

let rec (interpret_agreement @ total) : (h : Pref.heap) @ immutable -> (cut : int) ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h eta x})) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below h x cut) || rho x === eta x})) @ total ->
    (tree : bounded) @ immutable -> {u : unit | unfolded h tree} ->
    {u : unit | interpret rho eta (scheme h cut tree) === eta (bound_root tree)}
      @ ghost = fun h cut order rho eta eta_model equal tree premise -> ghost_ (
    unfolded_def h tree; bound_root_def tree;
    let p = bound_root tree in let schema = scheme h cut tree in
    scheme_def h cut tree; interpret_def rho eta schema; at_level_def h p;
    let level = at_level h p in close_level_def cut level;
    if not (close_level cut level === Generic) then (
      order p; ordered_def h p; below_def h p cut;
      equal p; ())
    else (
      eta_model p; equation_def h eta p;
      match tree with Tip _ -> ()
      | Through (_, child) ->
        interpret_agreement h cut order rho eta eta_model equal child (); ()
      | Fork (_, a, b) ->
        interpret_agreement h cut order rho eta eta_model equal a ();
        interpret_agreement h cut order rho eta eta_model equal b (); ()))


let (relative_interpret @ total) : (saved : Pref.heap) @ immutable ->
    (h : Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h x cut) || originates saved h cut x o}
      @ immutable)) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h eta x})) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below saved x cut) || rho x === eta x})) @ total ->
    (tree : bounded) @ immutable -> {u : unit | unfolded h tree} ->
    {u : unit | interpret rho eta (scheme h cut tree) === eta (bound_root tree)}
      @ ghost = fun saved h cut prior order rho rho_model eta eta_model equal tree premise -> ghost_ (
    let determined : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below h x cut) || rho x === eta x}) @ total = fun x ->
      if below h x cut then (
        let origin = prior x in
        origin_agreement saved h cut rho rho_model eta eta_model equal x origin (); ())
      else () in
    let () = interpret_agreement h cut order rho eta eta_model determined tree () in ())

let (finite_assignment @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (want : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | instance_at h rho want x && below h x bound} ->
    {u : unit | want x === rho x} @ ghost = fun h rho want x bound premise -> ghost_ (
    instance_at_def h rho want x;
    below_def h x bound; at_level_def h x; ())

let (instance_model @ total) : (h : Pref.heap) @ immutable ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (want : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (wanted : ((x : node Pref.t) @ immutable -> {u : unit | instance_at h rho want x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | equation h want x} @ ghost =
  fun h order rho model want wanted x -> ghost_ (
    order x; model x; wanted x; ordered_def h x;
    equation_def h rho x; equation_def h want x; instance_at_def h rho want x;
    match H.at h x with None -> () | Some v ->
      match v.level with Generic -> () | Finite n ->
      children_below_def h v.desc n;
      match v.desc with Var | Bool -> ()
      | Link q -> wanted q; finite_assignment h rho want q n (); ()
      | Arrow (a, b) -> wanted a; wanted b;
        finite_assignment h rho want a n ();
        finite_assignment h rho want b n (); ())

let (with_relative_model @ total) : (saved : Pref.heap) @ immutable ->
    (h : Pref.heap) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (forest : ((x : node Pref.t) @ immutable ->
      {t : bounded | not (H.mem h x) || (bound_root t === x && unfolded h t)} @ immutable)) @ total ->
    (coverage : ((x : node Pref.t) @ immutable -> {u : unit | covered h cut pool x})) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (roots : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below saved x cut) || below h x cut})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (tree : bounded) @ immutable -> {u : unit | pool_scoped h pool && unfolded h tree} ->
    (claim : bool) ->
    (use : ((eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h eta x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable ->
        {u : unit | not (below saved x cut) || eta x === rho x})) @ total ->
      {u : unit | eta (bound_root tree) === interpret rho choices (scheme h cut tree)} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun saved h cut pool scope forest coverage order roots rho model choices tree premise claim use -> ghost_ (
    let after = closed_heap h cut pool in
    let scope_after : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem after x then source_ok after x else H.at after x === None}) @ total =
      fun x -> let () = closed_scope h scope cut pool x () in () in
    let trees : ((x : node Pref.t) @ immutable ->
      {s : template | not (H.mem after x) || (root s === x && template after s)} @ immutable) @ total =
      fun x -> let t = forest x in closed_observe h cut pool x (); closed_at_def h after cut pool x;
        let s = scheme h cut t in scheme_root h cut t;
        if H.mem h x then (scheme_valid h cut pool coverage t (); s) else s in
    let order_after : ((x : node Pref.t) @ immutable -> {u : unit | ordered after x}) @ total =
      fun x -> order x; coverage x; scope x; closed_observe h cut pool x (); closed_at_def h after cut pool x;
        if H.mem h x then (closed_ordered h cut pool x (); ())
        else (ordered_def after x; ()) in
    let model_after : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho x}) @ total =
      fun x -> model x; closed_model h cut pool rho x (); () in
    let[@def] eta : node Pref.t @ immutable total -> ty @ immutable total =
      fun x -> let t = trees x in interpret rho choices t in
    let values : ((x : node Pref.t) @ immutable ->
      {u : unit | let t = trees x in eta x === interpret rho choices t}) @ total =
      fun x -> eta_def x; () in
    let wanted : ((x : node Pref.t) @ immutable -> {u : unit | instance_at after rho eta x}) @ total =
      fun x -> let () = Copy_template_proofs.forest_instance after scope_after trees rho choices eta values x in () in
    let eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h eta x}) @ total =
      fun x -> instance_model after order_after rho model_after eta wanted x;
        closed_model h cut pool eta x (); () in
    let equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below saved x cut) || eta x === rho x}) @ total = fun x ->
      roots x; if below saved x cut then (
        closed_boundary h cut pool x (); wanted x;
        finite_assignment after rho eta x cut (); ()) else () in
    let s = scheme h cut tree in scheme_valid h cut pool coverage tree (); scheme_root h cut tree;
    Copy_template_proofs.forest_eval after trees rho choices eta values s ();
    let () = use eta eta_model equal () in ())

let (with_relative_copy @ total) : (saved : Pref.heap) @ immutable ->
    (h : Pref.heap) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (forest : ((x : node Pref.t) @ immutable ->
      {t : bounded | not (H.mem h x) || (bound_root t === x && unfolded h t)} @ immutable)) @ total ->
    (coverage : ((x : node Pref.t) @ immutable -> {u : unit | covered h cut pool x})) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h eta x})) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below saved x cut) || rho x === eta x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (tree : bounded) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | pool_scoped h pool && unfolded h tree && valid (closed_heap h cut pool) epoch depth d &&
      target_for (closed_heap h cut pool) d (bound_root tree) q} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable ->
        {u : unit | equation (heap (closed_heap h cut pool) epoch depth d) tau x})) @ total ->
      (preserved : ((x : node Pref.t) @ immutable ->
        {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | tau q === eta (bound_root tree)} -> {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost =
  fun saved h cut pool scope forest coverage order prior rho rho_model eta eta_model equal epoch depth d tree q premise claim use -> ghost_ (
    relative_interpret saved h cut prior order rho rho_model eta eta_model equal tree ();
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable ->
        {u : unit | equation (heap (closed_heap h cut pool) epoch depth d) tau x})) @ total ->
      (preserved : ((x : node Pref.t) @ immutable ->
        {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | tau q === interpret rho eta (scheme h cut tree)} ->
      {u : unit | claim}) @ total = fun tau next preserved fit ->
        let () = use tau next preserved () in () in
    let () = with_generalized_instance h scope forest cut pool coverage rho rho_model eta epoch depth d tree q
      () claim consume in ())
