open Copy_spec
open Level_spec
open Generalize_spec
open Generalize_proofs

let (scheme_root @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (t : bounded) @ immutable ->
    {u : unit | root (scheme h cut t) === bound_root t} @ ghost = fun h cut t -> ghost_ (
  scheme_def h cut t; bound_root_def t; let s = scheme h cut t in root_def s; let u = () in refine_ u)
let rec (scheme_valid @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    (coverage : ((x : node Pref.t) @ immutable -> {u : unit | covered h cut pool x})) @ total ->
    (t : bounded) @ immutable -> {u : unit | pool_scoped h pool && unfolded h t} ->
    {u : unit | template (closed_heap h cut pool) (scheme h cut t)} @ ghost = fun h cut pool coverage t premise -> ghost_ (
  let refine_ premise = premise in unfolded_def h t; bound_root_def t; scheme_def h cut t;
  let after = closed_heap h cut pool in let p = bound_root t in let s = scheme h cut t in
  coverage p; let u = () in closed_level h cut pool p (refine_ u);
  closed_observe h cut pool p (refine_ u); closed_at_def h after cut pool p;
  at_level_def h p; at_level_def after p;
  scheme_root h cut t; template_def after s; root_def s; finite_node_def after p;
  let level = at_level h p in close_level_def cut level;
  match t with
  | Tip _ -> let desc = Var in generic_desc_def after p desc; let desc = Bool in generic_desc_def after p desc; refine_ u
  | Through (_, c) -> scheme_valid h cut pool coverage c (refine_ u); scheme_root h cut c;
    let desc = Link (bound_root c) in generic_desc_def after p desc; refine_ u
  | Fork (_, a, b) -> scheme_valid h cut pool coverage a (refine_ u); scheme_valid h cut pool coverage b (refine_ u);
    scheme_root h cut a; scheme_root h cut b;
    let desc = Arrow (bound_root a, bound_root b) in generic_desc_def after p desc; refine_ u)

let (closed_scope @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (cut : int) -> (pool : pool) @ immutable -> (x : node Pref.t) @ immutable -> {u : unit | pool_scoped h pool} ->
    {u : unit | if H.mem (closed_heap h cut pool) x then source_ok (closed_heap h cut pool) x else H.at (closed_heap h cut pool) x === None} @ ghost =
  fun h scope cut pool x premise -> ghost_ (
    let refine_ premise = premise in scope x; let after = closed_heap h cut pool in let u = () in
    closed_observe h cut pool x (refine_ u); closed_at_def h after cut pool x;
    if H.mem h x then (closed_source h cut pool x (refine_ u); refine_ u) else refine_ u)

let (with_generalized_instance @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (forest : ((x : node Pref.t) @ immutable -> {t : bounded | not (H.mem h x) || (bound_root t === x && unfolded h t)} @ immutable)) @ total ->
    (cut : int) -> (pool : pool) @ immutable ->
    (coverage : ((x : node Pref.t) @ immutable -> {u : unit | covered h cut pool x})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (t : bounded) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | pool_scoped h pool && unfolded h t && valid (closed_heap h cut pool) epoch depth d
      && target_for (closed_heap h cut pool) d (bound_root t) q} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap (closed_heap h cut pool) epoch depth d) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | tau q === interpret rho choices (scheme h cut t)} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun h scope forest cut pool coverage rho model choices epoch depth d t q premise claim use -> ghost_ (
    let refine_ premise = premise in let saved = closed_heap h cut pool in
    let scope_saved : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None}) @ total =
      fun x -> let u = () in let refine_ u = closed_scope h scope cut pool x (refine_ u) in refine_ u in
    let trees : ((x : node Pref.t) @ immutable -> {s : template | not (H.mem saved x) || (root s === x && template saved s)} @ immutable) @ total =
      fun x -> let refine_ tree = forest x in let u = () in closed_observe h cut pool x (refine_ u); closed_at_def h saved cut pool x;
        let s = scheme h cut tree in scheme_root h cut tree;
        if H.mem h x then (scheme_valid h cut pool coverage tree (refine_ u); refine_ s) else refine_ s in
    let model_saved : ((x : node Pref.t) @ immutable -> {u : unit | equation saved rho x}) @ total =
      fun x -> model x; let u = () in closed_model h cut pool rho x (refine_ u); refine_ u in
    let s = scheme h cut t in
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x})) @ total ->
      {u : unit | tau q === interpret rho choices s} -> {u : unit | claim}) @ total = fun tau next equal fit ->
        let refine_ fit = fit in
        let equal_old : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x}) @ total =
          fun x -> equal x; let u = () in closed_observe h cut pool x (refine_ u); closed_at_def h saved cut pool x; refine_ u in
        let next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap (closed_heap h cut pool) epoch depth d) tau x}) @ total = refine_ next in
        let refine_ u = use tau next equal_old (refine_ fit) in refine_ u in
    let u = () in scheme_valid h cut pool coverage t (refine_ u); scheme_root h cut t;
    let refine_ u = Copy_template_proofs.with_scheme_instance saved scope_saved trees rho model_saved choices epoch depth d s q
      (refine_ u) claim consume in refine_ u)

let (with_generalized_choices @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    (coverage : ((x : node Pref.t) @ immutable -> {u : unit | covered h cut pool x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap (closed_heap h cut pool) epoch depth d) rho x})) @ total ->
    (t : bounded) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | pool_scoped h pool && unfolded h t && valid (closed_heap h cut pool) epoch depth d
      && target_for (closed_heap h cut pool) d (bound_root t) q} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | rho q === interpret rho choices (scheme h cut t)} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun h cut pool coverage epoch depth d rho model t q premise claim use -> ghost_ (
    let refine_ premise = premise in let saved = closed_heap h cut pool in let u = () in
    scheme_valid h cut pool coverage t (refine_ u); scheme_root h cut t; let s = scheme h cut t in
    let model : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) rho x}) @ total = refine_ model in
    let consume : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | rho q === interpret rho choices s} -> {u : unit | claim}) @ total = fun choices fit ->
        let refine_ fit = fit in let refine_ u = use choices (refine_ fit) in refine_ u in
    let refine_ u = Copy_template_proofs.with_instance_choices saved epoch depth d rho model s q (refine_ u) claim consume in refine_ u)
