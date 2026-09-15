open Copy_spec
open Level_unifier_spec
open Level_unifier_proofs
open Level_finite_spec
open Level_finite_proofs
open Level_mgu_spec

let rec (readback_factor @ total) :
    (h : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
    (t : tree) @ immutable -> {u : unit | finite h t} ->
    {u : unit | rho (tree_root t) === substitute rho (readback t)} @ ghost =
  fun h rho model t premise -> ghost_ (
    let refine_ premise = premise in
    finite_def h t; tree_root_def t; readback_def t;
    let x = tree_root t in model x; node_equation_def h rho x;
    let v = readback t in substitute_def rho v;
    let u = () in
    match t with
    | Free _ | Constant_tree _ -> refine_ u
    | Alias_tree (_, child) -> readback_factor h rho model child (refine_ u); refine_ u
    | Branch (_, a, b) ->
      readback_factor h rho model a (refine_ u);
      readback_factor h rho model b (refine_ u); refine_ u)

let (normal_model_at @ total) :
    (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x &&
        (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (normal : ((x : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees x in normalizes h sigma x t})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | node_equation h sigma x} @ ghost =
  fun h trees sigma normal x -> ghost_ (
    let agrees : (x : node Pref.t) @ immutable ->
        {u : unit | let refine_ t = trees x in not (H.mem h x) || sigma x === readback t}
        @ total = fun x ->
      normal x; let refine_ t = trees x in normalizes_def h sigma x t;
      let u = () in refine_ u in
    let refine_ u = readback_model_at h trees sigma agrees x in refine_ u)

let (mgu_solution_at @ total) :
    (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x &&
        (if H.mem after x then finite after t else observe after x === None)} @ immutable)) @ total ->
    (sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (normal : ((x : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees x in normalizes after sigma x t})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q true after d} ->
    {u : unit | node_equation h sigma x && sigma p === sigma q} @ ghost =
  fun h p q after d trees sigma normal x premise -> ghost_ (
    let refine_ premise = premise in
    let model : (x : node Pref.t) @ immutable -> {u : unit | node_equation after sigma x}
        @ total = fun x ->
      let refine_ u = normal_model_at after trees sigma normal x in refine_ u in
    let u = () in success_forward_at h sigma p q after d model x (refine_ u))

let (mgu_factor_at @ total) :
    (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x &&
        (if H.mem after x then finite after t else observe after x === None)} @ immutable)) @ total ->
    (sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (normal : ((x : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees x in normalizes after sigma x t})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q true after d && rho p === rho q} ->
    {u : unit | rho x === substitute rho (sigma x)} @ ghost =
  fun h p q after d trees sigma normal rho model x premise -> ghost_ (
    let refine_ premise = premise in
    normal x; let refine_ t = trees x in normalizes_def after sigma x t;
    let u = () in
    if H.mem after x then (
      let after_model : (x : node Pref.t) @ immutable -> {u : unit | node_equation after rho x}
          @ total = fun x ->
        let u = () in
        let refine_ u = success_backward_at h rho model p q after d x (refine_ u) in
        refine_ u in
      readback_factor after rho after_model t (refine_ u); refine_ u)
    else (
      let v = Variable x in substitute_def rho v; refine_ u))

let (mgu_support_at @ total) :
    (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable -> (t : tree) @ immutable ->
    {u : unit | unified h p q true after d && normalizes after sigma x t
      && not (H.mem h x)} ->
    {u : unit | sigma x === Variable x} @ ghost = fun h p q after d sigma x t premise -> ghost_ (
  let refine_ premise = premise in let u = () in
  unified_frame h p q true after d x (refine_ u);
  normalizes_def after sigma x t; refine_ u)

let (instance_solution_at @ total) :
    (h : node Pref.heap) @ immutable ->
    (sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h sigma x})) @ total ->
    (delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (instance : ((x : node Pref.t) @ immutable ->
      {u : unit | rho x === substitute delta (sigma x)})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | sigma p === sigma q} ->
    {u : unit | node_equation h rho x && rho p === rho q} @ ghost =
  fun h sigma model delta rho instance p q x premise -> ghost_ (
    let refine_ premise = premise in
    model x; node_equation_def h sigma x; node_equation_def h rho x;
    instance p; instance q; instance x;
    let u = () in
    match observe h x with
    | None | Some Var -> refine_ u
    | Some Bool -> let v = Boolean in substitute_def delta v; refine_ u
    | Some (Link y) -> instance y; refine_ u
    | Some (Arrow (a, b)) ->
      instance a; instance b;
      let v = Function (sigma a, sigma b) in substitute_def delta v; refine_ u)

let (with_mgu @ total) :
    (h : node Pref.heap) @ immutable ->
    (before_trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x &&
        (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    {u : unit | unified h p q true after d} ->
    (claim : bool) ->
    (use : ((sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (solution : ((x : node Pref.t) @ immutable ->
        {u : unit | node_equation h sigma x && sigma p === sigma q
          && sigma x === substitute sigma (sigma x)
          && (H.mem h x || sigma x === Variable x)})) @ total ->
      (factor : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
        (x : node Pref.t) @ immutable -> {u : unit | rho p === rho q} ->
        {u : unit | rho x === substitute rho (sigma x)})) @ total ->
      {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost = fun h before_trees p q after d premise claim use -> ghost_ (
  let refine_ premise = premise in
  let trees : (x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x &&
        (if H.mem after x then finite after t else observe after x === None)}
      @ immutable total = fun x ->
    let u = () in
    let refine_ t = unified_finite_at h before_trees p q true after d x (refine_ u) in
    refine_ t in
  let[@def] sigma : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
    let refine_ t = trees x in
    if H.mem after x then readback t else Variable x in
  let normal : (x : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees x in normalizes after sigma x t}
      @ total = fun x ->
    sigma_def x; let refine_ t = trees x in normalizes_def after sigma x t;
    let u = () in refine_ u in
  let model : (x : node Pref.t) @ immutable -> {u : unit | node_equation h sigma x}
      @ total = fun x ->
    let u = () in
    mgu_solution_at h p q after d trees sigma normal x (refine_ u); refine_ u in
  let solution : (x : node Pref.t) @ immutable ->
      {u : unit | node_equation h sigma x && sigma p === sigma q
        && sigma x === substitute sigma (sigma x)
        && (H.mem h x || sigma x === Variable x)} @ total = fun x ->
    let u = () in
    mgu_solution_at h p q after d trees sigma normal x (refine_ u);
    mgu_factor_at h p q after d trees sigma normal sigma model x (refine_ u);
    normal x; let refine_ t = trees x in
    if not (H.mem h x) then (
      mgu_support_at h p q after d sigma x t (refine_ u); refine_ u)
    else refine_ u in
  let factor : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (old_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
      (x : node Pref.t) @ immutable -> {u : unit | rho p === rho q} ->
      {u : unit | rho x === substitute rho (sigma x)}) @ total = fun rho old_model x equal ->
    let refine_ equal = equal in let u = () in
    let refine_ u = mgu_factor_at h p q after d trees sigma normal rho old_model x (refine_ u) in
    refine_ u in
  let refine_ u = use sigma solution factor in refine_ u)
