open Unifier_spec
open Unifier_proofs
open Unifier_finite_spec
open Unifier_finite_proofs
open Unifier_mgu_spec

let rec (readback_factor @ total) :
    (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (t : tree) @ immutable -> {u : unit | finite h t} ->
    {u : unit | rho (root t) === substitute rho (readback t)} @ ghost =
  fun h rho model t premise -> ghost_ (
    finite_def h t; root_def t; readback_def t;
    let x = root t in model x; equation_def h rho x;
    let v = readback t in substitute_def rho v;
    let u = () in
    match t with
    | Free _ | Boolean _ -> u
    | Alias (_, child) -> readback_factor h rho model child (u); u
    | Branch (_, a, b) ->
      readback_factor h rho model a (u);
      readback_factor h rho model b (u); u)

let (normal_model_at @ total) :
    (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ->
    (sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (normal : ((x : node Pref.t) @ immutable ->
      {u : unit | let t = trees x in normalizes h sigma x t})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | equation h sigma x} @ ghost =
  fun h trees sigma normal x -> ghost_ (
    let agrees : (x : node Pref.t) @ immutable ->
        {u : unit | let t = trees x in not (H.mem h x) || sigma x === readback t}
        @ total = fun x ->
      normal x; let t = trees x in normalizes_def h sigma x t;
      let u = () in u in
    let u = readback_model_at h trees sigma agrees x in u)

let (mgu_solution_at @ total) :
    (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem after x then finite after t else H.at after x === None)} @ immutable)) @ total ->
    (sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (normal : ((x : node Pref.t) @ immutable ->
      {u : unit | let t = trees x in normalizes after sigma x t})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q true after d} ->
    {u : unit | equation h sigma x && sigma p === sigma q} @ ghost =
  fun h p q after d trees sigma normal x premise -> ghost_ (
    let model : (x : node Pref.t) @ immutable -> {u : unit | equation after sigma x}
        @ total = fun x ->
      let u = normal_model_at after trees sigma normal x in u in
    let u = () in success_forward_at h sigma p q after d model x (u))

let (mgu_factor_at @ total) :
    (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem after x then finite after t else H.at after x === None)} @ immutable)) @ total ->
    (sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (normal : ((x : node Pref.t) @ immutable ->
      {u : unit | let t = trees x in normalizes after sigma x t})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q true after d && rho p === rho q} ->
    {u : unit | rho x === substitute rho (sigma x)} @ ghost =
  fun h p q after d trees sigma normal rho model x premise -> ghost_ (
    normal x; let t = trees x in normalizes_def after sigma x t;
    let u = () in
    if H.mem after x then (
      let after_model : (x : node Pref.t) @ immutable -> {u : unit | equation after rho x}
          @ total = fun x ->
        let u = () in
        let u = success_backward_at h rho model p q after d x (u) in
        u in
      readback_factor after rho after_model t (u); u)
    else (
      let v = TVar x in substitute_def rho v; u))

let (mgu_support_at @ total) :
    (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable -> (t : tree) @ immutable ->
    {u : unit | unified h p q true after d && normalizes after sigma x t
      && not (H.mem h x)} ->
    {u : unit | sigma x === TVar x} @ ghost = fun h p q after d sigma x t premise -> ghost_ (
  let u = () in
  unified_frame h p q true after d x (u);
  normalizes_def after sigma x t; u)

let (instance_solution_at @ total) :
    (h : Pref.heap) @ immutable ->
    (sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h sigma x})) @ total ->
    (delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (instance : ((x : node Pref.t) @ immutable ->
      {u : unit | rho x === substitute delta (sigma x)})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | sigma p === sigma q} ->
    {u : unit | equation h rho x && rho p === rho q} @ ghost =
  fun h sigma model delta rho instance p q x premise -> ghost_ (
    model x; equation_def h sigma x; equation_def h rho x;
    instance p; instance q; instance x;
    let u = () in
    match H.at h x with
    | None | Some Var -> u
    | Some Bool -> let v = TBool in substitute_def delta v; u
    | Some (Link y) -> instance y; u
    | Some (Arrow (a, b)) ->
      instance a; instance b;
      let v = TArrow (sigma a, sigma b) in substitute_def delta v; u)

let (with_mgu @ total) :
    (h : Pref.heap) @ immutable ->
    (before_trees : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    {u : unit | unified h p q true after d} ->
    (claim : bool) ->
    (use : ((sigma : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (solution : ((x : node Pref.t) @ immutable ->
        {u : unit | equation h sigma x && sigma p === sigma q
          && sigma x === substitute sigma (sigma x)
          && (H.mem h x || sigma x === TVar x)})) @ total ->
      (factor : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
        (x : node Pref.t) @ immutable -> {u : unit | rho p === rho q} ->
        {u : unit | rho x === substitute rho (sigma x)})) @ total ->
      {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost = fun h before_trees p q after d premise claim use -> ghost_ (
  let trees : (x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem after x then finite after t else H.at after x === None)}
      @ immutable total = fun x ->
    let u = () in
    let t = unified_finite_at h before_trees p q true after d x (u) in
    t in
  let[@def] sigma : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
    let t = trees x in
    if H.mem after x then readback t else TVar x in
  let normal : (x : node Pref.t) @ immutable ->
      {u : unit | let t = trees x in normalizes after sigma x t}
      @ total = fun x ->
    sigma_def x; let t = trees x in normalizes_def after sigma x t;
    let u = () in u in
  let model : (x : node Pref.t) @ immutable -> {u : unit | equation h sigma x}
      @ total = fun x ->
    let u = () in
    mgu_solution_at h p q after d trees sigma normal x (u); u in
  let solution : (x : node Pref.t) @ immutable ->
      {u : unit | equation h sigma x && sigma p === sigma q
        && sigma x === substitute sigma (sigma x)
        && (H.mem h x || sigma x === TVar x)} @ total = fun x ->
    let u = () in
    mgu_solution_at h p q after d trees sigma normal x (u);
    mgu_factor_at h p q after d trees sigma normal sigma model x (u);
    normal x; let t = trees x in
    if not (H.mem h x) then (
      mgu_support_at h p q after d sigma x t (u); u)
    else u in
  let factor : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (old_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
      (x : node Pref.t) @ immutable -> {u : unit | rho p === rho q} ->
      {u : unit | rho x === substitute rho (sigma x)}) @ total = fun rho old_model x equal ->
    let u = () in
    let u = mgu_factor_at h p q after d trees sigma normal rho old_model x (u) in
    u in
  let u = use sigma solution factor in u)
