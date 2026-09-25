open Unifier_spec
open Unifier_finite_spec
open Unifier_finite_proofs
open Stlc_spec
open Stlc_graph_proofs

let rec (context_equal @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
    (env : env) @ immutable -> {u : unit | env_allocated h env} ->
    {u : unit | context_of tau env === context_of rho env} @ ghost = fun h rho tau equal env premise -> ghost_ (
  let refine_ premise = premise in env_allocated_def h env; context_of_def rho env; context_of_def tau env;
  let u = () in match env with Empty -> refine_ u | Bind (p, rest) ->
    equal p; context_equal h rho tau equal rest (refine_ u); refine_ u)

let rec (constraints_equal @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
    (eqs : equations) @ immutable -> {u : unit | equations_allocated h eqs && satisfies rho eqs} ->
    {u : unit | satisfies tau eqs} @ ghost = fun h rho tau equal eqs premise -> ghost_ (
  let refine_ premise = premise in equations_allocated_def h eqs; satisfies_def rho eqs; satisfies_def tau eqs;
  let u = () in match eqs with Nothing -> refine_ u | Equal (p, q) -> equal p; equal q; refine_ u
  | And (a, b) -> constraints_equal h rho tau equal a (refine_ u);
    constraints_equal h rho tau equal b (refine_ u); refine_ u)

let (allocation_model_at @ total) : (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable -> (value : ty) @ immutable ->
    (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (update : ((x : node Pref.t) @ immutable -> {u : unit | tau x === (if x === p then value else rho x)})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p) && allocatable h v && describes rho v value} ->
    {u : unit | equation (H.put h p v) tau x} @ ghost = fun h trees rho model p v value tau update x premise -> ghost_ (
  let refine_ premise = premise in allocatable_def h v; describes_def rho v value;
  let after = H.put h p v in equation_def after tau x; update x;
  let u = () in
  if x === p then (
    match v with Var | Bool -> refine_ u | Link q -> update q; refine_ u
    | Arrow (a, b) -> update a; update b; refine_ u)
  else (
    let refine_ t = trees x in
    if H.mem h x then (
      finite_scope_at h t (refine_ u); scoped_def h x;
      model x; equation_def h rho x;
      match H.at h x with
      | None | Some (Var | Bool) -> refine_ u
      | Some (Link q) -> update q; refine_ u
      | Some (Arrow (a, b)) -> update a; update b; refine_ u)
    else refine_ u))

let (with_allocation_model @ total) : (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable -> (value : ty) @ immutable ->
    (after : Pref.heap) @ immutable ->
    {u : unit | not (H.mem h p) && allocatable h v && describes rho v value && after === H.put h p v} ->
    (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | tau p === value} -> {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost = fun h trees rho model p v value after premise claim use -> ghost_ (
  let refine_ premise = premise in
  let[@def] tau : node Pref.t @ immutable total -> ty @ immutable total =
    fun x -> if x === p then value else rho x in
  let update : (x : node Pref.t) @ immutable -> {u : unit | tau x === (if x === p then value else rho x)}
      @ total = fun x -> tau_def x; let u = () in refine_ u in
  let next : (x : node Pref.t) @ immutable -> {u : unit | equation after tau x}
      @ total = fun x -> let u = () in
    let refine_ u = allocation_model_at h trees rho model p v value tau update x (refine_ u) in refine_ u in
  let equal : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x}
      @ total = fun x -> update x; let u = () in refine_ u in
  tau_def p; let u = () in let refine_ u = use tau next equal (refine_ u) in refine_ u)
