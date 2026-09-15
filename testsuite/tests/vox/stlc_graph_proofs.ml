open Unifier_spec
open Unifier_finite_spec
open Unifier_finite_proofs
open Stlc_spec

let rec (lookup_exists @ total) : (h : node Pref.heap) @ immutable -> (env : env) @ immutable ->
    (i : index) @ immutable ->
    {u : unit | env_allocated h env && present (depth env) i} ->
    {u : unit | match lookup env i with None -> false | Some p -> H.mem h p} @ ghost =
  fun h env i premise -> ghost_ (
    let refine_ premise = premise in env_allocated_def h env; depth_def env;
    let n = depth env in present_def n i; lookup_def env i;
    let u = () in match env with
    | Empty -> refine_ u
    | Bind (_, rest) -> match i with
      | Z -> refine_ u
      | S i -> lookup_exists h rest i (refine_ u); refine_ u)

let rec (built_frame @ total) : (h : node Pref.heap) @ immutable -> (env : env) @ immutable ->
    (g : graph) @ immutable -> (after : node Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | built h env g after} ->
    {u : unit | H.mem after (root g) && (not (H.mem h x) || H.mem after x)} @ ghost =
  fun h env g after x premise -> ghost_ (
    let refine_ premise = premise in built_def h env g after; root_def g;
    let u = () in match g with
    | GVar _ | GBool _ -> refine_ u
    | GLam (arg, body, p, middle) ->
      let h1 = H.put h arg Var in let env1 = Bind (arg, env) in
      built_frame h1 env1 body middle x (refine_ u); refine_ u
    | GApp (f, a, p, arrow, h1, h2) ->
      built_frame h env f h1 x (refine_ u);
      built_frame h1 env a h2 x (refine_ u); refine_ u)

let rec (env_frame @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (keep : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem after x})) @ total ->
    (env : env) @ immutable -> {u : unit | env_allocated h env} ->
    {u : unit | env_allocated after env} @ ghost = fun h after keep env premise -> ghost_ (
  let refine_ premise = premise in env_allocated_def h env; env_allocated_def after env;
  let u = () in match env with Empty -> refine_ u | Bind (p, rest) ->
    keep p; env_frame h after keep rest (refine_ u); refine_ u)

let rec (equations_frame @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (keep : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem after x})) @ total ->
    (eqs : equations) @ immutable -> {u : unit | equations_allocated h eqs} ->
    {u : unit | equations_allocated after eqs} @ ghost = fun h after keep eqs premise -> ghost_ (
  let refine_ premise = premise in equations_allocated_def h eqs; equations_allocated_def after eqs;
  let u = () in match eqs with
  | Nothing -> refine_ u | Equal (p, q) -> keep p; keep q; refine_ u
  | And (a, b) -> equations_frame h after keep a (refine_ u);
    equations_frame h after keep b (refine_ u); refine_ u)

let rec (built_equations @ total) : (h : node Pref.heap) @ immutable -> (env : env) @ immutable ->
    (g : graph) @ immutable -> (after : node Pref.heap) @ immutable ->
    {u : unit | built h env g after} ->
    {u : unit | equations_allocated after (constraints g)} @ ghost =
  fun h env g after premise -> ghost_ (
    let refine_ premise = premise in built_def h env g after; constraints_def g;
    let cs = constraints g in equations_allocated_def after cs;
    let u = () in match g with
    | GVar _ | GBool _ -> refine_ u
    | GLam (arg, body, p, middle) ->
      let h1 = H.put h arg Var in let env1 = Bind (arg, env) in
      built_equations h1 env1 body middle (refine_ u);
      let keep : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem middle x) || H.mem after x}
          @ total = fun x -> let u = () in refine_ u in
      let cs = constraints body in equations_frame middle after keep cs (refine_ u); refine_ u
    | GApp (f, a, p, arrow, h1, h2) ->
      built_equations h env f h1 (refine_ u); built_equations h1 env a h2 (refine_ u);
      let keep1 : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || H.mem after x}
          @ total = fun x -> let u = () in built_frame h1 env a h2 x (refine_ u); refine_ u in
      let keep2 : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || H.mem after x}
          @ total = fun x -> let u = () in refine_ u in
      let cf = constraints f in let ca = constraints a in
      equations_frame h1 after keep1 cf (refine_ u); equations_frame h2 after keep2 ca (refine_ u);
      let pair = And (cf, ca) in equations_allocated_def after pair;
      let rf = root f in built_frame h env f h1 rf (refine_ u); keep1 rf;
      let last = Equal (rf, arrow) in equations_allocated_def after last; refine_ u)

let rec (built_finite_at @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ->
    (env : env) @ immutable -> (g : graph) @ immutable -> (after : node Pref.heap) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | built h env g after} ->
    {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem after x then finite after t else H.at after x === None)} @ immutable ghost =
  fun h trees env g after x premise -> ghost_ (
    let refine_ premise = premise in built_def h env g after;
    let u = () in match g with
    | GVar _ -> let refine_ t = trees x in refine_ t
    | GBool p -> let v = Bool in allocatable_def h v;
      let refine_ t = allocation_finite_at h trees p v x (refine_ u) in refine_ t
    | GLam (arg, body, p, middle) ->
      let h1 = H.put h arg Var in let env1 = Bind (arg, env) in
      let trees1 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h1 x then finite h1 t else H.at h1 x === None)} @ immutable total = fun x ->
        let v = Var in allocatable_def h v; let u = () in
        let refine_ t = allocation_finite_at h trees arg v x (refine_ u) in refine_ t in
      let trees2 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem middle x then finite middle t else H.at middle x === None)} @ immutable total = fun x ->
        let u = () in let refine_ t = built_finite_at h1 trees1 env1 body middle x (refine_ u) in refine_ t in
      built_frame h1 env1 body middle arg (refine_ u);
      let v = Arrow (arg, root body) in allocatable_def middle v;
      let refine_ t = allocation_finite_at middle trees2 p v x (refine_ u) in refine_ t
    | GApp (f, a, p, arrow, h1, h2) ->
      let trees1 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h1 x then finite h1 t else H.at h1 x === None)} @ immutable total = fun x ->
        let u = () in let refine_ t = built_finite_at h trees env f h1 x (refine_ u) in refine_ t in
      let trees2 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h2 x then finite h2 t else H.at h2 x === None)} @ immutable total = fun x ->
        let u = () in let refine_ t = built_finite_at h1 trees1 env a h2 x (refine_ u) in refine_ t in
      let h3 = H.put h2 p Var in
      let trees3 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h3 x then finite h3 t else H.at h3 x === None)} @ immutable total = fun x ->
        let v = Var in allocatable_def h2 v; let u = () in
        let refine_ t = allocation_finite_at h2 trees2 p v x (refine_ u) in refine_ t in
      built_frame h1 env a h2 p (refine_ u);
      let v = Arrow (root a, p) in allocatable_def h3 v;
      let refine_ t = allocation_finite_at h3 trees3 arrow v x (refine_ u) in refine_ t)

let (allocation_mem @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    {u : unit | H.mem (H.put h p v) p} @ ghost = fun h p v -> ghost_ (
  let u = () in refine_ u)

let rec (built_contents @ total) : (h : node Pref.heap) @ immutable -> (env : env) @ immutable ->
    (g : graph) @ immutable -> (after : node Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | built h env g after && H.mem h x} ->
    {u : unit | H.at after x === H.at h x} @ ghost = fun h env g after x premise -> ghost_ (
  let refine_ premise = premise in built_def h env g after;
  let u = () in match g with
  | GVar _ | GBool _ -> refine_ u
  | GLam (arg, body, p, middle) ->
    let h1 = H.put h arg Var in let env1 = Bind (arg, env) in
    built_frame h1 env1 body middle x (refine_ u);
    built_contents h1 env1 body middle x (refine_ u); refine_ u
  | GApp (f, a, p, arrow, h1, h2) ->
    built_frame h env f h1 x (refine_ u);
    built_frame h1 env a h2 x (refine_ u);
    built_contents h env f h1 x (refine_ u);
    built_contents h1 env a h2 x (refine_ u); refine_ u)

let rec (lookup_context @ total) :
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (env : env) @ immutable -> (i : index) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | lookup env i === Some p} ->
    {u : unit | lookup_type (context_of rho env) i === Some (rho p)} @ ghost =
  fun rho env i p premise -> ghost_ (
    let refine_ premise = premise in lookup_def env i; context_of_def rho env;
    let ctx = context_of rho env in lookup_type_def ctx i;
    let u = () in match env with Empty -> refine_ u | Bind (_, rest) -> match i with
    | Z -> refine_ u | S i -> lookup_context rho rest i p (refine_ u); refine_ u)

let rec (generation_sound @ total) : (h : node Pref.heap) @ immutable -> (env : env) @ immutable ->
    (g : graph) @ immutable -> (after : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after x) || equation after rho x})) @ total ->
    {u : unit | built h env g after && satisfies rho (constraints g)} ->
    {u : unit | typed (context_of rho env) (source g) (rho (root g)) (derive rho g)} @ ghost =
  fun h env g after rho model premise -> ghost_ (
    let refine_ premise = premise in built_def h env g after; source_def g; root_def g;
    constraints_def g; derive_def rho g;
    let ctx = context_of rho env in let e = source g in let t = rho (root g) in let d = derive rho g in
    typed_def ctx e t d; let r = root g in
    let u = () in built_frame h env g after r (refine_ u); model r; equation_def after rho r;
    match g with
    | GVar (i, p) -> lookup_context rho env i p (refine_ u); refine_ u
    | GBool _ -> refine_ u
    | GLam (arg, body, p, middle) ->
      let h1 = H.put h arg Var in let env1 = Bind (arg, env) in
      let mid : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem middle x) || equation middle rho x}
          @ total = fun x -> model x; equation_def after rho x; equation_def middle rho x;
        let u = () in refine_ u in
      generation_sound h1 env1 body middle rho mid (refine_ u);
      context_of_def rho env1; refine_ u
    | GApp (f, a, p, arrow, h1, h2) ->
      let cs = constraints g in satisfies_def rho cs;
      let pair = And (constraints f, constraints a) in satisfies_def rho pair;
      let last = Equal (root f, arrow) in satisfies_def rho last;
      model arrow; equation_def after rho arrow;
      let mid2 : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || equation h2 rho x}
          @ total = fun x -> model x; equation_def after rho x; equation_def h2 rho x;
        let u = () in refine_ u in
      let mid1 : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || equation h1 rho x}
          @ total = fun x -> let u = () in
        if H.mem h1 x then (
          built_frame h1 env a h2 x (refine_ u);
          built_contents h1 env a h2 x (refine_ u); mid2 x;
          equation_def h1 rho x; equation_def h2 rho x; refine_ u)
        else refine_ u in
      generation_sound h env f h1 rho mid1 (refine_ u);
      generation_sound h1 env a h2 rho mid2 (refine_ u); refine_ u)
