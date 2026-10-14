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
    match env with
    | Empty -> ()
    | Bind (_, rest) -> match i with
      | Z -> ()
      | S i -> lookup_exists h rest i (); ())

let rec (built_frame @ total) : (h : node Pref.heap) @ immutable -> (env : env) @ immutable ->
    (g : graph) @ immutable -> (after : node Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | built h env g after} ->
    {u : unit | H.mem after (root g) && (not (H.mem h x) || H.mem after x)} @ ghost =
  fun h env g after x premise -> ghost_ (
    let refine_ premise = premise in built_def h env g after; root_def g;
    match g with
    | GVar _ | GBool _ -> ()
    | GLam (arg, body, p, middle) ->
      let h1 = H.put h arg Var in let env1 = Bind (arg, env) in
      built_frame h1 env1 body middle x (); ()
    | GRec (arg, result, p, body) ->
      let h1 = H.put h arg Var in let h2 = H.put h1 result Var in
      let h3 = H.put h2 p (Arrow (arg, result)) in
      let env1 = Bind (arg, Bind (p, env)) in
      built_frame h3 env1 body after x ();
      built_frame h3 env1 body after p (); ()
    | GApp (f, a, p, arrow, h1, h2) ->
      built_frame h env f h1 x ();
      built_frame h1 env a h2 x (); ())

let rec (env_frame @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (keep : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem after x})) @ total ->
    (env : env) @ immutable -> {u : unit | env_allocated h env} ->
    {u : unit | env_allocated after env} @ ghost = fun h after keep env premise -> ghost_ (
  let refine_ premise = premise in env_allocated_def h env; env_allocated_def after env;
  match env with Empty -> () | Bind (p, rest) ->
    keep p; env_frame h after keep rest (); ())

let rec (equations_frame @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (keep : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || H.mem after x})) @ total ->
    (eqs : equations) @ immutable -> {u : unit | equations_allocated h eqs} ->
    {u : unit | equations_allocated after eqs} @ ghost = fun h after keep eqs premise -> ghost_ (
  let refine_ premise = premise in equations_allocated_def h eqs; equations_allocated_def after eqs;
  match eqs with
  | Nothing -> () | Equal (p, q) -> keep p; keep q; ()
  | And (a, b) -> equations_frame h after keep a ();
    equations_frame h after keep b (); ())

let rec (built_equations @ total) : (h : node Pref.heap) @ immutable -> (env : env) @ immutable ->
    (g : graph) @ immutable -> (after : node Pref.heap) @ immutable ->
    {u : unit | built h env g after} ->
    {u : unit | equations_allocated after (constraints g)} @ ghost =
  fun h env g after premise -> ghost_ (
    let refine_ premise = premise in built_def h env g after; constraints_def g;
    let cs = constraints g in equations_allocated_def after cs;
    match g with
    | GVar _ | GBool _ -> ()
    | GLam (arg, body, p, middle) ->
      let h1 = H.put h arg Var in let env1 = Bind (arg, env) in
      built_equations h1 env1 body middle ();
      let keep : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem middle x) || H.mem after x}
          @ total = fun x -> () in
      let cs = constraints body in equations_frame middle after keep cs (); ()
    | GRec (arg, result, p, body) ->
      let h1 = H.put h arg Var in let h2 = H.put h1 result Var in
      let h3 = H.put h2 p (Arrow (arg, result)) in
      let env1 = Bind (arg, Bind (p, env)) in
      built_equations h3 env1 body after ();
      built_frame h3 env1 body after result ();
      let last = Equal (root body, result) in equations_allocated_def after last; ()
    | GApp (f, a, p, arrow, h1, h2) ->
      built_equations h env f h1 (); built_equations h1 env a h2 ();
      let keep1 : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || H.mem after x}
          @ total = fun x -> built_frame h1 env a h2 x (); () in
      let keep2 : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || H.mem after x}
          @ total = fun x -> () in
      let cf = constraints f in let ca = constraints a in
      equations_frame h1 after keep1 cf (); equations_frame h2 after keep2 ca ();
      let pair = And (cf, ca) in equations_allocated_def after pair;
      let rf = root f in built_frame h env f h1 rf (); keep1 rf;
      let last = Equal (rf, arrow) in equations_allocated_def after last; ())

let rec (built_finite_at @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ->
    (env : env) @ immutable -> (g : graph) @ immutable -> (after : node Pref.heap) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | built h env g after} ->
    {t : tree | Unifier_finite_spec.root t === x &&
      (if H.mem after x then finite after t else H.at after x === None)} @ immutable ghost =
  fun h trees env g after x premise -> ghost_ (
    let refine_ premise = premise in built_def h env g after;
    match g with
    | GVar _ -> trees x
    | GBool p -> let v = Bool in allocatable_def h v;
      allocation_finite_at h trees p v x ()
    | GLam (arg, body, p, middle) ->
      let h1 = H.put h arg Var in let env1 = Bind (arg, env) in
      let trees1 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h1 x then finite h1 t else H.at h1 x === None)} @ immutable total = fun x ->
        let v = Var in allocatable_def h v; 
        allocation_finite_at h trees arg v x () in
      let trees2 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem middle x then finite middle t else H.at middle x === None)} @ immutable total = fun x ->
        built_finite_at h1 trees1 env1 body middle x () in
      built_frame h1 env1 body middle arg ();
      let v = Arrow (arg, root body) in allocatable_def middle v;
      allocation_finite_at middle trees2 p v x ()
    | GRec (arg, result, p, body) ->
      let h1 = H.put h arg Var in let h2 = H.put h1 result Var in
      let h3 = H.put h2 p (Arrow (arg, result)) in
      let env1 = Bind (arg, Bind (p, env)) in
      let v = Var in allocatable_def h v; allocatable_def h1 v;
      let trees1 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h1 x then finite h1 t else H.at h1 x === None)} @ immutable total = fun x ->
        allocation_finite_at h trees arg v x () in
      let trees2 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h2 x then finite h2 t else H.at h2 x === None)} @ immutable total = fun x ->
        allocation_finite_at h1 trees1 result v x () in
      let arrow = Arrow (arg, result) in allocatable_def h2 arrow;
      let trees3 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h3 x then finite h3 t else H.at h3 x === None)} @ immutable total = fun x ->
        allocation_finite_at h2 trees2 p arrow x () in
      built_finite_at h3 trees3 env1 body after x ()
    | GApp (f, a, p, arrow, h1, h2) ->
      let trees1 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h1 x then finite h1 t else H.at h1 x === None)} @ immutable total = fun x ->
        built_finite_at h trees env f h1 x () in
      let trees2 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h2 x then finite h2 t else H.at h2 x === None)} @ immutable total = fun x ->
        built_finite_at h1 trees1 env a h2 x () in
      let h3 = H.put h2 p Var in
      let trees3 : (x : node Pref.t) @ immutable -> {t : tree | Unifier_finite_spec.root t === x &&
          (if H.mem h3 x then finite h3 t else H.at h3 x === None)} @ immutable total = fun x ->
        let v = Var in allocatable_def h2 v; 
        allocation_finite_at h2 trees2 p v x () in
      built_frame h1 env a h2 p ();
      let v = Arrow (root a, p) in allocatable_def h3 v;
      allocation_finite_at h3 trees3 arrow v x ())

let (allocation_mem @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    {u : unit | H.mem (H.put h p v) p} @ ghost = fun h p v -> ghost_ (
  ())

let (allocation_keeps_mem @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h x) || H.mem (H.put h p v) x} @ ghost =
  fun h p v x -> ghost_ ()

let rec (built_contents @ total) : (h : node Pref.heap) @ immutable -> (env : env) @ immutable ->
    (g : graph) @ immutable -> (after : node Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | built h env g after && H.mem h x} ->
    {u : unit | H.at after x === H.at h x} @ ghost = fun h env g after x premise -> ghost_ (
  let refine_ premise = premise in built_def h env g after;
  match g with
  | GVar _ | GBool _ -> ()
  | GLam (arg, body, p, middle) ->
    let h1 = H.put h arg Var in let env1 = Bind (arg, env) in
    built_frame h1 env1 body middle x ();
    built_contents h1 env1 body middle x (); ()
  | GRec (arg, result, p, body) ->
    let h1 = H.put h arg Var in let h2 = H.put h1 result Var in
    let h3 = H.put h2 p (Arrow (arg, result)) in
    let env1 = Bind (arg, Bind (p, env)) in
    built_contents h3 env1 body after x (); ()
  | GApp (f, a, p, arrow, h1, h2) ->
    built_frame h env f h1 x ();
    built_frame h1 env a h2 x ();
    built_contents h env f h1 x ();
    built_contents h1 env a h2 x (); ())

let rec (lookup_context @ total) :
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (env : env) @ immutable -> (i : index) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | lookup env i === Some p} ->
    {u : unit | lookup_type (context_of rho env) i === Some (rho p)} @ ghost =
  fun rho env i p premise -> ghost_ (
    let refine_ premise = premise in lookup_def env i; context_of_def rho env;
    let ctx = context_of rho env in lookup_type_def ctx i;
    match env with Empty -> () | Bind (_, rest) -> match i with
    | Z -> () | S i -> lookup_context rho rest i p (); ())

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
    built_frame h env g after r (); model r; equation_def after rho r;
    match g with
    | GVar (i, p) -> lookup_context rho env i p (); ()
    | GBool _ -> ()
    | GLam (arg, body, p, middle) ->
      let h1 = H.put h arg Var in let env1 = Bind (arg, env) in
      let mid : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem middle x) || equation middle rho x}
          @ total = fun x -> model x; equation_def after rho x; equation_def middle rho x;
        () in
      generation_sound h1 env1 body middle rho mid ();
      context_of_def rho env1; ()
    | GRec (arg, result, p, body) ->
      let h1 = H.put h arg Var in let h2 = H.put h1 result Var in
      let h3 = H.put h2 p (Arrow (arg, result)) in
      let env1 = Bind (arg, Bind (p, env)) in
      let cs = constraints g in satisfies_def rho cs;
      let last = Equal (root body, result) in satisfies_def rho last;
      built_contents h3 env1 body after p ();
      generation_sound h3 env1 body after rho model ();
      let rest = Bind (p, env) in context_of_def rho rest; context_of_def rho env1; ()
    | GApp (f, a, p, arrow, h1, h2) ->
      let cs = constraints g in satisfies_def rho cs;
      let pair = And (constraints f, constraints a) in satisfies_def rho pair;
      let last = Equal (root f, arrow) in satisfies_def rho last;
      model arrow; equation_def after rho arrow;
      let mid2 : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h2 x) || equation h2 rho x}
          @ total = fun x -> model x; equation_def after rho x; equation_def h2 rho x;
        () in
      let mid1 : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h1 x) || equation h1 rho x}
          @ total = fun x -> 
        if H.mem h1 x then (
          built_frame h1 env a h2 x ();
          built_contents h1 env a h2 x (); mid2 x;
          equation_def h1 rho x; equation_def h2 rho x; ())
        else () in
      generation_sound h env f h1 rho mid1 ();
      generation_sound h1 env a h2 rho mid2 (); ())
