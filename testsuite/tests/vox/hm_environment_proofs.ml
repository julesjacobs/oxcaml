open Copy_spec
open Level_spec
open Hm_environment_spec
module D = Hm_declarative

let rec (interpret_boundary_agreement @ total) : (s : template) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (boundary_member s x) || rho x === tau x})) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    {u : unit | interpret rho choices s === interpret tau choices s} @ ghost =
  fun s rho tau equal choices -> ghost_ (
    interpret_def rho choices s; interpret_def tau choices s;
    (match s with
    | Boundary p -> boundary_member_def s p; equal p; ()
    | Parameter _ | Constant _ -> ()
    | Product (_, a, b) ->
      let left : ((x : node Pref.t) @ immutable ->
        {u : unit | not (boundary_member a x) || rho x === tau x}) @ total = fun x ->
        boundary_member_def s x; equal x; let u = () in u in
      let right : ((x : node Pref.t) @ immutable ->
        {u : unit | not (boundary_member b x) || rho x === tau x}) @ total = fun x ->
        boundary_member_def s x; equal x; let u = () in u in
      interpret_boundary_agreement a rho tau left choices;
      interpret_boundary_agreement b rho tau right choices; ()
    | Indirect (_, child) ->
      let next : ((x : node Pref.t) @ immutable ->
        {u : unit | not (boundary_member child x) || rho x === tau x}) @ total = fun x ->
        boundary_member_def s x; equal x; let u = () in u in
      interpret_boundary_agreement child rho tau next choices; ());
    let u = () in u)

let rec (boundary_below @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (s : template) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | boundary_bound h depth s && boundary_member s p} ->
    {u : unit | below h p depth} @ ghost = fun h depth s p premise -> ghost_ (
    boundary_bound_def h depth s;
    boundary_member_def s p; let u = () in match s with
    | Boundary _ | Parameter _ | Constant _ -> u
    | Product (_, a, b) -> if boundary_member a p then
      (boundary_below h depth a p (u); u)
      else (boundary_below h depth b p (u); u)
    | Indirect (_, child) -> boundary_below h depth child p (u); u)

let rec (aligned_lookup @ total) : (g : D.context) @ immutable ->
    (ts : templates) @ immutable -> (i : D.index) @ immutable ->
    {u : unit | aligned g ts} ->
    {u : unit | match D.lookup g i, template_lookup ts i with
      None, None | Some _, Some _ -> true | _ -> false} @ ghost =
  fun g ts i premise -> ghost_ (
    aligned_def g ts;
    D.lookup_def g i; template_lookup_def ts i;
    let u = () in match g with
    | D.Empty_context -> u
    | D.Binding (_, rest) -> match ts with No_templates -> u
      | Template_binding (_, tail) -> match i with D.Z -> u
        | D.S i -> aligned_lookup rest tail i (u); u)

let rec (env_lookup @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (env : env) @ immutable -> (ts : templates) @ immutable ->
    (i : D.index) @ immutable -> (s : template) @ immutable ->
    {u : unit | env_at h depth env ts && template_lookup ts i === Some s} ->
    {u : unit | lookup env i === Some (root s)
      && template h s && boundary_bound h depth s} @ ghost =
  fun h depth env ts i s premise -> ghost_ (
    env_at_def h depth env ts;
    lookup_def env i; template_lookup_def ts i;
    let u = () in match env with
    | Empty -> u
    | Bind (_, rest) -> match ts with No_templates -> u
      | Template_binding (_, tail) -> match i with D.Z -> u
        | D.S i -> env_lookup h depth rest tail i s (u); u)

let rec (template_transport @ total) : (h : Pref.heap) @ immutable ->
    (after : Pref.heap) @ immutable -> (depth : int) ->
    (frame : ((p : node Pref.t) @ immutable ->
      {u : unit | protected_at h after depth p})) @ total ->
    (s : template) @ immutable ->
    {u : unit | template h s && boundary_bound h depth s} ->
    {u : unit | template after s && boundary_bound after depth s} @ ghost =
  fun h after depth frame s premise -> ghost_ (
    template_def h s; template_def after s;
    boundary_bound_def h depth s; boundary_bound_def after depth s;
    root_def s; let p = root s in frame p; protected_at_def h after depth p;
    let desc = head_desc s in head_desc_def s;
    generic_desc_def h p desc; generic_desc_def after p desc;
    finite_node_def after p; below_def after p depth; at_level_def after p;
    let u = () in match s with
    | Boundary _ | Parameter _ | Constant _ -> u
    | Product (_, a, b) -> template_transport h after depth frame a (u);
      template_transport h after depth frame b (u); u
    | Indirect (_, child) -> template_transport h after depth frame child (u); u)

let rec (env_transport @ total) : (h : Pref.heap) @ immutable ->
    (after : Pref.heap) @ immutable -> (depth : int) ->
    (frame : ((p : node Pref.t) @ immutable ->
      {u : unit | protected_at h after depth p})) @ total ->
    (env : env) @ immutable -> (ts : templates) @ immutable ->
    {u : unit | env_at h depth env ts} ->
    {u : unit | env_at after depth env ts} @ ghost =
  fun h after depth frame env ts premise -> ghost_ (
    env_at_def h depth env ts;
    env_at_def after depth env ts; let u = () in match env with
    | Empty -> u
    | Bind (_, rest) -> match ts with No_templates -> u
      | Template_binding (s, tail) -> template_transport h after depth frame s (u);
        env_transport h after depth frame rest tail (u); u)

let (allocation_protected @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable -> (depth : int) ->
    (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h p)
      && (if H.mem h x then source_ok h x else H.at h x === None)} ->
    {u : unit | protected_at h (H.put h p v) depth x} @ ghost =
  fun h p v depth x premise -> ghost_ (
    let after = H.put h p v in
    Copy_heap_proofs.put_frame h p v x; protected_at_def h after depth x;
    below_def h x depth; below_def after x depth;
    at_level_def h x; at_level_def after x;
    let u = () in u)

let (copy_protected @ total) : (h : Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (protected_depth : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | valid h epoch depth d
      && (if H.mem h x then source_ok h x else H.at h x === None)} ->
    {u : unit | protected_at h (heap h epoch depth d) protected_depth x} @ ghost =
  fun h epoch depth d protected_depth x premise -> ghost_ (
    let after = heap h epoch depth d in
    let u = () in Copy_heap_proofs.history_at h epoch depth d x (u);
    protected_at_def h after protected_depth x;
    below_def h x protected_depth; below_def after x protected_depth;
    at_level_def h x; at_level_def after x; u)

let (close_protected @ total) : (h : Pref.heap) @ immutable ->
    (cut : int) -> (pool : Generalize_spec.pool) @ immutable ->
    (depth : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | depth <= cut && Generalize_spec.pool_scoped h pool} ->
    {u : unit | protected_at h (Generalize_spec.closed_heap h cut pool) depth x} @ ghost =
  fun h cut pool depth x premise -> ghost_ (
    let after = Generalize_spec.closed_heap h cut pool in
    let u = () in Generalize_proofs.closed_observe h cut pool x (u);
    Generalize_spec.closed_at_def h after cut pool x;
    protected_at_def h after depth x;
    below_def h x depth; below_def after x depth;
    at_level_def h x; at_level_def after x;
    (match H.at h x with None -> () | Some v -> Generalize_spec.close_level_def cut v.level; ());
    u)

let (unify_protected @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : Pref.heap) @ immutable ->
    (d : Optimized_unifier_spec.derivation) @ immutable -> (depth : int) ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Optimized_unifier_spec.unified h p q ok after d} ->
    {u : unit | protected_at h after depth x} @ ghost =
  fun h p q ok after d depth x premise -> ghost_ (
    let u = () in
    Optimized_metadata.unified_frame h p q ok after d x (u);
    Optimized_metadata.unified_scratch h p q ok after d x (u);
    Level_unifier_metadata.scratch_frame_def h after x;
    protected_at_def h after depth x;
    below_def h x depth; below_def after x depth;
    at_level_def h x; at_level_def after x;
    (match H.at h x, H.at after x with
    | Some v, Some w -> decreases_def v.level w.level; () | _ -> ()); u)

let (protected_trans @ total) : (h : Pref.heap) @ immutable ->
    (middle : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (depth : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | protected_at h middle depth x && protected_at middle after depth x} ->
    {u : unit | protected_at h after depth x} @ ghost =
  fun h middle after depth x premise -> ghost_ (
    protected_at_def h middle depth x;
    protected_at_def middle after depth x; protected_at_def h after depth x;
    let u = () in u)

module T = Hm_type_proofs

let (realize_empty @ total) :
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (xi : (D.index @ immutable total -> ty @ immutable total)) @ total ->
    (i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
    (schema : template) @ immutable -> (args : T.values) @ immutable ->
    {u : unit | D.lookup D.Empty_context i === Some sigma && template_lookup No_templates i === Some schema
      && T.values_length args === D.arity sigma} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun rho xi i sigma schema args premise claim use -> ghost_ (
    let g = D.Empty_context in
    D.lookup_def g i; let u = () in u)

let rec (eval_empty @ total) : (args : T.values) @ immutable ->
    (xi : (D.index @ immutable total -> ty @ immutable total)) @ total -> (a : D.mono) @ immutable ->
    {u : unit | args === T.No_values} ->
    {u : unit | T.eval_prefixed args xi a === T.eval xi a} @ ghost =
  fun args xi a premise -> ghost_ (
    T.eval_prefixed_def args xi a; T.eval_def xi a;
    let u = () in match a with
    | D.Parameter i -> T.prefix_def args xi i; u
    | D.Free _ | D.Boolean -> u
    | D.Function (a, b) -> eval_empty args xi a (u);
      eval_empty args xi b (u); u)

let (realize_monomorphic @ total) :
    (g : D.context) @ immutable -> (ts : templates) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (xi : (D.index @ immutable total -> ty @ immutable total)) @ total ->
    (realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
    (schema : template) @ immutable -> (args : T.values) @ immutable ->
    {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
      && T.values_length args === D.arity sigma} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
      {u : unit | claim})) @ total -> {u : unit | claim})) @ total ->
    (p : node Pref.t) @ immutable -> (a : D.mono) @ immutable ->
    {u : unit | rho p === T.eval xi a} ->
    (i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
    (schema : template) @ immutable -> (args : T.values) @ immutable ->
    {u : unit | D.lookup (D.Binding (D.Forall (D.Z, a), g)) i === Some sigma && template_lookup (Template_binding (Boundary p, ts)) i === Some schema
      && T.values_length args === D.arity sigma} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun g ts rho xi realize p a fit i sigma schema args premise claim use -> ghost_ (
    let z = D.Z in let mono = D.Forall (z, a) in
    let next = D.Binding (mono, g) in let bound = Boundary p in
    let next_ts = Template_binding (bound, ts) in
    D.lookup_def next i; template_lookup_def next_ts i;
    let u = () in match i with
    | D.Z -> D.arity_def mono; T.values_length_def args;
      (match args with
      | T.No_values ->
        T.meaning_def xi mono args; eval_empty args xi a (u);
        interpret_def rho rho bound;
        let u = use rho (u) in u
      | T.Value _ -> u)
    | D.S i -> let u = realize i sigma schema args (u) claim use in u)

let (realize_weaken @ total) :
    (g : D.context) @ immutable -> (ts : templates) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (xi : (D.index @ immutable total -> ty @ immutable total)) @ total ->
    (realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
    (schema : template) @ immutable -> (args : T.values) @ immutable ->
    {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
      && T.values_length args === D.arity sigma} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
      {u : unit | claim})) @ total -> {u : unit | claim})) @ total ->
    (ambient : T.values) @ immutable -> (zeta : (D.index @ immutable total -> ty @ immutable total)) @ total ->
    (equal : ((j : D.index) @ immutable ->
      {u : unit | zeta j === T.prefix ambient xi j})) @ total ->
    (i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
    (schema : template) @ immutable -> (args : T.values) @ immutable ->
    {u : unit | D.lookup (D.weaken_context (T.values_length ambient) g) i === Some sigma && template_lookup ts i === Some schema
      && T.values_length args === D.arity sigma} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices schema === T.meaning zeta sigma args} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun g ts rho xi realize ambient zeta equal i sigma schema args premise claim use -> ghost_ (
    let k = T.values_length ambient in
    T.lookup_weaken k g i; let u = () in match D.lookup g i with
    | None -> u
    | Some original ->
      D.weaken_scheme_def k original;
      D.arity_def sigma; D.arity_def original;
      let forward : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho choices schema === T.meaning xi original args} ->
        {u : unit | claim}) @ total = fun choices fit ->
        let u = () in
        T.meaning_weaken args ambient xi zeta equal original (u);
        let u = use choices (u) in u in
      let u = realize i original schema args (u) claim forward in u)

let rec (lookup_boundary @ total) : (ts : templates) @ immutable ->
    (i : D.index) @ immutable -> (s : template) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | template_lookup ts i === Some s && boundary_member s p} ->
    {u : unit | environment_boundary ts p} @ ghost = fun ts i s p premise -> ghost_ (
    template_lookup_def ts i; environment_boundary_def ts p;
    let u = () in match ts with No_templates -> u
    | Template_binding (_, rest) -> match i with D.Z -> u
      | D.S i -> lookup_boundary rest i s p (u); u)

let rec (environment_boundary_owned @ total) : (h : Pref.heap) @ immutable ->
    (depth : int) -> (env : env) @ immutable -> (ts : templates) @ immutable ->
    (p : node Pref.t) @ immutable ->
    {u : unit | env_at h depth env ts && environment_boundary ts p} ->
    {u : unit | H.mem h p} @ ghost = fun h depth env ts p premise -> ghost_ (
    env_at_def h depth env ts; environment_boundary_def ts p;
    let u = () in match env with Empty -> u
    | Bind (_, rest) -> match ts with No_templates -> u
      | Template_binding (s, tail) -> if boundary_member s p then
        (boundary_below h depth s p (u); below_def h p depth; u)
        else (environment_boundary_owned h depth rest tail p (u); u))

let (realize_transport @ total) :
    (g : D.context) @ immutable -> (ts : templates) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (xi : (D.index @ immutable total -> ty @ immutable total)) @ total ->
    (realize : ((i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
    (schema : template) @ immutable -> (args : T.values) @ immutable ->
    {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
      && T.values_length args === D.arity sigma} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
      {u : unit | claim})) @ total -> {u : unit | claim})) @ total ->
    (equal : ((p : node Pref.t) @ immutable ->
      {u : unit | not (environment_boundary ts p) || rho p === tau p})) @ total ->
    (i : D.index) @ immutable -> (sigma : D.scheme) @ immutable ->
    (schema : template) @ immutable -> (args : T.values) @ immutable ->
    {u : unit | D.lookup g i === Some sigma && template_lookup ts i === Some schema
      && T.values_length args === D.arity sigma} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | interpret tau choices schema === T.meaning xi sigma args} ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun g ts rho tau xi realize equal i sigma schema args premise claim use -> ghost_ (
    let boundaries : ((p : node Pref.t) @ immutable ->
        {u : unit | not (boundary_member schema p) || rho p === tau p}) @ total = fun p ->
      if boundary_member schema p then (
        let u = () in lookup_boundary ts i schema p (u);
        equal p; u) else let u = () in u in
    let forward : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | interpret rho choices schema === T.meaning xi sigma args} ->
        {u : unit | claim}) @ total = fun choices fit ->
      interpret_boundary_agreement schema rho tau boundaries choices;
      let u = () in let u = use choices (u) in u in
    let u = () in let u = realize i sigma schema args (u) claim forward in u)

let (cleanup_protected @ total) : (h : Pref.heap) @ immutable ->
    (after : Pref.heap) @ immutable -> (trail : Generalize_spec.pool) @ immutable ->
    (depth : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | Copy_cleanup_spec.swept_at h after trail x} ->
    {u : unit | protected_at h after depth x} @ ghost =
  fun h after trail depth x premise -> ghost_ (
    Copy_cleanup_spec.swept_at_def h after trail x;
    protected_at_def h after depth x; below_def h x depth; below_def after x depth;
    at_level_def h x; at_level_def after x; let u = () in u)

let (active_template @ total) : (h : Pref.heap) @ immutable ->
    (schema : template) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | template h schema && root schema === p && active h p} ->
    {u : unit | schema === Boundary p} @ ghost = fun h schema p premise -> ghost_ (
      let u = () in
      Copy_heap_proofs.template_head h schema p (u);
      head_generic_def schema; root_def schema; active_def h p; at_level_def h p;
      u)
