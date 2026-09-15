open Copy_spec
open Level_spec
open Generalize_spec
open Provenance_spec
open Hm_execution_spec
open Hm_runtime_spec

let rec (cleanup_path @ total) : (h : Pref.heap) @ immutable ->
    (after : Pref.heap) @ immutable -> (trail : pool) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable ->
      {u : unit | Copy_cleanup_spec.swept_at h after trail x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (path : path) @ immutable -> {u : unit | reaches h p q path} ->
    {u : unit | reaches after p q path} @ ghost = fun h after trail frame p q path premise -> ghost_ (
    let refine_ premise = premise in reaches_def h p q path; reaches_def after p q path;
    let u = () in match path with Stop -> refine_ u | Step (next, rest) ->
      frame p; Copy_cleanup_spec.swept_at_def h after trail p;
      edge_def h p next; edge_def after p next;
      cleanup_path h after trail frame next q rest (refine_ u); refine_ u)

let (clean_copy_origin @ total) : (saved : Pref.heap) @ immutable ->
    (h : Pref.heap) @ immutable -> (cut : int) ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid h epoch depth d && depth > cut} ->
    {o : origin | not (below (copy_heap h epoch depth d) x cut) ||
      originates saved (copy_heap h epoch depth d) cut x o} @ immutable ghost =
  fun saved h cut scope prior epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in copy_heap_def h epoch depth d;
    let raw = heap h epoch depth d in let after = copy_heap h epoch depth d in
    let trail = Pooled_spec.touched d in
    let frame : ((y : node Pref.t) @ immutable ->
      {u : unit | Copy_cleanup_spec.swept_at raw after trail y}) @ total = fun y ->
      let u = () in let refine_ u = Clean_copy.result_at h epoch depth d y (refine_ u) in refine_ u in
    let u = () in let refine_ o = Provenance_proofs.copy_origin saved h cut scope prior epoch depth d x (refine_ u) in
    frame x; Copy_cleanup_spec.swept_at_def raw after trail x;
    below_def raw x cut; below_def after x cut; at_level_def raw x; at_level_def after x;
    if below after x cut then (
      originates_def saved raw cut x o; originates_def saved after cut x o;
      match o with Origin (p, path) -> cleanup_path raw after trail frame p x path (refine_ u); refine_ o)
    else refine_ o)

let (allocated_origin @ total) : (saved : Pref.heap) @ immutable ->
    (h : Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (depth : int) -> (p : node Pref.t) @ immutable -> (desc : desc) @ immutable ->
    {u : unit | allocated h depth p desc && depth > cut && H.at h p === None} ->
    ((x : node Pref.t) @ immutable ->
      {o : origin | not (below (H.put h p (cell desc depth)) x cut) ||
        originates saved (H.put h p (cell desc depth)) cut x o} @ immutable) @ total ghost =
  fun saved h cut prior depth p desc premise -> ghost_ (
    let refine_ premise = premise in allocated_def h depth p desc; cell_def desc depth;
    let v = cell desc depth in
    let out : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below (H.put h p (cell desc depth)) x cut) || originates saved (H.put h p (cell desc depth)) cut x o} @ immutable) @ total = fun x ->
      let u = () in let refine_ o = Provenance_proofs.allocation_origin saved h cut prior p v x (refine_ u) in refine_ o in out)

open Hm_runtime_proofs
open Hm_let_runtime_proofs

let rec (run_origin @ total) : (saved : Pref.heap) @ immutable -> (cut : int) -> (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (e : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && cut < depth} ->
    {o : origin | not (below after x cut) || originates saved after cut x o} @ immutable ghost =
  fun saved cut h depth pool facts prior env e after final_pool x premise -> ghost_ (
    let refine_ premise = premise in ran_def h depth pool env e after final_pool; result_def e;
    let u = () in match e with
    | RVar (i, _, epoch, d) -> (match Hm_environment_spec.lookup env i with
      None -> let refine_ o = prior x in refine_ o | Some _ ->
      let scope : ((y : node Pref.t) @ immutable ->
        {u : unit | if H.mem h y then source_ok h y else H.at h y === None}) @ total = fun y ->
        facts y; runtime_at_def h depth pool y; safe_def h y; let u = () in refine_ u in
      let refine_ o = clean_copy_origin saved h cut scope prior epoch depth d x (refine_ u) in refine_ o)
    | RBool p -> facts x; let desc : desc = Bool in allocated_def h depth p desc;
      facts p; runtime_at_def h depth pool p; safe_def h p;
      let next = allocated_origin saved h cut prior depth p desc (refine_ u) in
      let refine_ o = next x in refine_ o
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let start = H.put h arg (cell var depth) in
      let next_pool = Entry (arg, pool) in let next_env = Hm_environment_spec.Bind (arg, env) in
      allocated_def h depth arg var;
      let start_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at start depth next_pool y}) @ total = fun y ->
        facts y; let u = () in let refine_ u = allocate_runtime h depth pool arg var y (refine_ u) in refine_ u in
      facts arg; runtime_at_def h depth pool arg; safe_def h arg;
      let start_prior = allocated_origin saved h cut prior depth arg var (refine_ u) in
      let middle_prior : ((y : node Pref.t) @ immutable ->
        {o : origin | not (below middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let u = () in let refine_ o = run_origin saved cut start depth next_pool start_facts (refine_ start_prior) next_env body middle body_pool y (refine_ u) in refine_ o in
      (match result body with None -> let refine_ o = middle_prior x in refine_ o
      | Some b -> match out with None -> let refine_ o = prior x in refine_ o
        | Some p -> let desc = Arrow (arg, b) in allocated_def middle depth p desc;
          run_invariant start depth next_pool start_facts next_env body middle body_pool p (refine_ u);
          safe_def middle p;
          let next = allocated_origin saved middle cut middle_prior depth p desc (refine_ u) in
          let refine_ o = next x in refine_ o)
    | RApp_left (left, _) -> let refine_ o = run_origin saved cut h depth pool facts prior env left after final_pool x (refine_ u) in refine_ o
    | RApp_right (left, right, middle, left_pool) ->
      let middle_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at middle depth left_pool y}) @ total = fun y ->
        let u = () in let refine_ u = run_invariant h depth pool facts env left middle left_pool y (refine_ u) in refine_ u in
      let middle_prior : ((y : node Pref.t) @ immutable ->
        {o : origin | not (below middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let u = () in let refine_ o = run_origin saved cut h depth pool facts prior env left middle left_pool y (refine_ u) in refine_ o in
      let refine_ o = run_origin saved cut middle depth left_pool middle_facts middle_prior env right after final_pool x (refine_ u) in refine_ o
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      let facts1 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h1 depth pool1 y}) @ total = fun y ->
        let u = () in let refine_ u = run_invariant h depth pool facts env left h1 pool1 y (refine_ u) in refine_ u in
      let prior1 : ((y : node Pref.t) @ immutable ->
        {o : origin | not (below h1 y cut) || originates saved h1 cut y o} @ immutable) @ total = fun y ->
        let u = () in let refine_ o = run_origin saved cut h depth pool facts prior env left h1 pool1 y (refine_ u) in refine_ o in
      let facts2 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h2 depth pool2 y}) @ total = fun y ->
        let u = () in let refine_ u = run_invariant h1 depth pool1 facts1 env right h2 pool2 y (refine_ u) in refine_ u in
      let prior2 : ((y : node Pref.t) @ immutable ->
        {o : origin | not (below h2 y cut) || originates saved h2 cut y o} @ immutable) @ total = fun y ->
        let u = () in let refine_ o = run_origin saved cut h1 depth pool1 facts1 (refine_ prior1) env right h2 pool2 y (refine_ u) in refine_ o in
      (match result left with None -> let refine_ o = prior x in refine_ o | Some f -> match result right with None -> let refine_ o = prior x in refine_ o | Some a ->
        let var : desc = Var in let h3 = H.put h2 p (cell var depth) in let pool3 = Entry (p, pool2) in
        let desc = Arrow (a, p) in let h4 = H.put h3 arrow (cell desc depth) in
        allocated_def h2 depth p var; allocated_def h3 depth arrow desc;
        let facts3 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h3 depth pool3 y}) @ total = fun y ->
          facts2 y; let u = () in let refine_ u = allocate_runtime h2 depth pool2 p var y (refine_ u) in refine_ u in
      facts2 p; runtime_at_def h2 depth pool2 p; safe_def h2 p;
      let prior3 = allocated_origin saved h2 cut (refine_ prior2) depth p var (refine_ u) in
      facts3 arrow; runtime_at_def h3 depth pool3 arrow; safe_def h3 arrow;
      let prior4 = allocated_origin saved h3 cut (refine_ prior3) depth arrow desc (refine_ u) in
        let refine_ o = Provenance_proofs.unified_origin saved h4 cut (refine_ prior4) f arrow ok after d x (refine_ u) in refine_ o)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in let pool1 = Entry (arg, pool) in
      let h2 = H.put h1 res v in let pool2 = Entry (res, pool1) in
      let desc = Arrow (arg, res) in let h3 = H.put h2 self (cell desc depth) in let pool3 = Entry (self, pool2) in
      allocated_def h depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
      let facts1 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h1 depth pool1 y}) @ total = fun y ->
        facts y; let u = () in let refine_ u = allocate_runtime h depth pool arg var y (refine_ u) in refine_ u in
      facts arg; runtime_at_def h depth pool arg; safe_def h arg;
      let prior1 = allocated_origin saved h cut prior depth arg var (refine_ u) in
      let facts2 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h2 depth pool2 y}) @ total = fun y ->
        facts1 y; let u = () in let refine_ u = allocate_runtime h1 depth pool1 res var y (refine_ u) in refine_ u in
      facts1 res; runtime_at_def h1 depth pool1 res; safe_def h1 res;
      let prior2 = allocated_origin saved h1 cut (refine_ prior1) depth res var (refine_ u) in
      let facts3 : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h3 depth pool3 y}) @ total = fun y ->
        facts2 y; let u = () in let refine_ u = allocate_runtime h2 depth pool2 self desc y (refine_ u) in refine_ u in
      facts2 self; runtime_at_def h2 depth pool2 self; safe_def h2 self;
      let prior3 = allocated_origin saved h2 cut (refine_ prior2) depth self desc (refine_ u) in
      let next_env = Hm_environment_spec.Bind (arg, Hm_environment_spec.Bind (self, env)) in
      (match result body with None ->
        let refine_ o = run_origin saved cut h3 depth pool3 facts3 (refine_ prior3) next_env body middle body_pool x (refine_ u) in refine_ o
      | Some b -> match finish with Aborted -> let refine_ o = prior x in refine_ o | Unified (ok, d) ->
      let middle_prior : ((y : node Pref.t) @ immutable ->
        {o : origin | not (below middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let u = () in let refine_ o = run_origin saved cut h3 depth pool3 facts3 (refine_ prior3) next_env body middle body_pool y (refine_ u) in refine_ o in
      let refine_ o = Provenance_proofs.unified_origin saved middle cut middle_prior b res ok after d x (refine_ u) in refine_ o)
    | RLet_left (rhs, _) ->
      let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
      ran_def h child_depth empty env rhs after final_pool;
      let child_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h child_depth empty y}) @ total = fun y ->
        facts y; let u = () in enter_runtime h depth pool y (refine_ u); refine_ u in
      let refine_ o = run_origin saved cut h child_depth empty child_facts prior env rhs after final_pool x (refine_ u) in refine_ o
    | RLet (rhs, body, middle, child_pool) ->
      let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
      ran_def h child_depth empty env rhs middle child_pool;
      let child_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h child_depth empty y}) @ total = fun y ->
        facts y; let u = () in enter_runtime h depth pool y (refine_ u); refine_ u in
      let middle_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at middle (depth + 1) child_pool y}) @ total = fun y ->
        let u = () in run_invariant h child_depth empty child_facts env rhs middle child_pool y (refine_ u); refine_ u in
      let middle_prior : ((y : node Pref.t) @ immutable ->
        {o : origin | not (below middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let u = () in let refine_ o = run_origin saved cut h child_depth empty child_facts prior env rhs middle child_pool y (refine_ u) in refine_ o in
      let closed = closed_heap middle depth child_pool in
      let transferred = Nested_pool_spec.transfer closed child_pool pool in
      let closed_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at closed depth transferred y}) @ total = fun y ->
        let u = () in close_runtime h depth pool facts env rhs middle child_pool middle_facts y (refine_ u); refine_ u in
      let closed_prior : ((y : node Pref.t) @ immutable ->
        {o : origin | not (below closed y cut) || originates saved closed cut y o} @ immutable) @ total = fun y ->
        let refine_ o = middle_prior y in let u = () in
        Provenance_proofs.closed_origin saved middle depth cut child_pool y o (refine_ u); refine_ o in
      (match result rhs with None -> let refine_ o = prior x in refine_ o | Some p -> let next_env = Hm_environment_spec.Bind (p, env) in
        let refine_ o = run_origin saved cut closed depth transferred closed_facts closed_prior next_env body after final_pool x (refine_ u) in refine_ o))

let (rhs_origin @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (rhs : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (child_pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | depth >= 0 && ran h (depth + 1) Generalize_spec.Empty env rhs after child_pool} ->
    {o : origin | not (below after x depth) || originates h after depth x o} @ immutable ghost =
  fun h depth pool facts env rhs after child_pool x premise -> ghost_ (
    let refine_ premise = premise in let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
    ran_def h child_depth empty env rhs after child_pool;
    let child_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h child_depth empty y}) @ total = fun y ->
      facts y; let u = () in enter_runtime h depth pool y (refine_ u); refine_ u in
    let prior : ((y : node Pref.t) @ immutable ->
      {o : origin | not (below h y depth) || originates h h depth y o} @ immutable) @ total = fun y ->
      let refine_ o = Provenance_proofs.initial_origin h depth y in refine_ o in
    let u = () in let refine_ o = run_origin h depth h child_depth empty child_facts prior env rhs after child_pool x (refine_ u) in refine_ o)

let (rhs_interpret @ total) : (h : Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable -> {u : unit | runtime_at h depth pool x})) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (rhs : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (child_pool : pool) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho_model : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho x})) @ total ->
    (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation after eta x})) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below h x depth) || rho x === eta x})) @ total ->
    (tree : bounded) @ immutable ->
    {u : unit | depth >= 0 && ran h (depth + 1) Generalize_spec.Empty env rhs after child_pool && unfolded after tree} ->
    {u : unit | interpret rho eta (Generalize_spec.scheme after depth tree) === eta (bound_root tree)} @ ghost =
  fun h depth pool facts env rhs after child_pool rho rho_model eta eta_model equal tree premise -> ghost_ (
    let refine_ premise = premise in let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
    ran_def h child_depth empty env rhs after child_pool;
    let child_facts : ((y : node Pref.t) @ immutable -> {u : unit | runtime_at h child_depth empty y}) @ total = fun y ->
      facts y; let u = () in enter_runtime h depth pool y (refine_ u); refine_ u in
    let prior : ((y : node Pref.t) @ immutable ->
      {o : origin | not (below after y depth) || originates h after depth y o} @ immutable) @ total = fun y ->
      let u = () in let refine_ o = rhs_origin h depth pool facts env rhs after child_pool y (refine_ u) in refine_ o in
    let order : ((y : node Pref.t) @ immutable -> {u : unit | ordered after y}) @ total = fun y ->
      let u = () in run_invariant h child_depth empty child_facts env rhs after child_pool y (refine_ u);
      safe_def after y; refine_ u in
    let u = () in Relative_generalization.relative_interpret h after depth prior order rho rho_model eta eta_model equal tree (refine_ u); refine_ u)
