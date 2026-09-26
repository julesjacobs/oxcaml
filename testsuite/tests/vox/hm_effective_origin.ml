open Copy_spec
open Level_spec
open Level_finite_spec
open Generalize_spec
open Provenance_spec
open Leaf_provenance_spec
open Hm_effective_execution_spec
module C = Copy_certificate_spec
module F = Hm_effective_forest
let rec (cleanup_path @ total) : (h : node Pref.heap) @ immutable ->
    (after : node Pref.heap) @ immutable -> (trail : pool) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable ->
      {u : unit | Copy_cleanup_spec.swept_at h after trail x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (path : path) @ immutable -> {u : unit | reaches h p q path} ->
    {u : unit | reaches after p q path} @ ghost = fun h after trail frame p q path premise -> ghost_ (
    reaches_def h p q path; reaches_def after p q path;
    match path with Stop -> () | Step (next, rest) ->
      frame p; Copy_cleanup_spec.swept_at_def h after trail p;
      edge_def h p next; edge_def after p next;
      cleanup_path h after trail frame next q rest (); ())


let rec (history_absent @ total) : (h : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h x || H.at h x === None} ->
    {u : unit | H.mem (heap h epoch depth d) x || H.at (heap h epoch depth d) x === None} @ ghost =
  fun h epoch depth d x premise -> ghost_ (
    heap_def h epoch depth d; match d with
    | Clean -> ()
    | Start -> let v = cell Bool depth in Copy_heap_proofs.put_frame h epoch v x; ()
    | Fresh (rest, p, q, old, desc) ->
      history_absent h epoch depth rest x ();
      let mid = heap h epoch depth rest in let v = cell desc depth in
      let next = H.put mid q v in let w = session_mark rest old epoch q in
      Copy_heap_proofs.put_frame next p w x; ()
    | Alias (rest, p, q, old) ->
      history_absent h epoch depth rest x ();
      let mid = heap h epoch depth rest in let w = session_mark rest old epoch q in
      Copy_heap_proofs.put_frame mid p w x; ())
let rec (copy_leaf_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable ->
    (certificate : Representative_certificate.certificate) @ immutable -> (cut : int) ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | H.mem h x || H.at h x === None})) @ total ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o}
      @ immutable)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | C.certified_valid h certificate epoch depth d && depth > cut} ->
    {o : origin | not (low_var (heap h epoch depth d) x cut) ||
      originates saved (heap h epoch depth d) cut x o} @ immutable ghost =
  fun saved h certificate cut scope prior epoch depth d x premise -> ghost_ (
    C.certified_valid_def h certificate epoch depth d;
    heap_def h epoch depth d; match d with
    | Clean -> let refine_ o = prior x in refine_ o
    | Start -> scope epoch; let desc : desc = Bool in
      let v = cell desc depth in cell_def desc depth;
      let refine_ o = Leaf_provenance_proofs.allocation_leaf_origin saved h cut prior epoch v x () in refine_ o
    | Fresh (rest, p, q, old, desc) -> let mid = heap h epoch depth rest in
      let prior1 : ((x : node Pref.t) @ immutable ->
        {o : origin | not (low_var mid x cut) || originates saved mid cut x o}
        @ immutable) @ total = fun x -> let refine_ o = copy_leaf_origin saved h certificate cut scope prior epoch depth rest x () in refine_ o in
      scope q; history_absent h epoch depth rest q ();
      let v = cell desc depth in cell_def desc depth;
      let refine_ o = Leaf_provenance_proofs.allocation_leaf_origin saved mid cut prior1 q v x () in
      let h1 = H.put mid q v in
      Hm_effective_registration.history_at h certificate epoch depth rest p (); Leaf_provenance_proofs.mark_leaf_origin rest saved h1 cut p old epoch q x o (); refine_ o
    | Alias (rest, p, q, old) -> let mid = heap h epoch depth rest in
      let refine_ o = copy_leaf_origin saved h certificate cut scope prior epoch depth rest x () in
      Hm_effective_registration.history_at h certificate epoch depth rest p ();
      Leaf_provenance_proofs.mark_leaf_origin rest saved mid cut p old epoch q x o (); refine_ o)


let (clean_copy_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (certificate : Representative_certificate.certificate) @ immutable -> (cut : int) ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | H.mem h x || H.at h x === None})) @ total ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | C.certified_valid h certificate epoch depth d && depth > cut} ->
    {o : origin | not (low_var (copy_heap h epoch depth d) x cut) ||
      originates saved (copy_heap h epoch depth d) cut x o} @ immutable ghost =
  fun saved h certificate cut scope prior epoch depth d x premise -> ghost_ (
    copy_heap_def h epoch depth d;
    let raw = heap h epoch depth d in let after = copy_heap h epoch depth d in
    let trail = Pooled_spec.touched d in
    let frame : ((y : node Pref.t) @ immutable ->
      {u : unit | Copy_cleanup_spec.swept_at raw after trail y}) @ total = fun y ->
      let () = Hm_effective_registration.result_at h certificate epoch depth d y () in () in
    let refine_ o = copy_leaf_origin saved h certificate cut scope prior epoch depth d x () in
    frame x; Copy_cleanup_spec.swept_at_def raw after trail x;
    below_def raw x cut; below_def after x cut;
    low_var_def raw x cut; low_var_def after x cut;
    Level_unifier_spec.observe_def raw x; Level_unifier_spec.observe_def after x; at_level_def raw x; at_level_def after x;
    if low_var after x cut then (
      originates_def saved raw cut x o; originates_def saved after cut x o;
      match o with Origin (p, path) -> cleanup_path raw after trail frame p x path (); refine_ o)
    else refine_ o)

let (allocated_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (depth : int) -> (p : node Pref.t) @ immutable -> (desc : desc) @ immutable ->
    {u : unit | allocated h depth p desc && depth > cut && H.at h p === None} ->
    ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var (H.put h p (cell desc depth)) x cut) ||
        originates saved (H.put h p (cell desc depth)) cut x o} @ immutable) @ total ghost =
  fun saved h cut prior depth p desc premise -> ghost_ (
    allocated_def h depth p desc; cell_def desc depth;
    let v = cell desc depth in
    let out : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var (H.put h p (cell desc depth)) x cut) || originates saved (H.put h p (cell desc depth)) cut x o} @ immutable) @ total = fun x ->
      let refine_ o = Leaf_provenance_proofs.allocation_leaf_origin saved h cut prior p v x () in refine_ o in out)

let rec (run_origin @ total) : (saved : node Pref.heap) @ immutable -> (cut : int) -> (h : node Pref.heap) @ immutable -> (depth : int) ->
    (pool : pool) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else Level_unifier_spec.observe h x === None)} @ immutable)) @ total ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o} @ immutable)) @ total ->
    (env : Hm_environment_spec.env) @ immutable -> (e : execution) @ immutable ->
    (after : node Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && cut < depth} ->
    {o : origin | not (low_var after x cut) || originates saved after cut x o} @ immutable ghost =
  fun saved cut h depth pool trees prior env e after final_pool x premise -> ghost_ (
    ran_def h depth pool env e after final_pool; result_def e;
    match e with
    | RShared _ -> let refine_ o = prior x in refine_ o
    | RVar (i, target, epoch, d, certificate) -> (match Hm_environment_spec.lookup env i with
      None -> let refine_ o = prior x in refine_ o | Some original ->
      C.certifies_def h certificate epoch depth d original target;
      let scope : ((y : node Pref.t) @ immutable ->
        {u : unit | H.mem h y || H.at h y === None}) @ total = fun y ->
        let refine_ _tree = trees y in Level_unifier_spec.observe_def h y; () in
      let refine_ o = clean_copy_origin saved h certificate cut scope prior epoch depth d x () in refine_ o)
    | RBool p | RFalse p -> let desc : desc = Bool in allocated_def h depth p desc;
      let refine_ _tree = trees p in Level_unifier_spec.observe_def h p;
      let next = allocated_origin saved h cut prior depth p desc () in
      let refine_ o = next x in refine_ o
    | RWord (_, p) -> let desc : desc = Word in allocated_def h depth p desc;
      let refine_ _tree = trees p in Level_unifier_spec.observe_def h p;
      let next = allocated_origin saved h cut prior depth p desc () in
      let refine_ o = next x in refine_ o
    | RNil (arg, p) ->
      let var = Var in let middle = H.put h arg (cell var depth) in
      allocated_def h depth arg var;
      let refine_ _tree = trees arg in Level_unifier_spec.observe_def h arg;
      let prior1 = allocated_origin saved h cut prior depth arg var () in
      let trees1 = F.allocated_forest h trees depth arg var () in
      let desc = List arg in allocated_def middle depth p desc;
      let refine_ _tree = trees1 p in Level_unifier_spec.observe_def middle p;
      let prior2 = allocated_origin saved middle cut (refine_ prior1) depth p desc () in
      let refine_ o = prior2 x in refine_ o
    | RCaseList (_, _, _, body) ->
      let refine_ o = run_origin saved cut h depth pool trees prior env body after final_pool x () in refine_ o
    | RIf (_, _, _, body) ->
      let refine_ o = run_origin saved cut h depth pool trees prior env body after final_pool x () in refine_ o
    | RPrimitive (op, _, _, body, middle, body_pool, out) ->
      let prior1 : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let refine_ o = run_origin saved cut h depth pool trees prior env body middle body_pool y () in refine_ o in
      (match result body with None -> let refine_ o = prior1 x in refine_ o
      | Some _ -> match out with None -> unreachable_ () | Some p ->
        let desc = primitive_desc op in allocated_def middle depth p desc;
        let refine_ _tree = F.run_forest h trees depth pool env body middle body_pool p () in
        Level_unifier_spec.observe_def middle p;
        let next = allocated_origin saved middle cut prior1 depth p desc () in let refine_ o = next x in refine_ o)
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let start = H.put h arg (cell var depth) in
      let next_pool = Entry (arg, pool) in let next_env = Hm_environment_spec.Bind (arg, env) in
      allocated_def h depth arg var;
      let start_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem start y then finite start t else Level_unifier_spec.observe start y === None)} @ immutable) @ total = fun y ->
        let out = F.allocated_forest h trees depth arg var () in let refine_ t = out y in refine_ t in
      let refine_ _tree = trees arg in Level_unifier_spec.observe_def h arg;
      let start_prior = allocated_origin saved h cut prior depth arg var () in
      let middle_prior : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let refine_ o = run_origin saved cut start depth next_pool start_trees (refine_ start_prior) next_env body middle body_pool y () in refine_ o in
      (match result body with None -> let refine_ o = middle_prior x in refine_ o
      | Some b -> match out with None -> let refine_ o = prior x in refine_ o
        | Some p -> let desc = Arrow (arg, b) in allocated_def middle depth p desc;
          let refine_ _tree = F.run_forest start start_trees depth next_pool next_env body middle body_pool p () in
          Level_unifier_spec.observe_def middle p;
          let next = allocated_origin saved middle cut middle_prior depth p desc () in
          let refine_ o = next x in refine_ o)
    | RApp_left (left, _) | RCons_left (left, _) -> let refine_ o = run_origin saved cut h depth pool trees prior env left after final_pool x () in refine_ o
    | RApp_right (left, right, middle, left_pool) | RCons_right (left, right, middle, left_pool) ->
      let middle_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else Level_unifier_spec.observe middle y === None)} @ immutable) @ total = fun y ->
        let refine_ t = F.run_forest h trees depth pool env left middle left_pool y () in refine_ t in
      let middle_prior : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let refine_ o = run_origin saved cut h depth pool trees prior env left middle left_pool y () in refine_ o in
      let refine_ o = run_origin saved cut middle depth left_pool middle_trees middle_prior env right after final_pool x () in refine_ o
    | RCons (left, right, h1, pool1, h2, pool2, p, ok, d) ->
      let trees1 : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h1 y then finite h1 t else Level_unifier_spec.observe h1 y === None)} @ immutable) @ total = fun y ->
        let refine_ t = F.run_forest h trees depth pool env left h1 pool1 y () in refine_ t in
      let prior1 : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var h1 y cut) || originates saved h1 cut y o} @ immutable) @ total = fun y ->
        let refine_ o = run_origin saved cut h depth pool trees prior env left h1 pool1 y () in refine_ o in
      let trees2 : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h2 y then finite h2 t else Level_unifier_spec.observe h2 y === None)} @ immutable) @ total = fun y ->
        let refine_ t = F.run_forest h1 trees1 depth pool1 env right h2 pool2 y () in refine_ t in
      let prior2 : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var h2 y cut) || originates saved h2 cut y o} @ immutable) @ total = fun y ->
        let refine_ o = run_origin saved cut h1 depth pool1 trees1 (refine_ prior1) env right h2 pool2 y () in refine_ o in
      (match result left with None -> let refine_ o = prior x in refine_ o | Some f -> match result right with None -> let refine_ o = prior x in refine_ o | Some a ->
        let desc = List f in let h3 = H.put h2 p (cell desc depth) in
        allocated_def h2 depth p desc;
        let refine_ _tree = trees2 p in Level_unifier_spec.observe_def h2 p;
        let prior3 = allocated_origin saved h2 cut (refine_ prior2) depth p desc () in
        let refine_ o = Effective_unifier_origin.unified_leaf_origin saved h3 cut (refine_ prior3) a p ok after d x () in refine_ o)
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      let trees1 : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h1 y then finite h1 t else Level_unifier_spec.observe h1 y === None)} @ immutable) @ total = fun y ->
        let refine_ t = F.run_forest h trees depth pool env left h1 pool1 y () in refine_ t in
      let prior1 : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var h1 y cut) || originates saved h1 cut y o} @ immutable) @ total = fun y ->
        let refine_ o = run_origin saved cut h depth pool trees prior env left h1 pool1 y () in refine_ o in
      let trees2 : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h2 y then finite h2 t else Level_unifier_spec.observe h2 y === None)} @ immutable) @ total = fun y ->
        let refine_ t = F.run_forest h1 trees1 depth pool1 env right h2 pool2 y () in refine_ t in
      let prior2 : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var h2 y cut) || originates saved h2 cut y o} @ immutable) @ total = fun y ->
        let refine_ o = run_origin saved cut h1 depth pool1 trees1 (refine_ prior1) env right h2 pool2 y () in refine_ o in
      (match result left with None -> let refine_ o = prior x in refine_ o | Some f -> match result right with None -> let refine_ o = prior x in refine_ o | Some a ->
        let var : desc = Var in let h3 = H.put h2 p (cell var depth) in
        let desc = Arrow (a, p) in let h4 = H.put h3 arrow (cell desc depth) in
        allocated_def h2 depth p var; allocated_def h3 depth arrow desc;
        let trees3 : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h3 y then finite h3 t else Level_unifier_spec.observe h3 y === None)} @ immutable) @ total = fun y ->
        let out = F.allocated_forest h2 trees2 depth p var () in let refine_ t = out y in refine_ t in
      let refine_ _tree = trees2 p in Level_unifier_spec.observe_def h2 p;
      let prior3 = allocated_origin saved h2 cut (refine_ prior2) depth p var () in
      let refine_ _tree = trees3 arrow in Level_unifier_spec.observe_def h3 arrow;
      let prior4 = allocated_origin saved h3 cut (refine_ prior3) depth arrow desc () in
        let refine_ o = Effective_unifier_origin.unified_leaf_origin saved h4 cut (refine_ prior4) f arrow ok after d x () in refine_ o)
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in let pool1 = Entry (arg, pool) in
      let h2 = H.put h1 res v in let pool2 = Entry (res, pool1) in
      let desc = Arrow (arg, res) in let h3 = H.put h2 self (cell desc depth) in let pool3 = Entry (self, pool2) in
      allocated_def h depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
      let trees1 : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h1 y then finite h1 t else Level_unifier_spec.observe h1 y === None)} @ immutable) @ total = fun y ->
        let out = F.allocated_forest h trees depth arg var () in let refine_ t = out y in refine_ t in
      let refine_ _tree = trees arg in Level_unifier_spec.observe_def h arg;
      let prior1 = allocated_origin saved h cut prior depth arg var () in
      let trees2 : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h2 y then finite h2 t else Level_unifier_spec.observe h2 y === None)} @ immutable) @ total = fun y ->
        let out = F.allocated_forest h1 trees1 depth res var () in let refine_ t = out y in refine_ t in
      let refine_ _tree = trees1 res in Level_unifier_spec.observe_def h1 res;
      let prior2 = allocated_origin saved h1 cut (refine_ prior1) depth res var () in
      let trees3 : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem h3 y then finite h3 t else Level_unifier_spec.observe h3 y === None)} @ immutable) @ total = fun y ->
        let out = F.allocated_forest h2 trees2 depth self desc () in let refine_ t = out y in refine_ t in
      let refine_ _tree = trees2 self in Level_unifier_spec.observe_def h2 self;
      let prior3 = allocated_origin saved h2 cut (refine_ prior2) depth self desc () in
      let next_env = Hm_environment_spec.Bind (arg, Hm_environment_spec.Bind (self, env)) in
      (match result body with None ->
        let refine_ o = run_origin saved cut h3 depth pool3 trees3 (refine_ prior3) next_env body middle body_pool x () in refine_ o
      | Some b -> match finish with Aborted -> let refine_ o = prior x in refine_ o | Unified (ok, d) ->
      let middle_prior : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let refine_ o = run_origin saved cut h3 depth pool3 trees3 (refine_ prior3) next_env body middle body_pool y () in refine_ o in
      let refine_ o = Effective_unifier_origin.unified_leaf_origin saved middle cut middle_prior b res ok after d x () in refine_ o)
    | RLet_left (rhs, _) ->
      let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
      ran_def h child_depth empty env rhs after final_pool;
            let refine_ o = run_origin saved cut h child_depth empty trees prior env rhs after final_pool x () in refine_ o
    | RLet (rhs, body, middle, child_pool) ->
      let child_depth = depth + 1 in let empty : pool = Generalize_spec.Empty in
      ran_def h child_depth empty env rhs middle child_pool;
            let middle_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem middle y then finite middle t else Level_unifier_spec.observe middle y === None)} @ immutable) @ total = fun y ->
        let refine_ t = F.run_forest h trees child_depth empty env rhs middle child_pool y () in refine_ t in
      let middle_prior : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var middle y cut) || originates saved middle cut y o} @ immutable) @ total = fun y ->
        let refine_ o = run_origin saved cut h child_depth empty trees prior env rhs middle child_pool y () in refine_ o in
      let closed = Representative_pool_spec.close_heap middle depth child_pool in
      let transferred = Representative_pool_spec.transfer_rep closed child_pool pool in
      Representative_pool_spec.close_heap_def middle depth child_pool;
      let filtered = Representative_level.representatives middle child_pool in
      Representative_level.representatives_scoped middle child_pool ();
      let closed_trees : ((y : node Pref.t) @ immutable ->
        {t : tree | tree_root t === y && (if H.mem closed y then finite closed t else Level_unifier_spec.observe closed y === None)} @ immutable) @ total = fun y ->
        let refine_ t = Forest_transport.closed_forest_at middle middle_trees depth filtered y () in refine_ t in
      let closed_prior : ((y : node Pref.t) @ immutable ->
        {o : origin | not (low_var closed y cut) || originates saved closed cut y o} @ immutable) @ total = fun y ->
        let refine_ o = middle_prior y in Leaf_provenance_proofs.closed_leaf_origin saved middle depth cut filtered y o (); refine_ o in
      (match result rhs with None -> let refine_ o = prior x in refine_ o | Some p -> let next_env = Hm_environment_spec.Bind (p, env) in
        let refine_ o = run_origin saved cut closed depth transferred closed_trees closed_prior next_env body after final_pool x () in refine_ o))

let (rhs_interpret @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else Level_unifier_spec.observe h x === None)} @ immutable)) @ total ->
    (depth : int) -> (env : Hm_environment_spec.env) @ immutable ->
    (rhs : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (child_pool : pool) @ immutable -> (heads : Effective_level.heads) @ total ->
    (order : ((x : node Pref.t) @ immutable ->
      {u : unit | Effective_level.valid_head after heads x
        && Effective_level.effective_ordered after heads x
        && (match Effective_level.level after heads x with Generic -> true | Finite n -> n >= 0)})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho_model : ((x : node Pref.t) @ immutable -> {u : unit | equation after rho x})) @ total ->
    (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation after eta x})) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below h x depth) || rho x === eta x})) @ total ->
    (tree : tree) @ immutable ->
    {u : unit | ran h (depth + 1) Generalize_spec.Empty env rhs after child_pool && finite after tree} ->
    {u : unit | interpret rho eta (Effective_template.scheme after heads depth tree) === eta (tree_root tree)} @ ghost =
  fun h trees depth env rhs after child_pool heads order rho rho_model eta eta_model equal tree premise -> ghost_ (
    let child_depth = depth + 1 in let empty = Generalize_spec.Empty in
    ran_def h child_depth empty env rhs after child_pool;
    let initial : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x depth) || originates h h depth x o} @ immutable) @ total = fun x ->
      let refine_ o = Provenance_proofs.initial_origin h depth x in low_var_def h x depth; refine_ o in
    let prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var after x depth) || originates h after depth x o} @ immutable) @ total = fun x ->
      let refine_ o = run_origin h depth h child_depth empty trees initial env rhs after child_pool x () in refine_ o in
    Hm_effective_agreement.relative_interpret h after heads depth prior order rho rho_model eta eta_model equal tree ();
    ())
