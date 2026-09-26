open Copy_spec
open Level_spec
open Generalize_spec
open Hm_environment_spec
open Hm_effective_execution_spec

let[@def] (preserved_bound @ total) (h : node Pref.heap @ immutable)
    (after : node Pref.heap @ immutable) (bound : int) (x : node Pref.t @ immutable) = ghost_ (
  H.mem h x && H.mem after x && (not (below h x bound) || below after x bound))

let (bound_trans @ total) : (h : node Pref.heap) @ immutable ->
    (mid : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (bound : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | preserved_bound h mid bound x && preserved_bound mid after bound x} ->
    {u : unit | preserved_bound h after bound x} @ ghost =
  fun h mid after bound x premise -> ghost_ (
    preserved_bound_def h mid bound x;
    preserved_bound_def mid after bound x; preserved_bound_def h after bound x;
    ())

let (unify_bound @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : Effective_unifier_spec.derivation) @ immutable ->
    (bound : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | Effective_unifier_spec.unified h p q ok after d && H.mem h x} ->
    {u : unit | preserved_bound h after bound x} @ ghost =
  fun h p q ok after d bound x premise -> ghost_ (
    Effective_unifier_metadata.cells h p q ok after d x ();
    Effective_unifier_metadata.cell_frame_def h after x;
    preserved_bound_def h after bound x;
    below_def h x bound; below_def after x bound;
    at_level_def h x; at_level_def after x;
    (match H.at h x, H.at after x with
     | Some a, Some b -> decreases_def a.level b.level; () | _ -> ()); ())

let (close_bound @ total) : (h : node Pref.heap) @ immutable ->
    (depth : int) -> (pool : pool) @ immutable ->
    (bound : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | pool_scoped h pool && bound <= depth && H.mem h x} ->
    {u : unit | preserved_bound h (Representative_pool_spec.close_heap h depth pool) bound x} @ ghost =
  fun h depth pool bound x premise -> ghost_ (
    Representative_pool_spec.close_heap_def h depth pool;
    let filtered = Representative_level.representatives h pool in
    let after = Representative_pool_spec.close_heap h depth pool in
    Representative_level.representatives_scoped h pool ();
    Generalize_proofs.closed_observe h depth filtered x ();
    closed_at_def h after depth filtered x;
    preserved_bound_def h after bound x;
    below_def h x bound; below_def after x bound;
    at_level_def h x; at_level_def after x;
    (match H.at h x with None -> () | Some v -> close_level_def depth v.level; ());
    ())

let (allocation_member @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (bound : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p) && H.mem h x} ->
    {u : unit | preserved_bound h (H.put h p v) bound x} @ ghost = fun h p v bound x premise -> ghost_ (
    let after = H.put h p v in preserved_bound_def h after bound x; below_def h x bound; below_def after x bound;
    at_level_def h x; at_level_def after x; ())
let (copy_member @ total) : (h : node Pref.heap) @ immutable -> (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : node Pref.t) @ immutable ->
    (depth : int) -> (d : history) @ immutable -> (bound : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | Copy_certificate_spec.certified_valid h certificate epoch depth d && H.mem h x} ->
    {u : unit | preserved_bound h (copy_heap h epoch depth d) bound x} @ ghost = fun h certificate epoch depth d bound x premise -> ghost_ (
    let raw = heap h epoch depth d in let after = copy_heap h epoch depth d in
    let trail = Pooled_spec.touched d in copy_heap_def h epoch depth d;
    Hm_effective_registration.history_at h certificate epoch depth d x ();
    Hm_effective_registration.result_at h certificate epoch depth d x (); Copy_cleanup_spec.swept_at_def raw after trail x;
    preserved_bound_def h after bound x; below_def h x bound; below_def after x bound;
    at_level_def h x; at_level_def after x; ())

let rec (run_member @ total) : (h : node Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (env : env) @ immutable -> (e : execution) @ immutable -> (after : node Pref.heap) @ immutable ->
    (final_pool : pool) @ immutable -> (bound : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | ran h depth pool env e after final_pool && bound <= depth && H.mem h x} ->
    {u : unit | preserved_bound h after bound x} @ ghost = fun h depth pool env e after final_pool bound x premise -> ghost_ (
    ran_def h depth pool env e after final_pool;
    match e with
    | RShared _ -> preserved_bound_def h h bound x; ()
    | RVar (i, target, epoch, d, certificate) -> (match lookup env i with None -> () | Some original ->
      Copy_certificate_spec.certifies_def h certificate epoch depth d original target;
      copy_member h certificate epoch depth d bound x (); ())
    | RBool p | RFalse p -> let desc : desc = Bool in let v = cell desc depth in allocated_def h depth p desc;
      allocation_member h p v bound x (); ()
    | RWord (_, p) -> let desc : desc = Word in let v = cell desc depth in allocated_def h depth p desc;
      allocation_member h p v bound x (); ()
    | RApp_left (left, _) | RCons_left (left, _) -> run_member h depth pool env left after final_pool bound x (); ()
    | RLet_left (rhs, _) -> let child_depth = depth + 1 in let empty : pool = Empty in
      ran_def h child_depth empty env rhs after final_pool;
      run_member h child_depth empty env rhs after final_pool bound x (); ()
    | RNil (arg, p) ->
      let var = Var in let v = cell var depth in let middle = H.put h arg v in
      let desc = List arg in let w = cell desc depth in
      allocated_def h depth arg var; allocated_def middle depth p desc;
      allocation_member h arg v bound x (); preserved_bound_def h middle bound x;
      allocation_member middle p w bound x (); bound_trans h middle after bound x (); ()
    | RCaseList (_, _, _, body) ->
      run_member h depth pool env body after final_pool bound x ()
    | RIf (_, _, _, body) ->
      run_member h depth pool env body after final_pool bound x ()
    | RPrimitive (op, _, _, body, middle, body_pool, out) ->
      run_member h depth pool env body middle body_pool bound x ();
      preserved_bound_def h middle bound x;
      (match result body with None -> () | Some _ -> match out with None -> () | Some p ->
        let desc = primitive_desc op in let v = cell desc depth in
        allocated_def middle depth p desc; allocation_member middle p v bound x ();
        bound_trans h middle after bound x (); ())
    | RLam (arg, body, middle, body_pool, out) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in
      let pool1 = Entry (arg, pool) in let env1 = Bind (arg, env) in
      allocated_def h depth arg var; allocation_member h arg v bound x ();
      preserved_bound_def h h1 bound x;
      run_member h1 depth pool1 env1 body middle body_pool bound x ();
      bound_trans h h1 middle bound x (); preserved_bound_def h middle bound x;
      (match result body with None -> () | Some b -> match out with None -> () | Some p ->
        let desc = Arrow (arg, b) in let w = cell desc depth in allocated_def middle depth p desc;
        allocation_member middle p w bound x ();
        bound_trans h middle after bound x (); ())
    | RApp_right (left, right, h1, pool1) | RCons_right (left, right, h1, pool1) ->
      run_member h depth pool env left h1 pool1 bound x (); preserved_bound_def h h1 bound x;
      run_member h1 depth pool1 env right after final_pool bound x ();
      bound_trans h h1 after bound x (); ()
    | RCons (left, right, h1, pool1, h2, pool2, p, ok, d) ->
      run_member h depth pool env left h1 pool1 bound x (); preserved_bound_def h h1 bound x;
      run_member h1 depth pool1 env right h2 pool2 bound x ();
      bound_trans h h1 h2 bound x (); preserved_bound_def h h2 bound x;
      (match result left with None -> () | Some f -> match result right with None -> () | Some a ->
        let desc = List f in let v = cell desc depth in let h3 = H.put h2 p v in
        allocated_def h2 depth p desc;
        allocation_member h2 p v bound x (); bound_trans h h2 h3 bound x ();
        unify_bound h3 a p ok after d bound x ();
        bound_trans h h3 after bound x (); ())
    | RApp (left, right, h1, pool1, h2, pool2, p, arrow, ok, d) ->
      run_member h depth pool env left h1 pool1 bound x (); preserved_bound_def h h1 bound x;
      run_member h1 depth pool1 env right h2 pool2 bound x ();
      bound_trans h h1 h2 bound x (); preserved_bound_def h h2 bound x;
      (match result left with None -> () | Some f -> match result right with None -> () | Some a ->
        let var : desc = Var in let v = cell var depth in let h3 = H.put h2 p v in
        let desc = Arrow (a, p) in let w = cell desc depth in let h4 = H.put h3 arrow w in
        allocated_def h2 depth p var; allocated_def h3 depth arrow desc;
        allocation_member h2 p v bound x (); preserved_bound_def h2 h3 bound x;
        allocation_member h3 arrow w bound x ();
        bound_trans h2 h3 h4 bound x (); bound_trans h h2 h4 bound x ();
        unify_bound h4 f arrow ok after d bound x ();
        bound_trans h h4 after bound x (); ())
    | RRec (arg, res, self, body, middle, body_pool, finish) ->
      let var : desc = Var in let v = cell var depth in let h1 = H.put h arg v in let h2 = H.put h1 res v in
      let desc = Arrow (arg, res) in let w = cell desc depth in let h3 = H.put h2 self w in
      let pool3 = Entry (self, Entry (res, Entry (arg, pool))) in let env3 = Bind (arg, Bind (self, env)) in
      allocated_def h depth arg var; allocated_def h1 depth res var; allocated_def h2 depth self desc;
      allocation_member h arg v bound x (); preserved_bound_def h h1 bound x;
      allocation_member h1 res v bound x (); preserved_bound_def h1 h2 bound x;
      allocation_member h2 self w bound x ();
      bound_trans h h1 h2 bound x (); bound_trans h h2 h3 bound x ();
      preserved_bound_def h h3 bound x;
      run_member h3 depth pool3 env3 body middle body_pool bound x ();
      bound_trans h h3 middle bound x (); preserved_bound_def h middle bound x;
      (match result body with None -> () | Some b -> match finish with Aborted -> () | Unified (ok, d) ->
        unify_bound middle b res ok after d bound x ();
        bound_trans h middle after bound x (); ())
    | RLet (rhs, body, middle, child_pool) ->
      let child_depth = depth + 1 in let empty : pool = Empty in
      ran_def h child_depth empty env rhs middle child_pool;
      run_member h child_depth empty env rhs middle child_pool bound x ();
      preserved_bound_def h middle bound x;
      let closed = Representative_pool_spec.close_heap middle depth child_pool in
      close_bound middle depth child_pool bound x ();
      bound_trans h middle closed bound x (); preserved_bound_def h closed bound x;
      let transferred = Representative_pool_spec.transfer_rep closed child_pool pool in
      (match result rhs with None -> () | Some p -> let env1 = Bind (p, env) in
        run_member closed depth transferred env1 body after final_pool bound x ();
        bound_trans h closed after bound x (); ()))
