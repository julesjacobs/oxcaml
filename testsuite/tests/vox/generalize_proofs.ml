open Copy_spec
open Copy_heap_proofs
open Level_spec
open Generalize_spec

let (close_idempotent @ total) : (cut : int) -> (v : level) ->
    {u : unit | close_level cut (close_level cut v) === close_level cut v} @ ghost = fun cut v -> ghost_ (
  close_level_def cut v; let next = close_level cut v in close_level_def cut next; ())
let (close_source @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (old : node) @ immutable -> (cut : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && H.at h p === Some old} ->
    {u : unit | H.mem h x === H.mem (H.put h p (close_cell cut old)) x
      && source_ok h x === source_ok (H.put h p (close_cell cut old)) x} @ ghost = fun h p old cut x premise -> ghost_ (
  close_cell_def cut old; let v = close_cell cut old in
  let after = H.put h p v in source_ok_def h x; source_ok_def after x;
  (match H.at h x with None -> () | Some a ->
    (match a.memo with Empty_memo | Forward _ -> () | Memo (stamp, _) -> ());
    (match a.desc with Var | Bool | Word -> () | Link q | List q -> ()
    | Arrow (a, b) -> ())); ())
let rec (pool_write @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (old : node) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    {u : unit | H.mem h p && H.at h p === Some old && pool_scoped h pool} ->
    {u : unit | pool_scoped (H.put h p (close_cell cut old)) pool} @ ghost = fun h p old cut pool premise -> ghost_ (
  let after = H.put h p (close_cell cut old) in
  pool_scoped_def h pool; pool_scoped_def after pool; match pool with Empty -> ()
  | Entry (x, rest) -> close_source h p old cut x (); pool_write h p old cut rest (); ())
let rec (closed_observe @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | pool_scoped h pool} ->
    {u : unit | closed_at h (closed_heap h cut pool) cut pool x} @ ghost = fun h cut pool x premise -> ghost_ (
  pool_scoped_def h pool; closed_heap_def h cut pool;
  let after = closed_heap h cut pool in closed_at_def h after cut pool x; listed_def pool x;
  match pool with
  | Empty -> ()
  | Entry (p, rest) -> source_ok_def h p;
    match H.at h p with None -> () | Some old ->
      needs_close_def cut old.level;
      if not (needs_close cut old.level) then (
        closed_observe h cut rest x (); closed_at_def h after cut rest x;
        close_level_def cut old.level; ())
      else (
      let v = close_cell cut old in close_cell_def cut old;
      let mid = H.put h p v in close_source h p old cut x ();
      pool_write h p old cut rest (); closed_observe mid cut rest x ();
      closed_at_def mid after cut rest x; close_idempotent cut old.level; ()))

let (closed_level @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | pool_scoped h pool && H.mem h x && covered h cut pool x} ->
    {u : unit | H.mem h x === H.mem (closed_heap h cut pool) x
      && at_level (closed_heap h cut pool) x === close_level cut (at_level h x)} @ ghost = fun h cut pool x premise -> ghost_ (
  let after = closed_heap h cut pool in closed_observe h cut pool x (); closed_at_def h after cut pool x;
  covered_def h cut pool x; at_level_def h x; at_level_def after x;
  let level = at_level h x in close_level_def cut level;
  (match H.at h x with None -> () | Some v -> close_level_def cut v.level; ()); ())
let (closed_model @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | pool_scoped h pool} ->
    {u : unit | equation h rho x === equation (closed_heap h cut pool) rho x} @ ghost = fun h cut pool rho x premise -> ghost_ (
  let after = closed_heap h cut pool in closed_observe h cut pool x (); closed_at_def h after cut pool x;
  equation_def h rho x; equation_def after rho x; ())
let (closed_boundary @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | pool_scoped h pool && below h x cut} ->
    {u : unit | H.at (closed_heap h cut pool) x === H.at h x && below (closed_heap h cut pool) x cut} @ ghost =
  fun h cut pool x premise -> ghost_ (
    let after = closed_heap h cut pool in closed_observe h cut pool x (); closed_at_def h after cut pool x;
    below_def h x cut; below_def after x cut; at_level_def h x; at_level_def after x;
    (match H.at h x with None -> () | Some v -> close_level_def cut v.level; ()); ())
let (closed_source @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | pool_scoped h pool && source_ok h x} ->
    {u : unit | source_ok (closed_heap h cut pool) x} @ ghost = fun h cut pool x premise -> ghost_ (
  let after = closed_heap h cut pool in closed_observe h cut pool x (); closed_at_def h after cut pool x;
  source_ok_def h x; source_ok_def after x;
  (match H.at h x with None -> () | Some v ->
    (match v.memo with Empty_memo | Forward _ -> () | Memo (stamp, _) ->
      closed_observe h cut pool stamp (); closed_at_def h after cut pool stamp; ());
    (match v.desc with Var | Bool | Word -> () | Link q | List q ->
      closed_observe h cut pool q (); closed_at_def h after cut pool q; ()
    | Arrow (a, b) -> closed_observe h cut pool a (); closed_observe h cut pool b ();
      closed_at_def h after cut pool a; closed_at_def h after cut pool b; ())); ())
let rec (environment_bound @ total) : (h : node Pref.heap) @ immutable ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (cut : int) -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (path : path) @ immutable ->
    {u : unit | below h p cut && reaches h p q path} -> {u : unit | below h q cut} @ ghost = fun h order cut p q path premise -> ghost_ (
  reaches_def h p q path; match path with Stop -> ()
  | Step (x, rest) -> order p; ordered_def h p; below_def h p cut; at_level_def h p; edge_def h p x;
    (match H.at h p with None -> () | Some v -> match v.level with Generic -> () | Finite n ->
      children_below_def h v.desc n;
      (match v.desc with Var | Bool | Word -> () | Link a | List a -> below_def h a n; at_level_def h a; ()
      | Arrow (a, b) -> below_def h a n; at_level_def h a; below_def h b n; at_level_def h b; ()); ());
    below_def h x cut; at_level_def h x;
    environment_bound h order cut x q rest (); ())
let (environment_preserved @ total) : (h : node Pref.heap) @ immutable ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (cut : int) -> (pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (path : path) @ immutable ->
    {u : unit | pool_scoped h pool && below h p cut && reaches h p q path} ->
    {u : unit | H.at (closed_heap h cut pool) q === H.at h q && below (closed_heap h cut pool) q cut} @ ghost =
  fun h order cut pool p q path premise -> ghost_ (
    environment_bound h order cut p q path ();
    closed_boundary h cut pool q (); ())

let (closed_below @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> (bound : int) -> {u : unit | pool_scoped h pool && below h x bound && bound <= cut} ->
    {u : unit | below (closed_heap h cut pool) x bound} @ ghost = fun h cut pool x bound premise -> ghost_ (
  below_def h x bound; below_def h x cut; at_level_def h x;
  closed_boundary h cut pool x (); let after = closed_heap h cut pool in
  below_def after x cut; below_def after x bound; at_level_def after x; ())
let (closed_ordered @ total) : (h : node Pref.heap) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | pool_scoped h pool && H.mem h x && covered h cut pool x && ordered h x} ->
    {u : unit | ordered (closed_heap h cut pool) x} @ ghost = fun h cut pool x premise -> ghost_ (
  let after = closed_heap h cut pool in closed_level h cut pool x (); closed_observe h cut pool x (); closed_at_def h after cut pool x;
  at_level_def h x; at_level_def after x; ordered_def h x; ordered_def after x;
  match H.at h x with None -> () | Some v -> close_level_def cut v.level;
    match v.level with Generic -> () | Finite n ->
      if n > cut then () else (
        children_below_def h v.desc n; children_below_def after v.desc n;
        (match v.desc with Var | Bool | Word -> () | Link q | List q -> closed_below h cut pool q n (); ()
        | Arrow (a, b) -> closed_below h cut pool a n (); closed_below h cut pool b n (); ()); ()))
let (ordered_scope @ total) : (h : node Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | source_ok h x && ordered h x} -> {u : unit | finite_scope h x} @ ghost = fun h x premise -> ghost_ (
  finite_scope_def h x; source_ok_def h x; ordered_def h x; match H.at h x with None -> () | Some v -> match v.level with Generic ->
    active_def h x; at_level_def h x; ()
  | Finite n -> children_below_def h v.desc n;
    (match v.desc with Var | Bool | Word -> () | Link q | List q -> below_def h q n; active_def h q; ()
    | Arrow (a, b) -> below_def h a n; active_def h a; below_def h b n; active_def h b; ()); ())

let (coverage_after_unify @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : Level_unifier_spec.derivation) @ immutable -> (cut : int) -> (pool : pool) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | Level_unifier_spec.unified h p q ok after d && covered h cut pool x} ->
    {u : unit | covered after cut pool x} @ ghost = fun h p q ok after d cut pool x premise -> ghost_ (
  Level_unifier_proofs.unified_frame h p q ok after d x ();
  Level_unifier_metadata.unified_scratch h p q ok after d x ();
  Level_unifier_metadata.scratch_frame_def h after x;
  covered_def h cut pool x; covered_def after cut pool x; at_level_def h x; at_level_def after x;
  (match H.at h x, H.at after x with Some a, Some b -> decreases_def a.level b.level; () | _ -> ()); ())
