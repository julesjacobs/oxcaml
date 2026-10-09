open Copy_spec
open Level_spec
open Level_unifier_spec
open Representative_level
module G = Generalize_spec

type heads = (node Pref.t @ immutable total -> representative @ immutable total)

let[@def] (valid_head @ total) (h : node Pref.heap @ immutable)
    (heads : heads @ total) (p : node Pref.t @ immutable) = ghost_ (
  not (H.mem h p) || resolves h p (heads p).root (heads p).path)

let[@def] (level @ total) (h : node Pref.heap @ immutable)
    (heads : heads @ total) (p : node Pref.t @ immutable) = ghost_ (
  if H.mem h p then at_level h (heads p).root else Generic)

let[@def] (effective_below @ total) (h : node Pref.heap @ immutable)
    (heads : heads @ total) (p : node Pref.t @ immutable) (cut : int) = ghost_ (
  H.mem h p && match level h heads p with
  | Generic -> false | Finite n -> 0 <= n && n <= cut)

let[@def] (effective_ordered @ total) (h : node Pref.heap @ immutable)
    (heads : heads @ total) (p : node Pref.t @ immutable) = ghost_ (
  match H.at h p with
  | None | Some {desc = Link _; _} -> true
  | Some v -> match v.level with
    | Generic -> true
    | Finite n -> n >= 0 && match v.desc with
      | Var | Bool | Link _ -> true
      | Arrow (a, b) -> effective_below h heads a n && effective_below h heads b n)

let (head_terminal @ total) : (h : node Pref.heap) @ immutable ->
    (heads : heads) @ total -> (p : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && valid_head h heads p} ->
    {u : unit | H.mem h (heads p).root && terminal h (heads p).root} @ ghost =
  fun h heads p premise -> ghost_ (
    valid_head_def h heads p;
    let r = heads p in Compression_path_proofs.resolution_terminal h p r.root r.path ();
    ())

let (terminal_level @ total) : (h : node Pref.heap) @ immutable ->
    (heads : heads) @ total -> (p : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && terminal h p && valid_head h heads p} ->
    {u : unit | level h heads p === at_level h p} @ ghost =
  fun h heads p premise -> ghost_ (
    valid_head_def h heads p;
    level_def h heads p; let r = heads p in terminal_here h p r.root r.path (); ())

let (closed_head @ total) : (h : node Pref.heap) @ immutable ->
    (heads : heads) @ total -> (cut : int) -> (pool : G.pool) @ immutable ->
    (p : node Pref.t) @ immutable ->
    {u : unit | G.pool_scoped h pool && valid_head h heads p} ->
    {u : unit | valid_head (Representative_pool_spec.close_heap h cut pool) heads p} @ ghost =
  fun h heads cut pool p premise -> ghost_ (
    let after = Representative_pool_spec.close_heap h cut pool in
    Representative_pool_spec.close_heap_def h cut pool;
    let filtered = representatives h pool in representatives_scoped h pool ();
    let frame : ((x : node Pref.t) @ immutable ->
      {u : unit | H.mem h x === H.mem after x && observe h x === observe after x}) @ total = fun x ->
        G.closed_at_def h after cut filtered x;
        Generalize_proofs.closed_observe h cut filtered x ();
        observe_def h x; observe_def after x; () in
    frame p; valid_head_def h heads p; valid_head_def after heads p;
    let r = heads p in resolution_frame h after frame p r.root r.path;
    ())

let (closed_level @ total) : (h : node Pref.heap) @ immutable ->
    (heads : heads) @ total -> (cut : int) -> (pool : G.pool) @ immutable ->
    (p : node Pref.t) @ immutable ->
    {u : unit | G.pool_scoped h pool && valid_head h heads p
      && representative_covered h cut pool (heads p).root} ->
    {u : unit | level (Representative_pool_spec.close_heap h cut pool) heads p
      === G.close_level cut (level h heads p)} @ ghost =
  fun h heads cut pool p premise -> ghost_ (
    let after = Representative_pool_spec.close_heap h cut pool in
    Representative_pool_spec.close_heap_def h cut pool;
    let filtered = representatives h pool in representatives_scoped h pool ();
    Generalize_proofs.closed_observe h cut filtered p ();
    G.closed_at_def h after cut filtered p;
    level_def h heads p; level_def after heads p;
    if H.mem h p then (
      head_terminal h heads p ();
      valid_head_def h heads p; let r = heads p in
      close_resolution h cut pool p r.root r.path (); ())
    else (let generic = Generic in G.close_level_def cut generic; ()))

let (closed_boundary @ total) : (h : node Pref.heap) @ immutable ->
    (heads : heads) @ total -> (cut : int) -> (pool : G.pool) @ immutable ->
    (p : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | G.pool_scoped h pool && valid_head h heads p
      && effective_below h heads p bound && bound <= cut} ->
    {u : unit | let after = Representative_pool_spec.close_heap h cut pool in
      H.at after p === H.at h p && effective_below after heads p bound} @ ghost =
  fun h heads cut pool p bound premise -> ghost_ (
    let after = Representative_pool_spec.close_heap h cut pool in
    Representative_pool_spec.close_heap_def h cut pool;
    let filtered = representatives h pool in representatives_scoped h pool ();
    Generalize_proofs.closed_observe h cut filtered p ();
    G.closed_at_def h after cut filtered p;
    effective_below_def h heads p bound; level_def h heads p;
    let r = heads p in
    head_terminal h heads p ();
    Level_spec.below_def h r.root bound;
    Generalize_proofs.closed_below h cut filtered r.root bound ();
    Level_spec.below_def after r.root bound;
    effective_below_def after heads p bound; level_def after heads p;
    representatives_member h pool p;
    if terminal h p then (
      terminal_level h heads p (); at_level_def h p;
      match H.at h p with None -> () | Some v -> G.close_level_def cut v.level; ())
    else ();
    ())

let (closed_ordered @ total) : (h : node Pref.heap) @ immutable ->
    (heads : heads) @ total ->
    (valid : ((x : node Pref.t) @ immutable -> {u : unit | valid_head h heads x})) @ total ->
    (cut : int) -> (pool : G.pool) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | G.pool_scoped h pool && H.mem h p && representative_covered h cut pool p
      && effective_ordered h heads p} ->
    {u : unit | effective_ordered (Representative_pool_spec.close_heap h cut pool) heads p} @ ghost =
  fun h heads valid cut pool p premise -> ghost_ (
    let after = Representative_pool_spec.close_heap h cut pool in
    Representative_pool_spec.close_heap_def h cut pool;
    let filtered = representatives h pool in representatives_scoped h pool ();
    Generalize_proofs.closed_observe h cut filtered p ();
    G.closed_at_def h after cut filtered p;
    effective_ordered_def h heads p; effective_ordered_def after heads p;
    representative_covered_def h cut pool p;
    representatives_member h pool p;
    terminal_def h p; observe_def h p; G.covered_def h cut pool p;
    at_level_def h p;
    match H.at h p with None -> () | Some v ->
      match v.desc with Link _ -> () | Var | Bool | Arrow _ ->
        representatives_covered h cut pool p;
        representative_covered_def h cut filtered p;
        Generalize_proofs.closed_level h cut filtered p ();
        at_level_def after p;
        G.close_level_def cut v.level;
        match v.level with Generic -> () | Finite n ->
          if n > cut then () else (
            (match v.desc with Var | Bool | Link _ -> () | Arrow (a, b) ->
              valid a; valid b;
              closed_boundary h heads cut pool a n ();
              closed_boundary h heads cut pool b n (); ());
            ()))

let (link_level @ total) : (h : node Pref.heap) @ immutable ->
    (heads : heads) @ total -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && H.mem h q && valid_head h heads p
      && valid_head h heads q && observe h p === Some (Link q)} ->
    {u : unit | level h heads p === level h heads q} @ ghost =
  fun h heads p q premise -> ghost_ (
    valid_head_def h heads p;
    valid_head_def h heads q; level_def h heads p; level_def h heads q;
    let left = heads p in let right = heads q in
    resolves_def h p left.root left.path; terminal_def h p;
    match left.path with Here -> () | Via (_, rest) ->
      unique h q left.root rest right.root right.path (); ())

let[@def] (effective_active @ total) (h : node Pref.heap @ immutable)
    (heads : heads @ total) (p : node Pref.t @ immutable) = ghost_ (
  H.mem h p && match level h heads p with Generic -> false | Finite n -> n >= 0)

let[@def] (effective_scope @ total) (h : node Pref.heap @ immutable)
    (heads : heads @ total) (p : node Pref.t @ immutable) = ghost_ (
  source_ok h p && (not (effective_active h heads p) || match observe h p with
    | None -> false | Some (Var | Bool) -> true
    | Some (Link q) -> effective_active h heads q
    | Some (Arrow (a, b)) -> effective_active h heads a && effective_active h heads b))

let (ordered_scope @ total) : (h : node Pref.heap) @ immutable ->
    (heads : heads) @ total ->
    (valid : ((x : node Pref.t) @ immutable -> {u : unit | valid_head h heads x})) @ total ->
    (p : node Pref.t) @ immutable ->
    {u : unit | source_ok h p && effective_ordered h heads p} ->
    {u : unit | effective_scope h heads p} @ ghost =
  fun h heads valid p premise -> ghost_ (
    effective_scope_def h heads p;
    source_ok_def h p; observe_def h p; effective_ordered_def h heads p;
    effective_active_def h heads p; valid p;
    match H.at h p with None -> () | Some v ->
      match v.desc with
      | Link q -> valid q; link_level h heads p q ();
        effective_active_def h heads q; ()
      | Var | Bool -> ()
      | Arrow (a, b) ->
        terminal_def h p; terminal_level h heads p ();
        at_level_def h p;
        effective_active_def h heads a; effective_active_def h heads b;
        (match v.level with Generic -> () | Finite n ->
          effective_below_def h heads a n; effective_below_def h heads b n; ());
        ())

let (generic_child_rejected @ total) : (h : node Pref.heap) @ immutable ->
    (heads : heads) @ total -> (p : node Pref.t) @ immutable ->
    (a : node Pref.t) @ immutable -> (b : node Pref.t) @ immutable -> (n : int) ->
    {u : unit | observe h p === Some (Arrow (a, b)) && at_level h p === Finite n
      && level h heads a === Generic} ->
    {u : unit | not (effective_ordered h heads p)} @ ghost =
  fun h heads p a b n premise -> ghost_ (
    observe_def h p; at_level_def h p;
    effective_ordered_def h heads p; effective_below_def h heads a n;
    ())

let (terminal_children @ total) : (h : node Pref.heap) @ immutable ->
    (heads : heads) @ total -> (p : node Pref.t) @ immutable ->
    (a : node Pref.t) @ immutable -> (b : node Pref.t) @ immutable ->
    {u : unit | active h p && terminal h p && valid_head h heads p
      && effective_scope h heads p && observe h p === Some (Arrow (a, b))} ->
    {u : unit | effective_active h heads p
      && effective_active h heads a && effective_active h heads b}
      @ ghost = fun h heads p a b premise -> ghost_ (
    active_def h p;
    terminal_level h heads p ();
    effective_active_def h heads p;
    effective_scope_def h heads p;
    ())
