open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec

type representative = {root : node Pref.t; path : resolution}

let rec (from_forest @ total) : (h : node Pref.heap) @ immutable ->
    (tree : tree) @ immutable -> {u : unit | finite h tree} ->
    {r : representative | resolves h (tree_root tree) r.root r.path
      && H.mem h r.root && terminal h r.root} @ immutable ghost =
  fun h tree premise -> ghost_ (
    finite_def h tree; tree_root_def tree;
    let p = tree_root tree in
    match tree with
    | Free _ | Constant_tree _ | Branch _ ->
      terminal_def h p; let path = Here in resolves_def h p p path;
      let out = {root = p; path} in out
    | Alias_tree (_, child) ->
      let out = from_forest h child () in
      let q = tree_root child in let path = Via (q, out.path) in
      resolves_def h p out.root path;
      let result = {root = out.root; path} in result)

let rec (unique @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (left : node Pref.t) @ immutable ->
    (path : resolution) @ immutable -> (right : node Pref.t) @ immutable ->
    (other : resolution) @ immutable ->
    {u : unit | resolves h p left path && resolves h p right other} ->
    {u : unit | left === right} @ ghost = fun h p left path right other premise -> ghost_ (
      resolves_def h p left path; resolves_def h p right other;
      match path with
      | Here -> terminal_def h p;
        (match other with Here -> () | Via _ -> ()); ()
      | Via (q, rest) ->
        (match other with
        | Here -> terminal_def h p; ()
        | Via (_, tail) ->
          unique h q left rest right tail (); ()))

let[@def] (level_at @ total) (h : node Pref.heap @ immutable)
    (p : node Pref.t @ immutable) (root : node Pref.t @ immutable)
    (path : resolution @ immutable) (level : level) = ghost_ (
  resolves h p root path && at_level h root === level)

let (level_unique @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (left : node Pref.t) @ immutable ->
    (path : resolution) @ immutable -> (right : node Pref.t) @ immutable ->
    (other : resolution) @ immutable -> (a : level) -> (b : level) ->
    {u : unit | level_at h p left path a && level_at h p right other b} ->
    {u : unit | a === b} @ ghost = fun h p left path right other a b premise -> ghost_ (
      level_at_def h p left path a;
      level_at_def h p right other b;
      unique h p left path right other (); ())

let rec (resolution_frame @ total) :
    (before : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable ->
      {u : unit | H.mem before x === H.mem after x && observe before x === observe after x})) @ total ->
    (p : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable ->
    (path : resolution) @ immutable ->
    {u : unit | resolves before p root path === resolves after p root path} @ ghost =
  fun before after frame p root path -> ghost_ (
    frame p; resolves_def before p root path; resolves_def after p root path;
    (match path with
    | Here -> terminal_def before p; terminal_def after p; ()
    | Via (q, rest) -> resolution_frame before after frame q root rest; ());
    ())

let[@def] (representative_covered @ total) (h : node Pref.heap @ immutable)
    (cut : int) (pool : Generalize_spec.pool @ immutable)
    (p : node Pref.t @ immutable) = ghost_ (
  not (terminal h p) || Generalize_spec.covered h cut pool p)

let (close_representative @ total) : (h : node Pref.heap) @ immutable ->
    (cut : int) -> (pool : Generalize_spec.pool) @ immutable ->
    (p : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable ->
    (path : resolution) @ immutable ->
    {u : unit | Generalize_spec.pool_scoped h pool && resolves h p root path
      && H.mem h root && terminal h root && representative_covered h cut pool root} ->
    {u : unit | let after = Generalize_spec.closed_heap h cut pool in
      resolves after p root path
      && at_level after root === Generalize_spec.close_level cut (at_level h root)} @ ghost =
  fun h cut pool p root path premise -> ghost_ (
    representative_covered_def h cut pool root;
    let after = Generalize_spec.closed_heap h cut pool in
    let frame : ((x : node Pref.t) @ immutable ->
      {u : unit | H.mem h x === H.mem after x && observe h x === observe after x}) @ total = fun x ->
      Generalize_proofs.closed_observe h cut pool x ();
      Generalize_spec.closed_at_def h after cut pool x;
      observe_def h x; observe_def after x; () in
    resolution_frame h after frame p root path;
    Generalize_proofs.closed_level h cut pool root (); ())

let (stale_link_example @ total) : (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> {u : unit | not (p === q)} ->
    {u : unit | let empty = H.empty () in
      let leaf = {desc = Var; level = Finite 2; memo = Empty_memo; visited = false} in
      let alias = {desc = Link q; level = Finite 2; memo = Empty_memo; visited = false} in
      let heap = H.put (H.put empty q leaf) p alias in
      let pool = Generalize_spec.Entry (q, Generalize_spec.Empty) in
      let after = Generalize_spec.closed_heap heap 0 pool in
      at_level after p === Finite 2
      && level_at after p q (Via (q, Here)) Generic
      && not (finite_scope after p)} @ ghost = fun p q premise -> ghost_ (
    let empty = H.empty () in
    let leaf = {desc = Var; level = Finite 2; memo = Empty_memo; visited = false} in
    let alias = {desc = Link q; level = Finite 2; memo = Empty_memo; visited = false} in
    let before = H.put empty q leaf in let heap = H.put before p alias in
    let none = Generalize_spec.Empty in let pool = Generalize_spec.Entry (q, none) in
    Generalize_spec.closed_heap_def heap 0 pool;
    let level = Finite 2 in Generalize_spec.needs_close_def 0 level;
    Generalize_spec.close_cell_def 0 leaf; Generalize_spec.close_level_def 0 level;
    let changed = H.put heap q (Generalize_spec.close_cell 0 leaf) in
    let closed = Generalize_spec.close_cell 0 leaf in
    Copy_heap_proofs.put_frame heap q closed p;
    Copy_heap_proofs.put_frame heap q closed q;
    Generalize_spec.closed_heap_def changed 0 none;
    let after = Generalize_spec.closed_heap heap 0 pool in
    let here = Here in let path = Via (q, here) in
    at_level_def after p; at_level_def after q;
    observe_def after p; observe_def after q; terminal_def after q;
    resolves_def after p q path; resolves_def after q q here;
    let generic = Generic in level_at_def after p q path generic;
    finite_scope_def after p; source_ok_def after p;
    active_def after p; active_def after q;
    ())

let rec (resolved_below @ total) : (h : node Pref.heap) @ immutable ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (p : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable ->
    (path : resolution) @ immutable -> (bound : int) ->
    {u : unit | resolves h p root path && below h p bound} ->
    {u : unit | below h root bound} @ ghost = fun h order p root path bound premise -> ghost_ (
      resolves_def h p root path;
      match path with
      | Here -> ()
      | Via (q, rest) ->
        order p; ordered_def h p; below_def h p bound; at_level_def h p; observe_def h p;
        (match H.at h p with None -> () | Some v -> match v.level with Generic -> () | Finite n ->
          children_below_def h v.desc n; below_def h q n; below_def h q bound; ());
        resolved_below h order q root rest bound (); ())

let (terminal_here @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (r : node Pref.t) @ immutable ->
    (path : resolution) @ immutable ->
    {u : unit | terminal h p && resolves h p r path} ->
    {u : unit | p === r} @ ghost = fun h p r path premise -> ghost_ (
      terminal_def h p; resolves_def h p r path;
      ())

let[@def] rec (representatives @ total) (h : node Pref.heap @ immutable)
    (pool : Generalize_spec.pool @ immutable) = ghost_ (match pool with
  | Generalize_spec.Empty -> Generalize_spec.Empty
  | Generalize_spec.Entry (p, rest) ->
    if terminal h p then Generalize_spec.Entry (p, representatives h rest)
    else representatives h rest)

let rec (representatives_member @ total) : (h : node Pref.heap) @ immutable ->
    (pool : Generalize_spec.pool) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | Generalize_spec.listed (representatives h pool) p ===
      (terminal h p && Generalize_spec.listed pool p)} @ ghost = fun h pool p -> ghost_ (
      representatives_def h pool; Generalize_spec.listed_def pool p;
      (match pool with Generalize_spec.Empty ->
        let empty = Generalize_spec.Empty in Generalize_spec.listed_def empty p; ()
      | Generalize_spec.Entry (q, rest) ->
        representatives_member h rest p;
        let next = representatives h rest in let entry = Generalize_spec.Entry (q, next) in
        Generalize_spec.listed_def entry p; ());
      ())

let (representatives_covered @ total) : (h : node Pref.heap) @ immutable ->
    (cut : int) -> (pool : Generalize_spec.pool) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | representative_covered h cut pool p ===
      representative_covered h cut (representatives h pool) p} @ ghost = fun h cut pool p -> ghost_ (
      let filtered = representatives h pool in
      representative_covered_def h cut pool p; representative_covered_def h cut filtered p;
      Generalize_spec.covered_def h cut pool p; Generalize_spec.covered_def h cut filtered p;
      representatives_member h pool p; ())

let[@def] (boundary @ total) (h : node Pref.heap @ immutable) (p : node Pref.t @ immutable)
    (r : representative @ immutable) (bound : int) = ghost_ (
  resolves h p r.root r.path && below h r.root bound)

let (boundary_model @ total) : (h : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (r : representative) @ immutable -> (bound : int) ->
    {u : unit | boundary h p r bound} ->
    {u : unit | rho p === rho r.root} @ ghost = fun h rho model p r bound premise -> ghost_ (
      boundary_def h p r bound;
      Level_unifier_proofs.resolution_model h rho model p r.root r.path (); ())

let rec (representatives_scoped @ total) : (h : node Pref.heap) @ immutable ->
    (pool : Generalize_spec.pool) @ immutable ->
    {u : unit | Generalize_spec.pool_scoped h pool} ->
    {u : unit | Generalize_spec.pool_scoped h (representatives h pool)} @ ghost = fun h pool premise -> ghost_ (
      Generalize_spec.pool_scoped_def h pool;
      representatives_def h pool;
      match pool with
      | Generalize_spec.Empty -> ()
      | Generalize_spec.Entry (p, rest) ->
        representatives_scoped h rest ();
        let tail = representatives h rest in let result = Generalize_spec.Entry (p, tail) in
        Generalize_spec.pool_scoped_def h result; ())

let (close_resolution @ total) : (h : node Pref.heap) @ immutable ->
    (cut : int) -> (pool : Generalize_spec.pool) @ immutable ->
    (p : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable ->
    (path : resolution) @ immutable ->
    {u : unit | Generalize_spec.pool_scoped h pool && resolves h p root path
      && H.mem h root && terminal h root && representative_covered h cut pool root} ->
    {u : unit | let after = Generalize_spec.closed_heap h cut (representatives h pool) in
      resolves after p root path
      && at_level after root === Generalize_spec.close_level cut (at_level h root)} @ ghost =
  fun h cut pool p root path premise -> ghost_ (
    representatives_scoped h pool ();
    representatives_covered h cut pool root;
    let filtered = representatives h pool in
    close_representative h cut filtered p root path (); ())

let (close_models @ total) : (h : node Pref.heap) @ immutable ->
    (cut : int) -> (pool : Generalize_spec.pool) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Generalize_spec.pool_scoped h pool} ->
    {u : unit | equation h rho x ===
      equation (Generalize_spec.closed_heap h cut (representatives h pool)) rho x} @ ghost =
  fun h cut pool rho x premise -> ghost_ (
    representatives_scoped h pool ();
    let filtered = representatives h pool in
    Generalize_proofs.closed_model h cut filtered rho x (); ())

let rec (filter_frame @ total) : (h : node Pref.heap) @ immutable ->
    (after : node Pref.heap) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | observe h x === observe after x})) @ total ->
    (pool : Generalize_spec.pool) @ immutable ->
    {u : unit | representatives h pool === representatives after pool} @ ghost =
  fun h after frame pool -> ghost_ (
    representatives_def h pool; representatives_def after pool;
    (match pool with Generalize_spec.Empty -> () | Generalize_spec.Entry (p, rest) ->
      frame p; terminal_def h p; terminal_def after p;
      filter_frame h after frame rest; ());
    ())

let rec (close_forest @ total) : (h : node Pref.heap) @ immutable ->
    (cut : int) -> (pool : Generalize_spec.pool) @ immutable ->
    (tree : tree) @ immutable ->
    {u : unit | Generalize_spec.pool_scoped h pool && finite h tree} ->
    {u : unit | finite (Generalize_spec.closed_heap h cut (representatives h pool)) tree} @ ghost =
  fun h cut pool tree premise -> ghost_ (
    representatives_scoped h pool ();
    let filtered = representatives h pool in
    let after = Generalize_spec.closed_heap h cut filtered in
    tree_root_def tree;
    let p = tree_root tree in
    Generalize_proofs.closed_observe h cut filtered p ();
    Generalize_spec.closed_at_def h after cut filtered p;
    observe_def h p; observe_def after p;
    finite_def h tree; finite_def after tree;
    (match tree with Free _ | Constant_tree _ -> ()
      | Alias_tree (_, child) -> close_forest h cut pool child (); ()
      | Branch (_, a, b) -> close_forest h cut pool a ();
        close_forest h cut pool b (); ());
    ())
