open Copy_spec
open Level_spec
open Level_unifier_spec

let (redirect_terminal @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | terminal (H.put h p (redirect h p q)) x} ->
    {u : unit | terminal h x} @ ghost = fun h p q x premise -> ghost_ (
      let v = redirect h p q in
      let after = H.put h p v in
      redirect_def h p q; let link = Link q in cell_def link 0;
      terminal_def h x; terminal_def after x;
      observe_def h x; observe_def after x;
      ())

let rec (base_terminal @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d && terminal after x} ->
    {u : unit | terminal h x} @ ghost = fun h p q ok after d x premise -> ghost_ (
      unified_def h p q ok after d;
      match d with
      | Same | Constants | Occurs_left _ | Occurs_right _ | Clash -> ()
      | Bind_left _ -> redirect_terminal h p q x (); ()
      | Bind_right _ -> redirect_terminal h q p x (); ()
      | Swap rest -> base_terminal h q p ok after rest x (); ()
      | List_children (a, b, rest) | Resolve (a, b, _, _, rest) ->
        base_terminal h a b ok after rest x (); ()
      | Scanned (needle, marks, rest) ->
        let mid = scan_heap h marks in
        base_terminal mid p q ok after rest x ();
        Marked_occurs_proofs.scan_observe h needle marks x ();
        terminal_def h x; terminal_def mid x; ()
      | Lowering (bound, edits, _, rest) ->
        let mid = lower_heap h bound edits in
        base_terminal mid p q ok after rest x ();
        Level_unifier_proofs.lower_observe h bound edits x ();
        terminal_def h x; terminal_def mid x; ()
      | Children (a, b, c, e, mid, left_ok, left, right) ->
        if left_ok then (
          base_terminal mid b e ok after right x ();
          base_terminal h a c left_ok mid left x (); ())
        else (base_terminal h a c left_ok mid left x (); ()))

let rec (compression_terminal @ total) : (h : node Pref.heap) @ immutable ->
    (after : node Pref.heap) @ immutable -> (d : Compression_spec.edits) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Compression_spec.rewritten h after d && terminal after x} ->
    {u : unit | terminal h x} @ ghost = fun h after d x premise -> ghost_ (
      Compression_spec.rewritten_def h after d;
      match d with
      | Compression_spec.Done -> ()
      | Compression_spec.Write (p, _, r, _, rest) ->
        let mid = H.put h p (redirect h p r) in
        compression_terminal mid after rest x ();
        redirect_terminal h p r x (); ())

let rec (unified_terminal @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : Optimized_unifier_spec.derivation) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Optimized_unifier_spec.unified h p q ok after d && terminal after x} ->
    {u : unit | terminal h x} @ ghost = fun h p q ok after d x premise -> ghost_ (
      Optimized_unifier_spec.unified_def h p q ok after d;
      match d with
      | Optimized_unifier_spec.Base old -> base_terminal h p q ok after old x (); ()
      | Optimized_unifier_spec.List_children (r, s, rest)
      | Optimized_unifier_spec.Resolve (r, s, _, _, rest) ->
        unified_terminal h r s ok after rest x (); ()
      | Optimized_unifier_spec.Children (a, b, c, e, mid, left_ok, left, right) ->
        if left_ok then (
          unified_terminal mid b e ok after right x ();
          unified_terminal h a c left_ok mid left x (); ())
        else (unified_terminal h a c left_ok mid left x (); ())
      | Optimized_unifier_spec.Post_link (mid, rest, source, target) ->
        let s = Level_finite_spec.tree_root source in
        let t = Level_finite_spec.tree_root target in
        redirect_terminal mid s t x ();
        unified_terminal h p q true mid rest x (); ()
      | Optimized_unifier_spec.Pre_compress (mid, edits, rest) ->
        unified_terminal mid p q ok after rest x ();
        compression_terminal h mid edits x (); ())

let (coverage @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : Optimized_unifier_spec.derivation) @ immutable ->
    (cut : int) -> (pool : Generalize_spec.pool) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Optimized_unifier_spec.unified h p q ok after d
      && Representative_level.representative_covered h cut pool x} ->
    {u : unit | Representative_level.representative_covered after cut pool x} @ ghost =
  fun h p q ok after d cut pool x premise -> ghost_ (
    Representative_level.representative_covered_def h cut pool x;
    Representative_level.representative_covered_def after cut pool x;
    if terminal after x then (
      unified_terminal h p q ok after d x ();
      Optimized_metadata.unified_frame h p q ok after d x ();
      Optimized_metadata.unified_scratch h p q ok after d x ();
      Level_unifier_metadata.scratch_frame_def h after x;
      Generalize_spec.covered_def h cut pool x;
      Generalize_spec.covered_def after cut pool x;
      at_level_def h x; at_level_def after x;
      (match H.at h x, H.at after x with
      | Some a, Some b -> decreases_def a.level b.level; () | _ -> ());
      ())
    else ())

let rec (redirect_head @ total) : (h : node Pref.heap) @ immutable ->
    (source : node Pref.t) @ immutable -> (target : node Pref.t) @ immutable ->
    (p : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable ->
    (path : resolution) @ immutable ->
    {u : unit | H.mem h source && terminal h source && H.mem h target
      && terminal h target && not (source === target) && resolves h p root path} ->
    {d : resolution | resolves (H.put h source (redirect h source target)) p
      (if root === source then target else root) d} @ immutable ghost =
  fun h source target p root path premise -> ghost_ (
    let v = redirect h source target in
    let after = H.put h source v in let next = if root === source then target else root in
    resolves_def h p root path; terminal_def h source;
    Level_unifier_proofs.observe_write h source v p;
    match path with
    | Here ->
      if root === source then (
        Level_unifier_proofs.observe_write h source v target;
        Level_unifier_proofs.redirect_desc h source target;
        terminal_def h target; terminal_def after target;
        let here = Here in let d = Via (target, here) in
        resolves_def after target target here; resolves_def after p next d; d)
      else (
        terminal_def h p; terminal_def after p;
        let d = Here in resolves_def after p next d; d)
    | Via (q, rest) ->
      let tail = redirect_head h source target q root rest () in
      let d = Via (q, tail) in resolves_def after p next d; d)

let (redirect_low @ total) : (h : node Pref.heap) @ immutable ->
    (source : node Pref.t) @ immutable -> (target : node Pref.t) @ immutable ->
    (p : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable ->
    (path : resolution) @ immutable -> (bound : int) ->
    {u : unit | H.mem h source && terminal h source && H.mem h target
      && terminal h target && not (source === target) && resolves h p root path
      && below h root bound && match at_level h source with
      | Generic -> false | Finite n -> below h target n} ->
    {d : resolution | let after = H.put h source (redirect h source target) in
      let next = if root === source then target else root in
      resolves after p next d && below after next bound} @ immutable ghost =
  fun h source target p root path bound premise -> ghost_ (
    let d = redirect_head h source target p root path () in
    let v = redirect h source target in let after = H.put h source v in
    let next = if root === source then target else root in
    below_def h root bound; below_def after next bound;
    at_level_def after next; at_level_def h root;
    at_level_def h source; at_level_def h target;
    (match at_level h source with Generic -> () | Finite n -> below_def h target n; ());
    d)
