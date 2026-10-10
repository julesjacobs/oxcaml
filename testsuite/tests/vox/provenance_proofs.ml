open Marked_occurs_proofs
open Copy_spec
open Copy_heap_proofs
open Level_spec
open Level_proofs
open Generalize_spec
open Lower_locality_spec
open Lower_locality_proofs
open Provenance_spec

let[@def] rec (append @ total) (a : path @ immutable)
    (b : path @ immutable) = ghost_ (match a with
  | Stop -> b | Step (p, rest) -> Step (p, append rest b))

let rec (append_reaches @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (r : node Pref.t) @ immutable -> (a : path) @ immutable ->
    (b : path) @ immutable ->
    {u : unit | reaches h p q a && reaches h q r b} ->
    {u : unit | reaches h p r (append a b)} @ ghost =
  fun h p q r a b premise -> ghost_ (
    append_def a b;
    reaches_def h p q a; let out = append a b in reaches_def h p r out;
    match a with Stop -> ()
    | Step (next, rest) -> append_reaches h next q r rest b ();
      ())

let rec (bounded_path @ total) : (h : node Pref.heap) @ immutable ->
    (bound : int) -> (tree : bounded) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | bounded h bound tree && contains tree x} ->
    {p : path | reaches h (bound_root tree) x p} @ immutable ghost =
  fun h bound tree x premise -> ghost_ (
    bounded_def h bound tree;
    contains_def tree x; bound_root_def tree; let root = bound_root tree in
    if root === x then (let p = Stop in reaches_def h root x p; p)
    else match tree with
    | Tip _ -> let p = Stop in reaches_def h root x p; p
    | Through (_, child) ->
      let next = bound_root child in edge_def h root next;
      let rest = bounded_path h bound child x () in
      let p = Step (next, rest) in reaches_def h root x p; p
    | Fork (_, a, b) -> if contains a x then (
      let next = bound_root a in edge_def h root next;
      let rest = bounded_path h bound a x () in
      let p = Step (next, rest) in reaches_def h root x p; p)
      else (
      let next = bound_root b in edge_def h root next;
      let rest = bounded_path h bound b x () in
      let p = Step (next, rest) in reaches_def h root x p; p))

let rec (lower_path @ total) : (h : node Pref.heap) @ immutable ->
    (bound : int) -> (edits : lowering) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (path : path) @ immutable ->
    {u : unit | lower_valid h bound edits && reaches h p q path} ->
    {u : unit | reaches (lower_heap h bound edits) p q path} @ ghost =
  fun h bound edits p q path premise -> ghost_ (
    let after = lower_heap h bound edits in
    reaches_def h p q path; reaches_def after p q path;
    match path with Stop -> ()
    | Step (next, rest) -> lowering_at h bound edits p ();
      lower_frame_def h after p; edge_def h p next; edge_def after p next;
      lower_path h bound edits next q rest (); ())

let rec (link_path @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (a : node Pref.t) @ immutable -> (b : node Pref.t) @ immutable ->
    (path : path) @ immutable ->
    {u : unit | Level_unifier_spec.observe h p === Some Var &&
      reaches h a b path} ->
    {u : unit | reaches (H.put h p (Level_unifier_spec.redirect h p q))
      a b path} @ ghost = fun h p q a b path premise -> ghost_ (
    let v = Level_unifier_spec.redirect h p q in let after = H.put h p v in
    reaches_def h a b path; reaches_def after a b path;
    match path with Stop -> ()
    | Step (next, rest) -> Level_unifier_spec.observe_def h p;
      edge_def h a next; edge_def after a next; link_path h p q next b rest (); ())

let rec (lower_floor @ total) : (h : node Pref.heap) @ immutable ->
    (bound : int) -> (edits : lowering) @ immutable -> (cut : int) ->
    (x : node Pref.t) @ immutable ->
    {u : unit | lower_valid h bound edits && bound > cut &&
      not (below h x cut)} ->
    {u : unit | not (below (lower_heap h bound edits) x cut)} @ ghost =
  fun h bound edits cut x premise -> ghost_ (
    lower_valid_def h bound edits;
    lower_heap_def h bound edits; let after = lower_heap h bound edits in
    below_def h x cut; below_def after x cut;
    at_level_def h x; at_level_def after x; match edits with
    | Keep -> ()
    | Lower (p, old, rest) ->
      lower_floor h bound rest cut x ();
      let mid = lower_heap h bound rest in let v = lower_cell old bound in
      below_def mid x cut; at_level_def mid x; lower_cell_def old bound;
      put_frame mid p v x; ()
    | Sequence (a, b) -> lower_floor h bound a cut x ();
      let mid = lower_heap h bound a in
      lower_floor mid bound b cut x (); ())

let (link_below @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (cut : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && Level_unifier_spec.observe h p === Some Var} ->
    {u : unit | below (H.put h p (Level_unifier_spec.redirect h p q)) x cut
      === below h x cut} @ ghost = fun h p q cut x premise -> ghost_ (
    let v = Level_unifier_spec.redirect h p q in let after = H.put h p v in
    Level_unifier_spec.redirect_def h p q;
    Level_unifier_spec.observe_def h p; below_def h x cut; below_def after x cut;
    at_level_def h x; at_level_def after x; ())

let (link_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable -> (origin : origin) @ immutable ->
    {u : unit | Level_unifier_spec.observe h p === Some Var &&
      originates saved h cut x origin} ->
    {u : unit | originates saved
      (H.put h p (Level_unifier_spec.redirect h p q)) cut x origin}
      @ ghost = fun saved h cut p q x origin premise -> ghost_ (
    let after = H.put h p (Level_unifier_spec.redirect h p q) in
    originates_def saved h cut x origin;
    originates_def saved after cut x origin;
    match origin with Origin (root, path) ->
      link_path h p q root x path (); ())

let (lower_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (bound : int) -> (edits : lowering) @ immutable ->
    (x : node Pref.t) @ immutable -> (origin : origin) @ immutable ->
    {u : unit | lower_valid h bound edits && originates saved h cut x origin} ->
    {u : unit | originates saved (lower_heap h bound edits) cut x origin}
      @ ghost = fun saved h cut bound edits x origin premise -> ghost_ (
    let after = lower_heap h bound edits in
    originates_def saved h cut x origin;
    originates_def saved after cut x origin;
    match origin with Origin (root, path) ->
      lower_path h bound edits root x path (); ())

let (lower_bind_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h x cut) || originates saved h cut x o}
      @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (bound : int) -> (edits : lowering) @ immutable ->
    (tree : bounded) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && Level_unifier_spec.observe h p === Some Var &&
      at_level h p === Finite bound && lower_valid h bound edits &&
      confined edits tree && bound_root tree === q &&
      bounded (lower_heap h bound edits) bound tree} ->
    {o : origin | let mid = lower_heap h bound edits in
      let after = H.put mid p (Level_unifier_spec.redirect mid p q) in
      not (below after x cut) || originates saved after cut x o}
      @ immutable ghost = fun saved h cut prior p q bound edits tree x premise -> ghost_ (
    let mid = lower_heap h bound edits in
    let after = H.put mid p (Level_unifier_spec.redirect mid p q) in
    lower_valid_def h bound edits;
    Level_unifier_proofs.lower_observe h bound edits p ();
    lowering_at h bound edits p (); lower_frame_def h mid p;
    link_below mid p q cut x ();
    if not (below after x cut) then (let o = Origin (x, Stop) in o)
    else if below h x cut then (
      let o = prior x in lower_origin saved h cut bound edits x o ();
      link_origin saved mid cut p q x o (); o)
    else (
      if bound > cut then (lower_floor h bound edits cut x (); ());
      below_def h p cut;
      let source = prior p in
      lower_origin saved h cut bound edits p source ();
      link_origin saved mid cut p q p source ();
      originates_def saved after cut p source;
      if not (contains tree x) then (
        confined_frame h bound edits tree x ();
        lowering_at h bound edits x (); lower_frame_def h mid x;
        below_def h x cut; below_def mid x cut;
        at_level_def h x; at_level_def mid x; ());
      let tail = bounded_path mid bound tree x () in
      link_path mid p q q x tail ();
      let next = Step (q, tail) in reaches_def after p x next;
      edge_def after p q; Level_unifier_spec.redirect_def mid p q;
      Level_unifier_spec.observe_def mid p;
      let _ = Level_unifier_spec.redirect mid p q in match source with Origin (root, path) ->
        append_reaches after root p x path next ();
        let out = Origin (root, append path next) in
        originates_def saved after cut x out; out))

let rec (scan_path @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : Level_unifier_spec.marks) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (path : path) @ immutable ->
    {u : unit | Level_unifier_spec.marks_valid h needle d && reaches h p q path} ->
    {u : unit | reaches (Level_unifier_spec.scan_heap h d) p q path} @ ghost =
  fun h needle d p q path premise -> ghost_ (
    let after = Level_unifier_spec.scan_heap h d in
    reaches_def h p q path; reaches_def after p q path;
    match path with Stop -> ()
    | Step (next, rest) -> scan_at h needle d p ();
      edge_def h p next; edge_def after p next;
      scan_path h needle d next q rest (); ())

let rec (unified_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h x cut) || originates saved h cut x o}
      @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : Level_unifier_spec.derivation) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Level_unifier_spec.unified h p q ok after d} ->
    {o : origin | not (below after x cut) || originates saved after cut x o}
      @ immutable ghost = fun saved h cut prior p q ok after d x premise -> ghost_ (
    Level_unifier_spec.unified_def h p q ok after d;
    if not (below after x cut) then (let o = Origin (x, Stop) in o)
    else match d with
    | Same | Constants | Occurs_left _ | Occurs_right _ | Clash ->
      let o = prior x in o
    | Bind_left _ -> link_below h p q cut x ();
      let o = prior x in
      link_origin saved h cut p q x o (); o
    | Bind_right _ -> link_below h q p cut x ();
      let o = prior x in
      link_origin saved h cut q p x o (); o
    | Swap rest ->
      let o = unified_origin saved h cut prior q p ok after rest x () in o
    | Resolve (r, s, _, _, rest) | List_children (r, s, rest) ->
      let o = unified_origin saved h cut prior r s ok after rest x () in o
    | Scanned (needle, marks, rest) ->
      let mid = Level_unifier_spec.scan_heap h marks in
      let middle_prior : ((y : node Pref.t) @ immutable ->
        {o : origin | not (below mid y cut) || originates saved mid cut y o} @ immutable) @ total = fun y ->
        scan_below h needle marks y cut ();
        let o = prior y in originates_def saved h cut y o;
        originates_def saved mid cut y o;
        if below mid y cut then (match o with Origin (root, path) ->
          scan_path h needle marks root y path (); o) else o in
      let o = unified_origin saved mid cut middle_prior p q ok after rest x () in o
    | Lowering (bound, edits, tree, rest) ->
      let mid = lower_heap h bound edits in
      Level_unifier_spec.unified_def mid p q ok after rest;
      (match rest with Bind_left _ ->
        let o = lower_bind_origin saved h cut prior p q bound edits tree x () in o
      | _ -> let o = Origin (x, Stop) in o)
    | Children (a, b, c, e, mid, left_ok, left, right) ->
      if left_ok then (
        let prior1 : ((x : node Pref.t) @ immutable ->
          {o : origin | not (below mid x cut) || originates saved mid cut x o}
          @ immutable) @ total = fun x ->
          let o = unified_origin saved h cut prior a c left_ok mid left x () in o in
        let o = unified_origin saved mid cut prior1 b e ok after right x () in o)
      else (
        let o = unified_origin saved h cut prior a c left_ok mid left x () in o))

let rec (allocation_path @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    (a : node Pref.t) @ immutable -> (b : node Pref.t) @ immutable ->
    (path : path) @ immutable ->
    {u : unit | H.at h p === None && reaches h a b path} ->
    {u : unit | reaches (H.put h p v) a b path} @ ghost =
  fun h p v a b path premise -> ghost_ (
    let after = H.put h p v in
    reaches_def h a b path; reaches_def after a b path;
    match path with Stop -> ()
    | Step (next, rest) -> edge_def h a next; edge_def after a next;
      allocation_path h p v next b rest (); ())

let (allocation_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h x cut) || originates saved h cut x o}
      @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | H.at h p === None &&
      (match v.level with Generic -> true | Finite n -> n > cut)} ->
    {o : origin | not (below (H.put h p v) x cut) ||
      originates saved (H.put h p v) cut x o} @ immutable ghost =
  fun saved h cut prior p v x premise -> ghost_ (
    let after = H.put h p v in
    below_def h x cut; below_def after x cut;
    at_level_def h x; at_level_def after x;
    let o = prior x in
    if below after x cut then (
      originates_def saved h cut x o; originates_def saved after cut x o;
      match o with Origin (root, path) ->
        allocation_path h p v root x path (); o)
    else o)

let (initial_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (cut : int) -> (x : node Pref.t) @ immutable ->
    {o : origin | not (below saved x cut) || originates saved saved cut x o}
      @ immutable ghost = fun saved cut x -> ghost_ (
    let path = Stop in let o = Origin (x, path) in
    reaches_def saved x x path; originates_def saved saved cut x o; o)

let rec (mark_path @ total) : (session : history) @ immutable -> (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (old : node) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (target : node Pref.t) @ immutable ->
    (a : node Pref.t) @ immutable -> (b : node Pref.t) @ immutable ->
    (path : path) @ immutable ->
    {u : unit | H.at h p === Some old && reaches h a b path} ->
    {u : unit | reaches (H.put h p (session_mark session old epoch target)) a b path}
      @ ghost = fun session h p old epoch target a b path premise -> ghost_ (
    let v = session_mark session old epoch target in
    let after = H.put h p v in session_mark_def session old epoch target; mark_def old epoch target;
    reaches_def h a b path; reaches_def after a b path;
    match path with Stop -> ()
    | Step (next, rest) -> edge_def h a next; edge_def after a next;
      mark_path session h p old epoch target next b rest (); ())

let (mark_origin @ total) : (session : history) @ immutable -> (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (p : node Pref.t) @ immutable -> (old : node) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (target : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable -> (origin : origin) @ immutable ->
    {u : unit | H.mem h p && H.at h p === Some old &&
      (not (below h x cut) || originates saved h cut x origin)} ->
    {u : unit | not (below (H.put h p (session_mark session old epoch target)) x cut) ||
      originates saved (H.put h p (session_mark session old epoch target)) cut x origin}
      @ ghost = fun session saved h cut p old epoch target x origin premise -> ghost_ (
    let v = session_mark session old epoch target in
    let after = H.put h p v in session_mark_def session old epoch target; mark_def old epoch target; below_def h x cut; below_def after x cut;
    at_level_def h x; at_level_def after x;
    if below after x cut then (
      originates_def saved h cut x origin;
      originates_def saved after cut x origin;
      match origin with Origin (root, path) ->
        mark_path session h p old epoch target root x path (); ())
    else ())

let rec (copy_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h x cut) || originates saved h cut x o}
      @ immutable)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid h epoch depth d && depth > cut} ->
    {o : origin | not (below (heap h epoch depth d) x cut) ||
      originates saved (heap h epoch depth d) cut x o} @ immutable ghost =
  fun saved h cut scope prior epoch depth d x premise -> ghost_ (
    valid_def h epoch depth d;
    heap_def h epoch depth d; match d with
    | Clean -> let o = prior x in o
    | Start -> scope epoch; let desc : desc = Bool in
      let v = cell desc depth in cell_def desc depth;
      let o = allocation_origin saved h cut prior epoch v x () in o
    | Fresh (rest, p, q, old, desc) -> let mid = heap h epoch depth rest in
      let prior1 : ((x : node Pref.t) @ immutable ->
        {o : origin | not (below mid x cut) || originates saved mid cut x o}
        @ immutable) @ total = fun x -> let o = copy_origin saved h cut scope prior epoch depth rest x () in o in
      Copy_model_proofs.history_scope h scope epoch depth rest q ();
      let v = cell desc depth in cell_def desc depth;
      let o = allocation_origin saved mid cut prior1 q v x () in
      let h1 = H.put mid q v in
      history_grows h epoch depth rest p (); mark_origin rest saved h1 cut p old epoch q x o (); o
    | Alias (rest, p, q, old) -> let mid = heap h epoch depth rest in
      let o = copy_origin saved h cut scope prior epoch depth rest x () in
      history_grows h epoch depth rest p ();
      mark_origin rest saved mid cut p old epoch q x o (); o)

let rec (closed_path @ total) : (h : node Pref.heap) @ immutable ->
    (cut : int) -> (pool : pool) @ immutable ->
    (a : node Pref.t) @ immutable -> (b : node Pref.t) @ immutable ->
    (path : path) @ immutable ->
    {u : unit | pool_scoped h pool && reaches h a b path} ->
    {u : unit | reaches (closed_heap h cut pool) a b path} @ ghost =
  fun h cut pool a b path premise -> ghost_ (
    let after = closed_heap h cut pool in
    reaches_def h a b path; reaches_def after a b path;
    match path with Stop -> ()
    | Step (next, rest) -> Generalize_proofs.closed_observe h cut pool a ();
      closed_at_def h after cut pool a; edge_def h a next; edge_def after a next;
      closed_path h cut pool next b rest (); ())

let (closed_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (origin_cut : int) -> (pool : pool) @ immutable ->
    (x : node Pref.t) @ immutable -> (origin : origin) @ immutable ->
    {u : unit | pool_scoped h pool &&
      (not (below h x origin_cut) || originates saved h origin_cut x origin)} ->
    {u : unit | not (below (closed_heap h cut pool) x origin_cut) ||
      originates saved (closed_heap h cut pool) origin_cut x origin} @ ghost =
  fun saved h cut origin_cut pool x origin premise -> ghost_ (
    let after = closed_heap h cut pool in
    Generalize_proofs.closed_observe h cut pool x ();
    closed_at_def h after cut pool x;
    below_def h x origin_cut; below_def after x origin_cut;
    at_level_def h x; at_level_def after x;
    (match H.at h x with None -> () | Some v -> close_level_def cut v.level; ());
    if below after x origin_cut then (
      originates_def saved h origin_cut x origin;
      originates_def saved after origin_cut x origin;
      match origin with Origin (root, path) ->
        closed_path h cut pool root x path (); ())
    else ())

let (origin_below @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (roots : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below saved x cut) || below h x cut})) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (x : node Pref.t) @ immutable -> (origin : origin) @ immutable ->
    {u : unit | originates saved h cut x origin} ->
    {u : unit | below h x cut} @ ghost = fun saved h cut roots order x origin premise -> ghost_ (
    originates_def saved h cut x origin;
    match origin with Origin (root, path) ->
      roots root; Generalize_proofs.environment_bound h order cut root x path ())

let (nongeneric_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (below h x cut) || originates saved h cut x o}
      @ immutable)) @ total -> (x : node Pref.t) @ immutable ->
    {u : unit | pool_scoped h pool && active h x && covered h cut pool x &&
      not (at_level (closed_heap h cut pool) x === Generic)} ->
    {o : origin | originates saved h cut x o} @ immutable ghost =
  fun saved h cut pool prior x premise -> ghost_ (
    active_def h x;
    Generalize_proofs.closed_level h cut pool x ();
    let level = at_level h x in close_level_def cut level;
    below_def h x cut; let o = prior x in o)

let (generic_excludes_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) -> (pool : pool) @ immutable ->
    (roots : ((x : node Pref.t) @ immutable ->
      {u : unit | not (below saved x cut) || below h x cut})) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (x : node Pref.t) @ immutable -> (origin : origin) @ immutable ->
    {u : unit | pool_scoped h pool && active h x && covered h cut pool x &&
      at_level (closed_heap h cut pool) x === Generic} ->
    {u : unit | not (originates saved h cut x origin)} @ ghost =
  fun saved h cut pool roots order x origin premise -> ghost_ (
    active_def h x;
    Generalize_proofs.closed_level h cut pool x ();
    let level = at_level h x in close_level_def cut level;
    if originates saved h cut x origin then (
      origin_below saved h cut roots order x origin ();
      below_def h x cut; ())
    else ())

let (unified_below @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : Level_unifier_spec.derivation) @ immutable ->
    (cut : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | Level_unifier_spec.unified h p q ok after d && below h x cut} ->
    {u : unit | below after x cut} @ ghost = fun h p q ok after d cut x premise -> ghost_ (
    Level_unifier_proofs.unified_frame h p q ok after d x ();
    Level_unifier_metadata.unified_scratch h p q ok after d x ();
    Level_unifier_metadata.scratch_frame_def h after x;
    below_def h x cut; below_def after x cut;
    at_level_def h x; at_level_def after x;
    (match H.at h x, H.at after x with
    | Some a, Some b -> decreases_def a.level b.level; () | _ -> ()); ())
