open Copy_spec
open Level_spec
open Level_unifier_spec
open Generalize_spec
open Hm_effective_runtime
module E = Effective_level
module R = Representative_level

let (saved_level @ total) : (h : node Pref.heap) @ immutable ->
    (a : E.heads) @ total -> (b : E.heads) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p) && H.mem h x && E.valid_head h a x
      && E.valid_head (H.put h p v) b x} ->
    {u : unit | E.level (H.put h p v) b x === E.level h a x} @ ghost =
  fun h a b p v x premise -> ghost_ (
    let after = H.put h p v in
    let frame : ((y : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h y) || (H.mem after y
        && observe h y === observe after y)}) @ total = fun y ->
      observe_def h y; observe_def after y; () in
    E.valid_head_def h a x; E.valid_head_def after b x;
    E.level_def h a x; E.level_def after b x;
    let old = a x in let next = b x in E.head_terminal h a x ();
    Effective_copy_metadata.resolution_grows h after frame x
      old.root old.path ();
    R.unique after x old.root old.path next.root next.path ();
    at_level_def h old.root; at_level_def after old.root; ())

let (saved_below @ total) : (h : node Pref.heap) @ immutable ->
    (a : E.heads) @ total -> (b : E.heads) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | not (H.mem h p) && E.effective_below h a x bound
      && E.valid_head h a x && E.valid_head (H.put h p v) b x} ->
    {u : unit | E.effective_below (H.put h p v) b x bound} @ ghost =
  fun h a b p v x bound premise -> ghost_ (
    let after = H.put h p v in
    E.effective_below_def h a x bound;
    E.effective_below_def after b x bound;
    saved_level h a b p v x ();
    ())

let (allocate_runtime @ total) : (h : node Pref.heap) @ immutable ->
    (a : E.heads) @ total -> (b : E.heads) @ total ->
    (depth : int) -> (pool : pool) @ immutable ->
    (p : node Pref.t) @ immutable -> (desc : desc) @ immutable ->
    (va : ((x : node Pref.t) @ immutable ->
      {u : unit | E.valid_head h a x})) @ total ->
    (vb : ((x : node Pref.t) @ immutable ->
      {u : unit | E.valid_head (H.put h p (cell desc depth)) b x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p) && depth >= 0
      && (match desc with Var | Bool | Word -> true | Link _ -> false
        | List l -> E.effective_below h a l depth
        | Arrow (l, r) -> E.effective_below h a l depth
          && E.effective_below h a r depth)
      && runtime_at h a depth pool x} ->
    {u : unit | runtime_at (H.put h p (cell desc depth)) b depth
      (Entry (p, pool)) x} @ ghost =
  fun h a b depth pool p desc va vb x premise -> ghost_ (
    let v = cell desc depth in
    cell_def desc depth; payload_scoped_def h v;
    let after = H.put h p v in let next = Entry (p, pool) in
    runtime_at_def h a depth pool x; safe_def h a x;
    depth_bound_def h a depth x;
    (match desc with Var | Bool | Word | Link _ -> ()
    | List l -> E.effective_below_def h a l depth; va l; vb l; saved_below h a b p v l depth (); ()
    | Arrow (l, r) ->
      E.effective_below_def h a l depth;
      E.effective_below_def h a r depth;
      va l; va r; vb l; vb r;
      saved_below h a b p v l depth ();
      saved_below h a b p v r depth (); ());
    Pooled_allocation_proofs.allocation_source h p v x ();
    vb x;
    E.effective_ordered_def h a x; E.effective_ordered_def after b x;
    if H.mem h x then (
      saved_level h a b p v x ();
      (match H.at h x with
      | Some {desc = List l; level = Finite n; _} -> va l; vb l; saved_below h a b p v l n (); ()
      | Some {desc = Arrow (l, r); level = Finite n; _} ->
        va l; va r; vb l; vb r;
        saved_below h a b p v l n ();
        saved_below h a b p v r n (); ()
      | _ -> ()); ())
    else if x === p then (
      terminal_def after p; observe_def after p;
      E.terminal_level after b p ();
      at_level_def after p; ()) else ();
    E.effective_below_def h a x depth;
    E.effective_below_def after b x depth;
    let cut = depth - 1 in
    R.representative_covered_def h cut pool x;
    R.representative_covered_def after cut next x;
    terminal_def h x; terminal_def after x;
    observe_def h x; observe_def after x;
    covered_def h cut pool x; covered_def after cut next x;
    listed_def next x; at_level_def h x; at_level_def after x;
    runtime_at_def after b depth next x; safe_def after b x;
    depth_bound_def after b depth x; ())

let (allocated_below @ total) : (h : node Pref.heap) @ immutable ->
    (depth : int) -> (p : node Pref.t) @ immutable -> (desc : desc) @ immutable ->
    (heads : E.heads) @ total ->
    {u : unit | Hm_effective_execution_spec.allocated h depth p desc
      && (match desc with Link _ -> false | _ -> true)
      && E.valid_head (H.put h p (cell desc depth)) heads p} ->
    {u : unit | E.effective_below (H.put h p (cell desc depth)) heads p depth} @ ghost =
  fun h depth p desc heads premise -> ghost_ (
    Hm_effective_execution_spec.allocated_def h depth p desc;
    let v = cell desc depth in let after = H.put h p v in
    cell_def desc depth; terminal_def after p; observe_def after p;
    E.terminal_level after heads p ();
    at_level_def after p; E.effective_below_def after heads p depth; ())
