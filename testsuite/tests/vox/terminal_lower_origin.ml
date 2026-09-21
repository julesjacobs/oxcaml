open Copy_spec
open Copy_heap_proofs
open Level_spec
open Level_proofs
open Generalize_spec
open Lower_locality_spec
open Lower_locality_proofs
open Provenance_spec
open Provenance_proofs
open Leaf_provenance_spec
let rec (bounded_path @ total) : (h : node Pref.heap) @ immutable ->
    (bound : int) -> (tree : bounded) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Terminal_lower_spec.terminal_bounded h bound tree && contains tree x} ->
    {p : path | reaches h (bound_root tree) x p} @ immutable ghost =
  fun h bound tree x premise -> ghost_ (
    let refine_ premise = premise in Terminal_lower_spec.terminal_bounded_def h bound tree;
    contains_def tree x; bound_root_def tree; let root = bound_root tree in
    if root === x then (let p = Stop in reaches_def h root x p; refine_ p)
    else let u = () in match tree with
    | Tip _ -> let p = Stop in reaches_def h root x p; refine_ p
    | Through (_, child) ->
      let next = bound_root child in edge_def h root next;
      let refine_ rest = bounded_path h bound child x (refine_ u) in
      let p = Step (next, rest) in reaches_def h root x p; refine_ p
    | Fork (_, a, b) -> if contains a x then (
      let next = bound_root a in edge_def h root next;
      let refine_ rest = bounded_path h bound a x (refine_ u) in
      let p = Step (next, rest) in reaches_def h root x p; refine_ p)
      else (
      let next = bound_root b in edge_def h root next;
      let refine_ rest = bounded_path h bound b x (refine_ u) in
      let p = Step (next, rest) in reaches_def h root x p; refine_ p))

let rec (lower_path @ total) : (h : node Pref.heap) @ immutable ->
    (bound : int) -> (edits : lowering) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (path : path) @ immutable ->
    {u : unit | Terminal_lower_spec.terminal_valid h bound edits && reaches h p q path} ->
    {u : unit | reaches (lower_heap h bound edits) p q path} @ ghost =
  fun h bound edits p q path premise -> ghost_ (
    let refine_ premise = premise in let after = lower_heap h bound edits in
    reaches_def h p q path; reaches_def after p q path;
    let u = () in match path with Stop -> refine_ u
    | Step (next, rest) -> Terminal_lower_proofs.lowering_at h bound edits p (refine_ u);
      lower_frame_def h after p; edge_def h p next; edge_def after p next;
      lower_path h bound edits next q rest (refine_ u); refine_ u)

let rec (lower_floor @ total) : (h : node Pref.heap) @ immutable ->
    (bound : int) -> (edits : lowering) @ immutable -> (cut : int) ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Terminal_lower_spec.terminal_valid h bound edits && bound > cut &&
      not (below h x cut)} ->
    {u : unit | not (below (lower_heap h bound edits) x cut)} @ ghost =
  fun h bound edits cut x premise -> ghost_ (
    let refine_ premise = premise in Terminal_lower_spec.terminal_valid_def h bound edits;
    lower_heap_def h bound edits; let after = lower_heap h bound edits in
    below_def h x cut; below_def after x cut;
    at_level_def h x; at_level_def after x; let u = () in match edits with
    | Keep -> refine_ u
    | Lower (p, old, rest) ->
      lower_floor h bound rest cut x (refine_ u);
      let mid = lower_heap h bound rest in let v = lower_cell old bound in
      below_def mid x cut; at_level_def mid x; lower_cell_def old bound;
      put_frame mid p v x; refine_ u
    | Sequence (a, b) -> lower_floor h bound a cut x (refine_ u);
      let mid = lower_heap h bound a in
      lower_floor mid bound b cut x (refine_ u); refine_ u)

let (lower_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (bound : int) -> (edits : lowering) @ immutable ->
    (x : node Pref.t) @ immutable -> (origin : origin) @ immutable ->
    {u : unit | Terminal_lower_spec.terminal_valid h bound edits && originates saved h cut x origin} ->
    {u : unit | originates saved (lower_heap h bound edits) cut x origin}
      @ ghost = fun saved h cut bound edits x origin premise -> ghost_ (
    let refine_ premise = premise in let after = lower_heap h bound edits in
    originates_def saved h cut x origin;
    originates_def saved after cut x origin;
    let u = () in match origin with Origin (root, path) ->
      lower_path h bound edits root x path (refine_ u); refine_ u)

let (lower_bind_leaf_origin @ total) : (saved : node Pref.heap) @ immutable ->
    (h : node Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o}
      @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (bound : int) -> (edits : lowering) @ immutable ->
    (tree : bounded) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && Level_unifier_spec.observe h p === Some Var &&
      at_level h p === Finite bound && Terminal_lower_spec.terminal_valid h bound edits &&
      confined edits tree && bound_root tree === q &&
      Terminal_lower_spec.terminal_bounded (lower_heap h bound edits) bound tree} ->
    {o : origin | let mid = lower_heap h bound edits in
      let after = H.put mid p (Level_unifier_spec.redirect mid p q) in
      not (low_var after x cut) || originates saved after cut x o}
      @ immutable ghost = fun saved h cut prior p q bound edits tree x premise -> ghost_ (
    let refine_ premise = premise in let mid = lower_heap h bound edits in
    let after = H.put mid p (Level_unifier_spec.redirect mid p q) in
    Terminal_lower_spec.terminal_valid_def h bound edits;
    let u = () in Terminal_lower_proofs.lowering_at h bound edits p (refine_ u); lower_frame_def h mid p; Level_unifier_spec.observe_def h p; Level_unifier_spec.observe_def mid p;
    Terminal_lower_proofs.lowering_at h bound edits p (refine_ u); lower_frame_def h mid p;
    link_below mid p q cut x (refine_ u);
    low_var_def after x cut; low_var_def h x cut; low_var_def h p cut;
    Terminal_lower_proofs.lowering_at h bound edits x (refine_ u); lower_frame_def h mid x; Level_unifier_spec.observe_def h x; Level_unifier_spec.observe_def mid x;
    let v = Level_unifier_spec.redirect mid p q in
    Level_unifier_proofs.observe_write mid p v x;
    Level_unifier_proofs.redirect_desc mid p q;
    if not (low_var after x cut) then (let o = Origin (x, Stop) in refine_ o)
    else if below h x cut then (
      let refine_ o = prior x in lower_origin saved h cut bound edits x o (refine_ u);
      link_origin saved mid cut p q x o (refine_ u); refine_ o)
    else (
      if bound > cut then (lower_floor h bound edits cut x (refine_ u); ());
      below_def h p cut;
      let refine_ source = prior p in
      lower_origin saved h cut bound edits p source (refine_ u);
      link_origin saved mid cut p q p source (refine_ u);
      originates_def saved after cut p source;
      if not (contains tree x) then (
        confined_frame h bound edits tree x (refine_ u);
        Terminal_lower_proofs.lowering_at h bound edits x (refine_ u); lower_frame_def h mid x;
        below_def h x cut; below_def mid x cut;
        at_level_def h x; at_level_def mid x; ());
      let refine_ tail = bounded_path mid bound tree x (refine_ u) in
      link_path mid p q q x tail (refine_ u);
      let next = Step (q, tail) in reaches_def after p x next;
      edge_def after p q; Level_unifier_spec.redirect_def mid p q;
      Level_unifier_spec.observe_def mid p;
      let v = Level_unifier_spec.redirect mid p q in put_frame mid p v p;
      match source with Origin (root, path) ->
        append_reaches after root p x path next (refine_ u);
        let out = Origin (root, append path next) in
        originates_def saved after cut x out; refine_ out))
