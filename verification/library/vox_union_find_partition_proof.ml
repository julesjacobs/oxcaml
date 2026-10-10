module M = Vox_union_find_model
module F = Vox_union_find_forest
module S = Vox_union_find_spec
module Q = Vox_partition
module P = Ghost_pref

let[@def] rec project (paths : M.path list @ immutable) :
    M.elem Q.bindings @ immutable ghost = ghost_ (
  match paths with
  | [] -> []
  | p :: rest -> (M.head p, M.root p) :: project rest)

let rec (lookup @ total) : (paths : M.path list) @ immutable ->
    (x : M.elem) @ immutable ->
    {u : unit | Q.lookup (project paths) x ===
      (if F.member x paths then Some (F.representative x paths) else None)}
      @ ghost = fun paths x -> ghost_ (
  project_def paths; F.member_def x paths; F.representative_def x paths;
  F.lookup_def x paths; Q.lookup_def (project paths) x;
  (match paths with
  | [] -> ()
  | _ :: rest ->
      lookup rest x; F.representative_def x rest);
  ())

let rec (absent_root @ total) : (paths : M.path list) @ immutable ->
    (x : M.elem) @ immutable ->
    {u : unit | if not (F.member x paths) then
      F.representative x paths === x else true} @ ghost =
    fun paths x -> ghost_ (
  F.member_def x paths; F.representative_def x paths; F.lookup_def x paths;
  (match paths with
  | [] -> M.root_def (M.Stop x)
  | _ :: rest -> absent_root rest x; F.representative_def x rest);
  ())

let (contains @ total) : (paths : M.path list) @ immutable ->
    (x : M.elem) @ immutable ->
    {u : unit | Q.contains (project paths) x = F.member x paths}
      @ ghost = fun paths x -> ghost_ (
  lookup paths x; Q.contains_def (project paths) x; ())

let (root @ total) : (paths : M.path list) @ immutable ->
    (x : M.elem) @ immutable ->
    {u : unit | Q.representative (project paths) x === F.representative x paths}
      @ ghost = fun paths x -> ghost_ (
  lookup paths x; absent_root paths x; Q.representative_def (project paths) x;
    ())

let rec (size @ total) : (paths : M.path list) @ immutable ->
    {u : unit | Q.size (project paths) = F.size paths} @ ghost =
    fun paths -> ghost_ (
  project_def paths; Q.size_def (project paths); F.size_def paths;
  (match paths with [] -> () | _ :: rest -> size rest);
  ())

let rec (unique @ total) : (h : M.node P.heap) @ immutable ->
    (paths : M.path list) @ immutable ->
    {u : unit | if F.valid h paths then Q.unique (project paths) else true}
      @ ghost = fun h paths -> ghost_ (
  F.valid_def h paths; project_def paths; Q.unique_def (project paths);
  (match paths with
  | [] -> ()
  | p :: rest -> contains rest (M.head p); unique h rest);
  ())

let rec (closed @ total) : (h : M.node P.heap) @ immutable ->
    (paths : M.path list) @ immutable ->
    (queries : M.path list) @ immutable ->
    {u : unit | if F.valid h paths && F.valid h queries &&
      F.complete paths queries then
      Q.closed (project paths) (project queries) else true} @ ghost =
    fun h paths queries -> ghost_ (
  F.valid_def h queries; F.complete_def paths queries;
  project_def queries; Q.closed_def (project paths) (project queries);
  (match queries with
  | [] -> ()
  | p :: rest ->
      let r = M.root p in
      F.closed_root paths p; M.terminal h p; M.is_root_def h r;
      F.lookup_valid h r paths;
      M.valid_def h (M.Stop r); M.head_def (M.Stop r);
      M.root_def (M.Stop r);
      M.unique_root h (F.lookup r paths) (M.Stop r);
      F.representative_def r paths;
      contains paths r; root paths r;
      closed h paths rest);
  ())

let (valid @ total) : (h : M.node P.heap) @ immutable ->
    (paths : M.path list) @ immutable ->
    {u : unit | if F.valid h paths && F.complete paths paths then
      Q.valid (project paths) else true} @ ghost = fun h paths -> ghost_ (
  unique h paths; closed h paths paths; Q.valid_def (project paths); ())

let (empty @ total) : unit ->
    {u : unit | project [] === []} @ ghost = fun () -> ghost_ (
  project_def []; ())

let (add_singleton @ total) : (paths : M.path list) @ immutable ->
    (x : M.elem) @ immutable ->
    {u : unit | project (M.Stop x :: paths) ===
      Q.add_singleton (project paths) x} @ ghost = fun paths x -> ghost_ (
  project_def (M.Stop x :: paths); M.head_def (M.Stop x);
  M.root_def (M.Stop x); Q.add_singleton_def (project paths) x; ())

let rec (refresh @ total) : (h : M.node P.heap) @ immutable ->
    (selected : M.path) @ immutable -> (paths : M.path list) @ immutable ->
    {u : unit | if M.valid h selected && F.valid h paths then
      project (F.refresh selected paths) === project paths else true}
      @ ghost = fun h selected paths -> ghost_ (
  F.valid_def h paths; F.refresh_def selected paths; project_def paths;
  (match paths with
  | [] -> project_def []
  | p :: rest ->
      M.refresh_valid h selected p; refresh h selected rest;
      project_def (M.refresh selected p :: F.refresh selected rest));
  ())

let (find @ total) : (h : M.node P.heap) @ immutable ->
    (paths : M.path list) @ immutable -> (x : M.elem) @ immutable ->
    {u : unit | if F.valid h paths && F.member x paths then
      project (S.find_paths paths x) === project paths else true}
      @ ghost = fun h paths x -> ghost_ (
  F.lookup_valid h x paths; refresh h (F.lookup x paths) paths;
  S.find_paths_def paths x; ())

let rec (join @ total) : (h : M.node P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
    (paths : M.path list) @ immutable ->
    {u : unit | if F.valid h paths && M.is_root h x && M.is_root h y &&
      M.rank h x + 1 >= 0 then
      project (F.join h x y paths) ===
        Q.redirect (project paths) x y (M.winner h x y) else true}
      @ ghost = fun h x y paths -> ghost_ (
  F.valid_def h paths; F.join_def h x y paths; project_def paths;
  Q.redirect_def (project paths) x y (M.winner h x y);
  (match paths with
  | [] -> project_def []
  | p :: rest ->
      M.joined_valid h x y p; join h x y rest;
      project_def (M.joined_path h x y p :: F.join h x y rest));
  ())

let (union @ total) : (h : M.node P.heap) @ immutable ->
    (paths : M.path list) @ immutable -> (x : M.elem) @ immutable ->
    (y : M.elem) @ immutable ->
    {u : unit | let first = S.find_paths paths x in
      let middle = S.find_heap h paths x in
      let after = S.find_heap middle first y in
      if F.valid h paths && F.complete paths paths && F.member x paths &&
        F.member y paths && M.rank after (F.representative x paths) + 1 >= 0
          then
        project (S.union_paths h paths x y) ===
          Q.merge_classes (project paths) x y (S.union_root h paths x y)
        else true} @ ghost = fun h paths x y -> ghost_ (
  let px = F.lookup x paths in
  F.lookup_valid h x paths; F.lookup_closed paths paths x;
  F.refresh_valid h px paths; F.refresh_complete h px paths paths;
  F.refresh_member h px paths y;
  F.refresh_representative h px paths y;
  find h paths x;
  S.find_paths_def paths x; S.find_heap_def h paths x;
  let first = S.find_paths paths x in
  let middle = S.find_heap h paths x in
  let py = F.lookup y first in
  F.lookup_valid middle y first;
  F.refresh_valid middle py first;
  find middle first y;
  S.find_paths_def first y; S.find_heap_def middle first y;
  let second = S.find_paths first y in
  let after = S.find_heap middle first y in
  S.find_root h paths x; S.find_root middle first y;
  Vox_union_find_mass.compressed_root middle py (F.representative x paths);
  join after (F.representative x paths) (F.representative y first) second;
  root paths x; root paths y;
  S.union_paths_def h paths x y; S.union_root_def h paths x y;
  Q.merge_classes_def (project paths) x y (S.union_root h paths x y);
  ())
