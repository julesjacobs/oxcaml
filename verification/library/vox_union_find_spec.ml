module M = Vox_union_find_model
module F = Vox_union_find_forest
module D = Vox_union_find_mass
module P = Ghost_pref

let[@def] find_paths (paths : M.path list @ immutable) (x : M.elem @ immutable) =
  ghost_ (F.refresh (F.lookup x paths) paths)
let[@def] find_heap (h : P.heap @ immutable) (paths : M.path list @ immutable)
    (x : M.elem @ immutable) = ghost_ (M.compressed h (F.lookup x paths))
let[@def] union_paths (h : P.heap @ immutable) (paths : M.path list @ immutable)
    (x : M.elem @ immutable) (y : M.elem @ immutable) = ghost_ (
  let first = find_paths paths x in
  let middle = find_heap h paths x in
  let second = find_paths first y in
  let after = find_heap middle first y in
  F.join after (F.representative x paths) (F.representative y first) second)
let[@def] union_heap (h : P.heap @ immutable) (paths : M.path list @ immutable)
    (x : M.elem @ immutable) (y : M.elem @ immutable) = ghost_ (
  let first = find_paths paths x in
  let middle = find_heap h paths x in
  let after = find_heap middle first y in
  M.linked after (F.representative x paths) (F.representative y first))
let[@def] union_root (h : P.heap @ immutable) (paths : M.path list @ immutable)
    (x : M.elem @ immutable) (y : M.elem @ immutable) = ghost_ (
  let first = find_paths paths x in
  let middle = find_heap h paths x in
  let after = find_heap middle first y in
  M.winner after (F.representative x paths) (F.representative y first))

let (find_root @ total) : (h : P.heap) @ immutable ->
    (paths : M.path list) @ immutable -> (x : M.elem) @ immutable ->
    {u : unit | if F.valid h paths && F.complete paths paths && F.member x paths then
      M.is_root (find_heap h paths x) (F.representative x paths) &&
      F.member (F.representative x paths) (find_paths paths x) else true} @ ghost =
    fun h paths x -> ghost_ (
  let p = F.lookup x paths in
  F.lookup_valid h x paths; F.lookup_closed paths paths x;
  F.closed_root paths p; M.terminal h p;
  D.compressed_root h p (M.root p); F.refresh_member h p paths (M.root p);
  F.representative_def x paths; find_heap_def h paths x; find_paths_def paths x;
  let u = () in refine_ u)

let (union_representative @ total) : (h : P.heap) @ immutable ->
    (paths : M.path list) @ immutable -> (x : M.elem) @ immutable ->
    (y : M.elem) @ immutable -> (q : M.elem) @ immutable ->
    {u : unit | let first = find_paths paths x in
      let middle = find_heap h paths x in
      let after = find_heap middle first y in
      if F.valid h paths && F.complete paths paths && F.member x paths &&
        F.member y paths && F.member q paths &&
        M.rank after (F.representative x paths) + 1 >= 0 then
        F.representative q (union_paths h paths x y) ===
          (if F.representative q paths === F.representative x paths ||
              F.representative q paths === F.representative y paths then
            union_root h paths x y else F.representative q paths) else true}
      @ ghost = fun h paths x y q -> ghost_ (
  let p = F.lookup x paths in
  F.lookup_valid h x paths; F.lookup_closed paths paths x;
  F.refresh_valid h p paths; F.refresh_complete h p paths paths;
  F.refresh_member h p paths y; F.refresh_member h p paths q;
  F.refresh_representative h p paths y; F.refresh_representative h p paths q;
  find_paths_def paths x; find_heap_def h paths x;
  let first = find_paths paths x in let middle = find_heap h paths x in
  find_root h paths x;
  let py = F.lookup y first in
  F.lookup_valid middle y first; F.lookup_closed first first y;
  F.refresh_valid middle py first; F.refresh_member middle py first q;
  F.refresh_representative middle py first q;
  find_paths_def first y; find_heap_def middle first y;
  let second = find_paths first y in let after = find_heap middle first y in
  find_root middle first y;
  D.compressed_root middle py (F.representative x paths);
  F.join_representative after (F.representative x paths)
    (F.representative y first) second q;
  union_paths_def h paths x y; union_root_def h paths x y;
  let u = () in refine_ u)
