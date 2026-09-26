module M = Vox_union_find_model
module F = Vox_union_find_forest
module R = Vox_union_find_rank
module D = Vox_union_find_mass
module P = Ghost_pref
module H = P.Heap

let[@def] admissible (cap : Bigint.t) (h : Vox_union_find_model.node P.heap @ immutable)
    (x : M.elem @ immutable) (y : M.elem @ immutable)
    (paths : M.path list @ immutable) = ghost_ (
  F.valid h paths && R.all_ordered cap h paths &&
  M.is_root h x && M.is_root h y && M.rank h x + 1 >= 0 &&
  F.member x paths && F.member y paths &&
  D.mass h paths <= Bigint.sub (F.size paths) (D.components h paths) &&
  F.size paths <= cap)

let (at @ total) : (h : Vox_union_find_model.node P.heap) @ immutable -> (x : M.elem) @ immutable ->
    (y : M.elem) @ immutable -> (q : M.elem) @ immutable ->
    {u : unit | H.at (M.linked h x y) q ===
      (if x === y then H.at h q
       else if q === (if M.rank h x < M.rank h y then x else y) then
         Some (M.Link (M.rank h q, M.winner h x y))
       else if q === x && M.rank h x = M.rank h y then
         Some (M.Root (M.rank h x + 1))
       else H.at h q)} @ ghost = fun h x y q -> ghost_ (
  M.linked_def h x y; M.winner_def h x y;
  if x === y then ()
  else if M.rank h x < M.rank h y then (
    let _ = H.at (H.put h x (M.Link (M.rank h x, y))) q in
    ())
  else if M.rank h y < M.rank h x then (
    let _ = H.at (H.put h y (M.Link (M.rank h y, x))) q in
    ())
  else (
    let middle = H.put h y (M.Link (M.rank h y, x)) in
    let _ = H.at middle q in
    let _ = H.at (H.put middle x (M.Root (M.rank h x + 1))) q in
    ()))

let rec (ordered @ total) : (cap : Bigint.t) -> (h : Vox_union_find_model.node P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
    (paths : M.path list) @ immutable -> (p : M.path) @ immutable ->
    {u : unit | if admissible cap h x y paths && M.valid h p &&
      R.ordered cap h p && F.closed paths p then
      R.ordered cap (M.linked h x y) (M.joined_path h x y p) else true} @ ghost =
    fun cap h x y paths p -> ghost_ (
  admissible_def cap h x y paths;
  M.valid_def h p; R.ordered_def cap h p; F.closed_def paths p;
  M.head_def p; M.joined_path_def h x y p;
  M.is_root_def h x; M.is_root_def h y;
  M.rank_def h x; M.rank_def h y;
  R.weight_def h x; R.weight_def h y;
  let after = M.linked h x y in
  D.linked_capacity cap h x y paths (M.head p);
  D.linked_weight h x y (M.head p);
  if x === y then (
    M.linked_def h x y; ())
  else (
    let loser = if M.rank h x < M.rank h y then x else y in
    let winner = if M.rank h x < M.rank h y then y else x in
    M.extend_def loser winner p;
    match p with
    | M.Stop q ->
        if q === loser then (
          D.linked_capacity cap h x y paths winner;
          D.linked_weight h x y winner;
          R.ordered_def cap after (M.Step (q, M.Stop winner));
          M.head_def (M.Step (q, M.Stop winner));
          R.ordered_def cap after (M.Stop winner); M.head_def (M.Stop winner);
          ())
        else (
          R.ordered_def cap after p;
          ())
    | M.Step (q, rest) ->
        ordered cap h x y paths rest;
        M.joined_valid h x y rest; M.joined_path_def h x y rest;
        D.linked_weight h x y (M.head rest);
        R.ordered_def cap after (M.Step (q, M.extend loser winner rest));
        M.head_def (M.Step (q, M.extend loser winner rest));
        ()))

let rec (all_ordered @ total) : (cap : Bigint.t) -> (h : Vox_union_find_model.node P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
    (paths : M.path list) @ immutable -> (queries : M.path list) @ immutable ->
    {u : unit | if admissible cap h x y paths && F.valid h queries &&
      R.all_ordered cap h queries && F.complete paths queries then
      R.all_ordered cap (M.linked h x y) (F.join h x y queries) else true}
      @ ghost = fun cap h x y paths queries -> ghost_ (
  F.valid_def h queries; R.all_ordered_def cap h queries;
  F.complete_def paths queries; F.join_def h x y queries;
  let after = M.linked h x y in
  match queries with
  | [] -> R.all_ordered_def cap after []; ()
  | p :: rest ->
      ordered cap h x y paths p; all_ordered cap h x y paths rest;
      R.all_ordered_def cap after (M.joined_path h x y p :: F.join h x y rest);
      ())
