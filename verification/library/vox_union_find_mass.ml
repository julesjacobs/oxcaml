module M = Vox_union_find_model
module F = Vox_union_find_forest
module R = Vox_union_find_rank
module P = Ghost_pref
module H = P.Heap

let[@def] rec mass (h : Vox_union_find_model.node P.heap @ immutable) (paths : M.path list @ immutable) =
  ghost_ (match paths with [] -> 0Z | p :: rest ->
    Bigint.add (R.weight h (M.head p)) (mass h rest))
let[@def] rec components (h : Vox_union_find_model.node P.heap @ immutable)
    (paths : M.path list @ immutable) = ghost_ (match paths with
  | [] -> 0Z
  | p :: rest -> Bigint.add (if M.is_root h (M.head p) then 1Z else 0Z)
      (components h rest))

let rec (mass_bounds @ total) : (cap : Bigint.t) -> (h : Vox_union_find_model.node P.heap) @ immutable ->
    (paths : M.path list) @ immutable -> (x : M.elem) @ immutable ->
    {u : unit | if R.all_ordered cap h paths then mass h paths >= 0Z &&
      (if F.member x paths then R.weight h x <= mass h paths else true)
      else true} @ ghost = fun cap h paths x -> ghost_ (
  R.all_ordered_def cap h paths; mass_def h paths; F.member_def x paths;
  (match paths with
  | [] -> ()
  | p :: rest -> R.bounds cap h p; mass_bounds cap h rest x);
  let u = () in refine_ u)

let rec (component_bounds @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (paths : M.path list) @ immutable -> (x : M.elem) @ immutable ->
    {u : unit | 0Z <= components h paths && components h paths <= F.size paths &&
      (if F.member x paths && M.is_root h x then 1Z <= components h paths
       else true)} @ ghost = fun h paths x -> ghost_ (
  components_def h paths; F.member_def x paths; F.size_def paths;
  (match paths with [] -> () | _ :: rest -> component_bounds h rest x);
  let u = () in refine_ u)

let rec (compressed_root @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (p : M.path) @ immutable -> (x : M.elem) @ immutable ->
    {u : unit | if M.valid h p then
      M.is_root (M.compressed h p) x = M.is_root h x else true} @ ghost =
    fun h p x -> ghost_ (
  M.valid_def h p; M.head_def p; M.compressed_def h p;
  M.is_root_def h x;
  let after = M.compressed h p in
  M.is_root_def after x;
  match p with
  | M.Stop _ -> let u = () in refine_ u
  | M.Step (y, rest) ->
      compressed_root h rest x;
      let middle = M.compressed h rest in
      M.is_root_def middle x;
      M.is_root_def after x;
      let _ = H.mem (H.put middle y (M.Link (M.rank h y, M.root rest))) x in
      let _ = H.at (H.put middle y (M.Link (M.rank h y, M.root rest))) x in
      if x === y then (let u = () in refine_ u)
      else (let u = () in refine_ u))

let rec (compression @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (p : M.path) @ immutable -> (paths : M.path list) @ immutable ->
    {u : unit | if M.valid h p && F.valid h paths then
      mass (M.compressed h p) (F.refresh p paths) = mass h paths &&
      components (M.compressed h p) (F.refresh p paths) = components h paths
      else true} @ ghost = fun h p paths -> ghost_ (
  F.valid_def h paths; F.refresh_def p paths;
  mass_def h paths; components_def h paths;
  let after = M.compressed h p in
  match paths with
  | [] -> mass_def after []; components_def after []; let u = () in refine_ u
  | q :: rest ->
      M.refresh_valid h p q;
      M.compressed_rank h p (M.head q); compressed_root h p (M.head q);
      R.weight_def after (M.head q); R.weight_def h (M.head q);
      compression h p rest;
      mass_def after (M.refresh p q :: F.refresh p rest);
      components_def after (M.refresh p q :: F.refresh p rest);
      let u = () in refine_ u)

let (linked_weight @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
    (q : M.elem) @ immutable ->
    {u : unit | if M.is_root h x && M.is_root h y && M.rank h x + 1 >= 0 then
      R.weight (M.linked h x y) q = Bigint.add (R.weight h q)
        (if not (x === y) && M.rank h x = M.rank h y && q === x then 1Z else 0Z)
      else true} @ ghost = fun h x y q -> ghost_ (
  M.linked_def h x y;
  M.is_root_def h x; M.is_root_def h y;
  M.rank_def h x; M.rank_def h y; M.rank_def h q;
  M.rank_def (M.linked h x y) q;
  R.weight_def h q; R.weight_def (M.linked h x y) q;
  if x === y then let u = () in refine_ u
  else if M.rank h x < M.rank h y then (
    M.rank_def (H.put h x (M.Link (M.rank h x, y))) q;
    let u = () in refine_ u)
  else if M.rank h y < M.rank h x then (
    M.rank_def (H.put h y (M.Link (M.rank h y, x))) q;
    let u = () in refine_ u)
  else (
    let middle = H.put h y (M.Link (M.rank h y, x)) in
    M.rank_def middle q;
    M.rank_def (H.put middle x (M.Root (M.rank h x + 1))) q;
    let u = () in refine_ u))

let (linked_root @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
    (q : M.elem) @ immutable ->
    {u : unit | if M.is_root h x && M.is_root h y && M.rank h x + 1 >= 0 then
      M.is_root (M.linked h x y) q = (M.is_root h q &&
        (x === y || not (q === (if M.rank h x < M.rank h y then x else y))))
      else true} @ ghost = fun h x y q -> ghost_ (
  M.linked_def h x y;
  M.is_root_def h x; M.is_root_def h y; M.is_root_def h q;
  M.is_root_def (M.linked h x y) q;
  M.rank_def h x; M.rank_def h y;
  if x === y then let u = () in refine_ u
  else if M.rank h x < M.rank h y then (
    let after = H.put h x (M.Link (M.rank h x, y)) in
    M.is_root_def after q;
    let _ = H.mem after q in let _ = H.at after q in
    let u = () in refine_ u)
  else if M.rank h y < M.rank h x then (
    let after = H.put h y (M.Link (M.rank h y, x)) in
    M.is_root_def after q;
    let _ = H.mem after q in let _ = H.at after q in
    let u = () in refine_ u)
  else (
    let middle = H.put h y (M.Link (M.rank h y, x)) in
    let after = H.put middle x (M.Root (M.rank h x + 1)) in
    M.is_root_def middle q; M.is_root_def after q;
    let _ = H.mem middle q in let _ = H.at middle q in
    let _ = H.mem after q in let _ = H.at after q in
    let u = () in refine_ u))

let rec (link_sums @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
    (paths : M.path list) @ immutable ->
    {u : unit | if M.is_root h x && M.is_root h y && M.rank h x + 1 >= 0 &&
      F.valid h paths then
      mass (M.linked h x y) (F.join h x y paths) = Bigint.add (mass h paths)
        (if not (x === y) && M.rank h x = M.rank h y && F.member x paths
         then 1Z else 0Z) &&
      components (M.linked h x y) (F.join h x y paths) =
        Bigint.sub (components h paths)
          (if not (x === y) &&
            F.member (if M.rank h x < M.rank h y then x else y) paths
           then 1Z else 0Z) else true} @ ghost = fun h x y paths -> ghost_ (
  F.valid_def h paths; F.join_def h x y paths;
  F.member_def x paths;
  let loser = if M.rank h x < M.rank h y then x else y in
  F.member_def loser paths;
  mass_def h paths; components_def h paths;
  let after = M.linked h x y in
  match paths with
  | [] -> mass_def after []; components_def after []; let u = () in refine_ u
  | p :: rest ->
      M.joined_valid h x y p; link_sums h x y rest;
      linked_weight h x y (M.head p); linked_root h x y (M.head p);
      M.is_root_def h x; M.is_root_def h y;
      mass_def after (M.joined_path h x y p :: F.join h x y rest);
      components_def after (M.joined_path h x y p :: F.join h x y rest);
      let u = () in refine_ u)

let rec (fresh_sums @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (paths : M.path list) @ immutable -> (x : M.elem) @ immutable ->
    {u : unit | if F.valid h paths && not (H.mem h x) then
      mass (H.put h x (M.Root 0)) paths = mass h paths &&
      components (H.put h x (M.Root 0)) paths = components h paths else true}
      @ ghost = fun h paths x -> ghost_ (
  F.valid_def h paths;
  let after = H.put h x (M.Root 0) in
  mass_def h paths; mass_def after paths;
  components_def h paths; components_def after paths;
  (match paths with
  | [] -> ()
  | p :: rest ->
      M.valid_def h p; R.fresh_weight h x (M.head p);
      M.is_root_def h (M.head p); M.is_root_def after (M.head p);
      let _ = H.mem after (M.head p) in
      fresh_sums h rest x);
  let u = () in refine_ u)

let (allocate_sums @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (paths : M.path list) @ immutable -> (x : M.elem) @ immutable ->
    {u : unit | if F.valid h paths && not (H.mem h x) then
      mass (H.put h x (M.Root 0)) (M.Stop x :: paths) = mass h paths &&
      components (H.put h x (M.Root 0)) (M.Stop x :: paths) =
        Bigint.add (components h paths) 1Z else true} @ ghost =
    fun h paths x -> ghost_ (
  fresh_sums h paths x;
  let after = H.put h x (M.Root 0) in
  mass_def after (M.Stop x :: paths); components_def after (M.Stop x :: paths);
  M.head_def (M.Stop x); R.weight_def after x; M.rank_def after x;
  M.is_root_def after x; let _ = H.mem after x in
  let u = () in refine_ u)

let rec (linked_mass_bounds @ total) : (cap : Bigint.t) ->
    (h : Vox_union_find_model.node P.heap) @ immutable -> (x : M.elem) @ immutable ->
    (y : M.elem) @ immutable -> (paths : M.path list) @ immutable ->
    (q : M.elem) @ immutable ->
    {u : unit | if F.valid h paths && R.all_ordered cap h paths &&
      M.is_root h x && M.is_root h y && M.rank h x + 1 >= 0 then
      mass (M.linked h x y) (F.join h x y paths) >= 0Z &&
      (if F.member q paths then R.weight (M.linked h x y) q <=
        mass (M.linked h x y) (F.join h x y paths) else true) else true} @ ghost =
    fun cap h x y paths q -> ghost_ (
  F.valid_def h paths; R.all_ordered_def cap h paths;
  F.join_def h x y paths; F.member_def q paths;
  let after = M.linked h x y in
  match paths with
  | [] -> mass_def after []; let u = () in refine_ u
  | p :: rest ->
      R.bounds cap h p; M.joined_valid h x y p;
      linked_weight h x y (M.head p);
      linked_mass_bounds cap h x y rest q;
      mass_def after (M.joined_path h x y p :: F.join h x y rest);
      let u = () in refine_ u)

let (linked_capacity @ total) : (cap : Bigint.t) ->
    (h : Vox_union_find_model.node P.heap) @ immutable -> (x : M.elem) @ immutable ->
    (y : M.elem) @ immutable -> (paths : M.path list) @ immutable ->
    (q : M.elem) @ immutable ->
    {u : unit | if F.valid h paths && R.all_ordered cap h paths &&
      M.is_root h x && M.is_root h y && M.rank h x + 1 >= 0 &&
      F.member x paths && F.member y paths && F.member q paths &&
      mass h paths <= Bigint.sub (F.size paths) (components h paths) &&
      F.size paths <= cap then R.weight (M.linked h x y) q < cap else true}
      @ ghost = fun cap h x y paths q -> ghost_ (
  link_sums h x y paths; linked_mass_bounds cap h x y paths q;
  F.join_valid h x y paths;
  M.winner_def h x y;
  let winner = M.winner h x y in
  F.join_member h x y paths winner;
  linked_root h x y winner;
  component_bounds (M.linked h x y) (F.join h x y paths) winner;
  let u = () in refine_ u)
