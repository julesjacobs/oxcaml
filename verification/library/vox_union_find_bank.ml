module M = Vox_union_find_model
module R = Vox_union_find_rank
module F = Vox_union_find_forest
module A = Vox_ackermann
module V = Vox_union_find_potential
module P = Ghost_pref
module H = P.Heap

let[@def] value (cap : Bigint.t) (alpha : Bigint.t)
    (h : Vox_union_find_model.node P.heap @ immutable) (x : M.elem @ immutable) = ghost_ (
  match H.at h x with
  | Some (M.Root r) -> Bigint.mul alpha (Bigint.of_int r)
  | Some (M.Link (r, parent)) -> V.node_phi cap alpha (Bigint.of_int r)
      (R.weight h parent)
  | None -> 0Z)
let[@def] rec potential (cap : Bigint.t) (alpha : Bigint.t)
    (h : Vox_union_find_model.node P.heap @ immutable) (paths : M.path list @ immutable) = ghost_ (
  match paths with [] -> 0Z | p :: rest ->
    Bigint.add (value cap alpha h (M.head p)) (potential cap alpha h rest))

let (redirect_value @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (h : Vox_union_find_model.node P.heap) @ immutable -> (x : M.elem) @ immutable ->
    (r : M.elem) @ immutable -> (q : M.elem) @ immutable ->
    {u : unit | let after = H.put h x (M.Link (M.rank h x, r)) in
      if not (x === q) then value cap alpha after q = value cap alpha h q
      else value cap alpha after q =
        V.node_phi cap alpha (R.weight h x) (R.weight h r)} @ ghost =
    fun cap alpha h x r q -> ghost_ (
  let after = H.put h x (M.Link (M.rank h x, r)) in
  value_def cap alpha after q; value_def cap alpha h q;
  R.weight_def h x; R.redirect_weight h x r r;
  (match H.at h q with
  | Some (M.Link (_, parent)) -> R.redirect_weight h x r parent
  | _ -> ());
  let u = () in refine_ u)

let rec (redirect_potential @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (h : Vox_union_find_model.node P.heap) @ immutable -> (paths : M.path list) @ immutable ->
    (x : M.elem) @ immutable -> (r : M.elem) @ immutable ->
    {u : unit | if F.valid h paths then
      potential cap alpha (H.put h x (M.Link (M.rank h x, r))) paths =
        Bigint.add (potential cap alpha h paths)
          (if F.member x paths then
            Bigint.sub (V.node_phi cap alpha (R.weight h x) (R.weight h r))
              (value cap alpha h x) else 0Z) else true} @ ghost =
    fun cap alpha h paths x r -> ghost_ (
  F.valid_def h paths; F.member_def x paths;
  let after = H.put h x (M.Link (M.rank h x, r)) in
  potential_def cap alpha h paths; potential_def cap alpha after paths;
  (match paths with
  | [] -> ()
  | p :: rest ->
      redirect_potential cap alpha h rest x r;
      redirect_value cap alpha h x r (M.head p));
  let u = () in refine_ u)

let rec (refresh_potential @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (h : Vox_union_find_model.node P.heap) @ immutable -> (selected : M.path) @ immutable ->
    (paths : M.path list) @ immutable -> (observed : Vox_union_find_model.node P.heap) @ immutable ->
    {u : unit | if M.valid h selected && F.valid h paths then
      potential cap alpha observed (F.refresh selected paths) =
        potential cap alpha observed paths else true} @ ghost =
    fun cap alpha h selected paths observed -> ghost_ (
  F.valid_def h paths; F.refresh_def selected paths;
  potential_def cap alpha observed paths;
  (match paths with
  | [] -> potential_def cap alpha observed []
  | p :: rest ->
      M.refresh_valid h selected p;
      refresh_potential cap alpha h selected rest observed;
      potential_def cap alpha observed (M.refresh selected p :: F.refresh selected rest));
  let u = () in refine_ u)

let (compressed_value_frame @ total) : (cap : Bigint.t) ->
    (alpha : Bigint.t) -> (h : Vox_union_find_model.node P.heap) @ immutable ->
    (selected : M.path) @ immutable -> (x : M.elem) @ immutable ->
    {u : unit | if not (M.contains x selected) then
      value cap alpha (M.compressed h selected) x = value cap alpha h x
      else true} @ ghost = fun cap alpha h selected x -> ghost_ (
  M.compressed_frame h selected x;
  let after = M.compressed h selected in
  value_def cap alpha h x; value_def cap alpha after x;
  (match H.at h x with
  | Some (M.Link (_, parent)) ->
      M.compressed_rank h selected parent;
      R.weight_def after parent; R.weight_def h parent
  | _ -> ());
  let u = () in refine_ u)

module B = Vox_union_find_amortized
module C = Vox_union_find_path_cost

let (release_step @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (h : Vox_union_find_model.node P.heap) @ immutable -> (p : M.path) @ immutable ->
    {u : unit | if M.valid h p && R.ordered cap h p then
      B.release cap alpha h p =
        (match p with
        | M.Stop _ -> 0Z
        | M.Step (x, rest) -> Bigint.add
            (Bigint.sub (value cap alpha h x)
              (V.node_phi cap alpha (R.weight h x) (R.weight h (M.root p))))
            (B.release cap alpha h rest)) else true} @ ghost =
    fun cap alpha h p -> ghost_ (
  M.valid_def h p; R.ordered_def cap h p;
  M.head_def p; M.root_def p;
  B.release_def cap alpha h p; B.edges_def h p;
  match p with
  | M.Stop _ -> C.loss_def cap alpha (R.weight h (M.root p)) [];
      let u = () in refine_ u
  | M.Step (x, rest) ->
      value_def cap alpha h x; M.rank_def h x; R.weight_def h x;
      B.release_def cap alpha h rest;
      if R.weight h x <= 0Z then (
        V.node_phi_def cap alpha (R.weight h x) (R.weight h (M.head rest));
        V.node_phi_def cap alpha (R.weight h x) (R.weight h (M.root p));
        let u = () in refine_ u)
      else (
        let edge = {C.rank = R.weight h x; parent = R.weight h (M.head rest)} in
        C.loss_def cap alpha (R.weight h (M.root p)) (edge :: B.edges h rest);
        let u = () in refine_ u))

let rec (compression_potential @ total) : (cap : Bigint.t) ->
    (alpha : Bigint.t) -> (h : Vox_union_find_model.node P.heap) @ immutable ->
    (selected : M.path) @ immutable -> (paths : M.path list) @ immutable ->
    {u : unit | if M.valid h selected && R.ordered cap h selected &&
      F.valid h paths && F.closed paths selected then
      potential cap alpha (M.compressed h selected) (F.refresh selected paths) =
        Bigint.sub (potential cap alpha h paths) (B.release cap alpha h selected)
      else true} @ ghost = fun cap alpha h selected paths -> ghost_ (
  M.valid_def h selected; R.ordered_def cap h selected;
  F.closed_def paths selected; M.head_def selected; M.root_def selected;
  M.compressed_def h selected;
  release_step cap alpha h selected;
  match selected with
  | M.Stop _ ->
      F.refresh_valid h selected paths;
      refresh_potential cap alpha h selected paths h;
      let u = () in refine_ u
  | M.Step (x, rest) ->
      compression_potential cap alpha h rest paths;
      F.refresh_valid h rest paths;
      F.refresh_member h rest paths x;
      refresh_potential cap alpha h rest paths (M.compressed h rest);
      R.contains_rank cap h rest x; R.bounds cap h rest;
      compressed_value_frame cap alpha h rest x;
      M.compressed_rank h rest x; M.compressed_rank h rest (M.root rest);
      let middle = M.compressed h rest in
      R.weight_def middle x; R.weight_def h x;
      R.weight_def middle (M.root rest); R.weight_def h (M.root rest);
      redirect_potential cap alpha middle (F.refresh rest paths) x (M.root rest);
      let after = M.compressed h selected in
      refresh_potential cap alpha h rest paths after;
      refresh_potential cap alpha h selected paths after;
      let u = () in refine_ u)

let (value_nonnegative @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (h : Vox_union_find_model.node P.heap) @ immutable -> (p : M.path) @ immutable ->
    {u : unit | if M.valid h p && R.ordered cap h p && alpha >= 1Z &&
      A.iter cap alpha 1Z 1Z >= cap then value cap alpha h (M.head p) >= 0Z
      else true} @ ghost = fun cap alpha h p -> ghost_ (
  M.valid_def h p; R.ordered_def cap h p; M.head_def p;
  value_def cap alpha h (M.head p);
  R.weight_def h (M.head p); M.rank_def h (M.head p);
  match p with
  | M.Stop _ -> let u = () in refine_ u
  | M.Step (x, rest) ->
      R.bounds cap h rest;
      let rank = R.weight h x in
      let parent = R.weight h (M.head rest) in
      if M.valid h p && R.ordered cap h p && alpha >= 1Z &&
        A.iter cap alpha 1Z 1Z >= cap && rank > 0Z then (
        let u = () in V.analyze cap alpha rank parent (refine_ u);
        let u = () in refine_ u)
      else (
        V.node_phi_def cap alpha rank parent;
        let u = () in refine_ u))

let rec (nonnegative @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (h : Vox_union_find_model.node P.heap) @ immutable -> (paths : M.path list) @ immutable ->
    {u : unit | if F.valid h paths && R.all_ordered cap h paths &&
      alpha >= 1Z && A.iter cap alpha 1Z 1Z >= cap then
      potential cap alpha h paths >= 0Z else true} @ ghost =
    fun cap alpha h paths -> ghost_ (
  F.valid_def h paths; R.all_ordered_def cap h paths;
  potential_def cap alpha h paths;
  (match paths with
  | [] -> ()
  | p :: rest -> value_nonnegative cap alpha h p; nonnegative cap alpha h rest);
  let u = () in refine_ u)

let rec (fresh_potential @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (h : Vox_union_find_model.node P.heap) @ immutable -> (paths : M.path list) @ immutable ->
    (x : M.elem) @ immutable ->
    {u : unit | if F.valid h paths && not (H.mem h x) then
      potential cap alpha (H.put h x (M.Root 0)) paths = potential cap alpha h paths
      else true} @ ghost = fun cap alpha h paths x -> ghost_ (
  F.valid_def h paths;
  let after = H.put h x (M.Root 0) in
  potential_def cap alpha after paths; potential_def cap alpha h paths;
  (match paths with
  | [] -> ()
  | p :: rest ->
      M.valid_def h p; M.head_def p;
      value_def cap alpha h (M.head p); value_def cap alpha after (M.head p);
      (match p with
      | M.Stop _ -> ()
      | M.Step (_, tail) ->
          M.valid_def h tail; R.fresh_weight h x (M.head tail));
      fresh_potential cap alpha h rest x);
  let u = () in refine_ u)

let (allocate_potential @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (h : Vox_union_find_model.node P.heap) @ immutable -> (paths : M.path list) @ immutable ->
    (x : M.elem) @ immutable ->
    {u : unit | if F.valid h paths && not (H.mem h x) then
      potential cap alpha (H.put h x (M.Root 0)) (M.Stop x :: paths) =
        potential cap alpha h paths else true} @ ghost =
    fun cap alpha h paths x -> ghost_ (
  fresh_potential cap alpha h paths x;
  let after = H.put h x (M.Root 0) in
  potential_def cap alpha after (M.Stop x :: paths);
  M.head_def (M.Stop x); value_def cap alpha after x;
  let u = () in refine_ u)

module D = Vox_union_find_mass
module J = Vox_union_find_link

let (link_value @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (h : Vox_union_find_model.node P.heap) @ immutable -> (x : M.elem) @ immutable ->
    (y : M.elem) @ immutable -> (paths : M.path list) @ immutable ->
    (p : M.path) @ immutable ->
    {u : unit | if J.admissible cap h x y paths && M.valid h p &&
      R.ordered cap h p && F.closed paths p && alpha >= 1Z &&
      A.iter cap alpha 1Z 1Z >= cap then
      value cap alpha (M.linked h x y) (M.head p) <=
        Bigint.add (value cap alpha h (M.head p))
          (if not (x === y) && M.rank h x = M.rank h y && M.head p === x
           then alpha else 0Z) else true} @ ghost =
    fun cap alpha h x y paths p -> ghost_ (
  J.admissible_def cap h x y paths;
  M.valid_def h p; R.ordered_def cap h p; F.closed_def paths p; M.head_def p;
  M.is_root_def h x; M.is_root_def h y; M.rank_def h x; M.rank_def h y;
  R.weight_def h x; R.weight_def h y;
  let q = M.head p in
  let after = M.linked h x y in
  J.at h x y q; value_def cap alpha h q; value_def cap alpha after q;
  M.rank_def h q; R.weight_def h q;
  if J.admissible cap h x y paths && M.valid h p && R.ordered cap h p &&
    F.closed paths p && alpha >= 1Z && A.iter cap alpha 1Z 1Z >= cap then (
    match p with
    | M.Stop q ->
        let loser = if M.rank h x < M.rank h y then x else y in
        let winner = M.winner h x y in
        M.winner_def h x y;
        D.linked_capacity cap h x y paths winner;
        D.linked_weight h x y winner;
        let rank = R.weight h q in
        let parent = R.weight after winner in
        if not (x === y) && q === loser && rank > 0Z then (
          let u = () in V.analyze cap alpha rank parent (refine_ u);
          let u = () in refine_ u)
        else (
          V.node_phi_def cap alpha rank parent;
          let u = () in refine_ u)
    | M.Step (_, rest) ->
        R.bounds cap h rest; F.closed_def paths rest;
        let parent = M.head rest in
        D.linked_weight h x y parent; D.linked_capacity cap h x y paths parent;
        let rank = R.weight h q in
        let old_parent = R.weight h parent in
        let new_parent = R.weight after parent in
        if rank > 0Z then (
          let u = () in V.compression cap alpha rank old_parent new_parent (refine_ u);
          let u = () in refine_ u)
        else (
          V.node_phi_def cap alpha rank old_parent;
          V.node_phi_def cap alpha rank new_parent;
          let u = () in refine_ u))
  else let u = () in refine_ u)

let rec (link_potential @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (h : Vox_union_find_model.node P.heap) @ immutable -> (x : M.elem) @ immutable ->
    (y : M.elem) @ immutable -> (paths : M.path list) @ immutable ->
    (queries : M.path list) @ immutable ->
    {u : unit | if J.admissible cap h x y paths && F.valid h queries &&
      R.all_ordered cap h queries && F.complete paths queries &&
      alpha >= 1Z && A.iter cap alpha 1Z 1Z >= cap then
      potential cap alpha (M.linked h x y) (F.join h x y queries) <=
        Bigint.add (potential cap alpha h queries)
          (if not (x === y) && M.rank h x = M.rank h y && F.member x queries
           then alpha else 0Z) else true} @ ghost =
    fun cap alpha h x y paths queries -> ghost_ (
  J.admissible_def cap h x y paths;
  F.valid_def h queries; R.all_ordered_def cap h queries;
  F.complete_def paths queries; F.join_def h x y queries; F.member_def x queries;
  potential_def cap alpha h queries;
  let after = M.linked h x y in
  match queries with
  | [] -> potential_def cap alpha after []; let u = () in refine_ u
  | p :: rest ->
      link_value cap alpha h x y paths p;
      link_potential cap alpha h x y paths rest;
      M.joined_valid h x y p;
      potential_def cap alpha after (M.joined_path h x y p :: F.join h x y rest);
      let u = () in refine_ u)
