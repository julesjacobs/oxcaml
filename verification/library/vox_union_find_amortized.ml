module M = Vox_union_find_model
module R = Vox_union_find_rank
module C = Vox_union_find_path_cost
module A = Vox_ackermann
module P = Ghost_pref

let[@def] rec edges (h : Vox_union_find_model.node P.heap @ immutable) (p : M.path @ immutable) =
  ghost_ (match p with
  | M.Stop _ -> []
  | M.Step (x, rest) ->
      if R.weight h x <= 0Z then edges h rest
      else { C.rank = R.weight h x; parent = R.weight h (M.head rest) }
        :: edges h rest)

let rec (encode @ total) : (cap : Bigint.t) -> (h : Vox_union_find_model.node P.heap) @ immutable ->
    (p : M.path) @ immutable ->
    {u : unit | if R.ordered cap h p then
      C.chain (R.weight h (M.head p)) (R.weight h (M.root p)) (edges h p) &&
      C.length (edges h p) <= M.depth p &&
      M.depth p <= Bigint.add (C.length (edges h p)) 1Z &&
      (if R.weight h (M.head p) > 0Z then
        M.depth p = C.length (edges h p) else true) else true} @ ghost =
    fun cap h p -> ghost_ (
  R.ordered_def cap h p; R.bounds cap h p;
  M.head_def p; M.root_def p; M.depth_def p; edges_def h p;
  match p with
  | M.Stop _ ->
      C.chain_def (R.weight h (M.head p)) (R.weight h (M.root p)) [];
      C.length_def []; let u = () in refine_ u
  | M.Step (x, rest) ->
      encode cap h rest; R.bounds cap h rest;
      if R.weight h x <= 0Z then (
        C.chain_def (R.weight h x) (R.weight h (M.root rest)) (edges h rest);
        C.chain_def (R.weight h (M.head rest)) (R.weight h (M.root rest))
          (edges h rest);
        let u = () in refine_ u)
      else (
        let edge = {C.rank = R.weight h x;
          parent = R.weight h (M.head rest)} in
        C.chain_def (R.weight h x) (R.weight h (M.root rest))
          (edge :: edges h rest);
        C.length_def (edge :: edges h rest);
        let u = () in refine_ u))

let[@def] release (cap : Bigint.t) (alpha : Bigint.t)
    (h : Vox_union_find_model.node P.heap @ immutable) (p : M.path @ immutable) =
  ghost_ (C.loss cap alpha (R.weight h (M.root p)) (edges h p))
let[@def] find_fee (alpha : Bigint.t) = Bigint.add (Bigint.mul 4Z alpha) 8Z
let[@def] link_fee (alpha : Bigint.t) = Bigint.add (Bigint.mul 4Z alpha) 7Z
let[@def] union_fee (alpha : Bigint.t) = Bigint.add (Bigint.mul 12Z alpha) 24Z

let (find_bound @ total) : (cap : Bigint.t) -> (alpha : Bigint.t) ->
    (h : Vox_union_find_model.node P.heap) @ immutable -> (p : M.path) @ immutable ->
    {u : unit | if R.ordered cap h p && alpha >= 1Z &&
      A.iter cap alpha 1Z 1Z >= cap then
      release cap alpha h p >= 0Z &&
      Vox_union_find_worker.cost (M.depth p) <=
        Bigint.add (Bigint.mul 4Z (release cap alpha h p)) (find_fee alpha)
      else true} @ ghost = fun cap alpha h p -> ghost_ (
  if R.ordered cap h p && alpha >= 1Z && A.iter cap alpha 1Z 1Z >= cap then (
    encode cap h p; R.bounds cap h p;
    let root = R.weight h (M.root p) in
    let lower = R.weight h (M.head p) in
    let path = edges h p in
    let u = () in C.counting cap alpha lower root path (refine_ u);
    let u = () in C.bound cap alpha lower root path (refine_ u);
    release_def cap alpha h p;
    Vox_union_find_worker.cost_def (M.depth p); find_fee_def alpha;
    let u = () in refine_ u)
  else let u = () in refine_ u)
