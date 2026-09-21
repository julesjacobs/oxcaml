module M = Vox_union_find_model
module P = Ghost_pref
module H = P.Heap

let[@def] weight (h : P.heap @ immutable) (x : M.elem @ immutable) =
  ghost_ (Bigint.of_int (M.rank h x))
let[@def] rec ordered (cap : Bigint.t) (h : P.heap @ immutable)
    (p : M.path @ immutable) = ghost_ (
  0Z <= weight h (M.head p) && weight h (M.head p) < cap &&
  match p with
  | M.Stop _ -> true
  | M.Step (x, rest) -> weight h x < weight h (M.head rest) &&
      ordered cap h rest)

let rec (bounds @ total) : (cap : Bigint.t) -> (h : P.heap) @ immutable ->
    (p : M.path) @ immutable ->
    {u : unit | if ordered cap h p then
      0Z <= weight h (M.head p) &&
      weight h (M.head p) <= weight h (M.root p) &&
      weight h (M.root p) < cap &&
      (match p with M.Stop _ -> true | M.Step (x, _) ->
        weight h x < weight h (M.root p)) else true} @ ghost =
    fun cap h p -> ghost_ (
  ordered_def cap h p; M.head_def p; M.root_def p;
  (match p with M.Stop _ -> () | M.Step (_, rest) -> bounds cap h rest);
  let u = () in u)

let (redirect_weight @ total) : (h : P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (r : M.elem) @ immutable ->
    (q : M.elem) @ immutable ->
    {u : unit | weight (H.put h x (M.Link (M.rank h x, r))) q = weight h q}
      @ ghost = fun h x r q -> ghost_ (
  let after = H.put h x (M.Link (M.rank h x, r)) in
  weight_def after q; weight_def h q;
  M.rank_def after q; M.rank_def h q;
  let u = () in u)

let rec (redirect_ordered @ total) : (cap : Bigint.t) ->
    (h : P.heap) @ immutable -> (selected : M.path) @ immutable ->
    (x : M.elem) @ immutable -> (r : M.elem) @ immutable ->
    (p : M.path) @ immutable ->
    {u : unit | if M.valid h selected && ordered cap h selected &&
      M.head selected === x && M.root selected === r && not (x === r) &&
      M.valid h p && ordered cap h p then
      ordered cap (H.put h x (M.Link (M.rank h x, r))) (M.redirect x r p)
      else true} @ ghost = fun cap h selected x r p -> ghost_ (
  bounds cap h selected;
  M.valid_def h p; ordered_def cap h p; M.head_def p;
  M.redirect_def x r p;
  let after = H.put h x (M.Link (M.rank h x, r)) in
  match p with
  | M.Stop y ->
      ordered_def cap after p; redirect_weight h x r y;
      let u = () in u
  | M.Step (y, rest) ->
      if y === x then (
        ordered_def cap after (M.Step (y, M.Stop r));
        M.head_def (M.Step (y, M.Stop r));
        ordered_def cap after (M.Stop r); M.head_def (M.Stop r);
        redirect_weight h x r y; redirect_weight h x r r;
        ordered_def cap h selected; M.head_def selected; M.root_def selected;
        let u = () in u)
      else (
        redirect_ordered cap h selected x r rest;
        M.redirect_valid h selected x r rest;
        let next = M.redirect x r rest in
        ordered_def cap after (M.Step (y, next));
        M.head_def (M.Step (y, next));
        redirect_weight h x r y; redirect_weight h x r (M.head rest);
        let u = () in u))

let rec (refresh_ordered @ total) : (cap : Bigint.t) ->
    (h : P.heap) @ immutable -> (selected : M.path) @ immutable ->
    (p : M.path) @ immutable ->
    {u : unit | if M.valid h selected && ordered cap h selected &&
      M.valid h p && ordered cap h p then
      ordered cap (M.compressed h selected) (M.refresh selected p)
      else true} @ ghost = fun cap h selected p -> ghost_ (
  M.valid_def h selected; ordered_def cap h selected;
  M.head_def selected; M.root_def selected;
  M.compressed_def h selected; M.refresh_def selected p;
  match selected with
  | M.Stop _ -> let u = () in u
  | M.Step (x, rest) ->
      bounds cap h selected;
      refresh_ordered cap h rest p;
      refresh_ordered cap h rest selected;
      M.refresh_valid h rest p; M.refresh_valid h rest selected;
      M.compressed_rank h rest x;
      weight_def h x; weight_def h (M.root selected);
      let middle = M.compressed h rest in
      let witness = M.refresh rest selected in
      redirect_ordered cap middle witness x (M.root rest) (M.refresh rest p);
      let u = () in u)

let rec (contains_rank @ total) : (cap : Bigint.t) ->
    (h : P.heap) @ immutable -> (p : M.path) @ immutable ->
    (x : M.elem) @ immutable ->
    {u : unit | if ordered cap h p && M.contains x p then
      weight h (M.head p) <= weight h x && weight h x < weight h (M.root p)
      else true} @ ghost = fun cap h p x -> ghost_ (
  ordered_def cap h p; M.contains_def x p; M.head_def p; M.root_def p;
  (match p with
  | M.Stop _ -> ()
  | M.Step (_, rest) -> bounds cap h p; contains_rank cap h rest x);
  let u = () in u)

module F = Vox_union_find_forest
let[@def] rec all_ordered (cap : Bigint.t) (h : P.heap @ immutable)
    (paths : M.path list @ immutable) = ghost_ (match paths with
  | [] -> true | p :: rest -> ordered cap h p && all_ordered cap h rest)

let rec (lookup_ordered @ total) : (cap : Bigint.t) ->
    (h : P.heap) @ immutable -> (paths : M.path list) @ immutable ->
    (x : M.elem) @ immutable ->
    {u : unit | if all_ordered cap h paths && F.member x paths then
      ordered cap h (F.lookup x paths) else true} @ ghost =
    fun cap h paths x -> ghost_ (
  all_ordered_def cap h paths; F.member_def x paths; F.lookup_def x paths;
  (match paths with [] -> () | _ :: rest -> lookup_ordered cap h rest x);
  let u = () in u)

let rec (refresh_all_ordered @ total) : (cap : Bigint.t) ->
    (h : P.heap) @ immutable -> (selected : M.path) @ immutable ->
    (paths : M.path list) @ immutable ->
    {u : unit | if M.valid h selected && ordered cap h selected &&
      F.valid h paths && all_ordered cap h paths then
      all_ordered cap (M.compressed h selected) (F.refresh selected paths)
      else true} @ ghost = fun cap h selected paths -> ghost_ (
  F.valid_def h paths; all_ordered_def cap h paths; F.refresh_def selected paths;
  let after = M.compressed h selected in
  match paths with
  | [] -> all_ordered_def cap after []; let u = () in u
  | p :: rest ->
      refresh_ordered cap h selected p; refresh_all_ordered cap h selected rest;
      all_ordered_def cap after (M.refresh selected p :: F.refresh selected rest);
      let u = () in u)

let (fresh_weight @ total) : (h : P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (q : M.elem) @ immutable ->
    {u : unit | if H.mem h q && not (H.mem h x) then
      weight (H.put h x (M.Root 0)) q = weight h q else true} @ ghost =
    fun h x q -> ghost_ (
  let after = H.put h x (M.Root 0) in
  weight_def after q; weight_def h q; M.rank_def after q; M.rank_def h q;
  let u = () in u)

let rec (fresh_ordered @ total) : (cap : Bigint.t) -> (h : P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (p : M.path) @ immutable ->
    {u : unit | if M.valid h p && ordered cap h p && not (H.mem h x) then
      ordered cap (H.put h x (M.Root 0)) p else true} @ ghost =
    fun cap h x p -> ghost_ (
  M.valid_def h p; ordered_def cap h p;
  ordered_def cap (H.put h x (M.Root 0)) p;
  fresh_weight h x (M.head p); M.head_def p;
  (match p with
  | M.Stop _ -> ()
  | M.Step (_, rest) ->
      M.valid_def h rest; fresh_weight h x (M.head rest);
      fresh_ordered cap h x rest);
  let u = () in u)

let rec (fresh_all_ordered @ total) : (cap : Bigint.t) ->
    (h : P.heap) @ immutable -> (x : M.elem) @ immutable ->
    (paths : M.path list) @ immutable ->
    {u : unit | if F.valid h paths && all_ordered cap h paths &&
      not (H.mem h x) then all_ordered cap (H.put h x (M.Root 0)) paths
      else true} @ ghost = fun cap h x paths -> ghost_ (
  F.valid_def h paths; all_ordered_def cap h paths;
  all_ordered_def cap (H.put h x (M.Root 0)) paths;
  (match paths with
  | [] -> ()
  | p :: rest -> fresh_ordered cap h x p; fresh_all_ordered cap h x rest);
  let u = () in u)

let rec (weaken @ total) : (small : Bigint.t) -> (large : Bigint.t) ->
    (h : P.heap) @ immutable -> (p : M.path) @ immutable ->
    {u : unit | if small <= large && ordered small h p then ordered large h p
      else true} @ ghost = fun small large h p -> ghost_ (
  ordered_def small h p; ordered_def large h p;
  (match p with M.Stop _ -> () | M.Step (_, rest) -> weaken small large h rest);
  let u = () in u)

let rec (weaken_all @ total) : (small : Bigint.t) -> (large : Bigint.t) ->
    (h : P.heap) @ immutable -> (paths : M.path list) @ immutable ->
    {u : unit | if small <= large && all_ordered small h paths then
      all_ordered large h paths else true} @ ghost = fun small large h paths -> ghost_ (
  all_ordered_def small h paths; all_ordered_def large h paths;
  (match paths with [] -> () | p :: rest ->
    weaken small large h p; weaken_all small large h rest);
  let u = () in u)
