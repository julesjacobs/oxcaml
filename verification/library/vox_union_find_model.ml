module P = Ghost_pref
module H = P.Heap

type node : immutable_data = Root of int | Link of int * node P.t
type elem = node P.t
type path : immutable_data = Stop of elem | Step of elem * path [@@inductive]

let[@def] head (p : path @ immutable) =
  match p with Stop x -> x | Step (x, _) -> x
let[@def] rec root (p : path @ immutable) =
  match p with Stop x -> x | Step (_, tail) -> root tail
let[@def] rec depth (p : path @ immutable) =
  match p with Stop _ -> 0Z | Step (_, tail) -> Bigint.add 1Z (depth tail)
let[@def] tail (p : path @ immutable) =
  match p with Stop _ -> p | Step (_, rest) -> rest
let[@def] rank (h : P.heap @ immutable) (x : elem @ immutable) = ghost_ (
  match H.at h x with Some (Root r) | Some (Link (r, _)) -> r | _ -> 0)
let[@def] rec valid (h : P.heap @ immutable) (p : path @ immutable) = ghost_ (
  H.mem h (head p) &&
  match p with
  | Stop x -> (match H.at h x with Some (Root r) -> r >= 0 | _ -> false)
  | Step (x, rest) -> (match H.at h x with Some (Link (_, y)) -> y === head rest | _ -> false) && valid h rest)
let[@def] rec compressed (h : P.heap @ immutable) (p : path @ immutable) = ghost_ (
  match p with
  | Stop _ -> h
  | Step (x, rest) -> H.put (compressed h rest) x (Link (rank h x, root rest)))

let rec (depth_nonnegative @ total) (p : path @ immutable) :
    {u : unit | depth p >= 0Z} @ ghost = ghost_ (
  depth_def p;
  (match p with Stop _ -> () | Step (_, rest) -> depth_nonnegative rest);
  let u = () in u)

let (observe @ total) : (h : P.heap) @ immutable ->
    (p : path) @ immutable -> (x : elem) @ immutable -> (v : node) @ immutable ->
    {u : unit | if valid h p && x === head p && H.at h x === Some v then
      (match v with
      | Root _ -> depth p = 0Z && root p === x && compressed h p === h
      | Link (_, y) -> depth p > 0Z && valid h (tail p) &&
        head (tail p) === y &&
        depth (tail p) = Bigint.sub (depth p) 1Z &&
        root p === root (tail p) &&
        compressed h p === H.put (compressed h (tail p)) x
          (Link (rank h x, root (tail p)))) else true} @ ghost = fun h p x v -> ghost_ (
  valid_def h p; head_def p; depth_def p; root_def p;
  compressed_def h p; tail_def p;
  (match p with Stop _ -> () | Step (_, rest) -> depth_nonnegative rest);
  let u = () in u)

let rec (compressed_mem @ total) : (h : P.heap) @ immutable ->
    (p : path) @ immutable -> (x : elem) @ immutable ->
    {u : unit | if H.mem h x then H.mem (compressed h p) x else true}
    @ ghost = fun h p x -> ghost_ (
  compressed_def h p;
  (match p with
  | Stop _ -> ()
  | Step (y, rest) ->
      compressed_mem h rest x;
      let _ = H.mem (H.put (compressed h rest) y
        (Link (rank h y, root rest))) x in ());
  let u = () in u)

let rec (compressed_rank @ total) : (h : P.heap) @ immutable ->
    (p : path) @ immutable -> (x : elem) @ immutable ->
    {u : unit | rank (compressed h p) x = rank h x} @ ghost =
    fun h p x -> ghost_ (
  compressed_def h p;
  match p with
  | Stop _ -> let u = () in u
  | Step (y, rest) ->
      compressed_rank h rest x;
      rank_def (H.put (compressed h rest) y (Link (rank h y, root rest))) x;
      rank_def (compressed h rest) x; rank_def h x;
      let u = () in u)

let[@def] is_root (h : P.heap @ immutable) (x : elem @ immutable) = ghost_ (
  H.mem h x && match H.at h x with Some (Root r) -> r >= 0 | _ -> false)
let[@def] winner (h : P.heap @ immutable)
    (x : elem @ immutable) (y : elem @ immutable) = ghost_ (
  if rank h x < rank h y then y else x)
let[@def] linked (h : P.heap @ immutable)
    (x : elem @ immutable) (y : elem @ immutable) = ghost_ (
  if x === y then h
  else if rank h x < rank h y then H.put h x (Link (rank h x, y))
  else if rank h y < rank h x then H.put h y (Link (rank h y, x))
  else H.put (H.put h y (Link (rank h y, x))) x (Root (rank h x + 1)))

let rec (terminal @ total) : (h : P.heap) @ immutable ->
    (p : path) @ immutable ->
    {u : unit | if valid h p then is_root h (root p) else true} @ ghost =
    fun h p -> ghost_ (
  valid_def h p; root_def p; head_def p;
  match p with
  | Stop x -> is_root_def h x; let u = () in u
  | Step (_, rest) -> terminal h rest; let u = () in u)

let rec (unique_root @ total) : (h : P.heap) @ immutable ->
    (p : path) @ immutable -> (q : path) @ immutable ->
    {u : unit | if valid h p && valid h q && head p === head q then
      root p === root q else true} @ ghost = fun h p q -> ghost_ (
  valid_def h p; valid_def h q;
  head_def p; head_def q; root_def p; root_def q;
  (match p, q with
  | Step (_, ps), Step (_, qs) -> unique_root h ps qs
  | _ -> ());
  let u = () in u)

let[@def] rec redirect (x : elem @ immutable) (r : elem @ immutable)
    (p : path @ immutable) = ghost_ (
  match p with
  | Stop y -> Stop y
  | Step (y, rest) ->
      if y === x then Step (y, Stop r) else Step (y, redirect x r rest))

let rec (redirect_valid @ total) : (h : P.heap) @ immutable ->
    (selected : path) @ immutable -> (x : elem) @ immutable ->
    (r : elem) @ immutable -> (p : path) @ immutable ->
    {u : unit | if valid h selected && head selected === x &&
      root selected === r && not (x === r) && valid h p then
      valid (H.put h x (Link (rank h x, r))) (redirect x r p) &&
      head (redirect x r p) === head p && root (redirect x r p) === root p
      else true} @ ghost = fun h selected x r p -> ghost_ (
  terminal h selected; is_root_def h r;
  valid_def h p; head_def p; root_def p; redirect_def x r p;
  let after = H.put h x (Link (rank h x, r)) in
  match p with
  | Stop y ->
      unique_root h selected p;
      valid_def after (Stop y); head_def (Stop y); root_def (Stop y);
      let u = () in u
  | Step (y, rest) ->
      if y === x then (
        unique_root h selected p;
        valid_def after (Step (y, Stop r)); head_def (Step (y, Stop r));
        root_def (Step (y, Stop r));
        valid_def after (Stop r); head_def (Stop r); root_def (Stop r);
        let u = () in u)
      else (
        redirect_valid h selected x r rest;
        let next = redirect x r rest in
        valid_def after (Step (y, next)); head_def (Step (y, next));
        root_def (Step (y, next));
        let u = () in u))

let[@def] rec refresh (selected : path @ immutable) (query : path @ immutable) =
  ghost_ (match selected with
  | Stop _ -> query
  | Step (x, rest) -> redirect x (root rest) (refresh rest query))

let rec (refresh_valid @ total) : (h : P.heap) @ immutable ->
    (selected : path) @ immutable -> (query : path) @ immutable ->
    {u : unit | if valid h selected && valid h query then
      valid (compressed h selected) (refresh selected query) &&
      head (refresh selected query) === head query &&
      root (refresh selected query) === root query else true} @ ghost =
    fun h selected query -> ghost_ (
  valid_def h selected; head_def selected; root_def selected;
  compressed_def h selected; refresh_def selected query;
  match selected with
  | Stop _ -> let u = () in u
  | Step (x, rest) ->
      terminal h selected; is_root_def h (root selected);
      refresh_valid h rest query;
      refresh_valid h rest selected;
      compressed_rank h rest x;
      let middle = compressed h rest in
      let witness = refresh rest selected in
      let next = refresh rest query in
      redirect_valid middle witness x (root rest) next;
      let u = () in u)

let rec (fresh_valid @ total) : (h : P.heap) @ immutable ->
    (p : path) @ immutable -> (x : elem) @ immutable ->
    {u : unit | if valid h p && not (H.mem h x) then
      valid (H.put h x (Root 0)) p else true} @ ghost = fun h p x -> ghost_ (
  valid_def h p; head_def p;
  let after = H.put h x (Root 0) in
  valid_def after p;
  (match p with Stop _ -> () | Step (_, rest) -> fresh_valid h rest x);
  let u = () in u)

let[@def] rec extend (loser : elem @ immutable) (winner : elem @ immutable)
    (p : path @ immutable) = ghost_ (
  match p with
  | Stop x -> if x === loser then Step (x, Stop winner) else p
  | Step (x, rest) -> Step (x, extend loser winner rest))

let rec (extend_valid @ total) : (h : P.heap) @ immutable ->
    (loser : elem) @ immutable -> (winner : elem) @ immutable ->
    (p : path) @ immutable ->
    {u : unit | if valid h p && is_root h loser && is_root h winner &&
      not (loser === winner) then
      valid (H.put h loser (Link (rank h loser, winner))) (extend loser winner p) &&
      head (extend loser winner p) === head p &&
      root (extend loser winner p) ===
        (if root p === loser then winner else root p) else true} @ ghost =
    fun h loser winner p -> ghost_ (
  valid_def h p; head_def p; root_def p;
  is_root_def h loser; is_root_def h winner;
  extend_def loser winner p;
  let after = H.put h loser (Link (rank h loser, winner)) in
  match p with
  | Stop x ->
      if x === loser then (
        valid_def after (Step (x, Stop winner));
        head_def (Step (x, Stop winner)); root_def (Step (x, Stop winner));
        valid_def after (Stop winner); head_def (Stop winner);
        root_def (Stop winner);
        let u = () in u)
      else (valid_def after p; let u = () in u)
  | Step (x, rest) ->
      extend_valid h loser winner rest;
      let next = extend loser winner rest in
      valid_def after (Step (x, next)); head_def (Step (x, next));
      root_def (Step (x, next));
      let u = () in u)

let rec (root_rank_valid @ total) : (h : P.heap) @ immutable ->
    (x : elem) @ immutable -> (rank : int) -> (p : path) @ immutable ->
    {u : unit | if valid h p && is_root h x && rank >= 0 then
      valid (H.put h x (Root rank)) p else true} @ ghost =
    fun h x rank p -> ghost_ (
  valid_def h p; head_def p; is_root_def h x;
  let after = H.put h x (Root rank) in
  valid_def after p;
  (match p with Stop _ -> () | Step (_, rest) -> root_rank_valid h x rank rest);
  let u = () in u)

let[@def] joined_path (h : P.heap @ immutable)
    (x : elem @ immutable) (y : elem @ immutable) (p : path @ immutable) =
  ghost_ (if x === y then p
    else if rank h x < rank h y then extend x y p else extend y x p)

let (joined_valid @ total) : (h : P.heap) @ immutable ->
    (x : elem) @ immutable -> (y : elem) @ immutable ->
    (p : path) @ immutable ->
    {u : unit | if valid h p && is_root h x && is_root h y &&
      rank h x + 1 >= 0 then
      valid (linked h x y) (joined_path h x y p) &&
      head (joined_path h x y p) === head p &&
      root (joined_path h x y p) ===
        (if root p === x || root p === y then winner h x y else root p)
      else true} @ ghost = fun h x y p -> ghost_ (
  joined_path_def h x y p; linked_def h x y; winner_def h x y;
  if x === y then let u = () in u
  else if rank h x < rank h y then (
    extend_valid h x y p; let u = () in u)
  else (
    extend_valid h y x p;
    if rank h y < rank h x then let u = () in u
    else (
      let middle = H.put h y (Link (rank h y, x)) in
      is_root_def h x; is_root_def middle x;
      root_rank_valid middle x (rank h x + 1) (extend y x p);
      let u = () in u)))

let[@def] rec contains (x : elem @ immutable) (p : path @ immutable) =
  ghost_ (match p with
  | Stop _ -> false
  | Step (y, rest) -> x === y || contains x rest)

let rec (compressed_frame @ total) : (h : P.heap) @ immutable ->
    (p : path) @ immutable -> (x : elem) @ immutable ->
    {u : unit | if not (contains x p) then
      H.at (compressed h p) x === H.at h x else true} @ ghost =
    fun h p x -> ghost_ (
  contains_def x p; compressed_def h p;
  (match p with
  | Stop _ -> ()
  | Step (y, rest) ->
      compressed_frame h rest x;
      let _ = H.at (H.put (compressed h rest) y (Link (rank h y, root rest))) x in ());
  let u = () in u)
