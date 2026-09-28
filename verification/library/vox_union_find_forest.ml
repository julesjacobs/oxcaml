module M = Vox_union_find_model
module P = Ghost_pref
module H = P.Heap

let[@def] rec member (x : M.elem @ immutable) (paths : M.path list @ immutable) =
  ghost_ (match paths with
  | [] -> false
  | p :: rest -> x === M.head p || member x rest)
let[@def] rec size (paths : M.path list @ immutable) =
  match paths with [] -> 0Z | _ :: rest -> Bigint.add 1Z (size rest)
let[@def] rec valid (h : Vox_union_find_model.node P.heap @ immutable) (paths : M.path list @ immutable) =
  ghost_ (match paths with
  | [] -> true
  | p :: rest -> M.valid h p && not (member (M.head p) rest) && valid h rest)
let[@def] rec lookup (x : M.elem @ immutable) (paths : M.path list @ immutable) =
  ghost_ (match paths with
  | [] -> M.Stop x
  | p :: rest -> if x === M.head p then p else lookup x rest)
let[@def] representative (x : M.elem @ immutable)
    (paths : M.path list @ immutable) = ghost_ (M.root (lookup x paths))

let rec (lookup_valid @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (paths : M.path list) @ immutable ->
    {u : unit | if valid h paths && member x paths then
      M.valid h (lookup x paths) && M.head (lookup x paths) === x &&
      H.mem h x else true} @ ghost = fun h x paths -> ghost_ (
  valid_def h paths; member_def x paths; lookup_def x paths;
  (match paths with
  | [] -> ()
  | p :: rest -> M.valid_def h p; lookup_valid h x rest);
  ())

let[@def] rec refresh (selected : M.path @ immutable)
    (paths : M.path list @ immutable) = ghost_ (match paths with
  | [] -> []
  | p :: rest -> M.refresh selected p :: refresh selected rest)

let rec (refresh_member @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (selected : M.path) @ immutable -> (paths : M.path list) @ immutable ->
    (x : M.elem) @ immutable ->
    {u : unit | if M.valid h selected && valid h paths then
      member x (refresh selected paths) = member x paths else true} @ ghost =
    fun h selected paths x -> ghost_ (
  valid_def h paths; refresh_def selected paths; member_def x paths;
  match paths with
  | [] -> member_def x []; ()
  | p :: rest ->
      M.refresh_valid h selected p; refresh_member h selected rest x;
      member_def x (M.refresh selected p :: refresh selected rest);
      ())

let rec (refresh_valid @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (selected : M.path) @ immutable -> (paths : M.path list) @ immutable ->
    {u : unit | if M.valid h selected && valid h paths then
      valid (M.compressed h selected) (refresh selected paths) &&
      size (refresh selected paths) = size paths else true} @ ghost =
    fun h selected paths -> ghost_ (
  valid_def h paths; refresh_def selected paths; size_def paths;
  let after = M.compressed h selected in
  match paths with
  | [] -> valid_def after []; size_def []; ()
  | p :: rest ->
      M.refresh_valid h selected p; refresh_valid h selected rest;
      refresh_member h selected rest (M.head p);
      valid_def after (M.refresh selected p :: refresh selected rest);
      size_def (M.refresh selected p :: refresh selected rest);
      ())

let rec (refresh_representative @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (selected : M.path) @ immutable -> (paths : M.path list) @ immutable ->
    (x : M.elem) @ immutable ->
    {u : unit | if M.valid h selected && valid h paths then
      representative x (refresh selected paths) === representative x paths
      else true} @ ghost = fun h selected paths x -> ghost_ (
  valid_def h paths; refresh_def selected paths;
  representative_def x paths; representative_def x (refresh selected paths);
  lookup_def x paths;
  match paths with
  | [] -> lookup_def x []; ()
  | p :: rest ->
      M.refresh_valid h selected p;
      refresh_representative h selected rest x;
      representative_def x rest;
      representative_def x (refresh selected rest);
      lookup_def x (M.refresh selected p :: refresh selected rest);
      ())

let rec (fresh_valid @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (paths : M.path list) @ immutable -> (x : M.elem) @ immutable ->
    {u : unit | if valid h paths && not (H.mem h x) then
      valid (H.put h x (M.Root 0)) paths && not (member x paths)
      else true} @ ghost = fun h paths x -> ghost_ (
  valid_def h paths; member_def x paths;
  let after = H.put h x (M.Root 0) in
  valid_def after paths;
  (match paths with
  | [] -> ()
  | p :: rest -> M.valid_def h p; M.fresh_valid h p x; fresh_valid h rest x);
  ())

let (allocate_valid @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (paths : M.path list) @ immutable -> (x : M.elem) @ immutable ->
    {u : unit | if valid h paths && not (H.mem h x) then
      valid (H.put h x (M.Root 0)) (M.Stop x :: paths) &&
      size (M.Stop x :: paths) = Bigint.add (size paths) 1Z &&
      member x (M.Stop x :: paths) &&
      representative x (M.Stop x :: paths) === x else true} @ ghost =
    fun h paths x -> ghost_ (
  fresh_valid h paths x;
  let after = H.put h x (M.Root 0) in
  valid_def after (M.Stop x :: paths); M.valid_def after (M.Stop x);
  M.head_def (M.Stop x); M.root_def (M.Stop x);
  size_def (M.Stop x :: paths); member_def x (M.Stop x :: paths);
  representative_def x (M.Stop x :: paths); lookup_def x (M.Stop x :: paths);
  ())

let[@def] rec join (h : Vox_union_find_model.node P.heap @ immutable)
    (x : M.elem @ immutable) (y : M.elem @ immutable)
    (paths : M.path list @ immutable) = ghost_ (match paths with
  | [] -> []
  | p :: rest -> M.joined_path h x y p :: join h x y rest)

let rec (join_member @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
    (paths : M.path list) @ immutable -> (q : M.elem) @ immutable ->
    {u : unit | if valid h paths && M.is_root h x && M.is_root h y &&
      M.rank h x + 1 >= 0 then
      member q (join h x y paths) = member q paths else true} @ ghost =
    fun h x y paths q -> ghost_ (
  valid_def h paths; join_def h x y paths; member_def q paths;
  match paths with
  | [] -> member_def q []; ()
  | p :: rest ->
      M.joined_valid h x y p; join_member h x y rest q;
      member_def q (M.joined_path h x y p :: join h x y rest);
      ())

let rec (join_valid @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
    (paths : M.path list) @ immutable ->
    {u : unit | if valid h paths && M.is_root h x && M.is_root h y &&
      M.rank h x + 1 >= 0 then
      valid (M.linked h x y) (join h x y paths) &&
      size (join h x y paths) = size paths else true} @ ghost =
    fun h x y paths -> ghost_ (
  valid_def h paths; join_def h x y paths; size_def paths;
  let after = M.linked h x y in
  match paths with
  | [] -> valid_def after []; size_def []; ()
  | p :: rest ->
      M.joined_valid h x y p; join_valid h x y rest;
      join_member h x y rest (M.head p);
      valid_def after (M.joined_path h x y p :: join h x y rest);
      size_def (M.joined_path h x y p :: join h x y rest);
      ())

let rec (join_representative @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
    (paths : M.path list) @ immutable -> (q : M.elem) @ immutable ->
    {u : unit | if valid h paths && M.is_root h x && M.is_root h y &&
      M.rank h x + 1 >= 0 && member q paths then
      representative q (join h x y paths) ===
        (if representative q paths === x || representative q paths === y
         then M.winner h x y else representative q paths) else true} @ ghost =
    fun h x y paths q -> ghost_ (
  valid_def h paths; join_def h x y paths; member_def q paths;
  representative_def q paths; representative_def q (join h x y paths);
  lookup_def q paths;
  match paths with
  | [] -> ()
  | p :: rest ->
      M.joined_valid h x y p; join_representative h x y rest q;
      representative_def q rest; representative_def q (join h x y rest);
      lookup_def q (M.joined_path h x y p :: join h x y rest);
      ())

let[@def] rec closed (paths : M.path list @ immutable)
    (p : M.path @ immutable) = ghost_ (
  member (M.head p) paths &&
  match p with M.Stop _ -> true | M.Step (_, rest) -> closed paths rest)
let[@def] rec complete (paths : M.path list @ immutable)
    (queries : M.path list @ immutable) = ghost_ (match queries with
  | [] -> true | p :: rest -> closed paths p && complete paths rest)

let rec (closed_root @ total) : (paths : M.path list) @ immutable ->
    (p : M.path) @ immutable ->
    {u : unit | if closed paths p then member (M.root p) paths else true}
      @ ghost = fun paths p -> ghost_ (
  closed_def paths p; M.head_def p; M.root_def p;
  (match p with M.Stop _ -> () | M.Step (_, rest) -> closed_root paths rest);
  ())

let rec (lookup_closed @ total) : (paths : M.path list) @ immutable ->
    (queries : M.path list) @ immutable -> (x : M.elem) @ immutable ->
    {u : unit | if complete paths queries && member x queries then
      closed paths (lookup x queries) else true} @ ghost =
    fun paths queries x -> ghost_ (
  complete_def paths queries; member_def x queries; lookup_def x queries;
  (match queries with [] -> () | _ :: rest -> lookup_closed paths rest x);
  ())

let rec (redirect_closed @ total) : (paths : M.path list) @ immutable ->
    (x : M.elem) @ immutable -> (r : M.elem) @ immutable ->
    (p : M.path) @ immutable ->
    {u : unit | if closed paths p && member r paths then
      closed paths (M.redirect x r p) else true} @ ghost =
    fun paths x r p -> ghost_ (
  closed_def paths p; M.redirect_def x r p; M.head_def p;
  match p with
  | M.Stop _ -> ()
  | M.Step (y, rest) ->
      if y === x then (
        closed_def paths (M.Step (y, M.Stop r));
        M.head_def (M.Step (y, M.Stop r));
        closed_def paths (M.Stop r); M.head_def (M.Stop r);
        ())
      else (
        redirect_closed paths x r rest;
        closed_def paths (M.Step (y, M.redirect x r rest));
        M.head_def (M.Step (y, M.redirect x r rest));
        ()))

let rec (refresh_closed @ total) : (paths : M.path list) @ immutable ->
    (selected : M.path) @ immutable -> (query : M.path) @ immutable ->
    {u : unit | if closed paths selected && closed paths query then
      closed paths (M.refresh selected query) else true} @ ghost =
    fun paths selected query -> ghost_ (
  closed_def paths selected; M.refresh_def selected query;
  (match selected with
  | M.Stop _ -> ()
  | M.Step (x, rest) ->
      refresh_closed paths rest query; closed_root paths rest;
      redirect_closed paths x (M.root rest) (M.refresh rest query));
  ())

let rec (refresh_closed_domain @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (selected : M.path) @ immutable -> (paths : M.path list) @ immutable ->
    (p : M.path) @ immutable ->
    {u : unit | if M.valid h selected && valid h paths && closed paths p then
      closed (refresh selected paths) p else true} @ ghost =
    fun h selected paths p -> ghost_ (
  closed_def paths p; closed_def (refresh selected paths) p;
  refresh_member h selected paths (M.head p);
  (match p with
  | M.Stop _ -> ()
  | M.Step (_, rest) -> refresh_closed_domain h selected paths rest);
  ())

let rec (refresh_complete @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (selected : M.path) @ immutable -> (paths : M.path list) @ immutable ->
    (queries : M.path list) @ immutable ->
    {u : unit | if M.valid h selected && valid h paths &&
      closed paths selected && complete paths queries then
      complete (refresh selected paths) (refresh selected queries)
      else true} @ ghost = fun h selected paths queries -> ghost_ (
  complete_def paths queries; refresh_def selected queries;
  match queries with
  | [] -> complete_def (refresh selected paths) []; ()
  | p :: rest ->
      refresh_closed paths selected p;
      refresh_closed_domain h selected paths (M.refresh selected p);
      refresh_complete h selected paths rest;
      complete_def (refresh selected paths)
        (M.refresh selected p :: refresh selected rest);
      ())

let rec (extend_closed @ total) : (paths : M.path list) @ immutable ->
    (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
    (p : M.path) @ immutable ->
    {u : unit | if closed paths p && member y paths then
      closed paths (M.extend x y p) else true} @ ghost =
    fun paths x y p -> ghost_ (
  closed_def paths p; M.extend_def x y p; M.head_def p;
  match p with
  | M.Stop q ->
      if q === x then (
        closed_def paths (M.Step (q, M.Stop y));
        M.head_def (M.Step (q, M.Stop y));
        closed_def paths (M.Stop y); M.head_def (M.Stop y);
        ())
      else ()
  | M.Step (q, rest) ->
      extend_closed paths x y rest;
      closed_def paths (M.Step (q, M.extend x y rest));
      M.head_def (M.Step (q, M.extend x y rest));
      ())

let rec (join_closed_domain @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
    (paths : M.path list) @ immutable -> (p : M.path) @ immutable ->
    {u : unit | if valid h paths && M.is_root h x && M.is_root h y &&
      M.rank h x + 1 >= 0 && closed paths p then
      closed (join h x y paths) p else true} @ ghost =
    fun h x y paths p -> ghost_ (
  closed_def paths p; closed_def (join h x y paths) p;
  join_member h x y paths (M.head p);
  (match p with
  | M.Stop _ -> ()
  | M.Step (_, rest) -> join_closed_domain h x y paths rest);
  ())

let rec (join_complete @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
    (paths : M.path list) @ immutable -> (queries : M.path list) @ immutable ->
    {u : unit | if valid h paths && M.is_root h x && M.is_root h y &&
      M.rank h x + 1 >= 0 && member x paths && member y paths &&
      complete paths queries then
      complete (join h x y paths) (join h x y queries) else true} @ ghost =
    fun h x y paths queries -> ghost_ (
  complete_def paths queries; join_def h x y queries;
  match queries with
  | [] -> complete_def (join h x y paths) []; ()
  | p :: rest ->
      M.joined_path_def h x y p;
      extend_closed paths x y p; extend_closed paths y x p;
      join_closed_domain h x y paths (M.joined_path h x y p);
      join_complete h x y paths rest;
      complete_def (join h x y paths)
        (M.joined_path h x y p :: join h x y rest);
      ())

let rec (cons_closed @ total) : (paths : M.path list) @ immutable ->
    (added : M.path) @ immutable -> (p : M.path) @ immutable ->
    {u : unit | if closed paths p then closed (added :: paths) p else true}
      @ ghost = fun paths added p -> ghost_ (
  closed_def paths p; closed_def (added :: paths) p;
  member_def (M.head p) (added :: paths);
  (match p with M.Stop _ -> () | M.Step (_, rest) -> cons_closed paths added rest);
  ())

let rec (cons_complete @ total) : (paths : M.path list) @ immutable ->
    (added : M.path) @ immutable -> (queries : M.path list) @ immutable ->
    {u : unit | if complete paths queries then
      complete (added :: paths) queries else true} @ ghost =
    fun paths added queries -> ghost_ (
  complete_def paths queries; complete_def (added :: paths) queries;
  (match queries with
  | [] -> ()
  | p :: rest -> cons_closed paths added p; cons_complete paths added rest);
  ())

let (allocate_complete @ total) : (paths : M.path list) @ immutable ->
    (x : M.elem) @ immutable ->
    {u : unit | if complete paths paths then
      complete (M.Stop x :: paths) (M.Stop x :: paths) else true} @ ghost =
    fun paths x -> ghost_ (
  cons_complete paths (M.Stop x) paths;
  complete_def (M.Stop x :: paths) (M.Stop x :: paths);
  closed_def (M.Stop x :: paths) (M.Stop x);
  M.head_def (M.Stop x); member_def x (M.Stop x :: paths);
  ())

let[@def] rec addresses (paths : M.path list @ immutable) = ghost_ (
  match paths with [] -> [] | p :: rest -> M.head p :: addresses rest)

let rec (member_same @ total) : (left : M.path list) @ immutable ->
    (right : M.path list) @ immutable -> (x : M.elem) @ immutable ->
    {u : unit | if addresses left === addresses right then
      member x left = member x right else true} @ ghost = fun left right x -> ghost_ (
  addresses_def left; addresses_def right; member_def x left; member_def x right;
  (match left, right with
  | _ :: ps, _ :: qs -> member_same ps qs x
  | _ -> ());
  ())

let rec (refresh_addresses @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (selected : M.path) @ immutable -> (paths : M.path list) @ immutable ->
    {u : unit | if M.valid h selected && valid h paths then
      addresses (refresh selected paths) === addresses paths else true} @ ghost =
    fun h selected paths -> ghost_ (
  valid_def h paths; refresh_def selected paths; addresses_def paths;
  match paths with
  | [] -> addresses_def []; ()
  | p :: rest ->
      M.refresh_valid h selected p; refresh_addresses h selected rest;
      addresses_def (M.refresh selected p :: refresh selected rest);
      ())

let rec (join_addresses @ total) : (h : Vox_union_find_model.node P.heap) @ immutable ->
    (x : M.elem) @ immutable -> (y : M.elem) @ immutable ->
    (paths : M.path list) @ immutable ->
    {u : unit | if valid h paths && M.is_root h x && M.is_root h y &&
      M.rank h x + 1 >= 0 then addresses (join h x y paths) === addresses paths
      else true} @ ghost = fun h x y paths -> ghost_ (
  valid_def h paths; join_def h x y paths; addresses_def paths;
  match paths with
  | [] -> addresses_def []; ()
  | p :: rest ->
      M.joined_valid h x y p; join_addresses h x y rest;
      addresses_def (M.joined_path h x y p :: join h x y rest);
      ())
