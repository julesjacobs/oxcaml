open Copy_spec

let[@def] (at_level @ total) (h : Pref.heap @ immutable) (p : node Pref.t @ immutable) =
  ghost_ (match H.at h p with None -> Generic | Some v -> v.level)
let[@def] (active @ total) (h : Pref.heap @ immutable) (p : node Pref.t @ immutable) =
  ghost_ (H.mem h p && match at_level h p with Generic -> false | Finite n -> n >= 0)
let[@def] (below @ total) (h : Pref.heap @ immutable) (p : node Pref.t @ immutable) (bound : int) =
  ghost_ (H.mem h p && match at_level h p with Generic -> false | Finite n -> n >= 0 && n <= bound)
let[@def] (finite_scope @ total) (h : Pref.heap @ immutable) (p : node Pref.t @ immutable) =
  ghost_ (source_ok h p && match H.at h p with None -> false | Some v ->
    if active h p then match v.desc with Var | Bool -> true | Link q -> active h q
      | Arrow (a, b) -> active h a && active h b else true)
let[@def] (lower_cell @ total) (v : node @ immutable) (bound : int) =
  match v.level with Generic -> v | Finite n -> {v with level = Finite (if n < bound then n else bound)}
let[@def] (decreases @ total) (before : level) (after : level) = match before, after with
  | Generic, Generic -> true | Finite a, Finite b -> b <= a && (a < 0 || 0 <= b) | _ -> false
let[@def] (lower_frame @ total) (h : Pref.heap @ immutable) (after : Pref.heap @ immutable)
    (x : node Pref.t @ immutable) = ghost_ (H.mem after x === H.mem h x &&
    match H.at h x, H.at after x with
    | None, None -> true | Some a, Some b -> a.desc === b.desc && a.memo === b.memo && a.visited === b.visited && decreases a.level b.level
    | _ -> false)

let[@def] (children_below @ total) (h : Pref.heap @ immutable) (desc : desc @ immutable) (bound : int) = ghost_ (
  match desc with Var | Bool -> true | Link q -> below h q bound
  | Arrow (a, b) -> below h a bound && below h b bound)
let[@def] (ordered @ total) (h : Pref.heap @ immutable) (p : node Pref.t @ immutable) = ghost_ (
  match H.at h p with None -> true | Some v -> match v.level with
  | Generic -> true | Finite n -> n >= 0 && children_below h v.desc n)

type lowering = Keep | Lower of node Pref.t * node * lowering | Sequence of lowering * lowering [@@inductive]
let[@def] rec (lower_heap @ total) (h : Pref.heap @ immutable) (bound : int) (d : lowering @ immutable) =
  ghost_ (match d with Keep -> h | Lower (p, old, rest) ->
    H.put (lower_heap h bound rest) p (lower_cell old bound)
  | Sequence (a, b) -> lower_heap (lower_heap h bound a) bound b)
let[@def] rec (lower_valid @ total) (h : Pref.heap @ immutable) (bound : int) (d : lowering @ immutable) =
  ghost_ (bound >= 0 && match d with Keep -> true | Lower (p, old, rest) ->
    lower_valid h bound rest && H.mem (lower_heap h bound rest) p
    && H.at (lower_heap h bound rest) p === Some old
    && children_below (lower_heap h bound rest) old.desc bound
    && (match old.level with Generic -> false | Finite n -> n >= 0)
  | Sequence (a, b) -> lower_valid h bound a && lower_valid (lower_heap h bound a) bound b)

(* A completed traversal is separate from the sequence of level writes. *)
type bounded = Tip of node Pref.t | Through of node Pref.t * bounded
  | Fork of node Pref.t * bounded * bounded [@@inductive]
let[@def] (bound_root @ total) (t : bounded @ immutable) = match t with
  | Tip p | Through (p, _) | Fork (p, _, _) -> p
let[@def] rec (bounded @ total) (h : Pref.heap @ immutable) (limit : int) (t : bounded @ immutable) =
  ghost_ (below h (bound_root t) limit && match t with
  | Tip p -> (match H.at h p with Some {desc = (Var | Bool); _} -> true | _ -> false)
  | Through (p, child) -> (match H.at h p with Some {desc = Link q; _} -> q === bound_root child | _ -> false)
    && bounded h limit child
  | Fork (p, a, b) -> (match H.at h p with Some {desc = Arrow (x, y); _} -> x === bound_root a && y === bound_root b | _ -> false)
    && bounded h limit a && bounded h limit b)

type lowered = #{state : Pref.token; edits : lowering @@ ghost; tree : bounded @@ ghost}
