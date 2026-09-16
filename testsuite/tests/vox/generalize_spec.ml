open Copy_spec
open Level_spec

type pool = Empty | Entry of node Pref.t * pool [@@inductive]
let[@def] rec (listed @ total) (pool : pool @ immutable) (p : node Pref.t @ immutable) =
  ghost_ (match pool with Empty -> false | Entry (q, rest) -> p === q || listed rest p)
let[@def] rec (pool_scoped @ total) (h : node Pref.heap @ immutable) (pool : pool @ immutable) = ghost_ (
  match pool with Empty -> true | Entry (p, rest) -> H.mem h p && source_ok h p && pool_scoped h rest)
let[@def] (close_level @ total) (cut : int) (level : level) = match level with
  | Generic -> Generic | Finite n -> if n > cut then Generic else Finite n
let[@def] (needs_close @ total) (cut : int) (level : level) = match level with
  | Generic -> false | Finite n -> n > cut
let[@def] (close_cell @ total) (cut : int) (v : node @ immutable) = {v with level = close_level cut v.level}
let[@def] rec (closed_heap @ total) (h : node Pref.heap @ immutable) (cut : int) (pool : pool @ immutable) = ghost_ (
  match pool with Empty -> h | Entry (p, rest) -> match H.at h p with None -> closed_heap h cut rest
    | Some v -> closed_heap (if needs_close cut v.level then H.put h p (close_cell cut v) else h) cut rest)
let[@def] (closed_at @ total) (h : node Pref.heap @ immutable) (after : node Pref.heap @ immutable)
    (cut : int) (pool : pool @ immutable) (x : node Pref.t @ immutable) = ghost_ (
  H.mem h x === H.mem after x && match H.at h x, H.at after x with
  | None, None -> true | Some a, Some b -> a.desc === b.desc && a.memo === b.memo && a.visited === b.visited
    && b.level === (if listed pool x then close_level cut a.level else a.level)
  | _ -> false)
let[@def] (covered @ total) (h : node Pref.heap @ immutable) (cut : int) (pool : pool @ immutable)
    (x : node Pref.t @ immutable) = ghost_ (not (H.mem h x) || match at_level h x with
  | Generic -> true | Finite n -> n <= cut || listed pool x)

let[@def] rec (unfolded @ total) (h : node Pref.heap @ immutable) (t : bounded @ immutable) = ghost_ (
  H.mem h (bound_root t) && match t with
  | Tip p -> (match H.at h p with Some {desc = (Var | Bool); _} -> true | _ -> false)
  | Through (p, c) -> (match H.at h p with Some {desc = Link q; _} -> q === bound_root c | _ -> false) && unfolded h c
  | Fork (p, a, b) -> (match H.at h p with Some {desc = Arrow (x, y); _} -> x === bound_root a && y === bound_root b | _ -> false)
    && unfolded h a && unfolded h b)
let[@def] rec (scheme @ total) (h : node Pref.heap @ immutable) (cut : int) (t : bounded @ immutable) = ghost_ (
  let p = bound_root t in
  if not (close_level cut (at_level h p) === Generic) then Boundary p else
  match t with
  | Tip p -> (match H.at h p with Some {desc = Bool; _} -> Constant p | _ -> Parameter p)
  | Through (p, c) -> Indirect (p, scheme h cut c)
  | Fork (p, a, b) -> Product (p, scheme h cut a, scheme h cut b))

type path = Stop | Step of node Pref.t * path [@@inductive]
let[@def] (edge @ total) (h : node Pref.heap @ immutable) (p : node Pref.t @ immutable) (q : node Pref.t @ immutable) = ghost_ (
  match H.at h p with Some {desc = Link x; _} -> x === q
  | Some {desc = Arrow (a, b); _} -> a === q || b === q | _ -> false)
let[@def] rec (reaches @ total) (h : node Pref.heap @ immutable) (p : node Pref.t @ immutable)
    (q : node Pref.t @ immutable) (path : path @ immutable) = ghost_ (match path with
  | Stop -> p === q | Step (x, rest) -> edge h p x && reaches h x q rest)
