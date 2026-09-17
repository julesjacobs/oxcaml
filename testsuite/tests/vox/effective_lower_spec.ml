open Copy_spec
open Level_spec
module E = Effective_level

let[@def] (effective_children_below @ total) (h : Pref.heap @ immutable)
    (heads : E.heads @ total) (desc : desc @ immutable) (bound : int) = ghost_ (
  match desc with Var | Bool -> true | Link q -> E.effective_below h heads q bound
  | Arrow (a, b) -> E.effective_below h heads a bound && E.effective_below h heads b bound)

let[@def] rec (effective_lower_valid @ total) (h : Pref.heap @ immutable)
    (heads : E.heads @ total) (bound : int) (d : lowering @ immutable) = ghost_ (
  bound >= 0 && match d with
  | Keep -> true
  | Lower (p, old, rest) -> effective_lower_valid h heads bound rest
    && H.mem (lower_heap h bound rest) p && H.at (lower_heap h bound rest) p === Some old
    && effective_children_below (lower_heap h bound rest) heads old.desc bound
    && (match old.desc with Link _ -> false | _ -> true)
    && (match old.level with Generic -> false | Finite n -> n >= 0)
  | Sequence (a, b) -> effective_lower_valid h heads bound a
    && effective_lower_valid (lower_heap h bound a) heads bound b)

let[@def] rec (effective_bounded @ total) (h : Pref.heap @ immutable)
    (heads : E.heads @ total) (limit : int) (t : bounded @ immutable) = ghost_ (
  E.effective_below h heads (bound_root t) limit && match t with
  | Tip p -> (match H.at h p with Some {desc = (Var | Bool); _} -> true | _ -> false)
  | Through (p, child) -> (match H.at h p with Some {desc = Link q; _} -> q === bound_root child | _ -> false)
    && effective_bounded h heads limit child
  | Fork (p, a, b) -> (match H.at h p with Some {desc = Arrow (x, y); _} -> x === bound_root a && y === bound_root b | _ -> false)
    && effective_bounded h heads limit a && effective_bounded h heads limit b)
