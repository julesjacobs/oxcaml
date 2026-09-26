open Copy_spec
open Level_spec

let[@def] rec (terminal_valid @ total) (h : node Pref.heap @ immutable)
    (bound : int) (d : lowering @ immutable) = ghost_ (
  bound >= 0 && match d with
  | Keep -> true
  | Lower (p, old, rest) -> terminal_valid h bound rest
    && H.mem (lower_heap h bound rest) p && H.at (lower_heap h bound rest) p === Some old
    && (match old.desc with Link _ -> false | _ -> true)
    && (match old.level with Generic -> false | Finite n -> n >= 0)
  | Sequence (a, b) -> terminal_valid h bound a && terminal_valid (lower_heap h bound a) bound b)

let[@def] rec (terminal_bounded @ total) (h : node Pref.heap @ immutable)
    (limit : int) (tree : bounded @ immutable) = ghost_ (
  H.mem h (bound_root tree) && match tree with
  | Tip p -> below h p limit
    && (match H.at h p with Some {desc = (Var | Bool | Word); _} -> true | _ -> false)
  | Through (p, child) -> (match H.at h p with Some {desc = Link q; _} -> q === bound_root child
    | Some {desc = List q; _} -> q === bound_root child && below h p limit | _ -> false)
    && terminal_bounded h limit child
  | Fork (p, a, b) -> below h p limit
    && (match H.at h p with Some {desc = Arrow (x, y); _} -> x === bound_root a && y === bound_root b | _ -> false)
    && terminal_bounded h limit a && terminal_bounded h limit b)

let[@def] (completed @ total) (h : node Pref.heap @ immutable) (bound : int)
    (p : node Pref.t @ immutable) (after : node Pref.heap @ immutable)
    (d : lowering @ immutable) (tree : Level_spec.bounded @ immutable) = ghost_ (
  terminal_valid h bound d && after === lower_heap h bound d && bound_root tree === p
  && terminal_bounded after bound tree && Lower_locality_spec.confined d tree)
