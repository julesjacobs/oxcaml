open Copy_spec
open Level_spec

let[@def] rec (contains @ total) (tree : bounded @ immutable)
    (p : node Pref.t @ immutable) = ghost_ (match tree with
  | Tip q -> p === q
  | Through (q, child) -> p === q || contains child p
  | Fork (q, a, b) -> p === q || contains a p || contains b p)

let[@def] rec (confined @ total) (edits : lowering @ immutable)
    (tree : bounded @ immutable) = ghost_ (match edits with
  | Keep -> true
  | Lower (p, _, rest) -> contains tree p && confined rest tree
  | Sequence (a, b) -> confined a tree && confined b tree)
