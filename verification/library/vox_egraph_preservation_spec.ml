module Q = Vox_egraph_match_spec
module S = Vox_egraph_snapshot_spec

let[@def] rec (origins @ total) (before : Q.graph @ immutable)
    (after : Q.graph @ immutable) (count : int) = ghost_ (
  if count <= 0 then true
  else S.origin before (count - 1) === S.origin after (count - 1) &&
    origins before after (count - 1))
  [@@decreases if count > 0 then count else 0]

let[@def] (extends @ total) (before : Q.graph @ immutable) (after : Q.graph @ immutable) = ghost_ (
  before.count <= after.count && origins before after before.count)
