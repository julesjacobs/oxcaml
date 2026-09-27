(* What every public operation preserves; part of the trusted
   specification. [origins before after count]: every id below [count] has
   the same [Vox_egraph_snapshot_spec.origin] in both graphs. [extends
   before after]: the node count has not decreased and every id of
   [before] keeps its origin, so an id returned by [admit] stands for the
   same expression from then on. It says nothing about classes. *)

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
