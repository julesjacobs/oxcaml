open Copy_spec
open Level_spec
open Level_unifier_spec

type edits = Done | Write of node Pref.t * node Pref.t * node Pref.t * resolution * edits [@@inductive]
let[@def] rec (rewritten @ total) (h : Pref.heap @ immutable) (after : Pref.heap @ immutable) (d : edits @ immutable) = ghost_ (
  match d with Done -> after === h
  | Write (p, q, r, path, rest) -> active h p && observe h p === Some (Link q)
    && resolves h p r path && rewritten (H.put h p (redirect h p r)) after rest)

type result = #{ value : node Pref.t @@ aliased; state : Pref.token; edits : edits @@ ghost; path : resolution @@ ghost }
