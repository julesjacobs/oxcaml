open Copy_spec
open Level_spec
open Level_unifier_spec

open Compression_spec
let[@def] rec (effective_rewritten @ total) (h : node Pref.heap @ immutable) (after : node Pref.heap @ immutable) (d : edits @ immutable) = ghost_ (
  match d with Done -> after === h
  | Write (p, q, r, path, rest) -> H.mem h p && active h r && observe h p === Some (Link q)
    && resolves h p r path && effective_rewritten (H.put h p (redirect h p r)) after rest)

