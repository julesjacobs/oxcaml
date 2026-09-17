open Copy_spec
open Generalize_spec
open Representative_level

let[@def] (close_heap @ total) (h : Pref.heap @ immutable)
    (cut : int) (pool : pool @ immutable) = ghost_ (
  closed_heap h cut (representatives h pool))

let[@def] (retained_rep @ total) (h : Pref.heap @ immutable)
    (p : node Pref.t @ immutable) = ghost_ (
  Level_unifier_spec.terminal h p && Nested_pool_spec.retained h p)

let[@def] rec (transfer_rep @ total) (h : Pref.heap @ immutable)
    (child : pool @ immutable) (parent : pool @ immutable) = ghost_ (
  match child with Empty -> parent | Entry (p, rest) ->
    transfer_rep h rest (if retained_rep h p then Entry (p, parent) else parent))

