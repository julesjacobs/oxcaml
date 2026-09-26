open Copy_spec
open Level_spec
open Generalize_spec

type origin = Origin of node Pref.t * path [@@inductive]

let[@def] (originates @ total) (saved : node Pref.heap @ immutable)
    (h : node Pref.heap @ immutable) (cut : int) (x : node Pref.t @ immutable)
    (origin : origin @ immutable) = ghost_ (match origin with
  | Origin (root, path) -> below saved root cut && reaches h root x path)
