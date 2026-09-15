open Copy_spec
open Generalize_spec

let[@def] (clear_memo @ total) (v : node @ immutable) =
  {v with memo = Empty_memo}
let[@def] rec (swept @ total) (h : node Pref.heap @ immutable)
    (trail : pool @ immutable) = ghost_ (match trail with
  | Empty -> h
  | Entry (p, rest) -> match H.at h p with
    | None -> swept h rest
    | Some v -> swept (H.put h p (clear_memo v)) rest)
let[@def] (swept_at @ total) (h : node Pref.heap @ immutable)
    (after : node Pref.heap @ immutable) (trail : pool @ immutable)
    (x : node Pref.t @ immutable) = ghost_ (
  H.mem h x === H.mem after x && match H.at h x, H.at after x with
  | None, None -> true
  | Some a, Some b -> a.desc === b.desc && a.level === b.level && a.visited === b.visited
    && b.memo === (if listed trail x then Empty_memo else a.memo)
  | _ -> false)
