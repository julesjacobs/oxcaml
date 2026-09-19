open Copy_spec
open Generalize_spec

type instance = #{value : node Pref.t @@ aliased; state : Pref.token;
  pool : pool @@ aliased; epoch : node Pref.t @@ ghost;
  history : history @@ ghost}

let[@def] (clear_memo @ total) (v : node @ immutable) =
  {v with memo = Empty_memo}
let[@def] rec (swept @ total) (h : Pref.heap @ immutable)
    (trail : pool @ immutable) = ghost_ (match trail with
  | Empty -> h
  | Entry (p, rest) -> match H.at h p with
    | None -> swept h rest
    | Some v -> swept (H.put h p (clear_memo v)) rest)
let[@def] (swept_at @ total) (h : Pref.heap @ immutable)
    (after : Pref.heap @ immutable) (trail : pool @ immutable)
    (x : node Pref.t @ immutable) = ghost_ (
  H.mem h x === H.mem after x && match H.at h x, H.at after x with
  | None, None -> true
  | Some a, Some b -> a.desc === b.desc && a.level === b.level && a.visited === b.visited
    && b.memo === (if listed trail x then Empty_memo else a.memo)
  | _ -> false)
