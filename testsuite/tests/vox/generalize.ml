open Copy_spec
open Generalize_spec
open Generalize_proofs

let rec close : (h : Pref.heap) @ immutable ghost -> (cut : int) -> (pool : pool) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h && pool_scoped h pool}) @ unique ->
    {t : Pref.token | Pref.own t === closed_heap h cut pool} @ unique = fun h cut pool t ->
  let refine_ t = t in ghost_ (pool_scoped_def h pool; closed_heap_def h cut pool);
  match pool with
  | Empty -> refine_ t
  | Entry (p, rest) ->
    let t : {t : Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let refine_ old = Pref.read p (borrow_ t) in let refine_ t = t in
    let v = close_cell cut old in
    let t : {t : Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let refine_ t = Pref.write p v t in let mid = ghost_ (Pref.own (borrow_ t)) in
    ghost_ (let u = () in pool_write h p old cut rest (refine_ u));
    let t : {t : Pref.token | Pref.own t === mid && pool_scoped mid rest} = refine_ t in
    let refine_ t = close mid cut rest t in refine_ t
