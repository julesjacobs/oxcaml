open Copy_spec
open Generalize_spec
open Generalize_proofs

let rec close : (h : Pref.heap) @ immutable ghost -> (cut : int) -> (pool : pool) @ immutable ->
    (t : {t : Pref.token | Pref.own t === h && pool_scoped h pool}) @ unique ->
    {t : Pref.token | Pref.own t === closed_heap h cut pool} @ unique = fun h cut pool t ->
  ghost_ (pool_scoped_def h pool; closed_heap_def h cut pool);
  match pool with
  | Empty -> t
  | Entry (p, rest) ->
    let t : {t : Pref.token | H.mem (Pref.own t) p} = t in
    let old = Pref.read p (borrow_ t) in let change = needs_close cut old.level in
    if not change then (
      let t : {t : Pref.token | Pref.own t === h && pool_scoped h rest} = t in
      let t = close h cut rest t in t)
    else (
    let v = close_cell cut old in
    let t : {t : Pref.token | H.mem (Pref.own t) p} = t in
    let t = Pref.write p v t in let mid = ghost_ (Pref.own (borrow_ t)) in
    ghost_ (let u = () in pool_write h p old cut rest (u));
    let t : {t : Pref.token | Pref.own t === mid && pool_scoped mid rest} = t in
    let t = close mid cut rest t in t)
