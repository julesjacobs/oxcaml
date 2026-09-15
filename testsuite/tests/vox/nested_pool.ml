open Copy_spec
open Generalize_spec
open Nested_pool_spec
open Nested_pool_proofs

let rec drain : (h : Pref.heap) @ immutable ghost ->
    (child : pool) @ immutable -> (parent : pool) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h &&
      pool_scoped h child && pool_scoped h parent}) @ unique ->
    {r : closed | Pref.own r.#state === h &&
      r.#parent === transfer h child parent && pool_scoped h r.#parent}
      @ unique = fun h child parent state ->
  let refine_ state = state in
  ghost_ (pool_scoped_def h child; transfer_def h child parent);
  match child with
  | Empty -> let r = #{state; parent} in refine_ r
  | Entry (p, rest) ->
    let state : {t : Pref.token | H.mem (Pref.own t) p} = refine_ state in
    let refine_ v = Pref.read p (borrow_ state) in
    let refine_ state = state in ghost_ (retained_def h p);
    let next = match v.level with Generic -> parent
      | Finite _ -> Entry (p, parent) in
    ghost_ (pool_scoped_def h next);
    let state : {t : Pref.token | Pref.own t === h &&
      pool_scoped h rest && pool_scoped h next} = refine_ state in
    let refine_ r = drain h rest next state in refine_ r

let close : (h : Pref.heap) @ immutable ghost -> (cut : int) ->
    (child : pool) @ immutable -> (parent : pool) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h &&
      pool_scoped h child && pool_scoped h parent}) @ unique ->
    {r : closed | Pref.own r.#state === closed_heap h cut child &&
      r.#parent === transfer (closed_heap h cut child) child parent &&
      pool_scoped (Pref.own r.#state) r.#parent} @ unique =
  fun h cut child parent state ->
    let refine_ state = state in
    ghost_ (let u = () in closed_other_pool h cut child child (refine_ u);
      closed_other_pool h cut child parent (refine_ u));
    let state : {t : Pref.token | Pref.own t === h &&
      pool_scoped h child} = refine_ state in
    let refine_ state = Generalize.close h cut child state in
    let after = ghost_ (closed_heap h cut child) in
    let state : {t : Pref.token | Pref.own t === after &&
      pool_scoped after child && pool_scoped after parent} = refine_ state in
    let refine_ r = drain after child parent state in refine_ r
