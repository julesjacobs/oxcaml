open Copy_spec
open Level_spec
open Generalize_spec
open Nested_pool_spec
open Nested_pool_proofs

let rec drain : (h : node Pref.heap Ghost.t) @ immutable ->
    (child : pool) @ immutable -> (parent : pool) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost &&
      pool_scoped h.Ghost.ghost child && pool_scoped h.Ghost.ghost parent}) @ unique ->
    {r : closed | Pref.own r.#state === h.Ghost.ghost &&
      r.#parent === transfer h.Ghost.ghost child parent && pool_scoped h.Ghost.ghost r.#parent}
      @ unique = fun h child parent state ->
  let refine_ state = state in
  ghost_ (pool_scoped_def h.Ghost.ghost child; transfer_def h.Ghost.ghost child parent);
  match child with
  | Empty -> let r = #{state; parent} in refine_ r
  | Entry (p, rest) ->
    let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
    let refine_ v = Pref.read p (borrow_ state) in
    let refine_ state = state in ghost_ (retained_def h.Ghost.ghost p);
    let next = match v.level with Generic -> parent
      | Finite _ -> Entry (p, parent) in
    ghost_ (pool_scoped_def h.Ghost.ghost next);
    let state : {t : node Pref.token | Pref.own t === h.Ghost.ghost &&
      pool_scoped h.Ghost.ghost rest && pool_scoped h.Ghost.ghost next} = refine_ state in
    let refine_ r = drain h rest next state in refine_ r

let rec close : (h : node Pref.heap Ghost.t) @ immutable -> (cut : int) ->
    (child : pool) @ immutable -> (parent : pool) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost &&
      pool_scoped h.Ghost.ghost child && pool_scoped h.Ghost.ghost parent}) @ unique ->
    {r : closed | Pref.own r.#state === closed_heap h.Ghost.ghost cut child &&
      r.#parent === transfer (closed_heap h.Ghost.ghost cut child) child parent &&
      pool_scoped (Pref.own r.#state) r.#parent} @ unique =
  fun h cut child parent state ->
    let refine_ state = state in
    ghost_ (pool_scoped_def h.Ghost.ghost child; closed_heap_def h.Ghost.ghost cut child);
    match child with
    | Empty ->
      ghost_ (transfer_def h.Ghost.ghost child parent);
      let r = #{state; parent} in refine_ r
    | Entry (p, rest) ->
      let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ old = Pref.read p (borrow_ state) in let refine_ state = state in
      let change = needs_close cut old.level in
      let mid = ghost_ (if change then H.put h.Ghost.ghost p (close_cell cut old) else h.Ghost.ghost) in
      let state : {t : node Pref.token | Pref.own t === mid} =
        if change then (
          let v = close_cell cut old in
          let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
          let refine_ state = Pref.write p v state in refine_ state
        ) else refine_ state in
      let refine_ state = state in
      let next = if change then parent else match old.level with
        | Generic -> parent | Finite _ -> Entry (p, parent) in
      ghost_ (let u = () in
        if change then (
          Generalize_proofs.pool_write h.Ghost.ghost p old cut rest (refine_ u);
          Generalize_proofs.pool_write h.Ghost.ghost p old cut parent (refine_ u); ());
        needs_close_def cut old.level; close_cell_def cut old; close_level_def cut old.level;
        let v = close_cell cut old in Copy_heap_proofs.put_frame h.Ghost.ghost p v p;
        at_level_def mid p; retained_def mid p; pool_scoped_def mid next;
        Generalize_proofs.close_idempotent cut old.level;
        let level = at_level mid p in close_level_def cut level; ());
      let state : {t : node Pref.token | Pref.own t === mid && pool_scoped mid rest && pool_scoped mid next} = refine_ state in
      let heap_witness : node Pref.heap Ghost.t = {Ghost.ghost = mid} in
      let refine_ state = state in
      let after = ghost_ (closed_heap mid cut rest) in
      ghost_ (let u = () in closed_retained mid cut rest p (refine_ u);
        transfer_def after child parent; ());
      let refine_ out = close heap_witness cut rest next (refine_ state) in
      refine_ out
