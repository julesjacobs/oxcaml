open Copy_spec
open Generalize_spec
open Representative_level

open Representative_pool_spec

let rec close_and_transfer : (h : node Pref.heap Ghost.t) @ immutable -> (cut : int) ->
    (child : pool) @ immutable -> (parent : pool) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost &&
      pool_scoped h.Ghost.ghost child && pool_scoped h.Ghost.ghost parent}) @ unique ->
    {r : Nested_pool_spec.closed | Pref.own r.#state === close_heap h.Ghost.ghost cut child &&
      r.#parent === transfer_rep (close_heap h.Ghost.ghost cut child) child parent &&
      pool_scoped (Pref.own r.#state) r.#parent} @ unique =
  fun h cut child parent state ->
    let refine_ state = state in
    ghost_ (pool_scoped_def h.Ghost.ghost child;
      close_heap_def h.Ghost.ghost cut child; representatives_def h.Ghost.ghost child);
    match child with
    | Empty ->
      ghost_ (let empty = Empty in closed_heap_def h.Ghost.ghost cut empty;
        transfer_rep_def h.Ghost.ghost child parent);
      let r = #{Nested_pool_spec.state; parent} in refine_ r
    | Entry (p, rest) ->
      let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ old = Pref.read p (borrow_ state) in let refine_ state = state in
      ghost_ (Level_unifier_spec.observe_def h.Ghost.ghost p;
        Level_unifier_spec.terminal_def h.Ghost.ghost p);
      match old.desc with
      | Link _ ->
        ghost_ (let u = () in close_heap_def h.Ghost.ghost cut rest;
          representatives_scoped h.Ghost.ghost rest (refine_ u);
          let filtered = representatives h.Ghost.ghost rest in
          let after = close_heap h.Ghost.ghost cut rest in
          Generalize_proofs.closed_observe h.Ghost.ghost cut filtered p (refine_ u);
          closed_at_def h.Ghost.ghost after cut filtered p;
          Level_unifier_spec.observe_def after p; Level_unifier_spec.terminal_def after p;
          retained_rep_def after p; transfer_rep_def after child parent; ());
        let state : {t : node Pref.token | Pref.own t === h.Ghost.ghost &&
          pool_scoped h.Ghost.ghost rest && pool_scoped h.Ghost.ghost parent} = refine_ state in
        let refine_ out = close_and_transfer h cut rest parent state in refine_ out
      | Var | Bool | Arrow _ ->
        ghost_ (let filtered = representatives h.Ghost.ghost rest in
          let entry = Entry (p, filtered) in closed_heap_def h.Ghost.ghost cut entry; ());
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
            Generalize_proofs.pool_write h.Ghost.ghost p old cut parent (refine_ u);
            let v = close_cell cut old in
            let frame : ((x : node Pref.t) @ immutable ->
              {u : unit | Level_unifier_spec.observe h.Ghost.ghost x === Level_unifier_spec.observe mid x}) @ total = fun x ->
                Copy_heap_proofs.put_frame h.Ghost.ghost p v x; close_cell_def cut old;
                Level_unifier_spec.observe_def h.Ghost.ghost x; Level_unifier_spec.observe_def mid x;
                let u = () in refine_ u in
            filter_frame h.Ghost.ghost mid frame rest; ());
          needs_close_def cut old.level; close_cell_def cut old; close_level_def cut old.level;
          let v = close_cell cut old in Copy_heap_proofs.put_frame h.Ghost.ghost p v p;
          Level_spec.at_level_def mid p; retained_rep_def mid p; Nested_pool_spec.retained_def mid p;
          Level_unifier_spec.terminal_def mid p; Level_unifier_spec.observe_def mid p;
          pool_scoped_def mid next;
          Generalize_proofs.close_idempotent cut old.level;
          let level = Level_spec.at_level mid p in close_level_def cut level;
          close_heap_def mid cut rest; ());
        let state : {t : node Pref.token | Pref.own t === mid && pool_scoped mid rest && pool_scoped mid next} = refine_ state in
        let witness : node Pref.heap Ghost.t = {Ghost.ghost = mid} in
        let after = ghost_ (close_heap mid cut rest) in
        ghost_ (let u = () in Representative_pool_proofs.closed_retained_rep mid cut rest p (refine_ u);
          transfer_rep_def after child parent; ());
        let refine_ state = state in
        let refine_ out = close_and_transfer witness cut rest next (refine_ state) in refine_ out
