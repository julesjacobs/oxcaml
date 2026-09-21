open Copy_spec
open Level_spec
open Lower_locality_spec
open Effective_lower_spec
open Effective_lower_proofs
module E = Effective_level
module U = Level_unifier_spec

let write_level : (h : node Pref.heap Ghost.t) @ immutable -> (heads : E.heads Ghost.t) @ total ->
    (bound : int) -> (p : node Pref.t) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && bound >= 0
      && E.effective_active h.Ghost.ghost heads.Ghost.ghost p && U.terminal h.Ghost.ghost p && E.valid_head h.Ghost.ghost heads.Ghost.ghost p
      && match H.at h.Ghost.ghost p with None -> false | Some v -> effective_children_below h.Ghost.ghost heads.Ghost.ghost v.desc bound}) @ unique ->
    {r : Level_lower.written | effective_lower_valid h.Ghost.ghost heads.Ghost.ghost bound r.#edits
      && Pref.own r.#state === lower_heap h.Ghost.ghost bound r.#edits
      && E.effective_below (Pref.own r.#state) heads.Ghost.ghost p bound
      && confined r.#edits (Tip p)} @ unique =
  fun h heads bound p state ->
    let refine_ state = state in
    ghost_ (E.effective_active_def h.Ghost.ghost heads.Ghost.ghost p; let u = () in
      E.terminal_level h.Ghost.ghost heads.Ghost.ghost p (refine_ u); active_def h.Ghost.ghost p; at_level_def h.Ghost.ghost p;
      U.terminal_def h.Ghost.ghost p; U.observe_def h.Ghost.ghost p);
    let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
    let refine_ old = Pref.read p (borrow_ state) in let refine_ state = state in
    let v = lower_cell old bound in
    let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
    let refine_ state = Pref.write p v state in
    let edits = ghost_ (Lower (p, old, Keep)) in let after = ghost_ (lower_heap h.Ghost.ghost bound edits) in
    ghost_ (let empty = Keep in lower_heap_def h.Ghost.ghost bound empty;
      effective_lower_valid_def h.Ghost.ghost heads.Ghost.ghost bound empty;
      lower_heap_def h.Ghost.ghost bound edits; effective_lower_valid_def h.Ghost.ghost heads.Ghost.ghost bound edits;
      lower_cell_def old bound; Copy_heap_proofs.put_frame h.Ghost.ghost p v p;
      let u = () in lower_head h.Ghost.ghost heads.Ghost.ghost bound edits p (refine_ u);
      U.terminal_def after p; U.observe_def after p;
      E.terminal_level after heads.Ghost.ghost p (refine_ u);
      E.effective_below_def after heads.Ghost.ghost p bound; at_level_def after p;
      let tree = Tip p in contains_def tree p; confined_def edits tree; confined_def empty tree; ());
    let r = #{Level_lower.state; edits} in refine_ r
