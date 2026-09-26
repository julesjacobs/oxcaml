open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Structure_spec

let link : (h : (node Pref.heap) Ghost.t) @ immutable ->
    (source : (tree) Ghost.t) @ immutable ->
    (target : (tree) Ghost.t) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && linkable h.Ghost.ghost source.Ghost.ghost target.Ghost.ghost
      && tree_root source.Ghost.ghost === p && tree_root target.Ghost.ghost === q}) @ unique ->
    {r : result | Pref.own r.#state === H.put h.Ghost.ghost p (redirect h.Ghost.ghost p q)
      && r.#source === p && r.#target === q} @ unique = fun h source target p q state ->
    ghost_ (linkable_def h.Ghost.ghost source.Ghost.ghost target.Ghost.ghost; finite_def h.Ghost.ghost source.Ghost.ghost);
    let old = Pref.read p (borrow_ state) in
    let next = {old with desc = Link q} in
    ghost_ (redirect_def h.Ghost.ghost p q);
    let state = Pref.write p next state in
    let result = #{state; source = p; target = q} in result
