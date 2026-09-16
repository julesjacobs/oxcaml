open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Structure_spec

let link : (h : Pref.heap) @ immutable ghost ->
    (source : tree) @ immutable ghost -> (target : tree) @ immutable ghost ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h && linkable h source target
      && tree_root source === p && tree_root target === q}) @ unique ->
    {r : result | Pref.own r.#state === H.put h p (redirect h p q)
      && r.#source === p && r.#target === q} @ unique = fun h source target p q state ->
    let refine_ state = state in
    ghost_ (linkable_def h source target; finite_def h source);
    let state : {t : Pref.token | H.mem (Pref.own t) p} = refine_ state in
    let refine_ old = Pref.read p (borrow_ state) in
    let refine_ state = state in let next = {old with desc = Link q} in
    ghost_ (redirect_def h p q);
    let state : {t : Pref.token | H.mem (Pref.own t) p} = refine_ state in
    let refine_ state = Pref.write p next state in
    let result = #{state; source = p; target = q} in refine_ result
