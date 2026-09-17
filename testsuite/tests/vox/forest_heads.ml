open Copy_spec
open Level_unifier_spec
open Level_finite_spec
module R = Representative_level
module E = Effective_level

let (with_heads @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x &&
        (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (claim : bool) ->
    (use : ((heads : E.heads) @ total ->
      (witness : ((x : node Pref.t) @ immutable ->
        {u : unit | E.valid_head h heads x})) @ total ->
      {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun h trees claim use -> ghost_ (
    let raw : (x : node Pref.t) @ immutable total ->
        {r : R.representative | not (H.mem h x) || resolves h x r.root r.path} @ immutable total =
      fun x -> let u = () in
        if H.mem h x then (
          let refine_ t = trees x in
          let refine_ r = R.from_forest h t (refine_ u) in refine_ r)
        else let r = {R.root = x; path = Here} in refine_ r in
    let[@def] selected : E.heads = fun x -> let refine_ r = raw x in r in
    let witness : ((x : node Pref.t) @ immutable ->
        {u : unit | E.valid_head h selected x}) @ total = fun x ->
      selected_def x; let refine_ r = raw x in E.valid_head_def h selected x;
      let u = () in refine_ u in
    let refine_ u = use selected witness in refine_ u)

let (select @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x &&
        (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (x : node Pref.t) @ immutable total ->
    {r : R.representative | not (H.mem h x) || resolves h x r.root r.path} @ immutable total ghost =
  fun h trees x -> ghost_ (
    if H.mem h x then (
      let refine_ t = trees x in let u = () in
      let refine_ r = R.from_forest h t (refine_ u) in refine_ r)
    else let r = {R.root = x; path = Here} in refine_ r)
