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
      fun x -> if H.mem h x then (
          let t = trees x in
          let r = R.from_forest h t () in r)
        else let r = {R.root = x; path = Here} in r in
    let[@def] selected : E.heads = fun x -> let r = raw x in r in
    let witness : ((x : node Pref.t) @ immutable ->
        {u : unit | E.valid_head h selected x}) @ total = fun x ->
      selected_def x; let _ = raw x in E.valid_head_def h selected x;
      () in
    let () = use selected witness in ())

let (select @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x &&
        (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (x : node Pref.t) @ immutable total ->
    {r : R.representative | not (H.mem h x) || resolves h x r.root r.path} @ immutable total ghost =
  fun h trees x -> ghost_ (
    if H.mem h x then (
      let t = trees x in let r = R.from_forest h t () in r)
    else let r = {R.root = x; path = Here} in r)
