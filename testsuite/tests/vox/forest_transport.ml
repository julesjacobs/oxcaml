open Copy_spec
open Copy_heap_proofs
open Copy_model_proofs
open Level_unifier_spec
open Level_unifier_proofs
open Level_finite_spec
open Level_finite_proofs

let (mark_observe @ total) : (d : history) @ immutable -> (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (old : node) @ immutable -> (epoch : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | H.mem h p && H.at h p === Some old} ->
    {u : unit | H.mem (H.put h p (session_mark d old epoch q)) x === H.mem h x
      && observe (H.put h p (session_mark d old epoch q)) x === observe h x} @ ghost = fun d h p old epoch q x premise -> ghost_ (
  let v = session_mark d old epoch q in session_mark_def d old epoch q; mark_def old epoch q;
  observe_write h p v x; observe_def h x; ())
let rec (finite_frame @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable ->
    (frame : ((x : node Pref.t) @ immutable -> {u : unit | H.mem h x === H.mem after x && observe h x === observe after x})) @ total ->
    (t : tree) @ immutable -> {u : unit | finite h t} -> {u : unit | finite after t} @ ghost = fun h after frame t premise -> ghost_ (
  finite_def h t; finite_def after t; tree_root_def t;
  let x = tree_root t in frame x; match t with Free _ | Constant_tree _ -> ()
  | Alias_tree (_, c) -> finite_frame h after frame c (); ()
  | Branch (_, a, b) -> finite_frame h after frame a (); finite_frame h after frame b (); ())
let (mark_forest @ total) : (d : history) @ immutable -> (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (old : node) @ immutable -> (epoch : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (t : tree) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && H.at h p === Some old && tree_root t === x
      && (if H.mem h x then finite h t else observe h x === None)} ->
    {t : tree | tree_root t === x && (if H.mem (H.put h p (session_mark d old epoch q)) x then finite (H.put h p (session_mark d old epoch q)) t
      else observe (H.put h p (session_mark d old epoch q)) x === None)} @ immutable ghost = fun d h p old epoch q t x premise -> ghost_ (
  let after = H.put h p (session_mark d old epoch q) in
  let frame : ((x : node Pref.t) @ immutable -> {u : unit | H.mem h x === H.mem after x && observe h x === observe after x}) @ total =
    fun x -> let () = mark_observe d h p old epoch q x () in () in
  frame x; if H.mem h x then (finite_frame h after frame t (); t) else t)
let rec (copy_forest_at @ total) : (saved : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem saved x then finite saved t else observe saved x === None)} @ immutable)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d} ->
    {t : tree | tree_root t === x && (if H.mem (heap saved epoch depth d) x then finite (heap saved epoch depth d) t
      else observe (heap saved epoch depth d) x === None)} @ immutable ghost = fun saved trees epoch depth d x premise -> ghost_ (
  valid_def saved epoch depth d; heap_def saved epoch depth d; match d with
  | Clean -> let t = trees x in t
  | Start -> let desc = Bool in let v = cell desc depth in cell_def desc depth; allocatable_def saved v;
    let t = allocation_finite_at saved trees epoch v x () in t
  | Fresh (rest, p, q, old, desc) ->
    let mid = heap saved epoch depth rest in
    let prior : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem mid x then finite mid t else observe mid x === None)} @ immutable) @ total = fun x ->
      let t = copy_forest_at saved trees epoch depth rest x () in t in
    ready_scoped saved epoch depth rest old.desc desc ();
    let v = cell desc depth in cell_def desc depth; allocatable_def mid v;
    payload_scoped_def mid v;
    let t = allocation_finite_at mid prior q v x () in
    let h1 = H.put mid q v in history_grows saved epoch depth rest p (); let t = mark_forest rest h1 p old epoch q t x () in t
  | Alias (rest, p, q, old) -> let mid = heap saved epoch depth rest in
    let t = copy_forest_at saved trees epoch depth rest x () in
    history_grows saved epoch depth rest p ();
    let t = mark_forest rest mid p old epoch q t x () in t)

let (closed_forest_at @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (cut : int) -> (pool : Generalize_spec.pool) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | Generalize_spec.pool_scoped h pool} ->
    {t : tree | tree_root t === x && (if H.mem (Generalize_spec.closed_heap h cut pool) x then finite (Generalize_spec.closed_heap h cut pool) t
      else observe (Generalize_spec.closed_heap h cut pool) x === None)} @ immutable ghost = fun h trees cut pool x premise -> ghost_ (
  let after = Generalize_spec.closed_heap h cut pool in
  let frame : ((x : node Pref.t) @ immutable -> {u : unit | H.mem h x === H.mem after x && observe h x === observe after x}) @ total =
    fun x -> Generalize_proofs.closed_observe h cut pool x ();
      Generalize_spec.closed_at_def h after cut pool x; observe_def h x; observe_def after x; () in
  let t = trees x in frame x; if H.mem h x then (finite_frame h after frame t (); t) else t)

let[@def] rec (unfolding @ total) (t : tree @ immutable) = match t with
  | Free p | Constant_tree p -> Level_spec.Tip p
  | Alias_tree (p, c) -> Level_spec.Through (p, unfolding c)
  | Branch (p, a, b) -> Level_spec.Fork (p, unfolding a, unfolding b)
let (unfolding_root @ total) : (t : tree) @ immutable ->
    {u : unit | Level_spec.bound_root (unfolding t) === tree_root t} @ ghost = fun t -> ghost_ (
  tree_root_def t; unfolding_def t; let out = unfolding t in Level_spec.bound_root_def out; ())
let rec (unfolding_valid @ total) : (h : node Pref.heap) @ immutable -> (t : tree) @ immutable ->
    {u : unit | finite h t} -> {u : unit | Generalize_spec.unfolded h (unfolding t)} @ ghost = fun h t premise -> ghost_ (
  finite_def h t; tree_root_def t; unfolding_def t; unfolding_root t;
  let out = unfolding t in Level_spec.bound_root_def out; Generalize_spec.unfolded_def h out;
  let p = tree_root t in observe_def h p; match t with Free _ | Constant_tree _ -> ()
  | Alias_tree (_, c) -> unfolding_root c; unfolding_valid h c (); ()
  | Branch (_, a, b) -> unfolding_root a; unfolding_root b; unfolding_valid h a (); unfolding_valid h b (); ())

let (allocation_forest @ total) : (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    {u : unit | not (H.mem h p) && allocatable h v} ->
    ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem (H.put h p v) x then finite (H.put h p v) t else observe (H.put h p v) x === None)} @ immutable) @ total ghost =
  fun h trees p v premise -> ghost_ (
    let result : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem (H.put h p v) x then finite (H.put h p v) t else observe (H.put h p v) x === None)} @ immutable) @ total =
      fun x -> let t = allocation_finite_at h trees p v x () in t in result)
