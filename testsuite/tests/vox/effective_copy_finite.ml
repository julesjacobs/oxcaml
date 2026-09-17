open Copy_spec
open Copy_heap_proofs
open Level_unifier_spec
open Level_finite_spec
open Level_finite_proofs
open Forest_transport
open Effective_copy_spec
open Effective_copy_heap_proofs
open Effective_copy_metadata
let rec (copy_forest_at @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem saved x then finite saved t else observe saved x === None)} @ immutable)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d} ->
    {t : tree | tree_root t === x && (if H.mem (heap saved epoch depth d) x then finite (heap saved epoch depth d) t
      else observe (heap saved epoch depth d) x === None)} @ immutable ghost = fun saved heads trees epoch depth d x premise -> ghost_ (
  let refine_ premise = premise in effective_valid_def saved heads epoch depth d; heap_def saved epoch depth d; let u = () in
  match d with
  | Clean -> let refine_ t = trees x in refine_ t
  | Start -> let desc = Bool in let v = cell desc depth in cell_def desc depth; allocatable_def saved v;
    let refine_ t = allocation_finite_at saved trees epoch v x (refine_ u) in refine_ t
  | Fresh (rest, p, q, old, desc) ->
    let mid = heap saved epoch depth rest in
    let prior : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem mid x then finite mid t else observe mid x === None)} @ immutable) @ total = fun x ->
      let u = () in let refine_ t = copy_forest_at saved heads trees epoch depth rest x (refine_ u) in refine_ t in
    ready_scoped saved heads epoch depth rest old.desc desc (refine_ u);
    let v = cell desc depth in cell_def desc depth; allocatable_def mid v;
    payload_scoped_def mid v;
    let refine_ t = allocation_finite_at mid prior q v x (refine_ u) in
    let h1 = H.put mid q v in history_grows saved heads epoch depth rest p (refine_ u); put_frame mid q v p;
    let refine_ t = mark_forest rest h1 p old epoch q t x (refine_ u) in refine_ t
  | Alias (rest, p, q, old) -> let mid = heap saved epoch depth rest in
    let refine_ t = copy_forest_at saved heads trees epoch depth rest x (refine_ u) in
    history_grows saved heads epoch depth rest p (refine_ u);
    let refine_ t = mark_forest rest mid p old epoch q t x (refine_ u) in refine_ t)

