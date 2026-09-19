open Copy_spec
open Copy_heap_proofs
open Level_unifier_spec
open Level_finite_spec
open Level_finite_proofs
open Forest_transport
open Effective_copy_spec
open Effective_copy_heap_proofs
open Effective_copy_metadata
let rec (copy_forest_at @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem saved x then finite saved t else observe saved x === None)} @ immutable)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d} ->
    {t : tree | tree_root t === x && (if H.mem (heap saved epoch depth d) x then finite (heap saved epoch depth d) t
      else observe (heap saved epoch depth d) x === None)} @ immutable ghost = fun saved heads trees epoch depth d x premise -> ghost_ (
  effective_valid_def saved heads epoch depth d; heap_def saved epoch depth d; match d with
  | Clean -> let t = trees x in t
  | Start -> let desc = Bool in let v = cell desc depth in cell_def desc depth; allocatable_def saved v;
    let t = allocation_finite_at saved trees epoch v x () in t
  | Fresh (rest, p, q, old, desc) ->
    let mid = heap saved epoch depth rest in
    let prior : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem mid x then finite mid t else observe mid x === None)} @ immutable) @ total = fun x ->
      let t = copy_forest_at saved heads trees epoch depth rest x () in t in
    ready_scoped saved heads epoch depth rest old.desc desc ();
    let v = cell desc depth in cell_def desc depth; allocatable_def mid v;
    payload_scoped_def mid v;
    let t = allocation_finite_at mid prior q v x () in
    let h1 = H.put mid q v in history_grows saved heads epoch depth rest p (); let t = mark_forest rest h1 p old epoch q t x () in t
  | Alias (rest, p, q, old) -> let mid = heap saved epoch depth rest in
    let t = copy_forest_at saved heads trees epoch depth rest x () in
    history_grows saved heads epoch depth rest p ();
    let t = mark_forest rest mid p old epoch q t x () in t)

