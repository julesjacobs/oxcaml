open Copy_spec
open Copy_heap_proofs
open Leaf_provenance_spec
open Leaf_provenance_proofs
open Provenance_spec
open Effective_copy_spec
open Effective_copy_heap_proofs
let rec (copy_leaf_origin @ total) : (saved : Pref.heap) @ immutable ->
    (h : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total -> (cut : int) ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : origin | not (low_var h x cut) || originates saved h cut x o}
      @ immutable)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid h heads epoch depth d && depth > cut} ->
    {o : origin | not (low_var (heap h epoch depth d) x cut) ||
      originates saved (heap h epoch depth d) cut x o} @ immutable ghost =
  fun saved h heads cut scope prior epoch depth d x premise -> ghost_ (
    effective_valid_def h heads epoch depth d;
    heap_def h epoch depth d; let u = () in match d with
    | Clean -> let o = prior x in o
    | Start -> scope epoch; let desc : desc = Bool in
      let v = cell desc depth in cell_def desc depth;
      let o = allocation_leaf_origin saved h cut prior epoch v x (u) in o
    | Fresh (rest, p, q, old, desc) -> let mid = heap h epoch depth rest in
      let prior1 : ((x : node Pref.t) @ immutable ->
        {o : origin | not (low_var mid x cut) || originates saved mid cut x o}
        @ immutable) @ total = fun x -> let u = () in
        let o = copy_leaf_origin saved h heads cut scope prior epoch depth rest x (u) in o in
      Effective_copy_metadata.history_scope h heads scope epoch depth rest q (u);
      let v = cell desc depth in cell_def desc depth;
      let o = allocation_leaf_origin saved mid cut prior1 q v x (u) in
      let h1 = H.put mid q v in
      history_grows h heads epoch depth rest p (u); put_frame mid q v p;
      mark_leaf_origin rest saved h1 cut p old epoch q x o (u); o
    | Alias (rest, p, q, old) -> let mid = heap h epoch depth rest in
      let o = copy_leaf_origin saved h heads cut scope prior epoch depth rest x (u) in
      history_grows h heads epoch depth rest p (u);
      mark_leaf_origin rest saved mid cut p old epoch q x o (u); o)


