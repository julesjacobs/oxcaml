open Copy_spec
open Copy_heap_proofs
open Level_spec
open Generalize_spec
open Pooled_spec

let (allocation_source @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p) && payload_scoped h v && (not (H.mem h x) || source_ok h x)} ->
    {u : unit | not (H.mem (H.put h p v) x) || source_ok (H.put h p v) x} @ ghost = fun h p v x premise -> ghost_ (
  let refine_ premise = premise in let after = H.put h p v in
  payload_scoped_def h v; source_ok_def h x; source_ok_def after x; put_frame h p v x;
  let u = () in
  (match v.memo with Empty_memo -> () | Memo (stamp, _) -> put_frame h p v stamp; ());
  (match v.desc with Var | Bool -> () | Link q -> put_frame h p v q; () | Arrow (a, b) -> put_frame h p v a; put_frame h p v b; ());
  (match H.at h x with None -> () | Some old ->
    (match old.memo with Empty_memo -> () | Memo (stamp, _) -> put_frame h p v stamp; ());
    (match old.desc with Var | Bool -> () | Link q -> put_frame h p v q; () | Arrow (a, b) -> put_frame h p v a; put_frame h p v b; ())); refine_ u)
let rec (allocation_pool @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (pool : pool) @ immutable ->
    {u : unit | not (H.mem h p) && payload_scoped h v && pool_scoped h pool} ->
    {u : unit | pool_scoped (H.put h p v) pool} @ ghost = fun h p v pool premise -> ghost_ (
  let refine_ premise = premise in let after = H.put h p v in pool_scoped_def h pool; pool_scoped_def after pool;
  let u = () in match pool with Empty -> refine_ u | Entry (x, rest) ->
    put_frame h p v x; allocation_source h p v x (refine_ u); allocation_pool h p v rest (refine_ u); refine_ u)
let (allocation_coverage @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (pool : pool) @ immutable -> (cut : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p) && covered h cut pool x} ->
    {u : unit | covered (H.put h p v) cut (Entry (p, pool)) x} @ ghost = fun h p v pool cut x premise -> ghost_ (
  let refine_ premise = premise in let after = H.put h p v in let next = Entry (p, pool) in
  covered_def h cut pool x; covered_def after cut next x; listed_def next x;
  at_level_def h x; at_level_def after x; put_frame h p v x; let u = () in refine_ u)


let (allocation_scope_at @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p) && payload_scoped h v} ->
    {u : unit | if H.mem (H.put h p v) x then source_ok (H.put h p v) x else H.at (H.put h p v) x === None} @ ghost =
  fun h scope p v x premise -> ghost_ (
    let refine_ premise = premise in scope x; let u = () in allocation_source h p v x (refine_ u);
    put_frame h p v x; refine_ u)
let (allocation_below @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (x : node Pref.t) @ immutable -> (bound : int) ->
    {u : unit | not (H.mem h p) && below h x bound} -> {u : unit | below (H.put h p v) x bound} @ ghost =
  fun h p v x bound premise -> ghost_ (
    let refine_ premise = premise in let after = H.put h p v in below_def h x bound; below_def after x bound;
    at_level_def h x; at_level_def after x; put_frame h p v x; let u = () in refine_ u)
let (allocation_children @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (desc : desc) @ immutable -> (bound : int) ->
    {u : unit | not (H.mem h p) && children_below h desc bound} ->
    {u : unit | children_below (H.put h p v) desc bound} @ ghost = fun h p v desc bound premise -> ghost_ (
  let refine_ premise = premise in let after = H.put h p v in children_below_def h desc bound; children_below_def after desc bound;
  let u = () in match desc with Var | Bool -> refine_ u | Link q -> allocation_below h p v q bound (refine_ u); refine_ u
  | Arrow (a, b) -> allocation_below h p v a bound (refine_ u); allocation_below h p v b bound (refine_ u); refine_ u)
let (allocation_ordered @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (desc : desc) @ immutable -> (depth : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p) && depth >= 0 && children_below h desc depth && ordered h x} ->
    {u : unit | ordered (H.put h p (cell desc depth)) x} @ ghost = fun h p desc depth x premise -> ghost_ (
  let refine_ premise = premise in let v = cell desc depth in cell_def desc depth;
  let after = H.put h p v in put_frame h p v x; ordered_def h x; ordered_def after x;
  let u = () in allocation_children h p v desc depth (refine_ u);
  (match H.at h x with None -> () | Some old -> match old.level with Generic -> () | Finite n ->
    allocation_children h p v old.desc n (refine_ u); ()); refine_ u)
