open Copy_spec
open Level_spec
open Generalize_spec
open Generalize_proofs
open Nested_pool_spec

let rec (transfer_listed @ total) : (h : node Pref.heap) @ immutable ->
    (child : pool) @ immutable -> (parent : pool) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | listed (transfer h child parent) x ===
      (listed parent x || (listed child x && retained h x))} @ ghost =
  fun h child parent x -> ghost_ (
    transfer_def h child parent; listed_def child x;
    let u = () in match child with Empty -> refine_ u
    | Entry (p, rest) ->
      if retained h p then (
        let next = Entry (p, parent) in listed_def next x;
        transfer_listed h rest next x; refine_ u)
      else (transfer_listed h rest parent x; refine_ u))

let rec (transfer_scoped @ total) : (h : node Pref.heap) @ immutable ->
    (child : pool) @ immutable -> (parent : pool) @ immutable ->
    {u : unit | pool_scoped h child && pool_scoped h parent} ->
    {u : unit | pool_scoped h (transfer h child parent)} @ ghost =
  fun h child parent premise -> ghost_ (
    let refine_ premise = premise in transfer_def h child parent;
    pool_scoped_def h child; let u = () in match child with
    | Empty -> refine_ u
    | Entry (p, rest) -> if retained h p then (
      let next = Entry (p, parent) in pool_scoped_def h next;
      transfer_scoped h rest next (refine_ u); refine_ u)
      else (transfer_scoped h rest parent (refine_ u); refine_ u))

let rec (closed_other_pool @ total) : (h : node Pref.heap) @ immutable ->
    (cut : int) -> (child : pool) @ immutable ->
    (parent : pool) @ immutable ->
    {u : unit | pool_scoped h child && pool_scoped h parent} ->
    {u : unit | pool_scoped (closed_heap h cut child) parent} @ ghost =
  fun h cut child parent premise -> ghost_ (
    let refine_ premise = premise in let after = closed_heap h cut child in
    pool_scoped_def h parent; pool_scoped_def after parent;
    let u = () in match parent with Empty -> refine_ u
    | Entry (p, rest) -> closed_observe h cut child p (refine_ u);
      closed_at_def h after cut child p;
      closed_source h cut child p (refine_ u);
      closed_other_pool h cut child rest (refine_ u); refine_ u)

let (close_transfer_coverage @ total) : (h : node Pref.heap) @ immutable ->
    (cut : int) -> (child : pool) @ immutable ->
    (parent : pool) @ immutable -> (outer : int) ->
    (x : node Pref.t) @ immutable ->
    {u : unit | pool_scoped h child &&
      (covered h outer parent x || listed child x)} ->
    {u : unit | covered (closed_heap h cut child) outer
      (transfer (closed_heap h cut child) child parent) x} @ ghost =
  fun h cut child parent outer x premise -> ghost_ (
    let refine_ premise = premise in let after = closed_heap h cut child in
    let out = transfer after child parent in let u = () in
    closed_observe h cut child x (refine_ u);
    closed_at_def h after cut child x;
    transfer_listed after child parent x; retained_def after x;
    covered_def h outer parent x; covered_def after outer out x;
    at_level_def h x; at_level_def after x;
    (match H.at h x with None -> () | Some v ->
      close_level_def cut v.level; ()); refine_ u)

let (transferred_bound @ total) : (h : node Pref.heap) @ immutable ->
    (cut : int) -> (child : pool) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | pool_scoped h child && listed child x &&
      retained (closed_heap h cut child) x} ->
    {u : unit | match at_level (closed_heap h cut child) x with
      Generic -> false | Finite n -> n <= cut} @ ghost =
  fun h cut child x premise -> ghost_ (
    let refine_ premise = premise in let after = closed_heap h cut child in
    let u = () in closed_observe h cut child x (refine_ u);
    closed_at_def h after cut child x; retained_def after x;
    at_level_def after x;
    (match H.at h x with None -> () | Some v ->
      close_level_def cut v.level; ()); refine_ u)

let (closed_retained @ total) : (h : node Pref.heap) @ immutable -> (cut : int) ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | pool_scoped h pool && close_level cut (at_level h p) === at_level h p} ->
    {u : unit | retained (closed_heap h cut pool) p === retained h p} @ ghost =
  fun h cut pool p premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    Generalize_proofs.closed_observe h cut pool p (refine_ u);
    let after = closed_heap h cut pool in closed_at_def h after cut pool p;
    at_level_def h p; at_level_def after p; retained_def h p; retained_def after p;
    refine_ u)
