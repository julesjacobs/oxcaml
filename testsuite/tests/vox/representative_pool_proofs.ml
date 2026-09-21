open Copy_spec
open Generalize_spec
open Representative_level

open Representative_pool_spec

let rec (transfer_member @ total) : (h : node Pref.heap) @ immutable ->
    (child : pool) @ immutable -> (parent : pool) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | listed (transfer_rep h child parent) x ===
      (listed parent x || (listed child x && retained_rep h x))} @ ghost =
  fun h child parent x -> ghost_ (
    transfer_rep_def h child parent; listed_def child x;
    let u = () in match child with Empty -> refine_ u
    | Entry (p, rest) ->
      if retained_rep h p then (
        let next = Entry (p, parent) in listed_def next x;
        transfer_member h rest next x; refine_ u)
      else (transfer_member h rest parent x; refine_ u))

let (closed_coverage @ total) : (h : node Pref.heap) @ immutable ->
    (cut : int) -> (child : pool) @ immutable ->
    (parent : pool) @ immutable -> (outer : int) ->
    (x : node Pref.t) @ immutable ->
    {u : unit | pool_scoped h child &&
      (representative_covered h outer parent x || listed child x)} ->
    {u : unit | representative_covered (close_heap h cut child) outer
      (transfer_rep (close_heap h cut child) child parent) x} @ ghost =
  fun h cut child parent outer x premise -> ghost_ (
    let refine_ premise = premise in close_heap_def h cut child;
    let filtered = representatives h child in
    let after = close_heap h cut child in let u = () in
    representatives_scoped h child (refine_ u);
    Generalize_proofs.closed_observe h cut filtered x (refine_ u);
    closed_at_def h after cut filtered x;
    transfer_member after child parent x;
    retained_rep_def after x; Nested_pool_spec.retained_def after x;
    representative_covered_def h outer parent x;
    let output = transfer_rep after child parent in
    representative_covered_def after outer output x;
    Level_unifier_spec.terminal_def h x; Level_unifier_spec.terminal_def after x;
    Level_unifier_spec.observe_def h x; Level_unifier_spec.observe_def after x;
    representatives_member h child x;
    covered_def h outer parent x; covered_def after outer output x;
    Level_spec.at_level_def h x; Level_spec.at_level_def after x;
    (match H.at h x with None -> () | Some v -> close_level_def cut v.level; ());
    refine_ u)

let (transferred_level @ total) : (h : node Pref.heap) @ immutable ->
    (cut : int) -> (child : pool) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | pool_scoped h child && listed child x && retained_rep (close_heap h cut child) x} ->
    {u : unit | match Level_spec.at_level (close_heap h cut child) x with
      Generic -> false | Finite n -> n <= cut} @ ghost =
  fun h cut child x premise -> ghost_ (
    let refine_ premise = premise in close_heap_def h cut child;
    let filtered = representatives h child in let after = close_heap h cut child in
    let u = () in representatives_scoped h child (refine_ u);
    Generalize_proofs.closed_observe h cut filtered x (refine_ u);
    closed_at_def h after cut filtered x;
    retained_rep_def after x; Nested_pool_spec.retained_def after x;
    Level_unifier_spec.terminal_def h x; Level_unifier_spec.terminal_def after x;
    Level_unifier_spec.observe_def h x; Level_unifier_spec.observe_def after x;
    representatives_member h child x;
    Level_spec.at_level_def after x;
    (match H.at h x with None -> () | Some v -> close_level_def cut v.level; ());
    refine_ u)


let (closed_retained_rep @ total) : (h : node Pref.heap) @ immutable -> (cut : int) ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | pool_scoped h pool &&
      close_level cut (Level_spec.at_level h p) === Level_spec.at_level h p} ->
    {u : unit | retained_rep (close_heap h cut pool) p === retained_rep h p} @ ghost =
  fun h cut pool p premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    close_heap_def h cut pool; representatives_scoped h pool (refine_ u);
    let filtered = representatives h pool in let after = close_heap h cut pool in
    Generalize_proofs.closed_observe h cut filtered p (refine_ u);
    closed_at_def h after cut filtered p;
    Nested_pool_proofs.closed_retained h cut filtered p (refine_ u);
    Level_unifier_spec.observe_def h p; Level_unifier_spec.observe_def after p;
    Level_unifier_spec.terminal_def h p; Level_unifier_spec.terminal_def after p;
    retained_rep_def h p; retained_rep_def after p; refine_ u)
