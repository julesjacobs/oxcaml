open Copy_spec
open Generalize_spec

let[@def] (close_one @ total) (h : Pref.heap @ immutable) (cut : int)
    (p : node Pref.t @ immutable) = ghost_ (
  match H.at h p with None -> h | Some v ->
    if needs_close cut v.level then H.put h p (close_cell cut v) else h)

let (one_commutes @ total) : (h : Pref.heap) @ immutable -> (cut : int) ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | close_one (close_one h cut p) cut q ===
      close_one (close_one h cut q) cut p} @ ghost =
  fun h cut p q -> ghost_ (
    close_one_def h cut p; close_one_def h cut q;
    let hp = close_one h cut p in let hq = close_one h cut q in
    close_one_def hp cut q; close_one_def hq cut p;
    (match H.at h p with None -> () | Some a ->
      needs_close_def cut a.level; close_cell_def cut a;
      close_level_def cut a.level;
      let next = close_cell cut a in
      needs_close_def cut next.level;
      Copy_heap_proofs.put_frame h p next q; ());
    (match H.at h q with None -> () | Some b ->
      needs_close_def cut b.level; close_cell_def cut b;
      close_level_def cut b.level;
      let next = close_cell cut b in
      needs_close_def cut next.level;
      Copy_heap_proofs.put_frame h q next p; ());
    (match H.at h p, H.at h q with
    | Some a, Some b ->
      let ap = close_cell cut a in let bq = close_cell cut b in
      H.commute_law h p ap q bq; ()
    | _ -> ());
    ())

let rec (one_pool_commutes @ total) : (h : Pref.heap) @ immutable ->
    (cut : int) -> (p : node Pref.t) @ immutable -> (pool : pool) @ immutable ->
    {u : unit | closed_heap (close_one h cut p) cut pool ===
      close_one (closed_heap h cut pool) cut p} @ ghost =
  fun h cut p pool -> ghost_ (
    let hp = close_one h cut p in
    closed_heap_def h cut pool; closed_heap_def hp cut pool;
    match pool with
    | Empty -> ()
    | Entry (q, rest) ->
      close_one_def h cut q; close_one_def hp cut q;
      one_commutes h cut p q;
      let hq = close_one h cut q in
      one_pool_commutes hq cut p rest; ())

let rec (pools_commute @ total) : (h : Pref.heap) @ immutable ->
    (cut : int) -> (a : pool) @ immutable -> (b : pool) @ immutable ->
    {u : unit | closed_heap (closed_heap h cut a) cut b ===
      closed_heap (closed_heap h cut b) cut a} @ ghost =
  fun h cut a b -> ghost_ (
    let hb = closed_heap h cut b in
    closed_heap_def h cut a; closed_heap_def hb cut a;
    match a with Empty -> ()
    | Entry (p, rest) ->
      close_one_def h cut p; close_one_def hb cut p;
      one_pool_commutes h cut p b;
      let hp = close_one h cut p in
      pools_commute hp cut rest b; ())

let rec (unchanged @ total) : (h : Pref.heap) @ immutable -> (cut : int) ->
    (pool : pool) @ immutable ->
    (facts : ((p : node Pref.t) @ immutable ->
      {u : unit | not (listed pool p) || close_one h cut p === h})) @ total ->
    {u : unit | closed_heap h cut pool === h} @ ghost =
  fun h cut pool facts -> ghost_ (
    closed_heap_def h cut pool;
    match pool with Empty -> ()
    | Entry (p, rest) ->
      facts p; listed_def pool p; close_one_def h cut p;
      let next : ((x : node Pref.t) @ immutable ->
        {u : unit | not (listed rest x) || close_one h cut x === h}) @ total =
        fun x -> facts x; listed_def pool x; () in
      unchanged h cut rest next; ())

let (absorb @ total) : (h : Pref.heap) @ immutable -> (cut : int) ->
    (a : pool) @ immutable -> (b : pool) @ immutable ->
    (coverage : ((p : node Pref.t) @ immutable ->
      {u : unit | not (listed b p) ||
        (match H.at h p with None -> true | Some v ->
          not (needs_close cut v.level) || listed a p)})) @ total ->
    {u : unit | pool_scoped h a} ->
    {u : unit | closed_heap (closed_heap h cut a) cut b ===
      closed_heap h cut a} @ ghost =
  fun h cut a b coverage premise -> ghost_ (
    let after = closed_heap h cut a in
    let facts : ((p : node Pref.t) @ immutable ->
      {u : unit | not (listed b p) || close_one after cut p === after}) @ total =
      fun p ->
        coverage p; Generalize_proofs.closed_observe h cut a p ();
        closed_at_def h after cut a p; close_one_def after cut p;
        (match H.at h p with None -> () | Some v ->
          needs_close_def cut v.level; close_level_def cut v.level; ());
        (match H.at after p with None -> () | Some v ->
          needs_close_def cut v.level; ());
        () in
    unchanged after cut b facts; ())

let (same_closing @ total) : (h : Pref.heap) @ immutable -> (cut : int) ->
    (a : pool) @ immutable -> (b : pool) @ immutable ->
    (coverage : ((p : node Pref.t) @ immutable ->
      {u : unit | match H.at h p with None -> true | Some v ->
        not (needs_close cut v.level) || (listed a p === listed b p)})) @ total ->
    {u : unit | pool_scoped h a && pool_scoped h b} ->
    {u : unit | closed_heap h cut a === closed_heap h cut b} @ ghost =
  fun h cut a b coverage premise -> ghost_ (
    let ab : ((p : node Pref.t) @ immutable ->
      {u : unit | not (listed b p) ||
        (match H.at h p with None -> true | Some v ->
          not (needs_close cut v.level) || listed a p)}) @ total =
        fun p -> coverage p; () in
    let ba : ((p : node Pref.t) @ immutable ->
      {u : unit | not (listed a p) ||
        (match H.at h p with None -> true | Some v ->
          not (needs_close cut v.level) || listed b p)}) @ total =
        fun p -> coverage p; () in
    absorb h cut a b ab ();
    absorb h cut b a ba ();
    pools_commute h cut a b; ())

let (same_representative_closing @ total) :
    (h : Pref.heap) @ immutable -> (cut : int) ->
    (a : pool) @ immutable -> (b : pool) @ immutable ->
    (coverage : ((p : node Pref.t) @ immutable ->
      {u : unit | not (Level_unifier_spec.terminal h p) ||
        (match H.at h p with None -> true | Some v ->
          not (needs_close cut v.level) || (listed a p === listed b p))})) @ total ->
    {u : unit | pool_scoped h a && pool_scoped h b} ->
    {u : unit | Representative_pool_spec.close_heap h cut a ===
      Representative_pool_spec.close_heap h cut b} @ ghost =
  fun h cut a b coverage premise -> ghost_ (
    let pa = Representative_level.representatives h a in
    let pb = Representative_level.representatives h b in
    let filtered : ((p : node Pref.t) @ immutable ->
      {u : unit | match H.at h p with None -> true | Some v ->
        not (needs_close cut v.level) || (listed pa p === listed pb p)})
        @ total = fun p ->
      coverage p;
      Representative_level.representatives_member h a p;
      Representative_level.representatives_member h b p;
      () in
    Representative_level.representatives_scoped h a ();
    Representative_level.representatives_scoped h b ();
    Representative_pool_spec.close_heap_def h cut a;
    Representative_pool_spec.close_heap_def h cut b;
    same_closing h cut pa pb filtered (); ())

let rec (transfer_scoped @ total) : (h : Pref.heap) @ immutable ->
    (child : pool) @ immutable -> (parent : pool) @ immutable ->
    {u : unit | pool_scoped h child && pool_scoped h parent} ->
    {u : unit | pool_scoped h
      (Representative_pool_spec.transfer_rep h child parent)} @ ghost =
  fun h child parent premise -> ghost_ (
    Representative_pool_spec.transfer_rep_def h child parent;
    pool_scoped_def h child; match child with
    | Empty -> ()
    | Entry (p, rest) -> if Representative_pool_spec.retained_rep h p then (
      let next = Entry (p, parent) in pool_scoped_def h next;
      transfer_scoped h rest next (); ())
      else (transfer_scoped h rest parent (); ()))

let (closed_transfer_scoped @ total) : (h : Pref.heap) @ immutable ->
    (cut : int) -> (child : pool) @ immutable -> (parent : pool) @ immutable ->
    {u : unit | pool_scoped h child && pool_scoped h parent} ->
    {u : unit | let after = Representative_pool_spec.close_heap h cut child in
      pool_scoped after (Representative_pool_spec.transfer_rep after child parent)}
      @ ghost = fun h cut child parent premise -> ghost_ (
    Representative_pool_spec.close_heap_def h cut child;
    Representative_level.representatives_scoped h child ();
    let filtered = Representative_level.representatives h child in
    Nested_pool_proofs.closed_other_pool h cut filtered child ();
    Nested_pool_proofs.closed_other_pool h cut filtered parent ();
    let after = Representative_pool_spec.close_heap h cut child in
    transfer_scoped after child parent (); ())
