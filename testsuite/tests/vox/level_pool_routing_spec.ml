open Copy_spec
open Generalize_spec
module A = Vox_iarray

let[@def] (destination @ total) (h : Pref.heap @ immutable)
    (p : node Pref.t @ immutable) = ghost_ (
  match H.at h p with
  | Some {desc = (Var | Bool | Arrow _); level = Finite n; _} -> Some (if n < 0 then 0 else n)
  | _ -> None)

let[@def] (bucket @ total) (pools : pool iarray @ immutable) (i : int) =
  ghost_ (match A.at pools i with Some pool -> pool | None -> Empty)

let[@def] (insert @ total) (pools : pool iarray @ immutable)
    (i : int) (p : node Pref.t @ immutable) = ghost_ (
  A.updated pools i (Entry (p, bucket pools i)))

let[@def] rec (routable @ total) (h : Pref.heap @ immutable)
    (size : int) (pending : pool @ immutable) = ghost_ (
  match pending with Empty -> true | Entry (p, rest) ->
    H.mem h p && (match destination h p with
      None -> true | Some i -> 0 <= i && i < size)
    && routable h size rest)

let[@def] rec (route @ total) (h : Pref.heap @ immutable)
    (pending : pool @ immutable) (pools : pool iarray @ immutable) = ghost_ (
  match pending with Empty -> pools | Entry (p, rest) ->
    route h rest (match destination h p with
      None -> pools | Some i -> insert pools i p))

let (insert_length @ total) : (pools : pool iarray) @ immutable ->
    (i : int) -> (p : node Pref.t) @ immutable ->
    {u : unit | Iarray.length (insert pools i p) = Iarray.length pools}
    @ ghost = fun pools i p -> ghost_ (
  insert_def pools i p; let next = Entry (p, bucket pools i) in
  A.updated_length pools i next; ())

let (insert_member @ total) : (pools : pool iarray) @ immutable ->
    (i : int) -> (p : node Pref.t) @ immutable -> (j : int) ->
    (x : node Pref.t) @ immutable ->
    {u : unit | 0 <= i && i < Iarray.length pools} ->
    {u : unit | listed (bucket (insert pools i p) j) x ===
      (listed (bucket pools j) x || (i = j && p === x))} @ ghost =
  fun pools i p j x premise -> ghost_ (
    insert_def pools i p;
    let next = Entry (p, bucket pools i) in
    A.updated_read pools i next j;
    let changed = insert pools i p in bucket_def changed j;
    bucket_def pools j; listed_def next x;
    ())

let rec (route_length @ total) : (h : Pref.heap) @ immutable ->
    (pending : pool) @ immutable -> (pools : pool iarray) @ immutable ->
    {u : unit | Iarray.length (route h pending pools) = Iarray.length pools}
    @ ghost = fun h pending pools -> ghost_ (
  route_def h pending pools;
  match pending with Empty -> ()
  | Entry (p, rest) -> match destination h p with
    | None -> route_length h rest pools; ()
    | Some i -> insert_length pools i p;
      let next = insert pools i p in route_length h rest next; ())

let rec (route_member @ total) : (h : Pref.heap) @ immutable ->
    (pending : pool) @ immutable -> (pools : pool iarray) @ immutable ->
    (j : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | routable h (Iarray.length pools) pending} ->
    {u : unit | listed (bucket (route h pending pools) j) x ===
      (listed (bucket pools j) x ||
        (listed pending x && destination h x === Some j))} @ ghost =
  fun h pending pools j x premise -> ghost_ (
    let size = Iarray.length pools in routable_def h size pending;
    route_def h pending pools; listed_def pending x;
    match pending with Empty -> ()
    | Entry (p, rest) -> match destination h p with
      | None -> route_member h rest pools j x (); ()
      | Some i -> insert_length pools i p;
        insert_member pools i p j x ();
        let next = insert pools i p in
        route_member h rest next j x (); ())

let rec (routable_from_members @ total) : (h : Pref.heap) @ immutable ->
    (size : int) -> (pending : pool) @ immutable ->
    (facts : ((p : node Pref.t) @ immutable ->
      {u : unit | not (listed pending p) ||
        (H.mem h p && (match destination h p with
          None -> true | Some i -> 0 <= i && i < size))})) @ total ->
    {u : unit | routable h size pending} @ ghost =
  fun h size pending facts -> ghost_ (
    routable_def h size pending;
    match pending with Empty -> ()
    | Entry (p, rest) ->
      facts p; listed_def pending p;
      let next : ((x : node Pref.t) @ immutable ->
        {u : unit | not (listed rest x) ||
          (H.mem h x && (match destination h x with
            None -> true | Some i -> 0 <= i && i < size))}) @ total =
        fun x -> facts x; listed_def pending x; () in
      routable_from_members h size rest next; ())

let (closed_routable @ total) : (h : Pref.heap) @ immutable ->
    (cut : int) -> (child : pool) @ immutable -> (size : int) ->
    {u : unit | pool_scoped h child && 0 <= cut && cut < size} ->
    {u : unit | let after = Representative_pool_spec.close_heap h cut child in
      routable after size
        (Representative_pool_spec.transfer_rep after child Empty)} @ ghost =
  fun h cut child size premise -> ghost_ (
    let after = Representative_pool_spec.close_heap h cut child in
    let empty = Empty in
    let pending = Representative_pool_spec.transfer_rep after child empty in
    let facts : ((p : node Pref.t) @ immutable ->
      {u : unit | not (listed pending p) ||
        (H.mem after p && (match destination after p with
          None -> true | Some i -> 0 <= i && i < size))}) @ total = fun p ->
      Representative_pool_proofs.transfer_member after child empty p;
      listed_def empty p;
      if listed pending p then (
        Pooled_proofs.pool_member h child p ();
        Representative_pool_spec.close_heap_def h cut child;
        Representative_level.representatives_scoped h child ();
        let filtered = Representative_level.representatives h child in
        Generalize_proofs.closed_observe h cut filtered p ();
        closed_at_def h after cut filtered p;
        Representative_pool_proofs.transferred_level h cut child p ();
        Representative_pool_spec.retained_rep_def after p;
        Nested_pool_spec.retained_def after p;
        Level_spec.at_level_def after p;
        destination_def after p; ())
      else () in
    routable_from_members after size pending facts;
    ())
