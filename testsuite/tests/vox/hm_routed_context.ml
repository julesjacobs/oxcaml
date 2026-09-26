open Copy_spec
open Generalize_spec
module S = Level_pool_store
module P = Level_pool_execution

type context = {origin : S.store @@ ghost; store : S.store @@ ghost;
  execution : P.execution @@ ghost}

let[@def] (recorded @ total) (h : node Pref.heap @ immutable)
    (c : context @ immutable) = ghost_ (
  c.origin.S.depth = 0 && c.origin.S.pending === Empty
    && P.ran (H.empty ()) c.origin c.execution h c.store)

let (advance @ total) : (h : node Pref.heap) @ immutable ->
    (c : context) @ immutable -> (step : P.execution) @ immutable ->
    (after : node Pref.heap) @ immutable -> (next : S.store) @ immutable ->
    {u : unit | recorded h c && P.ran h c.store step after next} ->
    {r : context | recorded after r && r.origin === c.origin && r.store === next}
      @ ghost =
  fun h c step after next premise -> ghost_ (
    recorded_def h c;
    let execution = P.Sequence (c.execution, h, c.store, step) in
    let r = {origin = c.origin; store = next; execution} in
    let empty = H.empty () in
    P.ran_def empty c.origin execution after next;
    recorded_def after r; r)

let (position @ total) : (h : node Pref.heap) @ immutable ->
    (c : context) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | recorded h c} ->
    {i : int | S.located_at h c.store p i} @ ghost =
  fun h c p premise -> ghost_ (
    recorded_def h c;
    let empty = H.empty () in let zero = 0 in
    S.located_at_def empty c.origin p zero;
    P.preserves_location empty c.origin c.execution h c.store p zero ();
    let i = P.position empty c.origin c.execution p zero in i)

let (members @ total) : (h : node Pref.heap) @ immutable ->
    (c : context) @ immutable -> (i : int) -> (p : node Pref.t) @ immutable ->
    {u : unit | recorded h c} ->
    {u : unit | P.member h c.store i p} @ ghost =
  fun h c i p premise -> ghost_ (
    recorded_def h c;
    let empty = H.empty () in
    let facts : ((j : int) -> (x : node Pref.t) @ immutable ->
      {u : unit | P.member empty c.origin j x}) @ total = fun j x ->
      P.member_def empty c.origin j x; S.at_def c.origin j;
      let none = Empty in listed_def none x; () in
    P.preserves_members empty c.origin c.execution h c.store facts i p ();
    ())

let (scoped @ total) : (h : node Pref.heap) @ immutable ->
    (c : context) @ immutable ->
    (scope : ((p : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h p) || source_ok h p})) @ total ->
    {u : unit | recorded h c && 0 <= c.store.S.depth} ->
    {u : unit | pool_scoped h c.store.S.pending} @ ghost =
  fun h c scope premise -> ghost_ (
    let facts : ((i : int) -> (p : node Pref.t) @ immutable ->
      {u : unit | P.member h c.store i p}) @ total = fun i p ->
      members h c i p (); () in
    P.scoped h c.store facts scope c.store.S.depth ();
    S.at_def c.store c.store.S.depth; ())

let (allocated @ total) : (h : node Pref.heap) @ immutable ->
    (c : context) @ immutable -> (p : node Pref.t) @ immutable ->
    (desc : desc) @ immutable ->
    {u : unit | recorded h c && c.store.S.depth >= 0 && not (H.mem h p)} ->
    {r : context | r.origin === c.origin
      && r.store === {c.store with pending = Entry (p, c.store.S.pending)}
      && recorded (H.put h p (cell desc c.store.S.depth)) r} @ ghost =
  fun h c p desc premise -> ghost_ (
    let after = H.put h p (cell desc c.store.S.depth) in
    let next = {c.store with pending = Entry (p, c.store.S.pending)} in
    let step = P.Allocate (p, desc) in P.ran_def h c.store step after next;
    let r = advance h c step after next () in r)

let (copied @ total) : (h : node Pref.heap) @ immutable ->
    (c : context) @ immutable -> (epoch : node Pref.t) @ immutable ->
    (d : history) @ immutable ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    {u : unit | recorded h c && c.store.S.depth >= 0
      && Copy_certificate_spec.certified_valid h certificate epoch c.store.S.depth d} ->
    {r : context | r.origin === c.origin
      && r.store === {c.store with pending = Pooled_spec.registered c.store.S.pending epoch d}
      && recorded (Hm_effective_execution_spec.copy_heap h epoch c.store.S.depth d) r}
      @ ghost =
  fun h c epoch d certificate premise -> ghost_ (
    let after = Hm_effective_execution_spec.copy_heap h epoch c.store.S.depth d in
    let next = {c.store with pending = Pooled_spec.registered c.store.S.pending epoch d} in
    let step = P.Copy (epoch, d, certificate) in P.ran_def h c.store step after next;
    let r = advance h c step after next () in r)

let (unified @ total) : (h : node Pref.heap) @ immutable ->
    (c : context) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : node Pref.heap) @ immutable ->
    (d : Effective_unifier_spec.derivation) @ immutable ->
    {u : unit | recorded h c && Effective_unifier_spec.unified h p q ok after d} ->
    {r : context | r.origin === c.origin && r.store === c.store && recorded after r}
      @ ghost =
  fun h c p q ok after d premise -> ghost_ (
    let step = P.Unify (p, q, ok, d) in
    P.ran_def h c.store step after c.store;
    let r = advance h c step after c.store () in r)

let (entered @ total) : (h : node Pref.heap) @ immutable ->
    (c : context) @ immutable ->
    {u : unit | recorded h c && c.store.S.depth >= 0
      && c.store.S.depth + 1 >= 0 && c.store.S.depth < Iarray.length c.store.S.buckets} ->
    {r : context | r.origin === c.origin && recorded h r
      && r.store === {S.depth = c.store.S.depth + 1; pending = Empty;
        buckets = Vox_iarray.updated c.store.S.buckets c.store.S.depth c.store.S.pending}}
      @ ghost =
  fun h c premise -> ghost_ (
    let step = P.Enter in
    let next = {S.depth = c.store.S.depth + 1; pending = Empty;
      buckets = Vox_iarray.updated c.store.S.buckets c.store.S.depth c.store.S.pending} in
    P.ran_def h c.store step h next;
    let r = advance h c step h next () in r)

let (left @ total) : (h : node Pref.heap) @ immutable ->
    (c : context) @ immutable ->
    {u : unit | recorded h c && c.store.S.depth > 0
      && c.store.S.depth <= Iarray.length c.store.S.buckets
      && pool_scoped h c.store.S.pending} ->
    {r : context | r.origin === c.origin && r.store === S.closed_store h c.store
      && recorded (Representative_pool_spec.close_heap h (c.store.S.depth - 1) c.store.S.pending) r}
      @ ghost =
  fun h c premise -> ghost_ (
    let step = P.Leave in
    let next = S.closed_store h c.store in
    let after = Representative_pool_spec.close_heap h (c.store.S.depth - 1) c.store.S.pending in
    P.ran_def h c.store step after next;
    let r = advance h c step after next () in r)

let (capacity @ total) : (h : node Pref.heap) @ immutable -> (c : context) @ immutable ->
    {u : unit | recorded h c} ->
    {u : unit | Iarray.length c.store.S.buckets = Iarray.length c.origin.S.buckets}
      @ ghost = fun h c premise -> ghost_ (
    recorded_def h c; let empty = H.empty () in
    P.capacity empty c.origin c.execution h c.store (); ())

let (scoped_runtime @ total) : (h : node Pref.heap) @ immutable ->
    (c : context) @ immutable -> (heads : Effective_level.heads) @ total ->
    (depth : int) -> (pool : pool) @ immutable ->
    (facts : ((x : node Pref.t) @ immutable ->
      {u : unit | Hm_effective_runtime.runtime_at h heads depth pool x})) @ total ->
    {u : unit | recorded h c && c.store.S.depth >= 0} ->
    {u : unit | pool_scoped h c.store.S.pending} @ ghost =
  fun h c heads depth pool facts premise -> ghost_ (
    let scope : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || source_ok h x}) @ total = fun x ->
        facts x; Hm_effective_runtime.runtime_at_def h heads depth pool x;
        Hm_effective_runtime.safe_def h heads x; () in
    scoped h c scope (); ())

let (allocation_pool @ total) : (h : node Pref.heap) @ immutable ->
    (depth : int) -> (p : node Pref.t) @ immutable -> (desc : desc) @ immutable ->
    (pool : pool) @ immutable ->
    {u : unit | not (H.mem h p) && payload_scoped h (cell desc depth)
      && pool_scoped h pool} ->
    {u : unit | pool_scoped (H.put h p (cell desc depth)) (Entry (p, pool))}
      @ ghost = fun h depth p desc pool premise -> ghost_ (
    let v = cell desc depth in Pooled_allocation_proofs.allocation_pool h p v pool ();
    Pooled_allocation_proofs.allocation_source h p v p ();
    let after = H.put h p v in let next = Entry (p, pool) in
    pool_scoped_def after next; ())
