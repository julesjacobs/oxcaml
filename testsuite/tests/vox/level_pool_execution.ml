open Copy_spec
open Generalize_spec
open Level_pool_routing_spec
open Level_pool_store
module P = Representative_pool_spec
module E = Hm_effective_execution_spec

type execution =
  | Idle
  | Allocate of node Pref.t * desc
  | Copy of node Pref.t * history * Representative_certificate.certificate
  | Unify of node Pref.t * node Pref.t * bool * Effective_unifier_spec.derivation
  | Enter
  | Leave
  | Sequence of execution * Pref.heap * store * execution
  [@@inductive]

let[@def] rec (ran @ total) (h : Pref.heap @ immutable)
    (s : store @ immutable) (d : execution @ immutable)
    (after : Pref.heap @ immutable) (next : store @ immutable) = ghost_ (
  match d with
  | Idle -> after === h && next === s
  | Allocate (p, desc) -> s.depth >= 0 && not (H.mem h p)
    && after === H.put h p (cell desc s.depth)
    && next === {s with pending = Entry (p, s.pending)}
  | Copy (epoch, history, certificate) -> s.depth >= 0
    && Copy_certificate_spec.certified_valid h certificate epoch s.depth history
    && after === E.copy_heap h epoch s.depth history
    && next === {s with pending = Pooled_spec.registered s.pending epoch history}
  | Unify (p, q, ok, d) -> Effective_unifier_spec.unified h p q ok after d
    && next === s
  | Enter -> s.depth >= 0 && s.depth + 1 >= 0
    && s.depth < Iarray.length s.buckets && after === h
    && next === {depth = s.depth + 1; pending = Empty;
      buckets = Vox_iarray.updated s.buckets s.depth s.pending}
  | Leave -> s.depth > 0 && s.depth <= Iarray.length s.buckets
    && pool_scoped h s.pending
    && after === P.close_heap h (s.depth - 1) s.pending
    && next === closed_store h s
  | Sequence (a, middle, between, b) ->
    ran h s a middle between && ran middle between b after next)

let[@def] rec (position @ total) (h : Pref.heap @ immutable)
    (s : store @ immutable) (d : execution @ immutable)
    (p : node Pref.t @ immutable) (i : int) = ghost_ (
  match d with
  | Idle | Unify _ | Enter -> i
  | Allocate (q, _) -> if p === q then s.depth else i
  | Copy _ -> if H.mem h p then i else s.depth
  | Leave -> if i = s.depth then
      (match Level_pool_routing_spec.destination
        (P.close_heap h (s.depth - 1) s.pending) p with
        None -> 0 | Some j -> j) else i
  | Sequence (a, middle, between, b) ->
    position middle between b p (position h s a p i))

let rec (copy_new_level @ total) : (h : Pref.heap) @ immutable ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | Copy_certificate_spec.certified_valid h certificate epoch depth d} ->
    {u : unit | H.mem h x || not (H.mem (heap h epoch depth d) x)
      || Level_spec.at_level (heap h epoch depth d) x === Finite depth} @ ghost =
  fun h certificate epoch depth d x premise -> ghost_ (
    Copy_certificate_spec.certified_valid_def h certificate epoch depth d;
    heap_def h epoch depth d;
    let after = heap h epoch depth d in Level_spec.at_level_def after x;
    match d with
    | Clean -> ()
    | Start -> let desc = Bool in let v = cell desc depth in
      cell_def desc depth; Copy_heap_proofs.put_frame h epoch v x; ()
    | Fresh (rest, p, q, old, desc) ->
      copy_new_level h certificate epoch depth rest x ();
      let mid = heap h epoch depth rest in Level_spec.at_level_def mid x;
      let v = cell desc depth in cell_def desc depth;
      let first = H.put mid q v in
      let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
      Copy_heap_proofs.put_frame first p w x; ()
    | Alias (rest, p, q, old) ->
      copy_new_level h certificate epoch depth rest x ();
      let mid = heap h epoch depth rest in Level_spec.at_level_def mid x;
      let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
      Copy_heap_proofs.put_frame mid p w x; ())

let rec (preserves_location @ total) : (h : Pref.heap) @ immutable ->
    (s : store) @ immutable -> (d : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (next : store) @ immutable ->
    (x : node Pref.t) @ immutable -> (i : int) ->
    {u : unit | ran h s d after next && located_at h s x i} ->
    {u : unit | located_at after next x (position h s d x i)} @ ghost =
  fun h s d after next x i premise -> ghost_ (
    ran_def h s d after next;
    position_def h s d x i;
    let j = position h s d x i in
    let[@def] before_position : locations = fun _p -> i in
    let[@def] after_position : locations = fun _p -> j in
    before_position_def x; after_position_def x;
    location_at h s before_position x;
    location_at after next after_position x;
    location_at after next before_position x;
    match d with
    | Idle -> ()
    | Allocate (p, desc) ->
      allocated h s before_position p desc after_position x (); ()
    | Copy (epoch, history, certificate) ->
      let raw = heap h epoch s.depth history in let trail = Pooled_spec.touched history in
      Hm_effective_registration.result_at h certificate epoch s.depth history x ();
      Copy_cleanup_spec.swept_at_def raw after trail x;
      E.copy_heap_def h epoch s.depth history;
      copy_new_level h certificate epoch s.depth history x ();
      Level_spec.at_level_def raw x; Level_spec.at_level_def after x;
      located_at_def after next x j;
      if H.mem after x then (
        copied h s before_position certificate epoch history after_position x ();
        ())
      else ()
    | Unify (p, q, ok, proof) ->
      unified h s before_position p q ok after proof x (); ()
    | Enter -> entered h s before_position x (); ()
    | Leave -> left h s before_position after_position x (); ()
    | Sequence (a, middle, between, b) ->
      preserves_location h s a middle between x i ();
      let mid = position h s a x i in
      preserves_location middle between b after next x mid (); ())

let rec (extends @ total) : (h : Pref.heap) @ immutable ->
    (s : store) @ immutable -> (d : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (next : store) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | ran h s d after next} ->
    {u : unit | not (H.mem h x) || H.mem after x} @ ghost =
  fun h s d after next x premise -> ghost_ (
    ran_def h s d after next;
    match d with
    | Idle | Enter -> ()
    | Allocate (p, desc) -> let _ = cell desc s.depth in
      ()
    | Copy (epoch, history, certificate) ->
      Hm_effective_membership.copy_extends h certificate epoch s.depth history x
        (); ()
    | Unify (p, q, ok, proof) ->
      Effective_unifier_frame.unified_frame h p q ok after proof x ();
      ()
    | Leave -> let cut = s.depth - 1 in
      P.close_heap_def h cut s.pending;
      Representative_level.representatives_scoped h s.pending ();
      let filtered = Representative_level.representatives h s.pending in
      Generalize_proofs.closed_observe h cut filtered x ();
      closed_at_def h after cut filtered x; ()
    | Sequence (a, middle, between, b) ->
      extends h s a middle between x ();
      extends middle between b after next x (); ())

let[@def] (member @ total) (h : Pref.heap @ immutable)
    (s : store @ immutable) (i : int) (p : node Pref.t @ immutable) = ghost_ (
  i < 0 || i > s.depth || not (listed (at s i) p) || H.mem h p)

let rec (preserves_members @ total) : (h : Pref.heap) @ immutable ->
    (s : store) @ immutable -> (d : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (next : store) @ immutable ->
    (members : ((i : int) -> (p : node Pref.t) @ immutable ->
      {u : unit | member h s i p})) @ total ->
    (i : int) -> (x : node Pref.t) @ immutable ->
    {u : unit | ran h s d after next} ->
    {u : unit | member after next i x} @ ghost =
  fun h s d after next members i x premise -> ghost_ (
    ran_def h s d after next;
    member_def after next i x; at_def next i;
    members i x; member_def h s i x; at_def s i;
    extends h s d after next x ();
    match d with
    | Idle | Unify _ -> ()
    | Allocate (p, desc) ->
      let pending = Entry (p, s.pending) in listed_def pending x;
      let _ = cell desc s.depth in ()
    | Copy (epoch, history, certificate) ->
      let pending = Pooled_spec.registered s.pending epoch history in
      if listed pending x then (
        Hm_effective_registration.registered_member h certificate s.pending
          epoch s.depth history x ();
        Hm_effective_registration.result_at h certificate epoch s.depth history x ();
        let raw = heap h epoch s.depth history in let trail = Pooled_spec.touched history in
        Copy_cleanup_spec.swept_at_def raw after trail x;
        E.copy_heap_def h epoch s.depth history; ()) else ();
      ()
    | Enter ->
      let values = Vox_iarray.updated s.buckets s.depth s.pending in
      bucket_def values i; Vox_iarray.updated_read s.buckets s.depth s.pending i;
      bucket_def s.buckets i; let empty = Empty in listed_def empty x; ()
    | Leave ->
      let cut = s.depth - 1 in let empty = Empty in
      let retained = P.transfer_rep after s.pending empty in
      let routed = Level_pool_routing_spec.route after retained s.buckets in
      closed_store_def h s;
      let cleared = Vox_iarray.updated routed cut empty in
      bucket_def cleared i; bucket_def routed i;
      Vox_iarray.updated_read routed cut empty i;
      let size = Iarray.length s.buckets in
      Level_pool_routing_spec.closed_routable h cut s.pending size ();
      Level_pool_routing_spec.route_member after retained s.buckets i x ();
      Representative_pool_proofs.transfer_member after s.pending empty x;
      listed_def empty x;
      members s.depth x; member_def h s s.depth x; at_def s s.depth;
      ()
    | Sequence (a, middle, between, b) ->
      let prior : ((j : int) -> (p : node Pref.t) @ immutable ->
        {u : unit | member middle between j p}) @ total = fun j p ->
          preserves_members h s a middle between members j p
            (); () in
      preserves_members middle between b after next prior i x ();
      ())

let (scoped @ total) : (h : Pref.heap) @ immutable -> (s : store) @ immutable ->
    (members : ((i : int) -> (p : node Pref.t) @ immutable ->
      {u : unit | member h s i p})) @ total ->
    (scope : ((p : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h p) || source_ok h p})) @ total ->
    (i : int) -> {u : unit | 0 <= i && i <= s.depth} ->
    {u : unit | pool_scoped h (at s i)} @ ghost =
  fun h s members scope i premise -> ghost_ (
    let pool = at s i in
    let facts : ((p : node Pref.t) @ immutable ->
      {u : unit | not (listed pool p) || H.mem h p}) @ total = fun p ->
        members i p; member_def h s i p; () in
    let () = Pooled_proofs.pool_from_members h scope pool facts in ())

let rec (capacity @ total) : (h : Pref.heap) @ immutable ->
    (s : store) @ immutable -> (d : execution) @ immutable ->
    (after : Pref.heap) @ immutable -> (next : store) @ immutable ->
    {u : unit | ran h s d after next} ->
    {u : unit | Iarray.length s.buckets = Iarray.length next.buckets} @ ghost =
  fun h s d after next premise -> ghost_ (
    ran_def h s d after next;
    match d with
    | Idle | Allocate _ | Copy _ | Unify _ -> ()
    | Enter -> Vox_iarray.updated_length s.buckets s.depth s.pending; ()
    | Leave ->
      closed_store_def h s; let cut = s.depth - 1 in let empty = Empty in
      let retained = P.transfer_rep after s.pending empty in
      let routed = route after retained s.buckets in
      route_length after retained s.buckets;
      Vox_iarray.updated_length routed cut empty; ()
    | Sequence (a, middle, between, b) ->
      capacity h s a middle between ();
      capacity middle between b after next (); ())
