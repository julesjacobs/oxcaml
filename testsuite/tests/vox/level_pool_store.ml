open Copy_spec
open Generalize_spec
open Level_pool_routing_spec
module L = Level_unifier_spec

type locations = (node Pref.t @ immutable total -> int)
type store = {depth : int; pending : pool; buckets : pool iarray}

let[@def] (at @ total) (s : store @ immutable) (i : int) = ghost_ (
  if i = s.depth then s.pending else bucket s.buckets i)

let[@def] (located @ total) (h : node Pref.heap @ immutable)
    (s : store @ immutable) (where : locations @ total)
    (p : node Pref.t @ immutable) = ghost_ (
  not (H.mem h p) || not (L.terminal h p) ||
    match Level_spec.at_level h p with
    | Generic -> true
    | Finite n -> 0 <= n && n <= where p && where p <= s.depth
      && listed (at s (where p)) p)

let (current_coverage @ total) : (h : node Pref.heap) @ immutable ->
    (s : store) @ immutable -> (where : locations) @ total ->
    (p : node Pref.t) @ immutable ->
    {u : unit | s.depth >= 0 && located h s where p} ->
    {u : unit | Representative_level.representative_covered
      h (s.depth - 1) s.pending p} @ ghost =
  fun h s where p premise -> ghost_ (
    let refine_ premise = premise in located_def h s where p;
    let i = where p in at_def s i;
    let cut = s.depth - 1 in
    Representative_level.representative_covered_def h cut s.pending p;
    covered_def h cut s.pending p; Level_spec.at_level_def h p;
    let u = () in refine_ u)

let (same_closing @ total) : (h : node Pref.heap) @ immutable ->
    (s : store) @ immutable -> (where : locations) @ total ->
    (virtual_pool : pool) @ immutable ->
    (physical : ((p : node Pref.t) @ immutable ->
      {u : unit | located h s where p})) @ total ->
    (logical : ((p : node Pref.t) @ immutable ->
      {u : unit | Representative_level.representative_covered
        h (s.depth - 1) virtual_pool p})) @ total ->
    {u : unit | s.depth > 0 && pool_scoped h s.pending
      && pool_scoped h virtual_pool} ->
    {u : unit | Representative_pool_spec.close_heap h (s.depth - 1) s.pending
      === Representative_pool_spec.close_heap h (s.depth - 1) virtual_pool}
      @ ghost =
  fun h s where virtual_pool physical logical premise -> ghost_ (
    let refine_ premise = premise in let cut = s.depth - 1 in
    let coverage : ((p : node Pref.t) @ immutable ->
      {u : unit | not (L.terminal h p) ||
        (match H.at h p with None -> true | Some v ->
          not (needs_close cut v.level) ||
            (listed s.pending p === listed virtual_pool p))}) @ total =
      fun p ->
        physical p; logical p; let u = () in
        current_coverage h s where p (refine_ u);
        Representative_level.representative_covered_def h cut s.pending p;
        Representative_level.representative_covered_def h cut virtual_pool p;
        covered_def h cut s.pending p; covered_def h cut virtual_pool p;
        Level_spec.at_level_def h p;
        if L.terminal h p then (
          L.terminal_def h p; L.observe_def h p;
          let ownership : {u : unit | not (listed virtual_pool p) || H.mem h p} =
            (if listed virtual_pool p then
              (Pooled_proofs.pool_member h virtual_pool p (refine_ u); refine_ u)
             else refine_ u) in
          let refine_ ownership = ownership in
          if listed s.pending p then
            (Pooled_proofs.pool_member h s.pending p (refine_ u); ()) else ());
        (match H.at h p with None -> () | Some v ->
          needs_close_def cut v.level; ());
        refine_ u in
    let u = () in
    Pool_closing_equivalence.same_representative_closing
      h cut s.pending virtual_pool coverage (refine_ u); refine_ u)

let (allocated @ total) : (h : node Pref.heap) @ immutable ->
    (s : store) @ immutable -> (where : locations) @ total ->
    (p : node Pref.t) @ immutable -> (desc : desc) @ immutable ->
    (next_where : locations) @ total -> (x : node Pref.t) @ immutable ->
    {u : unit | s.depth >= 0 && not (H.mem h p) && located h s where x
      && next_where x = (if x === p then s.depth else where x)} ->
    {u : unit | located (H.put h p (cell desc s.depth))
      {s with pending = Entry (p, s.pending)} next_where x} @ ghost =
  fun h s where p desc next_where x premise -> ghost_ (
    let refine_ premise = premise in
    let v = cell desc s.depth in cell_def desc s.depth;
    let after = H.put h p v in
    let next = {s with pending = Entry (p, s.pending)} in
    located_def h s where x; located_def after next next_where x;
    Copy_heap_proofs.put_frame h p v x;
    L.terminal_def h x; L.terminal_def after x;
    L.observe_def h x; L.observe_def after x;
    Level_spec.at_level_def h x; Level_spec.at_level_def after x;
    let old_i = where x in let new_i = next_where x in
    at_def s old_i; at_def next new_i;
    let pending = Entry (p, s.pending) in listed_def pending x;
    let u = () in refine_ u)

let (unified @ total) : (h : node Pref.heap) @ immutable ->
    (s : store) @ immutable -> (where : locations) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : Effective_unifier_spec.derivation) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Effective_unifier_spec.unified h p q ok after d
      && located h s where x} ->
    {u : unit | located after s where x} @ ghost =
  fun h s where p q ok after d x premise -> ghost_ (
    let refine_ premise = premise in
    located_def h s where x; located_def after s where x;
    let u = () in
    if L.terminal after x then (
      Effective_unifier_pool.unified_terminal h p q ok after d x (refine_ u);
      Effective_unifier_metadata.cells h p q ok after d x (refine_ u);
      Effective_unifier_metadata.cell_frame_def h after x;
      Level_spec.at_level_def h x; Level_spec.at_level_def after x;
      (match H.at h x, H.at after x with
      | Some a, Some b -> Level_spec.decreases_def a.level b.level; ()
      | _ -> ()); ()) else ();
    refine_ u)

let (copied @ total) : (h : node Pref.heap) @ immutable ->
    (s : store) @ immutable -> (where : locations) @ total ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (d : history) @ immutable ->
    (next_where : locations) @ total -> (x : node Pref.t) @ immutable ->
    {u : unit | Copy_certificate_spec.certified_valid h certificate epoch s.depth d
      && located h s where x
      && next_where x = (if H.mem h x then where x else s.depth)
      && (match Level_spec.at_level
        (Hm_effective_execution_spec.copy_heap h epoch s.depth d) x with
        Generic -> true | Finite n -> H.mem h x || (0 <= n && n <= s.depth))} ->
    {u : unit | located (Hm_effective_execution_spec.copy_heap h epoch s.depth d)
      {s with pending = Pooled_spec.registered s.pending epoch d} next_where x}
      @ ghost =
  fun h s where certificate epoch d next_where x premise -> ghost_ (
    let refine_ premise = premise in
    let raw = heap h epoch s.depth d in
    let after = Hm_effective_execution_spec.copy_heap h epoch s.depth d in
    let trail = Pooled_spec.touched d in
    let next = {s with pending = Pooled_spec.registered s.pending epoch d} in
    let u = () in
    Hm_effective_registration.history_at h certificate epoch s.depth d x (refine_ u);
    Hm_effective_registration.result_at h certificate epoch s.depth d x (refine_ u);
    Hm_effective_registration.copy_new_member h s.pending certificate epoch s.depth d x (refine_ u);
    Copy_cleanup_spec.swept_at_def raw after trail x;
    Pooled_proofs.registered_keeps s.pending epoch d x;
    located_def h s where x; located_def after next next_where x;
    let i = where x in let j = next_where x in at_def s i; at_def next j;
    L.terminal_def h x; L.terminal_def after x;
    L.observe_def h x; L.observe_def after x;
    Level_spec.at_level_def h x; Level_spec.at_level_def after x;
    refine_ u)

let (entered @ total) : (h : node Pref.heap) @ immutable ->
    (s : store) @ immutable -> (where : locations) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | s.depth >= 0 && s.depth + 1 >= 0
      && s.depth < Iarray.length s.buckets && located h s where x} ->
    {u : unit | located h
      {depth = s.depth + 1; pending = Empty;
        buckets = Vox_iarray.updated s.buckets s.depth s.pending} where x}
        @ ghost =
  fun h s where x premise -> ghost_ (
    let refine_ premise = premise in
    let buckets = Vox_iarray.updated s.buckets s.depth s.pending in
    let next = {depth = s.depth + 1; pending = Empty; buckets} in
    let i = where x in
    located_def h s where x; located_def h next where x;
    at_def s i; at_def next i; bucket_def buckets i;
    Vox_iarray.updated_read s.buckets s.depth s.pending i;
    bucket_def s.buckets i; let u = () in refine_ u)

let[@def] (closed_store @ total) (h : node Pref.heap @ immutable)
    (s : store @ immutable) = ghost_ (
  let cut = s.depth - 1 in
  let after = Representative_pool_spec.close_heap h cut s.pending in
  let retained = Representative_pool_spec.transfer_rep after s.pending Empty in
  let routed = route after retained s.buckets in
  {depth = cut; pending = bucket routed cut;
    buckets = Vox_iarray.updated routed cut Empty})

let (left @ total) : (h : node Pref.heap) @ immutable ->
    (s : store) @ immutable -> (where : locations) @ total ->
    (next_where : locations) @ total -> (x : node Pref.t) @ immutable ->
    {u : unit | s.depth > 0 && s.depth <= Iarray.length s.buckets
      && pool_scoped h s.pending && located h s where x
      && next_where x = (if where x = s.depth then
        (match destination
          (Representative_pool_spec.close_heap h (s.depth - 1) s.pending) x with
          None -> 0 | Some i -> i) else where x)} ->
    {u : unit | located
      (Representative_pool_spec.close_heap h (s.depth - 1) s.pending)
      (closed_store h s) next_where x} @ ghost =
  fun h s where next_where x premise -> ghost_ (
    let refine_ premise = premise in let cut = s.depth - 1 in
    let after = Representative_pool_spec.close_heap h cut s.pending in
    let empty = Empty in
    let retained = Representative_pool_spec.transfer_rep after s.pending empty in
    let routed = route after retained s.buckets in
    let next = closed_store h s in closed_store_def h s;
    let u = () in
    Representative_pool_spec.close_heap_def h cut s.pending;
    Representative_level.representatives_scoped h s.pending (refine_ u);
    let filtered = Representative_level.representatives h s.pending in
    Generalize_proofs.closed_observe h cut filtered x (refine_ u);
    closed_at_def h after cut filtered x;
    Representative_level.representatives_member h s.pending x;
    located_def h s where x; located_def after next next_where x;
    L.terminal_def h x; L.terminal_def after x;
    L.observe_def h x; L.observe_def after x;
    Level_spec.at_level_def h x; Level_spec.at_level_def after x;
    destination_def after x;
    (match H.at h x with None -> () | Some v -> close_level_def cut v.level; ());
    let i = where x in let j = next_where x in
    at_def s i; at_def next j;
    let cleared = Vox_iarray.updated routed cut empty in
    bucket_def cleared j; Vox_iarray.updated_read routed cut empty j;
    bucket_def routed j;
    let size = Iarray.length s.buckets in
    closed_routable h cut s.pending size (refine_ u);
    route_member after retained s.buckets j x (refine_ u);
    Representative_pool_proofs.transfer_member after s.pending empty x;
    listed_def empty x; Representative_pool_spec.retained_rep_def after x;
    Nested_pool_spec.retained_def after x;
    refine_ u)

let[@def] (located_at @ total) (h : node Pref.heap @ immutable)
    (s : store @ immutable) (p : node Pref.t @ immutable) (i : int) = ghost_ (
  not (H.mem h p) || not (L.terminal h p) ||
    match Level_spec.at_level h p with
    | Generic -> true
    | Finite n -> 0 <= n && n <= i && i <= s.depth && listed (at s i) p)

let (location_at @ total) : (h : node Pref.heap) @ immutable ->
    (s : store) @ immutable -> (where : locations) @ total ->
    (p : node Pref.t) @ immutable ->
    {u : unit | located h s where p === located_at h s p (where p)} @ ghost =
  fun h s where p -> ghost_ (
    located_def h s where p; let i = where p in located_at_def h s p i;
    let u = () in refine_ u)
