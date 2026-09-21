open Pref_ring

let check_traversal : (sentinel : node) @ immutable ->
    (forward : node list) @ immutable -> (backward : node list) @ immutable ->
    (t : {t : node option Pref.token | sentinel.sentinel && present (Pref.own t) sentinel
      && H.at (Pref.own t) (field false sentinel) === Some (Some (head forward
          sentinel))
      && H.at (Pref.own t) (field true sentinel) === Some (Some (head backward
          sentinel))
      && path (Pref.own t) false forward sentinel
      && path (Pref.own t) true backward sentinel}) @ local read -> unit =
  fun sentinel forward backward t ->
  let refine_ t = t in
  let direction = false in
  let b : {t : node option Pref.token | sentinel.sentinel && present (Pref.own t) sentinel
      && H.at (Pref.own t) (field direction sentinel) === Some (Some (head
          forward sentinel))
      && path (Pref.own t) direction forward sentinel} = refine_ t in
  let refine_ observed = traverse direction sentinel forward b in
  assert (List.for_all2 ( == ) observed forward);
  let direction = true in
  let b : {t : node option Pref.token | sentinel.sentinel && present (Pref.own t) sentinel
      && H.at (Pref.own t) (field direction sentinel) === Some (Some (head
          backward sentinel))
      && path (Pref.own t) direction backward sentinel} = refine_ t in
  let refine_ observed = traverse direction sentinel backward b in
  assert (List.for_all2 ( == ) observed backward)

let (certify0 @ total) (stop : node @ immutable)
    (h : {h : node option Pref.heap | stop.sentinel
      && not (stop.prev === stop.next)
      && H.mem h stop.prev
      && H.mem h stop.next
      && H.at h stop.prev === Some (Some stop)
      && H.at h stop.next === Some (Some stop)} @ immutable) :
    {u : unit | let refine_ h = h in ring h stop []
      && path h false [] stop && path h true [] stop
      && present h stop && owns h (stop :: [])
      && H.at h (field false stop) === Some (Some (head [] stop))
      && H.at h (field true stop) === Some (Some (head [] stop))} @ ghost =
  let refine_ h = h in
  let expected = [] in
  let backward = [] in
  ghost_ (
    let refine_ unfolded = present_def h stop in
    let refine_ unfolded = ring_def h stop expected in
    let direction = false in
    let refine_ unfolded = field_def direction stop in
    let suffix = [] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let direction = true in
    let refine_ unfolded = field_def direction stop in
    let suffix = [] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let suffix = [] in
    let refine_ unfolded = linked_def h stop suffix stop in
    let suffix = [stop] in
    let refine_ unfolded = owns_def h suffix in
    let suffix = [] in
    let refine_ unfolded = owns_def h suffix in
    let u = () in
    let proof : {u : unit | ring h stop expected
      && path h false expected stop && path h true backward stop
      && present h stop && owns h (stop :: expected)
      && H.at h (field false stop) === Some (Some (head expected stop))
      && H.at h (field true stop) === Some (Some (head backward stop))
} = refine_ u in
    let refine_ proof = proof in refine_ proof)

let (certify1 @ total) (stop : node @ immutable) (n0 : node @ immutable)
    (h : {h : node option Pref.heap | stop.sentinel
      && not n0.sentinel
      && not (stop.prev === stop.next)
      && H.mem h stop.prev
      && H.mem h stop.next
      && not (n0.prev === n0.next)
      && H.mem h n0.prev
      && H.mem h n0.next
      && H.at h stop.prev === Some (Some n0)
      && H.at h stop.next === Some (Some n0)
      && H.at h n0.prev === Some (Some stop)
      && H.at h n0.next === Some (Some stop)} @ immutable) :
    {u : unit | let refine_ h = h in ring h stop [n0]
      && path h false [n0] stop && path h true [n0] stop
      && present h stop && owns h (stop :: [n0])
      && H.at h (field false stop) === Some (Some (head [n0] stop))
      && H.at h (field true stop) === Some (Some (head [n0] stop))} @ ghost =
  let refine_ h = h in
  let expected = [n0] in
  let backward = [n0] in
  ghost_ (
    let refine_ unfolded = present_def h stop in
    let refine_ unfolded = present_def h n0 in
    let refine_ unfolded = ring_def h stop expected in
    let direction = false in
    let refine_ unfolded = field_def direction stop in
    let suffix = [n0] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let refine_ unfolded = field_def direction n0 in
    let suffix = [] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let direction = true in
    let refine_ unfolded = field_def direction stop in
    let suffix = [n0] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let refine_ unfolded = field_def direction n0 in
    let suffix = [] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let suffix = [n0] in
    let refine_ unfolded = linked_def h stop suffix stop in
    let suffix = [] in
    let refine_ unfolded = linked_def h n0 suffix stop in
    let suffix = [stop; n0] in
    let refine_ unfolded = owns_def h suffix in
    let suffix = [n0] in
    let refine_ unfolded = owns_def h suffix in
    let suffix = [] in
    let refine_ unfolded = owns_def h suffix in
    let u = () in
    let proof : {u : unit | ring h stop expected
      && path h false expected stop && path h true backward stop
      && present h stop && owns h (stop :: expected)
      && H.at h (field false stop) === Some (Some (head expected stop))
      && H.at h (field true stop) === Some (Some (head backward stop))
} = refine_ u in
    let refine_ proof = proof in refine_ proof)

let (certify2 @ total) (stop : node @ immutable) (n0 : node @ immutable) (n1 :
    node @ immutable)
    (h : {h : node option Pref.heap | stop.sentinel
      && not n0.sentinel
      && not n1.sentinel
      && not (stop.prev === stop.next)
      && H.mem h stop.prev
      && H.mem h stop.next
      && not (n0.prev === n0.next)
      && H.mem h n0.prev
      && H.mem h n0.next
      && not (n1.prev === n1.next)
      && H.mem h n1.prev
      && H.mem h n1.next
      && H.at h stop.prev === Some (Some n1)
      && H.at h stop.next === Some (Some n0)
      && H.at h n0.prev === Some (Some stop)
      && H.at h n0.next === Some (Some n1)
      && H.at h n1.prev === Some (Some n0)
      && H.at h n1.next === Some (Some stop)} @ immutable) :
    {u : unit | let refine_ h = h in ring h stop [n0; n1]
      && path h false [n0; n1] stop && path h true [n1; n0] stop
      && present h stop && owns h (stop :: [n0; n1])
      && H.at h (field false stop) === Some (Some (head [n0; n1] stop))
      && H.at h (field true stop) === Some (Some (head [n1; n0] stop))} @ ghost
          =
  let refine_ h = h in
  let expected = [n0; n1] in
  let backward = [n1; n0] in
  ghost_ (
    let refine_ unfolded = present_def h stop in
    let refine_ unfolded = present_def h n0 in
    let refine_ unfolded = present_def h n1 in
    let refine_ unfolded = ring_def h stop expected in
    let direction = false in
    let refine_ unfolded = field_def direction stop in
    let suffix = [n0; n1] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let refine_ unfolded = field_def direction n0 in
    let suffix = [n1] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let refine_ unfolded = field_def direction n1 in
    let suffix = [] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let direction = true in
    let refine_ unfolded = field_def direction stop in
    let suffix = [n1; n0] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let refine_ unfolded = field_def direction n1 in
    let suffix = [n0] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let refine_ unfolded = field_def direction n0 in
    let suffix = [] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let suffix = [n0; n1] in
    let refine_ unfolded = linked_def h stop suffix stop in
    let suffix = [n1] in
    let refine_ unfolded = linked_def h n0 suffix stop in
    let suffix = [] in
    let refine_ unfolded = linked_def h n1 suffix stop in
    let suffix = [stop; n0; n1] in
    let refine_ unfolded = owns_def h suffix in
    let suffix = [n0; n1] in
    let refine_ unfolded = owns_def h suffix in
    let suffix = [n1] in
    let refine_ unfolded = owns_def h suffix in
    let suffix = [] in
    let refine_ unfolded = owns_def h suffix in
    let u = () in
    let proof : {u : unit | ring h stop expected
      && path h false expected stop && path h true backward stop
      && present h stop && owns h (stop :: expected)
      && H.at h (field false stop) === Some (Some (head expected stop))
      && H.at h (field true stop) === Some (Some (head backward stop))
} = refine_ u in
    let refine_ proof = proof in refine_ proof)

let (certify3 @ total) (stop : node @ immutable) (n0 : node @ immutable) (n1 :
    node @ immutable) (n2 : node @ immutable)
    (h : {h : node option Pref.heap | stop.sentinel
      && not n0.sentinel
      && not n1.sentinel
      && not n2.sentinel
      && not (stop.prev === stop.next)
      && H.mem h stop.prev
      && H.mem h stop.next
      && not (n0.prev === n0.next)
      && H.mem h n0.prev
      && H.mem h n0.next
      && not (n1.prev === n1.next)
      && H.mem h n1.prev
      && H.mem h n1.next
      && not (n2.prev === n2.next)
      && H.mem h n2.prev
      && H.mem h n2.next
      && H.at h stop.prev === Some (Some n2)
      && H.at h stop.next === Some (Some n0)
      && H.at h n0.prev === Some (Some stop)
      && H.at h n0.next === Some (Some n1)
      && H.at h n1.prev === Some (Some n0)
      && H.at h n1.next === Some (Some n2)
      && H.at h n2.prev === Some (Some n1)
      && H.at h n2.next === Some (Some stop)} @ immutable) :
    {u : unit | let refine_ h = h in ring h stop [n0; n1; n2]
      && path h false [n0; n1; n2] stop && path h true [n2; n1; n0] stop
      && present h stop && owns h (stop :: [n0; n1; n2])
      && H.at h (field false stop) === Some (Some (head [n0; n1; n2] stop))
      && H.at h (field true stop) === Some (Some (head [n2; n1; n0] stop))} @
          ghost =
  let refine_ h = h in
  let expected = [n0; n1; n2] in
  let backward = [n2; n1; n0] in
  ghost_ (
    let refine_ unfolded = present_def h stop in
    let refine_ unfolded = present_def h n0 in
    let refine_ unfolded = present_def h n1 in
    let refine_ unfolded = present_def h n2 in
    let refine_ unfolded = ring_def h stop expected in
    let direction = false in
    let refine_ unfolded = field_def direction stop in
    let suffix = [n0; n1; n2] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let refine_ unfolded = field_def direction n0 in
    let suffix = [n1; n2] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let refine_ unfolded = field_def direction n1 in
    let suffix = [n2] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let refine_ unfolded = field_def direction n2 in
    let suffix = [] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let direction = true in
    let refine_ unfolded = field_def direction stop in
    let suffix = [n2; n1; n0] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let refine_ unfolded = field_def direction n2 in
    let suffix = [n1; n0] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let refine_ unfolded = field_def direction n1 in
    let suffix = [n0] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let refine_ unfolded = field_def direction n0 in
    let suffix = [] in
    let refine_ unfolded = head_def suffix stop in
    let refine_ unfolded = path_def h direction suffix stop in
    let suffix = [n0; n1; n2] in
    let refine_ unfolded = linked_def h stop suffix stop in
    let suffix = [n1; n2] in
    let refine_ unfolded = linked_def h n0 suffix stop in
    let suffix = [n2] in
    let refine_ unfolded = linked_def h n1 suffix stop in
    let suffix = [] in
    let refine_ unfolded = linked_def h n2 suffix stop in
    let suffix = [stop; n0; n1; n2] in
    let refine_ unfolded = owns_def h suffix in
    let suffix = [n0; n1; n2] in
    let refine_ unfolded = owns_def h suffix in
    let suffix = [n1; n2] in
    let refine_ unfolded = owns_def h suffix in
    let suffix = [n2] in
    let refine_ unfolded = owns_def h suffix in
    let suffix = [] in
    let refine_ unfolded = owns_def h suffix in
    let u = () in
    let proof : {u : unit | ring h stop expected
      && path h false expected stop && path h true backward stop
      && present h stop && owns h (stop :: expected)
      && H.at h (field false stop) === Some (Some (head expected stop))
      && H.at h (field true stop) === Some (Some (head backward stop))
} = refine_ u in
    let refine_ proof = proof in refine_ proof)
