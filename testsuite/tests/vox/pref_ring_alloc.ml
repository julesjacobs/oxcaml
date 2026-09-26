open Pref_ring
open Pref_ring_proofs

let extend : (x : node) @ immutable -> (y : node) @ immutable -> (z : node) @
    immutable ->
  (flag : bool) -> (data : int) ->
  (t : {t : node option Pref.token | H.mem (Pref.own t) x.prev
    && H.mem (Pref.own t) x.next
    && H.mem (Pref.own t) y.prev
    && H.mem (Pref.own t) y.next
    && H.mem (Pref.own t) z.prev
    && H.mem (Pref.own t) z.next}) @ unique ->
  {r : created | let t = t in r.node.value = data
    && r.node.sentinel = flag
    && not (r.node.prev === r.node.next)
    && H.mem (Pref.own r.state) r.node.prev
      && H.at (Pref.own r.state) r.node.prev === Some (Some r.node)
    && H.mem (Pref.own r.state) r.node.next
      && H.at (Pref.own r.state) r.node.next === Some (Some r.node)
    && H.mem (Pref.own r.state) x.prev
    && H.at (Pref.own r.state) x.prev === H.at (Pref.own t) x.prev
    && not (r.node.prev === x.prev)
    && not (r.node.next === x.prev)
    && H.mem (Pref.own r.state) x.next
    && H.at (Pref.own r.state) x.next === H.at (Pref.own t) x.next
    && not (r.node.prev === x.next)
    && not (r.node.next === x.next)
    && H.mem (Pref.own r.state) y.prev
    && H.at (Pref.own r.state) y.prev === H.at (Pref.own t) y.prev
    && not (r.node.prev === y.prev)
    && not (r.node.next === y.prev)
    && H.mem (Pref.own r.state) y.next
    && H.at (Pref.own r.state) y.next === H.at (Pref.own t) y.next
    && not (r.node.prev === y.next)
    && not (r.node.next === y.next)
    && H.mem (Pref.own r.state) z.prev
    && H.at (Pref.own r.state) z.prev === H.at (Pref.own t) z.prev
    && not (r.node.prev === z.prev)
    && not (r.node.next === z.prev)
    && H.mem (Pref.own r.state) z.next
    && H.at (Pref.own r.state) z.next === H.at (Pref.own t) z.next
    && not (r.node.prev === z.next)
    && not (r.node.next === z.next)} @ unique =
  fun x y z flag data t ->
  let t = t in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let r = make_node flag data t in
  let n = r.node in
  let state = r.state in
  let proof = ghost_ (
    let h : {h : node option Pref.heap | H.mem h x.prev && H.mem h x.next
      && not (H.mem h n.prev) && not (H.mem h n.next)} = before in
    let proof = allocation_frame n x h in
    let result : {u : unit |
      H.mem (H.put (H.put (H.put (H.put before n.prev None) n.next None) n.prev
          (Some n)) n.next (Some n)) x.prev
        && H.mem (H.put (H.put (H.put (H.put before n.prev None) n.next None)
          n.prev (Some n)) n.next (Some n)) x.next
      && H.at (H.put (H.put (H.put (H.put before n.prev None) n.next None)
          n.prev (Some n)) n.next (Some n)) x.prev === H.at before x.prev
      && H.at (H.put (H.put (H.put (H.put before n.prev None) n.next None)
          n.prev (Some n)) n.next (Some n)) x.next === H.at before x.next
      && not (n.prev === x.prev) && not (n.prev === x.next)
      && not (n.next === x.prev)
        && not (n.next === x.next)} = proof in result) in
  let _proof = proof in
  let proof = ghost_ (
    let h : {h : node option Pref.heap | H.mem h y.prev && H.mem h y.next
      && not (H.mem h n.prev) && not (H.mem h n.next)} = before in
    let proof = allocation_frame n y h in
    let result : {u : unit |
      H.mem (H.put (H.put (H.put (H.put before n.prev None) n.next None) n.prev
          (Some n)) n.next (Some n)) y.prev
        && H.mem (H.put (H.put (H.put (H.put before n.prev None) n.next None)
          n.prev (Some n)) n.next (Some n)) y.next
      && H.at (H.put (H.put (H.put (H.put before n.prev None) n.next None)
          n.prev (Some n)) n.next (Some n)) y.prev === H.at before y.prev
      && H.at (H.put (H.put (H.put (H.put before n.prev None) n.next None)
          n.prev (Some n)) n.next (Some n)) y.next === H.at before y.next
      && not (n.prev === y.prev) && not (n.prev === y.next)
      && not (n.next === y.prev)
        && not (n.next === y.next)} = proof in result) in
  let _proof = proof in
  let proof = ghost_ (
    let h : {h : node option Pref.heap | H.mem h z.prev && H.mem h z.next
      && not (H.mem h n.prev) && not (H.mem h n.next)} = before in
    let proof = allocation_frame n z h in
    let result : {u : unit |
      H.mem (H.put (H.put (H.put (H.put before n.prev None) n.next None) n.prev
          (Some n)) n.next (Some n)) z.prev
        && H.mem (H.put (H.put (H.put (H.put before n.prev None) n.next None)
          n.prev (Some n)) n.next (Some n)) z.next
      && H.at (H.put (H.put (H.put (H.put before n.prev None) n.next None)
          n.prev (Some n)) n.next (Some n)) z.prev === H.at before z.prev
      && H.at (H.put (H.put (H.put (H.put before n.prev None) n.next None)
          n.prev (Some n)) n.next (Some n)) z.next === H.at before z.next
      && not (n.prev === z.prev) && not (n.prev === z.next)
      && not (n.next === z.prev)
        && not (n.next === z.next)} = proof in result) in
  let _proof = proof in
  let r = {node = n; state} in
  r

let make_frame () : {r : (int Pref.t, int) Pref.step |
    H.mem (Pref.own r.state) r.value
    && H.at (Pref.own r.state) r.value === Some 42} @ unique =
  let t = Pref.empty () in
  let v = 42 in
  let r = Pref.alloc v t in
  r
