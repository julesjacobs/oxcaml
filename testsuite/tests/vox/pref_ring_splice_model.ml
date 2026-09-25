open Pref_ring
open Pref_ring_proofs

let[@def] (after @ total) (h : Pref.heap @ immutable)
    (s : node @ immutable) (d : node @ immutable)
    (a : node @ immutable) (b : node @ immutable) =
  ghost_ (H.put (H.put (H.put (H.put (H.put (H.put (h) s.next (Some s)) s.prev
      (Some s)) d.next (Some a)) a.prev (Some d)) b.next (Some d)) d.prev (Some
      b))

let (contents @ total) (s : node @ immutable) (d : node @ immutable)
    (a : node @ immutable) (b : node @ immutable)
    (h : {h : Pref.heap | H.mem h s.prev
      && H.mem h s.next
      && H.at h s.prev === Some (Some b)
      && H.at h s.next === Some (Some a)
      && H.mem h d.prev
      && H.mem h d.next
      && H.at h d.prev === Some (Some d)
      && H.at h d.next === Some (Some d)
      && H.mem h a.prev
      && H.mem h a.next
      && H.at h a.prev === Some (Some s)
      && H.at h a.next === Some (Some b)
      && H.mem h b.prev
      && H.mem h b.next
      && H.at h b.prev === Some (Some a)
      && H.at h b.next === Some (Some s)
      && not (s.prev === s.next)
      && not (s.prev === d.prev)
      && not (s.prev === d.next)
      && not (s.prev === a.prev)
      && not (s.prev === a.next)
      && not (s.prev === b.prev)
      && not (s.prev === b.next)
      && not (s.next === d.prev)
      && not (s.next === d.next)
      && not (s.next === a.prev)
      && not (s.next === a.next)
      && not (s.next === b.prev)
      && not (s.next === b.next)
      && not (d.prev === d.next)
      && not (d.prev === a.prev)
      && not (d.prev === a.next)
      && not (d.prev === b.prev)
      && not (d.prev === b.next)
      && not (d.next === a.prev)
      && not (d.next === a.next)
      && not (d.next === b.prev)
      && not (d.next === b.next)
      && not (a.prev === a.next)
      && not (a.prev === b.prev)
      && not (a.prev === b.next)
      && not (a.next === b.prev)
      && not (a.next === b.next)
      && not (b.prev === b.next)} @ immutable) :
    {u : unit | let refine_ h = h in H.mem (after h s d a b) s.prev
      && H.mem (after h s d a b) s.next
      && H.at (after h s d a b) s.prev === Some (Some s)
      && H.at (after h s d a b) s.next === Some (Some s)
      && H.mem (after h s d a b) d.prev
      && H.mem (after h s d a b) d.next
      && H.at (after h s d a b) d.prev === Some (Some b)
      && H.at (after h s d a b) d.next === Some (Some a)
      && H.mem (after h s d a b) a.prev
      && H.mem (after h s d a b) a.next
      && H.at (after h s d a b) a.prev === Some (Some d)
      && H.at (after h s d a b) a.next === Some (Some b)
      && H.mem (after h s d a b) b.prev
      && H.mem (after h s d a b) b.next
      && H.at (after h s d a b) b.prev === Some (Some a)
      && H.at (after h s d a b) b.next === Some (Some d)} @ ghost =
  let refine_ h = h in
  ghost_ (
    let before = h in
    let after_model = after h s d a b in
    let refine_ expanded = after_def h s d a b in
    let h0 = before in
    let p = s.next in
    let v = Some s in
    let refine_ observed = put_observations h0 p v s in
    let refine_ observed = put_observations h0 p v d in
    let refine_ observed = put_observations h0 p v a in
    let refine_ observed = put_observations h0 p v b in
    let h1 = H.put h0 p v in
    let p = s.prev in
    let v = Some s in
    let refine_ observed = put_observations h1 p v s in
    let refine_ observed = put_observations h1 p v d in
    let refine_ observed = put_observations h1 p v a in
    let refine_ observed = put_observations h1 p v b in
    let h2 = H.put h1 p v in
    let p = d.next in
    let v = Some a in
    let refine_ observed = put_observations h2 p v s in
    let refine_ observed = put_observations h2 p v d in
    let refine_ observed = put_observations h2 p v a in
    let refine_ observed = put_observations h2 p v b in
    let h3 = H.put h2 p v in
    let p = a.prev in
    let v = Some d in
    let refine_ observed = put_observations h3 p v s in
    let refine_ observed = put_observations h3 p v d in
    let refine_ observed = put_observations h3 p v a in
    let refine_ observed = put_observations h3 p v b in
    let h4 = H.put h3 p v in
    let p = b.next in
    let v = Some d in
    let refine_ observed = put_observations h4 p v s in
    let refine_ observed = put_observations h4 p v d in
    let refine_ observed = put_observations h4 p v a in
    let refine_ observed = put_observations h4 p v b in
    let h5 = H.put h4 p v in
    let p = d.prev in
    let v = Some b in
    let refine_ observed = put_observations h5 p v s in
    let refine_ observed = put_observations h5 p v d in
    let refine_ observed = put_observations h5 p v a in
    let refine_ observed = put_observations h5 p v b in
    let h6 = H.put h5 p v in
    let u = () in
    let expanded : {u : unit | h6 === after_model} = refine_ u in
    let refine_ expanded = expanded in
    let proof : {u : unit | H.mem after_model s.prev
      && H.mem after_model s.next
      && H.at after_model s.prev === Some (Some s)
      && H.at after_model s.next === Some (Some s)
      && H.mem after_model d.prev
      && H.mem after_model d.next
      && H.at after_model d.prev === Some (Some b)
      && H.at after_model d.next === Some (Some a)
      && H.mem after_model a.prev
      && H.mem after_model a.next
      && H.at after_model a.prev === Some (Some d)
      && H.at after_model a.next === Some (Some b)
      && H.mem after_model b.prev
      && H.mem after_model b.next
      && H.at after_model b.prev === Some (Some a)
      && H.at after_model b.next === Some (Some d)} = refine_ u in
    let refine_ proof = proof in refine_ proof)
