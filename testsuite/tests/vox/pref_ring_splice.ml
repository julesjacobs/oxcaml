open Pref_ring
open Pref_ring_checks

let splice_demo : (s : node) @ immutable -> (d : node) @ immutable -> (a : node)
    @ immutable -> (b : node) @ immutable -> (t : {t : Pref.token | true
      && s.sentinel
      && not (s.prev === s.next)
      && H.mem (Pref.own t) s.prev
      && H.mem (Pref.own t) s.next
      && H.at (Pref.own t) s.prev === Some (Some s)
      && H.at (Pref.own t) s.next === Some (Some s)
      && d.sentinel
      && not (d.prev === d.next)
      && H.mem (Pref.own t) d.prev
      && H.mem (Pref.own t) d.next
      && H.at (Pref.own t) d.prev === Some (Some d)
      && H.at (Pref.own t) d.next === Some (Some d)
      && not a.sentinel
      && not (a.prev === a.next)
      && H.mem (Pref.own t) a.prev
      && H.mem (Pref.own t) a.next
      && H.at (Pref.own t) a.prev === Some (Some a)
      && H.at (Pref.own t) a.next === Some (Some a)
      && not b.sentinel
      && not (b.prev === b.next)
      && H.mem (Pref.own t) b.prev
      && H.mem (Pref.own t) b.next
      && H.at (Pref.own t) b.prev === Some (Some b)
      && H.at (Pref.own t) b.next === Some (Some b)
      && not (s === d)
      && not (s.prev === d.prev)
      && not (s.prev === d.next)
      && not (s.next === d.prev)
      && not (s.next === d.next)
      && not (s === a)
      && not (s.prev === a.prev)
      && not (s.prev === a.next)
      && not (s.next === a.prev)
      && not (s.next === a.next)
      && not (s === b)
      && not (s.prev === b.prev)
      && not (s.prev === b.next)
      && not (s.next === b.prev)
      && not (s.next === b.next)
      && not (d === a)
      && not (d.prev === a.prev)
      && not (d.prev === a.next)
      && not (d.next === a.prev)
      && not (d.next === a.next)
      && not (d === b)
      && not (d.prev === b.prev)
      && not (d.prev === b.next)
      && not (d.next === b.prev)
      && not (d.next === b.next)
      && not (a === b)
      && not (a.prev === b.prev)
      && not (a.prev === b.next)
      && not (a.next === b.prev)
      && not (a.next === b.next)}) @ unique -> unit =
  fun s d a b t ->
  let refine_ t = t in
  let refine_ t = Pref_ring_splice_setup.build_source s d a b (refine_ t) in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let after_model = ghost_ (Pref_ring_splice_model.after before s d a b) in
  let refine_ expanded = ghost_ (Pref_ring_splice_model.after_def before s d a
      b) in
  let proof = ghost_ (
    let refined : {h : Pref.heap | H.mem h s.prev
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
      && not (b.prev === b.next)} = refine_ before in
    let refine_ proof = Pref_ring_splice_model.contents s d a b refined in
    let result : {u : unit | H.mem after_model s.prev
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
      && H.at after_model b.next === Some (Some d)} = refine_ proof in result)
          in
  let refine_ proof = proof in
  let t : {t : Pref.token |
    H.mem (Pref.own t) s.next && H.mem (Pref.own t) s.prev
    && H.mem (Pref.own t) d.next && H.mem (Pref.own t) a.prev
    && H.mem (Pref.own t) b.next && H.mem (Pref.own t) d.prev
    && H.at (Pref.own t) s.next === Some (Some a)
    && H.at (Pref.own t) a.prev === Some (Some s)
    && H.at (Pref.own t) b.next === Some (Some s)
    && H.at (Pref.own t) s.prev === Some (Some b)
    && H.at (Pref.own t) d.next === Some (Some d)
    && H.at (Pref.own t) d.prev === Some (Some d)} = refine_ t in
  let refine_ t = splice_range s a b s d d t in
  let source_empty = [] in
  let source_empty_back = [] in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let proof = ghost_ (
    let refined : {h : Pref.heap | s.sentinel
      && not (s.prev === s.next)
      && H.mem h s.prev
      && H.mem h s.next
      && H.at h s.prev === Some (Some s)
      && H.at h s.next === Some (Some s)} = refine_ h in
    let refine_ proof = certify0 s refined in
    let result : {u : unit | ring h s [] && path h false [] s
      && path h true [] s && present h s && owns h (s :: [])
      && H.at h (field false s) === Some (Some (head [] s))
      && H.at h (field true s) === Some (Some (head [] s))} = refine_ proof in
          result) in
  let refine_ proof = proof in
  (let borrowed = borrow_ t in
  let borrowed : {t : Pref.token | s.sentinel && present (Pref.own t) s
    && H.at (Pref.own t) (field false s) === Some (Some (head source_empty s))
    && H.at (Pref.own t) (field true s) === Some (Some (head source_empty_back
        s))
    && path (Pref.own t) false source_empty s
    && path (Pref.own t) true source_empty_back s} = refine_ borrowed in
  check_traversal s source_empty source_empty_back borrowed);
  let destination_pair = [a; b] in
  let destination_pair_back = [b; a] in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let proof = ghost_ (
    let refined : {h : Pref.heap | d.sentinel
      && not a.sentinel
      && not b.sentinel
      && not (d.prev === d.next)
      && H.mem h d.prev
      && H.mem h d.next
      && not (a.prev === a.next)
      && H.mem h a.prev
      && H.mem h a.next
      && not (b.prev === b.next)
      && H.mem h b.prev
      && H.mem h b.next
      && H.at h d.prev === Some (Some b)
      && H.at h d.next === Some (Some a)
      && H.at h a.prev === Some (Some d)
      && H.at h a.next === Some (Some b)
      && H.at h b.prev === Some (Some a)
      && H.at h b.next === Some (Some d)} = refine_ h in
    let refine_ proof = certify2 d a b refined in
    let result : {u : unit | ring h d [a; b] && path h false [a; b] d
      && path h true [b; a] d && present h d && owns h (d :: [a; b])
      && H.at h (field false d) === Some (Some (head [a; b] d))
      && H.at h (field true d) === Some (Some (head [b; a] d))} = refine_ proof
          in result) in
  let refine_ proof = proof in
  (let borrowed = borrow_ t in
  let borrowed : {t : Pref.token | d.sentinel && present (Pref.own t) d
    && H.at (Pref.own t) (field false d) === Some (Some (head destination_pair
        d))
    && H.at (Pref.own t) (field true d) === Some (Some (head
        destination_pair_back d))
    && path (Pref.own t) false destination_pair d
    && path (Pref.own t) true destination_pair_back d} = refine_ borrowed in
  check_traversal d destination_pair destination_pair_back borrowed);
  ()
