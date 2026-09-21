open Pref_ring
open Pref_ring_checks

let reverse_demo : (s : node) @ immutable -> (a : node) @ immutable -> (b :
    node) @ immutable -> (c : node) @ immutable -> (t : {t : Pref.token | true
      && s.sentinel
      && not (s.prev === s.next)
      && H.mem (Pref.own t) s.prev
      && H.mem (Pref.own t) s.next
      && H.at (Pref.own t) s.prev === Some (Some s)
      && H.at (Pref.own t) s.next === Some (Some s)
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
      && not c.sentinel
      && not (c.prev === c.next)
      && H.mem (Pref.own t) c.prev
      && H.mem (Pref.own t) c.next
      && H.at (Pref.own t) c.prev === Some (Some c)
      && H.at (Pref.own t) c.next === Some (Some c)
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
      && not (s === c)
      && not (s.prev === c.prev)
      && not (s.prev === c.next)
      && not (s.next === c.prev)
      && not (s.next === c.next)
      && not (a === b)
      && not (a.prev === b.prev)
      && not (a.prev === b.next)
      && not (a.next === b.prev)
      && not (a.next === b.next)
      && not (a === c)
      && not (a.prev === c.prev)
      && not (a.prev === c.next)
      && not (a.next === c.prev)
      && not (a.next === c.next)
      && not (b === c)
      && not (b.prev === c.prev)
      && not (b.prev === c.next)
      && not (b.next === c.prev)
      && not (b.next === c.next)}) @ unique -> unit =
  fun s a b c t ->
  let t = Pref_ring_reverse_setup.build_source s a b c (t) in
  let forward = [a; b; c] in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let _proof = ghost_ (
    let refined : {h : Pref.heap | s.sentinel
      && not a.sentinel
      && not b.sentinel
      && not c.sentinel
      && not (s.prev === s.next)
      && H.mem h s.prev
      && H.mem h s.next
      && not (a.prev === a.next)
      && H.mem h a.prev
      && H.mem h a.next
      && not (b.prev === b.next)
      && H.mem h b.prev
      && H.mem h b.next
      && not (c.prev === c.next)
      && H.mem h c.prev
      && H.mem h c.next
      && H.at h s.prev === Some (Some c)
      && H.at h s.next === Some (Some a)
      && H.at h a.prev === Some (Some s)
      && H.at h a.next === Some (Some b)
      && H.at h b.prev === Some (Some a)
      && H.at h b.next === Some (Some c)
      && H.at h c.prev === Some (Some b)
      && H.at h c.next === Some (Some s)} = h in
    let proof = certify3 s a b c refined in
    let result : {u : unit | ring h s [a; b; c] && path h false [a; b; c] s
      && path h true [c; b; a] s && present h s && owns h (s :: [a; b; c])
      && H.at h (field false s) === Some (Some (head [a; b; c] s))
      && H.at h (field true s) === Some (Some (head [c; b; a] s))} =
          proof in result) in
  let direction = false in
  let collected : {ns : node list | ns === forward} =
    let borrowed = borrow_ t in
    let borrowed : {t : Pref.token | s.sentinel && present (Pref.own t) s
      && H.at (Pref.own t) (field direction s) === Some (Some (head forward s))
      && path (Pref.own t) direction forward s} = borrowed in
    let collected = traverse direction s forward borrowed in
    collected in
  let ns = s :: collected in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let t : {t : Pref.token | owns (Pref.own t) ns} = t in
  let t = reverse_nodes ns t in
  let _proof = ghost_ (
    let refined : {h : Pref.heap | H.mem h s.prev
      && H.mem h s.next
      && H.at h s.prev === Some (Some c)
      && H.at h s.next === Some (Some a)
      && H.mem h a.prev
      && H.mem h a.next
      && H.at h a.prev === Some (Some s)
      && H.at h a.next === Some (Some b)
      && H.mem h b.prev
      && H.mem h b.next
      && H.at h b.prev === Some (Some a)
      && H.at h b.next === Some (Some c)
      && H.mem h c.prev
      && H.mem h c.next
      && H.at h c.prev === Some (Some b)
      && H.at h c.next === Some (Some s)
      && not (s.prev === s.next)
      && not (s.prev === a.prev)
      && not (s.prev === a.next)
      && not (s.prev === b.prev)
      && not (s.prev === b.next)
      && not (s.prev === c.prev)
      && not (s.prev === c.next)
      && not (s.next === a.prev)
      && not (s.next === a.next)
      && not (s.next === b.prev)
      && not (s.next === b.next)
      && not (s.next === c.prev)
      && not (s.next === c.next)
      && not (a.prev === a.next)
      && not (a.prev === b.prev)
      && not (a.prev === b.next)
      && not (a.prev === c.prev)
      && not (a.prev === c.next)
      && not (a.next === b.prev)
      && not (a.next === b.next)
      && not (a.next === c.prev)
      && not (a.next === c.next)
      && not (b.prev === b.next)
      && not (b.prev === c.prev)
      && not (b.prev === c.next)
      && not (b.next === c.prev)
      && not (b.next === c.next)
      && not (c.prev === c.next)} = before in
    let proof = Pref_ring_reverse_model.contents s a b c refined in
    let result : {u : unit | H.mem (flipped_all before ns) s.prev
      && H.mem (flipped_all before ns) s.next
      && H.at (flipped_all before ns) s.prev === Some (Some a)
      && H.at (flipped_all before ns) s.next === Some (Some c)
      && H.mem (flipped_all before ns) a.prev
      && H.mem (flipped_all before ns) a.next
      && H.at (flipped_all before ns) a.prev === Some (Some b)
      && H.at (flipped_all before ns) a.next === Some (Some s)
      && H.mem (flipped_all before ns) b.prev
      && H.mem (flipped_all before ns) b.next
      && H.at (flipped_all before ns) b.prev === Some (Some c)
      && H.at (flipped_all before ns) b.next === Some (Some a)
      && H.mem (flipped_all before ns) c.prev
      && H.mem (flipped_all before ns) c.next
      && H.at (flipped_all before ns) c.prev === Some (Some s)
      && H.at (flipped_all before ns) c.next === Some (Some b)} = proof
          in result) in
  let reversed = [c; b; a] in
  let reversed_back = [a; b; c] in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let _proof = ghost_ (
    let refined : {h : Pref.heap | s.sentinel
      && not c.sentinel
      && not b.sentinel
      && not a.sentinel
      && not (s.prev === s.next)
      && H.mem h s.prev
      && H.mem h s.next
      && not (c.prev === c.next)
      && H.mem h c.prev
      && H.mem h c.next
      && not (b.prev === b.next)
      && H.mem h b.prev
      && H.mem h b.next
      && not (a.prev === a.next)
      && H.mem h a.prev
      && H.mem h a.next
      && H.at h s.prev === Some (Some a)
      && H.at h s.next === Some (Some c)
      && H.at h c.prev === Some (Some s)
      && H.at h c.next === Some (Some b)
      && H.at h b.prev === Some (Some c)
      && H.at h b.next === Some (Some a)
      && H.at h a.prev === Some (Some b)
      && H.at h a.next === Some (Some s)} = h in
    let proof = certify3 s c b a refined in
    let result : {u : unit | ring h s [c; b; a] && path h false [c; b; a] s
      && path h true [a; b; c] s && present h s && owns h (s :: [c; b; a])
      && H.at h (field false s) === Some (Some (head [c; b; a] s))
      && H.at h (field true s) === Some (Some (head [a; b; c] s))} =
          proof in result) in
  (let borrowed = borrow_ t in
  let borrowed : {t : Pref.token | s.sentinel && present (Pref.own t) s
    && H.at (Pref.own t) (field false s) === Some (Some (head reversed s))
    && H.at (Pref.own t) (field true s) === Some (Some (head reversed_back s))
    && path (Pref.own t) false reversed s
    && path (Pref.own t) true reversed_back s} = borrowed in
  check_traversal s reversed reversed_back borrowed);
  ()
