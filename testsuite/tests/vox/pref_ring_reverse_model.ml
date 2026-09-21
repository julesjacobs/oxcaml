open Pref_ring

let (contents @ total) (s : node @ immutable) (a : node @ immutable)
    (b : node @ immutable) (c : node @ immutable)
    (h : {h : Pref.heap | H.mem h s.prev
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
      && not (c.prev === c.next)} @ immutable) :
    {u : unit | H.mem (flipped_all h [s; a; b; c]) s.prev
      && H.mem (flipped_all h [s; a; b; c]) s.next
      && H.at (flipped_all h [s; a; b; c]) s.prev === Some (Some a)
      && H.at (flipped_all h [s; a; b; c]) s.next === Some (Some c)
      && H.mem (flipped_all h [s; a; b; c]) a.prev
      && H.mem (flipped_all h [s; a; b; c]) a.next
      && H.at (flipped_all h [s; a; b; c]) a.prev === Some (Some b)
      && H.at (flipped_all h [s; a; b; c]) a.next === Some (Some s)
      && H.mem (flipped_all h [s; a; b; c]) b.prev
      && H.mem (flipped_all h [s; a; b; c]) b.next
      && H.at (flipped_all h [s; a; b; c]) b.prev === Some (Some c)
      && H.at (flipped_all h [s; a; b; c]) b.next === Some (Some a)
      && H.mem (flipped_all h [s; a; b; c]) c.prev
      && H.mem (flipped_all h [s; a; b; c]) c.next
      && H.at (flipped_all h [s; a; b; c]) c.prev === Some (Some s)
      && H.at (flipped_all h [s; a; b; c]) c.next === Some (Some b)} @ ghost =
  ghost_ (
    let before = h in
    let ns = [s; a; b; c] in
    let suffix = [s; a; b; c] in
    let _unfolded = flipped_all_def before suffix in
    let _unfolded = flipped_def before s in
    let p = s.prev in
    let q = s.next in
    let _unfolded = value_def before p in
    let _unfolded = value_def before q in
    let _observed = flip_observations before s s in
    let _observed = flip_observations before s a in
    let _observed = flip_observations before s b in
    let _observed = flip_observations before s c in
    let h1 = flipped before s in
    let suffix = [a; b; c] in
    let _unfolded = flipped_all_def h1 suffix in
    let _unfolded = flipped_def h1 a in
    let p = a.prev in
    let q = a.next in
    let _unfolded = value_def h1 p in
    let _unfolded = value_def h1 q in
    let _observed = flip_observations h1 a s in
    let _observed = flip_observations h1 a a in
    let _observed = flip_observations h1 a b in
    let _observed = flip_observations h1 a c in
    let h2 = flipped h1 a in
    let suffix = [b; c] in
    let _unfolded = flipped_all_def h2 suffix in
    let _unfolded = flipped_def h2 b in
    let p = b.prev in
    let q = b.next in
    let _unfolded = value_def h2 p in
    let _unfolded = value_def h2 q in
    let _observed = flip_observations h2 b s in
    let _observed = flip_observations h2 b a in
    let _observed = flip_observations h2 b b in
    let _observed = flip_observations h2 b c in
    let h3 = flipped h2 b in
    let suffix = [c] in
    let _unfolded = flipped_all_def h3 suffix in
    let _unfolded = flipped_def h3 c in
    let p = c.prev in
    let q = c.next in
    let _unfolded = value_def h3 p in
    let _unfolded = value_def h3 q in
    let _observed = flip_observations h3 c s in
    let _observed = flip_observations h3 c a in
    let _observed = flip_observations h3 c b in
    let _observed = flip_observations h3 c c in
    let h4 = flipped h3 c in
    let empty = [] in
    let _unfolded = flipped_all_def h4 empty in
    let u = () in
    let _expanded : {u : unit | h4 === flipped_all before ns} = u in
    let u = () in
    let proof : {u : unit |
      H.at (flipped_all before ns) s.next === Some (Some c)
      && H.at (flipped_all before ns) s.prev === Some (Some a)
      && H.at (flipped_all before ns) a.prev === Some (Some b)
      && H.at (flipped_all before ns) a.next === Some (Some s)
      && H.at (flipped_all before ns) b.prev === Some (Some c)
      && H.at (flipped_all before ns) b.next === Some (Some a)
      && H.at (flipped_all before ns) c.prev === Some (Some s)
      && H.at (flipped_all before ns) c.next === Some (Some b)
      && H.mem (flipped_all before ns) s.prev
      && H.mem (flipped_all before ns) s.next
      && H.mem (flipped_all before ns) a.prev
      && H.mem (flipped_all before ns) a.next
      && H.mem (flipped_all before ns) b.prev
      && H.mem (flipped_all before ns) b.next
      && H.mem (flipped_all before ns) c.prev
      && H.mem (flipped_all before ns) c.next} = u in
    proof)
