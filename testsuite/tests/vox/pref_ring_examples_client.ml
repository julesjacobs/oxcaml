open Pref_ring

let splice_first_node : (s : node) @ immutable -> (d : node) @ immutable -> (a : node)
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
      && not (a.next === b.next)}) @ unique ->
    {r : Pref.token | H.at (Pref.own r) d.next === Some (Some a)} @ unique =
  fun s d a b t ->
  let refine_ result = Pref_ring_splice.splice_demo s d a b t in
  ghost_ (ring_def (Pref.own (borrow_ result)) d [a; b];
    head_def [a; b] d);
  refine_ result

let reverse_first_node : (s : node) @ immutable -> (a : node) @ immutable -> (b :
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
      && not (b.next === c.next)}) @ unique ->
    {r : Pref.token | H.at (Pref.own r) s.next === Some (Some c)} @ unique =
  fun s a b c t ->
  let refine_ result = Pref_ring_reverse.reverse_demo s a b c t in
  ghost_ (ring_def (Pref.own (borrow_ result)) s [c; b; a];
    head_def [c; b; a] s);
  refine_ result
