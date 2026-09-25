open Pref_ring

val splice_demo : (s : node) @ immutable -> (d : node) @ immutable -> (a : node)
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
    {r : Pref.token | ring (Pref.own r) s [] && ring (Pref.own r) d [a; b] &&
      path (Pref.own r) false [a; b] d && path (Pref.own r) true [b; a] d} @ unique
