open Pref_ring
open Pref_ring_proofs

let build_source : (s : node) @ immutable -> (a : node) @ immutable -> (b :
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
      && not (b.next === c.next)}) @ unique -> {r : Pref.token | H.mem (Pref.own
          r) s.prev
      && H.mem (Pref.own r) s.next
      && H.at (Pref.own r) s.prev === Some (Some c)
      && H.at (Pref.own r) s.next === Some (Some a)
      && H.mem (Pref.own r) a.prev
      && H.mem (Pref.own r) a.next
      && H.at (Pref.own r) a.prev === Some (Some s)
      && H.at (Pref.own r) a.next === Some (Some b)
      && H.mem (Pref.own r) b.prev
      && H.mem (Pref.own r) b.next
      && H.at (Pref.own r) b.prev === Some (Some a)
      && H.at (Pref.own r) b.next === Some (Some c)
      && H.mem (Pref.own r) c.prev
      && H.mem (Pref.own r) c.next
      && H.at (Pref.own r) c.prev === Some (Some b)
      && H.at (Pref.own r) c.next === Some (Some s)} @ unique =
  fun s a b c t ->
  let t = t in
  let initial = ghost_ (Pref.own (borrow_ t)) in
  let p0 = s.next in
  let v0 = Some a in
  let p1 = a.prev in
  let v1 = Some s in
  let p2 = a.next in
  let v2 = Some b in
  let p3 = b.prev in
  let v3 = Some a in
  let p4 = b.next in
  let v4 = Some c in
  let p5 = c.prev in
  let v5 = Some b in
  let p6 = c.next in
  let v6 = Some s in
  let p7 = s.prev in
  let v7 = Some c in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let observed = ghost_ (put_observations h p0 v0 s) in
  let observed = ghost_ (put_observations h p0 v0 a) in
  let observed = ghost_ (put_observations h p0 v0 b) in
  let observed = ghost_ (put_observations h p0 v0 c) in
  let t : {t : Pref.token | H.mem (Pref.own t) p0} = t in
  let t = Pref.write p0 v0 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let observed = ghost_ (put_observations h p1 v1 s) in
  let observed = ghost_ (put_observations h p1 v1 a) in
  let observed = ghost_ (put_observations h p1 v1 b) in
  let observed = ghost_ (put_observations h p1 v1 c) in
  let t : {t : Pref.token | H.mem (Pref.own t) p1} = t in
  let t = Pref.write p1 v1 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let observed = ghost_ (put_observations h p2 v2 s) in
  let observed = ghost_ (put_observations h p2 v2 a) in
  let observed = ghost_ (put_observations h p2 v2 b) in
  let observed = ghost_ (put_observations h p2 v2 c) in
  let t : {t : Pref.token | H.mem (Pref.own t) p2} = t in
  let t = Pref.write p2 v2 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let observed = ghost_ (put_observations h p3 v3 s) in
  let observed = ghost_ (put_observations h p3 v3 a) in
  let observed = ghost_ (put_observations h p3 v3 b) in
  let observed = ghost_ (put_observations h p3 v3 c) in
  let t : {t : Pref.token | H.mem (Pref.own t) p3} = t in
  let t = Pref.write p3 v3 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let observed = ghost_ (put_observations h p4 v4 s) in
  let observed = ghost_ (put_observations h p4 v4 a) in
  let observed = ghost_ (put_observations h p4 v4 b) in
  let observed = ghost_ (put_observations h p4 v4 c) in
  let t : {t : Pref.token | H.mem (Pref.own t) p4} = t in
  let t = Pref.write p4 v4 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let observed = ghost_ (put_observations h p5 v5 s) in
  let observed = ghost_ (put_observations h p5 v5 a) in
  let observed = ghost_ (put_observations h p5 v5 b) in
  let observed = ghost_ (put_observations h p5 v5 c) in
  let t : {t : Pref.token | H.mem (Pref.own t) p5} = t in
  let t = Pref.write p5 v5 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let observed = ghost_ (put_observations h p6 v6 s) in
  let observed = ghost_ (put_observations h p6 v6 a) in
  let observed = ghost_ (put_observations h p6 v6 b) in
  let observed = ghost_ (put_observations h p6 v6 c) in
  let t : {t : Pref.token | H.mem (Pref.own t) p6} = t in
  let t = Pref.write p6 v6 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let observed = ghost_ (put_observations h p7 v7 s) in
  let observed = ghost_ (put_observations h p7 v7 a) in
  let observed = ghost_ (put_observations h p7 v7 b) in
  let observed = ghost_ (put_observations h p7 v7 c) in
  let t : {t : Pref.token | H.mem (Pref.own t) p7} = t in
  let t = Pref.write p7 v7 t in
  let u = () in
  let _expanded : {u : unit | Pref.own t === H.put (H.put (H.put (H.put (H.put
      (H.put (H.put (H.put (initial) s.next (Some a)) a.prev (Some s)) a.next
      (Some b)) b.prev (Some a)) b.next (Some c)) c.prev (Some b)) c.next (Some
      s)) s.prev (Some c)} = u in
  let expanded = _expanded in
  t
