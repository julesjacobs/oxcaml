open Pref_ring

let build_source : (s : node) @ immutable -> (d : node) @ immutable -> (a :
    node) @ immutable -> (b : node) @ immutable -> (t : {t : Pref.token | true
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
      && not (a.next === b.next)}) @ unique -> {r : Pref.token | H.mem (Pref.own
          r) s.prev
      && H.mem (Pref.own r) s.next
      && H.at (Pref.own r) s.prev === Some (Some b)
      && H.at (Pref.own r) s.next === Some (Some a)
      && H.mem (Pref.own r) d.prev
      && H.mem (Pref.own r) d.next
      && H.at (Pref.own r) d.prev === Some (Some d)
      && H.at (Pref.own r) d.next === Some (Some d)
      && H.mem (Pref.own r) a.prev
      && H.mem (Pref.own r) a.next
      && H.at (Pref.own r) a.prev === Some (Some s)
      && H.at (Pref.own r) a.next === Some (Some b)
      && H.mem (Pref.own r) b.prev
      && H.mem (Pref.own r) b.next
      && H.at (Pref.own r) b.prev === Some (Some a)
      && H.at (Pref.own r) b.next === Some (Some s)} @ unique =
  fun s d a b t ->
  let refine_ t = t in
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
  let v4 = Some s in
  let p5 = s.prev in
  let v5 = Some b in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let refine_ observed = ghost_ (put_observations h p0 v0 s) in
  let refine_ observed = ghost_ (put_observations h p0 v0 d) in
  let refine_ observed = ghost_ (put_observations h p0 v0 a) in
  let refine_ observed = ghost_ (put_observations h p0 v0 b) in
  let t : {t : Pref.token | H.mem (Pref.own t) p0} = refine_ t in
  let refine_ t = Pref.write p0 v0 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let refine_ observed = ghost_ (put_observations h p1 v1 s) in
  let refine_ observed = ghost_ (put_observations h p1 v1 d) in
  let refine_ observed = ghost_ (put_observations h p1 v1 a) in
  let refine_ observed = ghost_ (put_observations h p1 v1 b) in
  let t : {t : Pref.token | H.mem (Pref.own t) p1} = refine_ t in
  let refine_ t = Pref.write p1 v1 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let refine_ observed = ghost_ (put_observations h p2 v2 s) in
  let refine_ observed = ghost_ (put_observations h p2 v2 d) in
  let refine_ observed = ghost_ (put_observations h p2 v2 a) in
  let refine_ observed = ghost_ (put_observations h p2 v2 b) in
  let t : {t : Pref.token | H.mem (Pref.own t) p2} = refine_ t in
  let refine_ t = Pref.write p2 v2 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let refine_ observed = ghost_ (put_observations h p3 v3 s) in
  let refine_ observed = ghost_ (put_observations h p3 v3 d) in
  let refine_ observed = ghost_ (put_observations h p3 v3 a) in
  let refine_ observed = ghost_ (put_observations h p3 v3 b) in
  let t : {t : Pref.token | H.mem (Pref.own t) p3} = refine_ t in
  let refine_ t = Pref.write p3 v3 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let refine_ observed = ghost_ (put_observations h p4 v4 s) in
  let refine_ observed = ghost_ (put_observations h p4 v4 d) in
  let refine_ observed = ghost_ (put_observations h p4 v4 a) in
  let refine_ observed = ghost_ (put_observations h p4 v4 b) in
  let t : {t : Pref.token | H.mem (Pref.own t) p4} = refine_ t in
  let refine_ t = Pref.write p4 v4 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let refine_ observed = ghost_ (put_observations h p5 v5 s) in
  let refine_ observed = ghost_ (put_observations h p5 v5 d) in
  let refine_ observed = ghost_ (put_observations h p5 v5 a) in
  let refine_ observed = ghost_ (put_observations h p5 v5 b) in
  let t : {t : Pref.token | H.mem (Pref.own t) p5} = refine_ t in
  let refine_ t = Pref.write p5 v5 t in
  let u = () in
  let _expanded : {u : unit | Pref.own t === H.put (H.put (H.put (H.put (H.put
      (H.put (initial) s.next (Some a)) a.prev (Some s)) a.next (Some b)) b.prev
      (Some a)) b.next (Some s)) s.prev (Some b)} = refine_ u in
  let refine_ expanded = _expanded in
  refine_ t
