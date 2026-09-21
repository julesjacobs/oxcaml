open Pref_ring
open Pref_ring_checks
open Pref_ring_heap

let insert_remove_demo : (s : node) @ immutable -> (a : node) @ immutable -> (b
    : node) @ immutable -> (t : {t : node option Pref.token | true
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
      && not (a === b)
      && not (a.prev === b.prev)
      && not (a.prev === b.next)
      && not (a.next === b.prev)
      && not (a.next === b.next)}) @ unique -> unit =
  fun s a b t ->
  let refine_ t = t in
  let empty = [] in
  let empty_back = [] in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let proof = ghost_ (
    let refined : {h : node option Pref.heap | s.sentinel
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
  let borrowed : {t : node option Pref.token | s.sentinel && present (Pref.own t) s
    && H.at (Pref.own t) (field false s) === Some (Some (head empty s))
    && H.at (Pref.own t) (field true s) === Some (Some (head empty_back s))
    && path (Pref.own t) false empty s
    && path (Pref.own t) true empty_back s} = refine_ borrowed in
  check_traversal s empty empty_back borrowed);
  let h = ghost_ (Pref.own (borrow_ t)) in
  let refine_ a0 = ghost_ (present_def h s) in
  let refine_ a1 = ghost_ (present_def h a) in
  let refine_ a2 = ghost_ (present_def h s) in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let proof = ghost_ (
    let q = b.prev in
    let h : {h : node option Pref.heap | not (q === s.next)
      && not (q === a.prev)
      && not (q === a.next)
      && not (q === s.prev)} = refine_ before in
    let refine_ proof = inserted_link_frame s a s q h in
    let result : {u : unit |
      H.mem (inserted before s a s) b.prev = H.mem before b.prev
      && H.at (inserted before s a s) b.prev === H.at before b.prev} = refine_
          proof in result) in
  let refine_ proof = proof in
  let proof = ghost_ (
    let q = b.next in
    let h : {h : node option Pref.heap | not (q === s.next)
      && not (q === a.prev)
      && not (q === a.next)
      && not (q === s.prev)} = refine_ before in
    let refine_ proof = inserted_link_frame s a s q h in
    let result : {u : unit |
      H.mem (inserted before s a s) b.next = H.mem before b.next
      && H.at (inserted before s a s) b.next === H.at before b.next} = refine_
          proof in result) in
  let refine_ proof = proof in
  let proof = ghost_ (
    let h : {h : node option Pref.heap | not (s.next === a.prev)
      && not (s.next === a.next)
      && not (s.next === s.prev)
      && not (a.prev === a.next)
      && not (a.prev === s.prev)
      && not (a.next === s.prev)} = refine_ before in
    let refine_ proof = insert_contents s a s h in
    let result : {u : unit | H.mem (inserted before s a s) s.next
      && H.at (inserted before s a s) s.next === Some (Some a)
      && H.mem (inserted before s a s) a.prev
        && H.at (inserted before s a s) a.prev === Some (Some s)
      && H.mem (inserted before s a s) a.next
        && H.at (inserted before s a s) a.next === Some (Some s)
      && H.mem (inserted before s a s) s.prev
        && H.at (inserted before s a s) s.prev === Some (Some a)} = refine_
          proof in result) in
  let refine_ proof = proof in
  let t : {t : node option Pref.token | present (Pref.own t) s
    && present (Pref.own t) a && present (Pref.own t) s
    && H.at (Pref.own t) s.next === Some (Some s)
    && H.at (Pref.own t) s.prev === Some (Some s)
    && H.at (Pref.own t) a.next === Some (Some a)
    && H.at (Pref.own t) a.prev === Some (Some a)
    && not (a === s) && not (a === s)} = refine_ t in
  let refine_ t = insert_between s a s t in
  let singleton = [a] in
  let singleton_back = [a] in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let proof = ghost_ (
    let refined : {h : node option Pref.heap | s.sentinel
      && not a.sentinel
      && not (s.prev === s.next)
      && H.mem h s.prev
      && H.mem h s.next
      && not (a.prev === a.next)
      && H.mem h a.prev
      && H.mem h a.next
      && H.at h s.prev === Some (Some a)
      && H.at h s.next === Some (Some a)
      && H.at h a.prev === Some (Some s)
      && H.at h a.next === Some (Some s)} = refine_ h in
    let refine_ proof = certify1 s a refined in
    let result : {u : unit | ring h s [a] && path h false [a] s
      && path h true [a] s && present h s && owns h (s :: [a])
      && H.at h (field false s) === Some (Some (head [a] s))
      && H.at h (field true s) === Some (Some (head [a] s))} = refine_ proof in
          result) in
  let refine_ proof = proof in
  (let borrowed = borrow_ t in
  let borrowed : {t : node option Pref.token | s.sentinel && present (Pref.own t) s
    && H.at (Pref.own t) (field false s) === Some (Some (head singleton s))
    && H.at (Pref.own t) (field true s) === Some (Some (head singleton_back s))
    && path (Pref.own t) false singleton s
    && path (Pref.own t) true singleton_back s} = refine_ borrowed in
  check_traversal s singleton singleton_back borrowed);
  let h = ghost_ (Pref.own (borrow_ t)) in
  let refine_ a0 = ghost_ (present_def h a) in
  let refine_ a1 = ghost_ (present_def h b) in
  let refine_ a2 = ghost_ (present_def h s) in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let proof = ghost_ (
    let q = s.next in
    let h : {h : node option Pref.heap | not (q === a.next)
      && not (q === b.prev)
      && not (q === b.next)
      && not (q === s.prev)} = refine_ before in
    let refine_ proof = inserted_link_frame a b s q h in
    let result : {u : unit |
      H.mem (inserted before a b s) s.next = H.mem before s.next
      && H.at (inserted before a b s) s.next === H.at before s.next} = refine_
          proof in result) in
  let refine_ proof = proof in
  let proof = ghost_ (
    let q = a.prev in
    let h : {h : node option Pref.heap | not (q === a.next)
      && not (q === b.prev)
      && not (q === b.next)
      && not (q === s.prev)} = refine_ before in
    let refine_ proof = inserted_link_frame a b s q h in
    let result : {u : unit |
      H.mem (inserted before a b s) a.prev = H.mem before a.prev
      && H.at (inserted before a b s) a.prev === H.at before a.prev} = refine_
          proof in result) in
  let refine_ proof = proof in
  let proof = ghost_ (
    let h : {h : node option Pref.heap | not (a.next === b.prev)
      && not (a.next === b.next)
      && not (a.next === s.prev)
      && not (b.prev === b.next)
      && not (b.prev === s.prev)
      && not (b.next === s.prev)} = refine_ before in
    let refine_ proof = insert_contents a b s h in
    let result : {u : unit | H.mem (inserted before a b s) a.next
      && H.at (inserted before a b s) a.next === Some (Some b)
      && H.mem (inserted before a b s) b.prev
        && H.at (inserted before a b s) b.prev === Some (Some a)
      && H.mem (inserted before a b s) b.next
        && H.at (inserted before a b s) b.next === Some (Some s)
      && H.mem (inserted before a b s) s.prev
        && H.at (inserted before a b s) s.prev === Some (Some b)} = refine_
          proof in result) in
  let refine_ proof = proof in
  let t : {t : node option Pref.token | present (Pref.own t) a
    && present (Pref.own t) b && present (Pref.own t) s
    && H.at (Pref.own t) a.next === Some (Some s)
    && H.at (Pref.own t) s.prev === Some (Some a)
    && H.at (Pref.own t) b.next === Some (Some b)
    && H.at (Pref.own t) b.prev === Some (Some b)
    && not (b === a) && not (b === s)} = refine_ t in
  let refine_ t = insert_between a b s t in
  let pair = [a; b] in
  let pair_back = [b; a] in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let proof = ghost_ (
    let refined : {h : node option Pref.heap | s.sentinel
      && not a.sentinel
      && not b.sentinel
      && not (s.prev === s.next)
      && H.mem h s.prev
      && H.mem h s.next
      && not (a.prev === a.next)
      && H.mem h a.prev
      && H.mem h a.next
      && not (b.prev === b.next)
      && H.mem h b.prev
      && H.mem h b.next
      && H.at h s.prev === Some (Some b)
      && H.at h s.next === Some (Some a)
      && H.at h a.prev === Some (Some s)
      && H.at h a.next === Some (Some b)
      && H.at h b.prev === Some (Some a)
      && H.at h b.next === Some (Some s)} = refine_ h in
    let refine_ proof = certify2 s a b refined in
    let result : {u : unit | ring h s [a; b] && path h false [a; b] s
      && path h true [b; a] s && present h s && owns h (s :: [a; b])
      && H.at h (field false s) === Some (Some (head [a; b] s))
      && H.at h (field true s) === Some (Some (head [b; a] s))} = refine_ proof
          in result) in
  let refine_ proof = proof in
  (let borrowed = borrow_ t in
  let borrowed : {t : node option Pref.token | s.sentinel && present (Pref.own t) s
    && H.at (Pref.own t) (field false s) === Some (Some (head pair s))
    && H.at (Pref.own t) (field true s) === Some (Some (head pair_back s))
    && path (Pref.own t) false pair s
    && path (Pref.own t) true pair_back s} = refine_ borrowed in
  check_traversal s pair pair_back borrowed);
  let h = ghost_ (Pref.own (borrow_ t)) in
  let refine_ ps = ghost_ (present_def h s) in
  let refine_ pa = ghost_ (present_def h a) in
  let refine_ pb = ghost_ (present_def h b) in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let proof = ghost_ (
    let h : {h : node option Pref.heap | not (s.next === b.prev)
      && not (s.next === a.next)
      && not (s.next === a.prev)
      && not (b.prev === a.next)
      && not (b.prev === a.prev)
      && not (a.next === a.prev)} = refine_ before in
    let refine_ proof = remove_contents s a b h in
    let result : {u : unit | H.mem (removed before s a b) s.next
      && H.at (removed before s a b) s.next === Some (Some b)
      && H.mem (removed before s a b) b.prev
        && H.at (removed before s a b) b.prev === Some (Some s)
      && H.mem (removed before s a b) a.next
        && H.at (removed before s a b) a.next === Some (Some a)
      && H.mem (removed before s a b) a.prev
        && H.at (removed before s a b) a.prev === Some (Some a)} = refine_ proof
          in result) in
  let refine_ proof = proof in
  let proof = ghost_ (
    let q = s.prev in
    let h : {h : node option Pref.heap | not (q === s.next)
      && not (q === b.prev)
      && not (q === a.next)
      && not (q === a.prev)} = refine_ before in
    let refine_ proof = removed_link_frame s a b q h in
    let result : {u : unit |
      H.mem (removed before s a b) s.prev = H.mem before s.prev
      && H.at (removed before s a b) s.prev === H.at before s.prev} = refine_
          proof in result) in
  let refine_ proof = proof in
  let proof = ghost_ (
    let q = b.next in
    let h : {h : node option Pref.heap | not (q === s.next)
      && not (q === b.prev)
      && not (q === a.next)
      && not (q === a.prev)} = refine_ before in
    let refine_ proof = removed_link_frame s a b q h in
    let result : {u : unit |
      H.mem (removed before s a b) b.next = H.mem before b.next
      && H.at (removed before s a b) b.next === H.at before b.next} = refine_
          proof in result) in
  let refine_ proof = proof in
  let t : {t : node option Pref.token | present (Pref.own t) s
    && present (Pref.own t) a && present (Pref.own t) b
    && not (a === s)
    && H.at (Pref.own t) s.next === Some (Some a)
    && H.at (Pref.own t) a.prev === Some (Some s)
    && H.at (Pref.own t) a.next === Some (Some b)
    && H.at (Pref.own t) b.prev === Some (Some a)} = refine_ t in
  let refine_ t = remove s s a b t in
  let after_remove = [b] in
  let after_remove_back = [b] in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let proof = ghost_ (
    let refined : {h : node option Pref.heap | s.sentinel
      && not b.sentinel
      && not (s.prev === s.next)
      && H.mem h s.prev
      && H.mem h s.next
      && not (b.prev === b.next)
      && H.mem h b.prev
      && H.mem h b.next
      && H.at h s.prev === Some (Some b)
      && H.at h s.next === Some (Some b)
      && H.at h b.prev === Some (Some s)
      && H.at h b.next === Some (Some s)} = refine_ h in
    let refine_ proof = certify1 s b refined in
    let result : {u : unit | ring h s [b] && path h false [b] s
      && path h true [b] s && present h s && owns h (s :: [b])
      && H.at h (field false s) === Some (Some (head [b] s))
      && H.at h (field true s) === Some (Some (head [b] s))} = refine_ proof in
          result) in
  let refine_ proof = proof in
  (let borrowed = borrow_ t in
  let borrowed : {t : node option Pref.token | s.sentinel && present (Pref.own t) s
    && H.at (Pref.own t) (field false s) === Some (Some (head after_remove s))
    && H.at (Pref.own t) (field true s) === Some (Some (head after_remove_back
        s))
    && path (Pref.own t) false after_remove s
    && path (Pref.own t) true after_remove_back s} = refine_ borrowed in
  check_traversal s after_remove after_remove_back borrowed);
  let u = () in
  let _detached : {u : unit | H.at (Pref.own t) a.prev === Some (Some a)
    && H.at (Pref.own t) a.next === Some (Some a)} = refine_ u in
  let t : {t : node option Pref.token |
      H.mem (Pref.own t) a.prev && H.mem (Pref.own t) a.next
      && H.at (Pref.own t) a.prev === Some (Some a)
      && H.at (Pref.own t) a.next === Some (Some a)
      && H.at (Pref.own t) s.next === Some (Some b)
      && H.at (Pref.own t) b.prev === Some (Some s)
      && not (a.prev === a.next)
      && not (s.next === a.prev) && not (s.next === a.next)
      && not (b.prev === a.prev) && not (b.prev === a.next)} = refine_ t in
  let refine_ parts = detach a s b t in
  let detached = parts.#left in
  let rest = parts.#right in
  let u = () in
  let detached_contents : {u : unit |
    H.at (Pref.own detached) a.prev === Some (Some a)
    && H.at (Pref.own detached) a.next === Some (Some a)
    && not (H.mem (Pref.own rest) a.prev)
    && not (H.mem (Pref.own rest) a.next)
    && H.at (Pref.own rest) s.next === Some (Some b)
    && H.at (Pref.own rest) b.prev === Some (Some s)} = refine_ u in
  let refine_ detached_contents = detached_contents in
  let p = a.next in
  let actual : {v : node option | v === Some a} =
    let borrowed = borrow_ detached in
    let borrowed : {t : node option Pref.token | H.mem (Pref.own t) p} = refine_ borrowed in
    let refine_ actual = Pref.read p borrowed in refine_ actual in
  let refine_ actual = actual in
  assert (match actual with Some n -> n == a | None -> false);
  let refine_ t = Pref.join detached rest in
  let _t = t in
  ()
