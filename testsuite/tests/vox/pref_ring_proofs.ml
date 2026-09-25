open Pref_ring

let (put_observations @ total) (h : Pref.heap @ immutable)
    (p : node option Pref.t @ immutable) (v : node option @ immutable)
    (n : node @ immutable) :
    {u : unit |
      H.mem (H.put h p v) n.prev = (p === n.prev || H.mem h n.prev)
      && H.mem (H.put h p v) n.next = (p === n.next || H.mem h n.next)
      && H.at (H.put h p v) n.prev ===
        (if p === n.prev then Some v else H.at h n.prev)
      && H.at (H.put h p v) n.next ===
        (if p === n.next then Some v else H.at h n.next)} @ ghost =
  ghost_ (let u = () in refine_ u)

let (flip_observations @ total) (h : Pref.heap @ immutable)
    (n : node @ immutable) (other : node @ immutable) :
    {u : unit |
      H.mem (flipped h n) other.prev =
        (n.next === other.prev || n.prev === other.prev || H.mem h other.prev)
      && H.mem (flipped h n) other.next =
        (n.next === other.next || n.prev === other.next || H.mem h other.next)
      && H.at (flipped h n) other.prev ===
        (if n.next === other.prev then Some (value h n.prev)
         else if n.prev === other.prev then Some (value h n.next)
         else H.at h other.prev)
      && H.at (flipped h n) other.next ===
        (if n.next === other.next then Some (value h n.prev)
         else if n.prev === other.next then Some (value h n.next)
         else H.at h other.next)} @ ghost =
  ghost_ (
    let refine_ definition = flipped_def h n in
    let u = () in
    let _expanded : {u : unit | flipped h n ===
      H.put (H.put h n.prev (value h n.next)) n.next (value h n.prev)} = refine_
          u in
    refine_ u)

let (allocation_frame @ total) (n : node @ immutable)
    (other : node @ immutable)
    (h : {h : Pref.heap | H.mem h other.prev && H.mem h other.next
      && not (H.mem h n.prev) && not (H.mem h n.next)} @ immutable) :
    {u : unit | let refine_ h = h in
      let after = H.put (H.put (H.put (H.put h n.prev None) n.next None)
        n.prev (Some n)) n.next (Some n) in
      H.mem after other.prev && H.mem after other.next
      && H.at after other.prev === H.at h other.prev
      && H.at after other.next === H.at h other.next
      && not (n.prev === other.prev) && not (n.prev === other.next)
      && not (n.next === other.prev) && not (n.next === other.next)} @ ghost =
  let refine_ h = h in
  ghost_ (let u = () in refine_ u)
