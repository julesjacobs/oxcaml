open Pref_ring

let (put_observations @ total) (h : node option Pref.heap @ immutable)
    (p : node option Pref.t @ immutable) (v : node option @ immutable)
    (n : node @ immutable) :
    {u : unit |
      H.mem (H.put h p v) n.prev = (p === n.prev || H.mem h n.prev)
      && H.mem (H.put h p v) n.next = (p === n.next || H.mem h n.next)
      && H.at (H.put h p v) n.prev ===
        (if p === n.prev then Some v else H.at h n.prev)
      && H.at (H.put h p v) n.next ===
        (if p === n.next then Some v else H.at h n.next)} @ ghost =
  ghost_ (())

let (flip_observations @ total) (h : node option Pref.heap @ immutable)
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
    let _definition = flipped_def h n in
    let u = () in
    let _expanded : {u : unit | flipped h n ===
      H.put (H.put h n.prev (value h n.next)) n.next (value h n.prev)} = u in
    u)

let (allocation_frame @ total) (n : node @ immutable)
    (other : node @ immutable)
    (h : {h : node option Pref.heap | H.mem h other.prev && H.mem h other.next
      && not (H.mem h n.prev) && not (H.mem h n.next)} @ immutable) :
    {u : unit | let h = h in
      let after = H.put (H.put (H.put (H.put h n.prev None) n.next None)
        n.prev (Some n)) n.next (Some n) in
      H.mem after other.prev && H.mem after other.next
      && H.at after other.prev === H.at h other.prev
      && H.at after other.next === H.at h other.next
      && not (n.prev === other.prev) && not (n.prev === other.next)
      && not (n.next === other.prev) && not (n.next === other.next)} @ ghost =
  let _h = h in
  ghost_ (())

let (isolated_four @ total) (h : node option Pref.heap @ immutable)
    (n0 : node @ immutable) (n1 : node @ immutable)
    (n2 : node @ immutable) (n3 : node @ immutable) :
    {u : unit | isolated h [n0; n1; n2; n3] === (true
      && not (n0.prev === n0.next)
      && H.mem h n0.prev
      && H.mem h n0.next
      && H.at h n0.prev === Some (Some n0)
      && H.at h n0.next === Some (Some n0)
      && not (n1.prev === n1.next)
      && H.mem h n1.prev
      && H.mem h n1.next
      && H.at h n1.prev === Some (Some n1)
      && H.at h n1.next === Some (Some n1)
      && not (n2.prev === n2.next)
      && H.mem h n2.prev
      && H.mem h n2.next
      && H.at h n2.prev === Some (Some n2)
      && H.at h n2.next === Some (Some n2)
      && not (n3.prev === n3.next)
      && H.mem h n3.prev
      && H.mem h n3.next
      && H.at h n3.prev === Some (Some n3)
      && H.at h n3.next === Some (Some n3)
      && not (n0 === n1)
      && not (n0.prev === n1.prev)
      && not (n0.prev === n1.next)
      && not (n0.next === n1.prev)
      && not (n0.next === n1.next)
      && not (n0 === n2)
      && not (n0.prev === n2.prev)
      && not (n0.prev === n2.next)
      && not (n0.next === n2.prev)
      && not (n0.next === n2.next)
      && not (n0 === n3)
      && not (n0.prev === n3.prev)
      && not (n0.prev === n3.next)
      && not (n0.next === n3.prev)
      && not (n0.next === n3.next)
      && not (n1 === n2)
      && not (n1.prev === n2.prev)
      && not (n1.prev === n2.next)
      && not (n1.next === n2.prev)
      && not (n1.next === n2.next)
      && not (n1 === n3)
      && not (n1.prev === n3.prev)
      && not (n1.prev === n3.next)
      && not (n1.next === n3.prev)
      && not (n1.next === n3.next)
      && not (n2 === n3)
      && not (n2.prev === n3.prev)
      && not (n2.prev === n3.next)
      && not (n2.next === n3.prev)
      && not (n2.next === n3.next))} @ ghost =
  ghost_ (isolated_def h [n0; n1; n2; n3];
    isolated_def h [n1; n2; n3];
    isolated_def h [n2; n3];
    isolated_def h [n3];
    isolated_def h [];
    present_def h n0;
    apart_all_def n0 [n1; n2; n3];
    apart_all_def n0 [n2; n3];
    apart_all_def n0 [n3];
    apart_all_def n0 [];
    present_def h n1;
    apart_all_def n1 [n2; n3];
    apart_all_def n1 [n3];
    apart_all_def n1 [];
    present_def h n2;
    apart_all_def n2 [n3];
    apart_all_def n2 [];
    present_def h n3;
    apart_all_def n3 [];
    apart_def n0 n1;
    apart_def n0 n2;
    apart_def n0 n3;
    apart_def n1 n2;
    apart_def n1 n3;
    apart_def n2 n3; ())
