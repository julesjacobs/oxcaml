open Pref_ring

let (insert_contents @ total) (left : node @ immutable) (n : node @ immutable)
    (right : node @ immutable)
    (h : {h : Pref.heap | not (left.next === n.prev)
      && not (left.next === n.next)
      && not (left.next === right.prev)
      && not (n.prev === n.next)
      && not (n.prev === right.prev)
      && not (n.next === right.prev)} @ immutable) :
    {u : unit | H.mem (inserted h left n right) left.next
      && H.at (inserted h left n right) left.next === Some (Some n)
      && H.mem (inserted h left n right) n.prev
        && H.at (inserted h left n right) n.prev === Some (Some left)
      && H.mem (inserted h left n right) n.next
        && H.at (inserted h left n right) n.next === Some (Some right)
      && H.mem (inserted h left n right) right.prev
        && H.at (inserted h left n right) right.prev === Some (Some n)} @ ghost
          =
  ghost_ (
    let _a = inserted_def h left n right in
    let _b = connected_def h left n in
    let h1 = connected h left n in
    let _c = connected_def h1 n right in
    let u = () in
    let _expanded : {u : unit | inserted h left n right ===
      H.put (H.put (H.put (H.put h left.next (Some n)) n.prev (Some left))
        n.next (Some right)) right.prev (Some n)} = u in
    u)

let (inserted_link_frame @ total) (left : node @ immutable) (n : node @
    immutable)
    (right : node @ immutable) (q : node option Pref.t @ immutable)
    (h : {h : Pref.heap | not (q === left.next)
      && not (q === n.prev)
      && not (q === n.next)
      && not (q === right.prev)} @ immutable) :
    {u : unit | H.mem (inserted h left n right) q = H.mem h q
      && H.at (inserted h left n right) q === H.at h q} @ ghost =
  ghost_ (
    let _a = inserted_def h left n right in
    let _b = connected_def h left n in
    let h1 = connected h left n in
    let _c = connected_def h1 n right in
    let u = () in
    let _expanded : {u : unit | inserted h left n right === H.put (H.put (H.put
        (H.put (h) left.next (Some n)) n.prev (Some left)) n.next (Some right))
        right.prev (Some n)} = u in
    u)


let (removed_link_frame @ total) (left : node @ immutable) (n : node @
    immutable)
    (right : node @ immutable) (q : node option Pref.t @ immutable)
    (h : {h : Pref.heap | not (q === left.next)
      && not (q === right.prev)
      && not (q === n.next)
      && not (q === n.prev)} @ immutable) :
    {u : unit | H.mem (removed h left n right) q = H.mem h q
      && H.at (removed h left n right) q === H.at h q} @ ghost =
  ghost_ (
    let _a = removed_def h left n right in
    let _b = connected_def h left right in
    let h1 = connected h left right in
    let _c = connected_def h1 n n in
    let u = () in
    let _expanded : {u : unit | removed h left n right === H.put (H.put (H.put
        (H.put (h) left.next (Some right)) right.prev (Some left)) n.next (Some
        n)) n.prev (Some n)} = u in
    u)


let (remove_contents @ total) (left : node @ immutable) (n : node @ immutable)
    (right : node @ immutable)
    (h : {h : Pref.heap | not (left.next === right.prev)
      && not (left.next === n.next)
      && not (left.next === n.prev)
      && not (right.prev === n.next)
      && not (right.prev === n.prev)
      && not (n.next === n.prev)} @ immutable) :
    {u : unit | H.mem (removed h left n right) left.next
      && H.at (removed h left n right) left.next === Some (Some right)
      && H.mem (removed h left n right) right.prev
        && H.at (removed h left n right) right.prev === Some (Some left)
      && H.mem (removed h left n right) n.next
        && H.at (removed h left n right) n.next === Some (Some n)
      && H.mem (removed h left n right) n.prev
        && H.at (removed h left n right) n.prev === Some (Some n)} @ ghost =
  ghost_ (
    let _a = removed_def h left n right in
    let _b = connected_def h left right in
    let h1 = connected h left right in
    let _c = connected_def h1 n n in
    let u = () in
    let _expanded : {u : unit | removed h left n right === H.put (H.put (H.put
        (H.put (h) left.next (Some right)) right.prev (Some left)) n.next (Some
        n)) n.prev (Some n)} = u in
    u)
