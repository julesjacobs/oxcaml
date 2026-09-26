module H = Pref.Heap
type node = {
  value : int;
  sentinel : bool;
  prev : node option Pref.t;
  next : node option Pref.t;
}
val present : node option Pref.heap @ immutable -> node @ immutable -> bool @ ghost @@ total
val present_def :
  (h : node option Pref.heap) @ immutable ->
  (n : node) @ immutable ->
  {u : unit
    | (present h n) ===
        (ghost_
           ((H.mem h n.prev) &&
              ((H.mem h n.next) && (not (n.prev === n.next)))))} @@ total
val owns : node option Pref.heap @ immutable -> node list @ immutable -> bool @ ghost @@ total
val owns_def :
  (h : node option Pref.heap) @ immutable ->
  (ns' : node list) @ immutable ->
  {u : unit
    | (owns h ns') ===
        (ghost_
           (match ns' with
            | [] -> true
            | n::ns -> (present h n) && (owns h ns)))} @@ total
val head : node list @ immutable -> node @ immutable -> node @@ total
val head_def :
  (ns : node list) @ immutable ->
  (stop : node) @ immutable ->
  {u : unit | (head ns stop) === (match ns with | [] -> stop | n::_ -> n)} @@ total
val field : bool -> node @ immutable -> node option Pref.t @@ total
val field_def :
  (backward : bool) ->
  (n : node) @ immutable ->
  {u : unit | (field backward n) === (if backward then n.prev else n.next)} @@ total
val path :
  node option Pref.heap @ immutable ->
  bool -> node list @ immutable -> node @ immutable -> bool @ ghost @@ total
val path_def :
  (h : node option Pref.heap) @ immutable ->
  (backward : bool) ->
  (ns : node list) @ immutable ->
  (stop : node) @ immutable ->
  {u : unit
    | (path h backward ns stop) ===
        (ghost_
           (match ns with
            | [] -> true
            | n::rest ->
                (not n.sentinel) &&
                  ((present h n) &&
                     (((H.at h (field backward n)) ===
                         (Some (Some (head rest stop))))
                        && (path h backward rest stop)))))} @@ total
val linked :
  node option Pref.heap @ immutable ->
  node @ immutable ->
  node list @ immutable -> node @ immutable -> bool @ ghost @@ total
val linked_def :
  (h : node option Pref.heap) @ immutable ->
  (previous : node) @ immutable ->
  (ns : node list) @ immutable ->
  (stop : node) @ immutable ->
  {u : unit
    | (linked h previous ns stop) ===
        (ghost_
           (match ns with
            | [] -> (H.at h stop.prev) === (Some (Some previous))
            | n::rest ->
                (not n.sentinel) &&
                  ((present h n) &&
                     (((H.at h n.prev) === (Some (Some previous))) &&
                        (((H.at h n.next) === (Some (Some (head rest stop))))
                           && (linked h n rest stop))))))} @@ total
val ring :
  node option Pref.heap @ immutable ->
  node @ immutable -> node list @ immutable -> bool @ ghost @@ total
val ring_def :
  (h : node option Pref.heap) @ immutable ->
  (sentinel : node) @ immutable ->
  (ns : node list) @ immutable ->
  {u : unit
    | (ring h sentinel ns) ===
        (ghost_
           (sentinel.sentinel &&
              ((present h sentinel) &&
                 (((H.at h sentinel.next) ===
                     (Some (Some (head ns sentinel))))
                    && (linked h sentinel ns sentinel)))))} @@ total
val apart : node @ immutable -> node @ immutable -> bool @ ghost @@ total
val apart_def : (a : node) @ immutable -> (b : node) @ immutable ->
  {u : unit | apart a b === (ghost_ (not (a === b) && not (a.prev === b.prev) &&
    not (a.prev === b.next) && not (a.next === b.prev) && not (a.next === b.next)))} @@ total
val apart_all : node @ immutable -> node list @ immutable -> bool @ ghost @@ total
val apart_all_def : (n : node) @ immutable -> (ns : node list) @ immutable ->
  {u : unit | apart_all n ns ===
    (ghost_ (match ns with [] -> true | next :: rest -> apart n next && apart_all n rest))} @@ total
val separated : node list @ immutable -> bool @ ghost @@ total
val separated_def : (ns : node list) @ immutable ->
  {u : unit | separated ns === (ghost_ (match ns with [] -> true | n :: rest ->
    not (n.prev === n.next) && apart_all n rest && separated rest))} @@ total
val isolated : node option Pref.heap @ immutable -> node list @ immutable -> bool @ ghost @@ total
val isolated_def : (h : node option Pref.heap) @ immutable -> (ns : node list) @ immutable ->
  {u : unit | isolated h ns === (ghost_ (match ns with [] -> true | n :: rest ->
    present h n && H.at h n.prev === Some (Some n) && H.at h n.next === Some (Some n) &&
    apart_all n rest && isolated h rest))} @@ total
val connected :
  node option Pref.heap @ immutable ->
  node @ immutable -> node @ immutable -> node option Pref.heap @ ghost @@ total
val connected_def :
  (h : node option Pref.heap) @ immutable ->
  (left : node) @ immutable ->
  (right : node) @ immutable ->
  {u : unit
    | (connected h left right) ===
        (ghost_
           (H.put (H.put h left.next (Some right)) right.prev (Some left)))} @@ total
val connect : (left : node) @ immutable ->
  (right : node) @ immutable ->
  (t : {t : node option Pref.token | H.mem (Pref.own t) left.next
      && H.mem (Pref.own t) right.prev}) @ unique ->
  {r : node option Pref.token | Pref.own r === connected (Pref.own
        t) left right}
      @ unique
val inserted :
  node option Pref.heap @ immutable ->
  node @ immutable ->
  node @ immutable -> node @ immutable -> node option Pref.heap @ ghost @@ total
val inserted_def :
  (h : node option Pref.heap) @ immutable ->
  (left : node) @ immutable ->
  (n : node) @ immutable ->
  (right : node) @ immutable ->
  {u : unit
    | (inserted h left n right) ===
        (ghost_ (connected (connected h left n) n right))} @@ total
val insert_between : (left : node) @ immutable ->
  (n : node) @ immutable ->
  (right : node) @ immutable ->
  (t : {t : node option Pref.token | present (Pref.own t) left
      && present (Pref.own t) n && present (Pref.own t) right
      && H.at (Pref.own t) left.next === Some (Some right)
      && H.at (Pref.own t) right.prev === Some (Some left)
      && H.at (Pref.own t) n.next === Some (Some n)
      && H.at (Pref.own t) n.prev === Some (Some n)
      && not (n === left) && not (n === right)}) @ unique ->
  {r : node option Pref.token | Pref.own r === inserted (Pref.own
        t) left n right}
      @ unique
val removed :
  node option Pref.heap @ immutable ->
  node @ immutable ->
  node @ immutable -> node @ immutable -> node option Pref.heap @ ghost @@ total
val removed_def :
  (h : node option Pref.heap) @ immutable ->
  (left : node) @ immutable ->
  (n : node) @ immutable ->
  (right : node) @ immutable ->
  {u : unit
    | (removed h left n right) ===
        (ghost_ (connected (connected h left right) n n))} @@ total
val remove : (sentinel : node) @ immutable ->
  (left : node) @ immutable ->
  (n : node) @ immutable ->
  (right : node) @ immutable ->
  (t : {t : node option Pref.token | present (Pref.own t) left
      && present (Pref.own t) n && present (Pref.own t) right
      && not (n === sentinel)
      && H.at (Pref.own t) left.next === Some (Some n)
      && H.at (Pref.own t) n.prev === Some (Some left)
      && H.at (Pref.own t) n.next === Some (Some right)
      && H.at (Pref.own t) right.prev === Some (Some n)}) @ unique ->
  {r : node option Pref.token | Pref.own r === removed (Pref.own t)
        left n right}
      @ unique
val value :
  node option Pref.heap @ immutable ->
  node option Pref.t @ immutable -> node option @ ghost @@ total
val value_def :
  (h : node option Pref.heap) @ immutable ->
  (p : node option Pref.t) @ immutable ->
  {u : unit
    | (value h p) ===
        (ghost_ (match H.at h p with | None -> None | Some v -> v))} @@ total
val flipped : node option Pref.heap @ immutable -> node @ immutable -> node option Pref.heap @ ghost @@ total
val flipped_def :
  (h : node option Pref.heap) @ immutable ->
  (n : node) @ immutable ->
  {u : unit
    | (flipped h n) ===
        (ghost_
           (H.put (H.put h n.prev (value h n.next)) n.next (value h n.prev)))} @@ total
val flipped_all :
  node option Pref.heap @ immutable -> node list @ immutable -> node option Pref.heap @ ghost @@ total
val flipped_all_def :
  (h : node option Pref.heap) @ immutable ->
  (ns : node list) @ immutable ->
  {u : unit
    | (flipped_all h ns) ===
        (ghost_
           (match ns with
            | [] -> h
            | n::rest -> flipped_all (flipped h n) rest))} @@ total
val reverse_nodes : (ns : node list) @ immutable ->
    (t : {t : node option Pref.token | owns (Pref.own t) ns}) @ unique ->
    {t' : node option Pref.token | Pref.own t' === flipped_all
        (Pref.own t) ns}
      @ unique
type created = { node : node @@ aliased; state : node option Pref.token; }
val make_node : (sentinel : bool) ->
  (value : int) ->
  (t : node option Pref.token) @ unique ->
  {r : created | r.node.value = value && r.node.sentinel = sentinel
      && not (H.mem (Pref.own t) r.node.prev)
      && not (H.mem (Pref.own t) r.node.next)
      && not (r.node.prev === r.node.next)
      && H.mem (Pref.own r.state) r.node.prev
      && H.mem (Pref.own r.state) r.node.next
      && present (Pref.own r.state) r.node
      && H.at (Pref.own r.state) r.node.prev === Some (Some r.node)
      && H.at (Pref.own r.state) r.node.next === Some (Some r.node)
      && Pref.own r.state === H.put (H.put
        (H.put (H.put (Pref.own t) r.node.prev None) r.node.next None)
        r.node.prev (Some r.node)) r.node.next (Some r.node)} @ unique
val splice_range : (left : node) @ immutable ->
  (first : node) @ immutable ->
  (last : node) @ immutable ->
  (right : node) @ immutable ->
  (destination_left : node) @ immutable ->
  (destination_right : node) @ immutable ->
  (t : {t : node option Pref.token |
      H.mem (Pref.own t) left.next && H.mem (Pref.own t) right.prev
      && H.mem (Pref.own t) destination_left.next
      && H.mem (Pref.own t) first.prev && H.mem (Pref.own t) last.next
      && H.mem (Pref.own t) destination_right.prev
      && H.at (Pref.own t) left.next === Some (Some first)
      && H.at (Pref.own t) first.prev === Some (Some left)
      && H.at (Pref.own t) last.next === Some (Some right)
      && H.at (Pref.own t) right.prev === Some (Some last)
      && H.at (Pref.own t) destination_left.next === Some (Some
          destination_right)
      && H.at (Pref.own t) destination_right.prev === Some (Some
          destination_left)}) @ unique ->
  {r : node option Pref.token | Pref.own r === H.put (H.put
        (H.put (H.put
          (H.put (H.put (Pref.own t) left.next (Some right)) right.prev (Some
              left))
          destination_left.next (Some first)) first.prev (Some
              destination_left))
        last.next (Some destination_right)) destination_right.prev (Some last)}
      @ unique
val traverse : (backward : bool) -> (sentinel : node) @ immutable ->
    (expected : node list) @ immutable ghost ->
    (t : {t : node option Pref.token | sentinel.sentinel && present (Pref.own t) sentinel
      && H.at (Pref.own t) (field backward sentinel) ===
        Some (Some (head expected sentinel))
      && path (Pref.own t) backward expected sentinel}) @ local read ->
    {ns : node list | ns === expected} @ immutable
val detach : (n : node) @ immutable ->
  (left : node) @ immutable ->
  (right : node) @ immutable ->
  (t : {t : node option Pref.token |
      H.mem (Pref.own t) n.prev && H.mem (Pref.own t) n.next
      && H.at (Pref.own t) n.prev === Some (Some n)
      && H.at (Pref.own t) n.next === Some (Some n)
      && H.at (Pref.own t) left.next === Some (Some right)
      && H.at (Pref.own t) right.prev === Some (Some left)
      && not (n.prev === n.next)
      && not (left.next === n.prev) && not (left.next === n.next)
      && not (right.prev === n.prev) && not (right.prev === n.next)}) @ unique ->
  {r : node option Pref.partition | let t = t in Pref.own r.#left === H.restrict (Pref.own t)
        (H.put (H.put (H.empty ()) n.prev (Some n)) n.next (Some n))
      && Pref.own r.#right === H.exclude (Pref.own t)
        (H.put (H.put (H.empty ()) n.prev (Some n)) n.next (Some n))
      && H.mem (Pref.own r.#left) n.prev && H.mem (Pref.own r.#left) n.next
      && H.at (Pref.own r.#left) n.prev === Some (Some n)
      && H.at (Pref.own r.#left) n.next === Some (Some n)
      && not (H.mem (Pref.own r.#right) n.prev)
      && not (H.mem (Pref.own r.#right) n.next)
      && H.at (Pref.own r.#right) left.next === Some (Some right)
      && H.at (Pref.own r.#right) right.prev === Some (Some left)} @ unique
