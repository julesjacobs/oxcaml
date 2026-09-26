module H = Pref.Heap

type node : immutable_data = {
  value : int;
  sentinel : bool;
  prev : node option Pref.t;
  next : node option Pref.t;
}

let[@def] (present @ total) (h : node option Pref.heap @ immutable)
    (n : node @ immutable) =
  ghost_ (H.mem h n.prev && H.mem h n.next && not (n.prev === n.next))

let[@def] rec (owns @ total) (h : node option Pref.heap @ immutable)
    (ns : node list @ immutable) =
  ghost_ (match ns with [] -> true | n :: ns -> present h n && owns h ns)

let[@def] (head @ total) (ns : node list @ immutable)
    (stop : node @ immutable) = match ns with [] -> stop | n :: _ -> n

let[@def] (tail @ total) (ns : node list @ immutable) =
  match ns with [] -> [] | _ :: ns -> ns

let[@def] (field @ total) (backward : bool) (n : node @ immutable) =
  if backward then n.prev else n.next

let[@def] rec (path @ total) (h : node option Pref.heap @ immutable) (backward : bool)
    (ns : node list @ immutable) (stop : node @ immutable) =
  ghost_ (match ns with
  | [] -> true
  | n :: rest -> not n.sentinel && present h n
      && H.at h (field backward n) === Some (Some (head rest stop))
      && path h backward rest stop)

let[@def] rec (linked @ total) (h : node option Pref.heap @ immutable)
    (previous : node @ immutable) (ns : node list @ immutable)
    (stop : node @ immutable) =
  ghost_ (match ns with
  | [] -> H.at h stop.prev === Some (Some previous)
  | n :: rest -> not n.sentinel && present h n
      && H.at h n.prev === Some (Some previous)
      && H.at h n.next === Some (Some (head rest stop))
      && linked h n rest stop)

let[@def] (ring @ total) (h : node option Pref.heap @ immutable)
    (sentinel : node @ immutable) (ns : node list @ immutable) =
  ghost_ (sentinel.sentinel && present h sentinel
    && H.at h sentinel.next === Some (Some (head ns sentinel))
    && linked h sentinel ns sentinel)

let[@def] (apart @ total) (a : node @ immutable) (b : node @ immutable) =
  ghost_ (not (a === b) && not (a.prev === b.prev) && not (a.prev === b.next) &&
    not (a.next === b.prev) && not (a.next === b.next))

let[@def] rec (apart_all @ total) (n : node @ immutable) (ns : node list @ immutable) =
  ghost_ (match ns with [] -> true | next :: rest -> apart n next && apart_all n rest)

let[@def] rec (separated @ total) (ns : node list @ immutable) =
  ghost_ (match ns with [] -> true | n :: rest ->
    not (n.prev === n.next) && apart_all n rest && separated rest)

let[@def] rec (isolated @ total) (h : node option Pref.heap @ immutable) (ns : node list @ immutable) =
  ghost_ (match ns with [] -> true | n :: rest ->
    present h n && H.at h n.prev === Some (Some n) && H.at h n.next === Some (Some n) &&
    apart_all n rest && isolated h rest)

let[@def] (connected @ total) (h : node option Pref.heap @ immutable)
    (left : node @ immutable) (right : node @ immutable) =
  ghost_ (H.put (H.put h left.next (Some right)) right.prev (Some left))

let connect (left : node @ immutable) (right : node @ immutable)
    (t : {t : node option Pref.token | H.mem (Pref.own t) left.next
      && H.mem (Pref.own t) right.prev} @ unique)
    : {r : node option Pref.token | Pref.own r === connected (Pref.own
        t) left right}
      @ unique =
  let before = ghost_ (Pref.own (borrow_ t)) in
  let _ = ghost_ (connected_def before left right) in
  let p = left.next in
  let q = right.prev in
  let right_value = Some right in
  let left_value = Some left in
  let t : {t : node option Pref.token | H.mem (Pref.own t) p} = t in
  let t = Pref.write p right_value t in
  let t : {t : node option Pref.token | H.mem (Pref.own t) q} = t in
  let t = Pref.write q left_value t in
  t

let[@def] (inserted @ total) (h : node option Pref.heap @ immutable)
    (left : node @ immutable) (n : node @ immutable) (right : node @ immutable)
        =
  ghost_ (connected (connected h left n) n right)

let (insert_access @ total) (left : node @ immutable) (n : node @ immutable)
    (right : node @ immutable)
    (before : {h : node option Pref.heap | present h left && present h n && present h right} @ immutable) :
    {u : unit | H.mem before left.next && H.mem before n.prev
      && H.mem (connected before left n) n.next
      && H.mem (connected before left n) right.prev
      && inserted before left n right ===
        connected (connected before left n) n right} @ ghost = ghost_ (
    let _ = present_def before left in
    let _ = present_def before n in
    let _ = present_def before right in
    let _ = inserted_def before left n right in
    let _ = connected_def before left n in
    let u = () in
    let proof : {u : unit | H.mem before left.next && H.mem before n.prev
      && H.mem (connected before left n) n.next
      && H.mem (connected before left n) right.prev
      && inserted before left n right ===
        connected (connected before left n) n right} = u in proof)

let insert_between (left : node @ immutable) (n : node @ immutable)
    (right : node @ immutable)
    (t : {t : node option Pref.token | present (Pref.own t) left
      && present (Pref.own t) n && present (Pref.own t) right
      && H.at (Pref.own t) left.next === Some (Some right)
      && H.at (Pref.own t) right.prev === Some (Some left)
      && H.at (Pref.own t) n.next === Some (Some n)
      && H.at (Pref.own t) n.prev === Some (Some n)
      && not (n === left) && not (n === right)} @ unique)
    : {r : node option Pref.token | Pref.own r === inserted (Pref.own
        t) left n right}
      @ unique =
  let before = ghost_ (Pref.own (borrow_ t)) in
  ghost_ (insert_access left n right before);
  let t : {t : node option Pref.token | H.mem (Pref.own t) left.next
    && H.mem (Pref.own t) n.prev} = t in
  let t = connect left n t in
  let t : {t : node option Pref.token | H.mem (Pref.own t) n.next
    && H.mem (Pref.own t) right.prev} = t in
  let t = connect n right t in
  t

let[@def] (removed @ total) (h : node option Pref.heap @ immutable)
    (left : node @ immutable) (n : node @ immutable) (right : node @ immutable)
        =
  ghost_ (connected (connected h left right) n n)

let (remove_access @ total) (left : node @ immutable) (n : node @ immutable)
    (right : node @ immutable)
    (before : {h : node option Pref.heap | present h left && present h n && present h right} @ immutable) :
    {u : unit | H.mem before left.next && H.mem before right.prev
      && H.mem (connected before left right) n.next
      && H.mem (connected before left right) n.prev
      && removed before left n right ===
        connected (connected before left right) n n} @ ghost = ghost_ (
    let _ = present_def before left in
    let _ = present_def before n in
    let _ = present_def before right in
    let _ = removed_def before left n right in
    let _ = connected_def before left right in
    let u = () in
    let proof : {u : unit | H.mem before left.next && H.mem before right.prev
      && H.mem (connected before left right) n.next
      && H.mem (connected before left right) n.prev
      && removed before left n right ===
        connected (connected before left right) n n} = u in proof)

let remove (sentinel : node @ immutable) (left : node @ immutable)
    (n : node @ immutable) (right : node @ immutable)
    (t : {t : node option Pref.token | present (Pref.own t) left
      && present (Pref.own t) n && present (Pref.own t) right
      && not (n === sentinel)
      && H.at (Pref.own t) left.next === Some (Some n)
      && H.at (Pref.own t) n.prev === Some (Some left)
      && H.at (Pref.own t) n.next === Some (Some right)
      && H.at (Pref.own t) right.prev === Some (Some n)} @ unique)
    : {r : node option Pref.token | Pref.own r === removed (Pref.own t)
        left n right}
      @ unique =
  let before = ghost_ (Pref.own (borrow_ t)) in
  ghost_ (remove_access left n right before);
  let t : {t : node option Pref.token | H.mem (Pref.own t) left.next
    && H.mem (Pref.own t) right.prev} = t in
  let t = connect left right t in
  let t : {t : node option Pref.token | H.mem (Pref.own t) n.next
    && H.mem (Pref.own t) n.prev} = t in
  let t = connect n n t in
  t

let read_link : (p : node option Pref.t) @ immutable ->
    (expected : node) @ immutable ghost ->
    (t : {t : node option Pref.token | H.mem (Pref.own t) p
      && H.at (Pref.own t) p === Some (Some expected)}) @ local read ->
    {n : node | n === expected} @ immutable = fun p expected t ->
  let b : {t : node option Pref.token | H.mem (Pref.own t) p} = t in
  let v = Pref.read p b in
  match v with
  | None -> failwith "unlinked node"
  | Some n -> n

let (walk_view @ total) (backward : bool) (cursor : node @ immutable)
    (stop : node @ immutable) (model : node list @ immutable)
    (h : {h : node option Pref.heap | cursor === head model stop && path h backward model stop} @ immutable) :
    {u : unit | (match model with
      | [] -> cursor === stop
      | n :: rest -> cursor === n && not cursor.sentinel
        && rest === tail model && present h cursor
        && H.at h (field backward cursor) === Some (Some (head rest stop))
        && path h backward rest stop)} @ ghost = ghost_ (
    let _ = path_def h backward model stop in
    let _ = head_def model stop in
    let _ = tail_def model in
    let u = () in
    let proof : {u : unit | (match model with
      | [] -> cursor === stop
      | n :: rest -> cursor === n && not cursor.sentinel
        && rest === tail model && present h cursor
        && H.at h (field backward cursor) === Some (Some (head rest stop))
        && path h backward rest stop)} = u in proof)

let rec walk : (backward : bool) -> (cursor : node) @ immutable ->
    (stop : node) @ immutable -> (model : node list) @ immutable ghost ->
    (t : {t : node option Pref.token | stop.sentinel && cursor === head model stop
      && path (Pref.own t) backward model stop}) @ local read ->
    {ns : node list | ns === model} @ immutable =
  fun backward cursor stop model t ->
  let h = ghost_ (Pref.own t) in
  ghost_ (walk_view backward cursor stop model h);
  if cursor.sentinel then
    let ns = [] in ns
  else
    let p = field backward cursor in
    let rest = ghost_ (tail model) in
    let expected = ghost_ (head rest stop) in
    let _ = ghost_ (
      let _ = present_def h cursor in
      let _ = field_def backward cursor in
      let u = () in
      let proof : {u : unit | H.mem h p} = u in proof) in
    let b : {t : node option Pref.token | H.mem (Pref.own t) p
      && H.at (Pref.own t) p === Some (Some expected)} = t in
    let next = read_link p expected b in
    let b : {t : node option Pref.token | stop.sentinel && next === head rest stop
      && path (Pref.own t) backward rest stop} = t in
    let ns = walk backward next stop rest b in
    let ns = cursor :: ns in ns

let[@def] (value @ total) (h : node option Pref.heap @ immutable)
    (p : node option Pref.t @ immutable) =
  ghost_ (match H.at h p with None -> None | Some v -> v)

let[@def] (flipped @ total) (h : node option Pref.heap @ immutable) (n : node @ immutable) =
  ghost_ (H.put (H.put h n.prev (value h n.next)) n.next (value h n.prev))

let[@def] rec (flipped_all @ total) (h : node option Pref.heap @ immutable)
    (ns : node list @ immutable) =
  ghost_ (match ns with [] -> h | n :: rest -> flipped_all (flipped h n) rest)

let rec (owns_put @ total) :
    (h : node option Pref.heap) @ immutable -> (ns : node list) @ immutable ->
    (p : node option Pref.t) @ immutable -> (v : node option) @ immutable ->
    {u : unit | not (owns h ns) || owns (H.put h p v) ns} @ ghost =
  fun h ns p v -> ghost_ (
    let updated = H.put h p v in
    let _ = owns_def h ns in
    let _ = owns_def updated ns in
    match ns with
    | [] -> ()
    | n :: rest ->
      let _ = present_def h n in
      let _ = present_def updated n in
      let _ = owns_put h rest p v in
      ())

let rec reverse_nodes : (ns : node list) @ immutable ->
    (t : {t : node option Pref.token | owns (Pref.own t) ns}) @ unique ->
    {t' : node option Pref.token | Pref.own t' === flipped_all
        (Pref.own t) ns}
      @ unique = fun ns t ->
  let before = ghost_ (Pref.own (borrow_ t)) in
  let _ = ghost_ (owns_def before ns) in
  let _ = ghost_ (flipped_all_def before ns) in
  match ns with
  | [] -> t
  | n :: rest ->
    let p = n.prev in
    let q = n.next in
    let _ = ghost_ (present_def before n) in
    let prev : {v : node option | Some v === H.at before p} =
      let b = borrow_ t in
      let b : {t : node option Pref.token | H.mem (Pref.own t) p} = b in
      let prev = Pref.read p b in prev in
    let next : {v : node option | Some v === H.at before q} =
      let b = borrow_ t in
      let b : {t : node option Pref.token | H.mem (Pref.own t) q} = b in
      let next = Pref.read q b in next in
    let _ = ghost_ (
      let _ = value_def before p in
      let _ = value_def before q in
      let _ = flipped_def before n in
      let _ = owns_put before rest p next in
      let h = H.put before p next in
      let _ = owns_put h rest q prev in
      let u = () in
      let proof : {u : unit |
        H.put (H.put before p next) q prev === flipped before n
        && owns (H.put (H.put before p next) q prev) rest} = u in proof)
            in
    let t : {t : node option Pref.token | H.mem (Pref.own t) p} = t in
    let t = Pref.write p next t in
    let t : {t : node option Pref.token | H.mem (Pref.own t) q} = t in
    let t = Pref.write q prev t in
    let t : {t : node option Pref.token | owns (Pref.own t) rest} = t in
    let t = reverse_nodes rest t in
    t

type created = { node : node @@ aliased; state : node option Pref.token }

let make_node (sentinel : bool) (value : int) (t : node option Pref.token @ unique)
    : {r : created | r.node.value = value && r.node.sentinel = sentinel
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
        r.node.prev (Some r.node)) r.node.next (Some r.node)} @ unique =
  let initial : node option = None in
  let a = Pref.alloc initial t in
  let prev = a.value in
  let t = a.state in
  let b = Pref.alloc initial t in
  let next = b.value in
  let t = b.state in
  let node = {value; sentinel; prev; next} in
  let v = Some node in
  let t : {t : node option Pref.token | H.mem (Pref.own t) prev} = t in
  let t = Pref.write prev v t in
  let t : {t : node option Pref.token | H.mem (Pref.own t) next} = t in
  let t = Pref.write next v t in
  let after = ghost_ (Pref.own (borrow_ t)) in
  let _ = ghost_ (present_def after node) in
  let result = {node; state = t} in
  result

let (mem_put @ total) (h : node option Pref.heap @ immutable)
    (p : node option Pref.t @ immutable) (v : node option @ immutable)
    (q : node option Pref.t @ immutable) :
    {u : unit | not (H.mem h q) || H.mem (H.put h p v) q} @ ghost =
  ghost_ (())

let connected_mem (h : node option Pref.heap @ immutable) (left : node @ immutable)
    (right : node @ immutable) (p : node option Pref.t @ immutable) :
    {u : unit | not (H.mem h p) || H.mem (connected h left right) p}
      @ ghost =
  ghost_ (
    connected_def h left right;
    mem_put h left.next (Some right) p;
    mem_put (H.put h left.next (Some right)) right.prev (Some left) p;
    ())

let splice_range (left : node @ immutable) (first : node @ immutable)
    (last : node @ immutable) (right : node @ immutable)
    (destination_left : node @ immutable) (destination_right : node @ immutable)
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
          destination_left)}
      @ unique)
    : {r : node option Pref.token | Pref.own r === H.put (H.put
        (H.put (H.put
          (H.put (H.put (Pref.own t) left.next (Some right)) right.prev (Some
              left))
          destination_left.next (Some first)) first.prev (Some
              destination_left))
        last.next (Some destination_right)) destination_right.prev (Some last)}
      @ unique =
  let before = ghost_ (Pref.own (borrow_ t)) in
  ghost_ (
    connected_def before left right;
    connected_mem before left right destination_left.next;
    connected_mem before left right first.prev;
    connected_mem before left right last.next;
    connected_mem before left right destination_right.prev);
  let t = connect left right t in
  let middle = ghost_ (Pref.own (borrow_ t)) in
  ghost_ (
    connected_def middle destination_left first;
    connected_mem middle destination_left first last.next;
    connected_mem middle destination_left first destination_right.prev);
  let t = connect destination_left first t in
  ghost_ (connected_def (Pref.own (borrow_ t)) last destination_right);
  connect last destination_right t

let traverse : (backward : bool) -> (sentinel : node) @ immutable ->
    (expected : node list) @ immutable ghost ->
    (t : {t : node option Pref.token | sentinel.sentinel && present (Pref.own t) sentinel
      && H.at (Pref.own t) (field backward sentinel) ===
        Some (Some (head expected sentinel))
      && path (Pref.own t) backward expected sentinel}) @ local read ->
    {ns : node list | ns === expected} @ immutable =
  fun backward sentinel expected t ->
  let h = ghost_ (Pref.own t) in
  let p = field backward sentinel in
  let start = ghost_ (head expected sentinel) in
  let _ = ghost_ (
    let _ = present_def h sentinel in
    let _ = field_def backward sentinel in
    let u = () in
    let proof : {u : unit | H.mem h p} = u in proof) in
  let b : {t : node option Pref.token | H.mem (Pref.own t) p
    && H.at (Pref.own t) p === Some (Some start)} = t in
  let first = read_link p start b in
  let b : {t : node option Pref.token | sentinel.sentinel
    && first === head expected sentinel
    && path (Pref.own t) backward expected sentinel} = t in
  let ns = walk backward first sentinel expected b in
  ns


let detach (n : node @ immutable) (left : node @ immutable)
    (right : node @ immutable)
    (t : {t : node option Pref.token |
      H.mem (Pref.own t) n.prev && H.mem (Pref.own t) n.next
      && H.at (Pref.own t) n.prev === Some (Some n)
      && H.at (Pref.own t) n.next === Some (Some n)
      && H.at (Pref.own t) left.next === Some (Some right)
      && H.at (Pref.own t) right.prev === Some (Some left)
      && not (n.prev === n.next)
      && not (left.next === n.prev) && not (left.next === n.next)
      && not (right.prev === n.prev) && not (right.prev === n.next)} @ unique) :
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
      && H.at (Pref.own r.#right) right.prev === Some (Some left)} @ unique =
  let selection = ghost_ (H.put (H.put (H.empty ()) n.prev (Some n))
    n.next (Some n)) in
  let parts = Pref.split selection t in
  parts
