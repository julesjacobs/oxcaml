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

let[@def] (connected @ total) (h : node option Pref.heap @ immutable)
    (left : node @ immutable) (right : node @ immutable) =
  ghost_ (H.put (H.put h left.next (Some right)) right.prev (Some left))

let connect (left : node @ immutable) (right : node @ immutable)
    (t : {t : node option Pref.token | H.mem (Pref.own t) left.next
      && H.mem (Pref.own t) right.prev} @ unique)
    : {r : node option Pref.token | let refine_ t = t in Pref.own r === connected (Pref.own
        t) left right
      && Pref.own r === H.put (H.put (Pref.own t) left.next (Some right))
        right.prev (Some left)}
      @ unique =
  let refine_ t = t in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let proof = ghost_ (connected_def before left right) in
  let refine_ proof = proof in
  let p = left.next in
  let q = right.prev in
  let right_value = Some right in
  let left_value = Some left in
  let t : {t : node option Pref.token | H.mem (Pref.own t) p} = refine_ t in
  let refine_ t = Pref.write p right_value t in
  let t : {t : node option Pref.token | H.mem (Pref.own t) q} = refine_ t in
  let refine_ t = Pref.write q left_value t in
  refine_ t

let[@def] (inserted @ total) (h : node option Pref.heap @ immutable)
    (left : node @ immutable) (n : node @ immutable) (right : node @ immutable)
        =
  ghost_ (connected (connected h left n) n right)

let insert_between (left : node @ immutable) (n : node @ immutable)
    (right : node @ immutable)
    (t : {t : node option Pref.token | present (Pref.own t) left
      && present (Pref.own t) n && present (Pref.own t) right
      && H.at (Pref.own t) left.next === Some (Some right)
      && H.at (Pref.own t) right.prev === Some (Some left)
      && H.at (Pref.own t) n.next === Some (Some n)
      && H.at (Pref.own t) n.prev === Some (Some n)
      && not (n === left) && not (n === right)} @ unique)
    : {r : node option Pref.token | let refine_ t = t in Pref.own r === inserted (Pref.own
        t) left n right
      && Pref.own r === H.put (H.put
        (H.put (H.put (Pref.own t) left.next (Some n)) n.prev (Some left))
        n.next (Some right)) right.prev (Some n)}
      @ unique =
  let refine_ t = t in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let proof = ghost_ (
    let refine_ unfolded = present_def before left in
    let refine_ b = present_def before n in
    let refine_ c = present_def before right in
    let refine_ d = inserted_def before left n right in
    let refine_ e = connected_def before left n in
    let u = () in
    let proof : {u : unit | H.mem before left.next && H.mem before n.prev
      && H.mem (connected before left n) n.next
      && H.mem (connected before left n) right.prev
      && inserted before left n right ===
        connected (connected before left n) n right} = refine_ u in proof) in
  let refine_ proof = proof in
  let t : {t : node option Pref.token | H.mem (Pref.own t) left.next
    && H.mem (Pref.own t) n.prev} = refine_ t in
  let refine_ t = connect left n t in
  let t : {t : node option Pref.token | H.mem (Pref.own t) n.next
    && H.mem (Pref.own t) right.prev} = refine_ t in
  let refine_ t = connect n right t in
  refine_ t

let[@def] (removed @ total) (h : node option Pref.heap @ immutable)
    (left : node @ immutable) (n : node @ immutable) (right : node @ immutable)
        =
  ghost_ (connected (connected h left right) n n)

let remove (sentinel : node @ immutable) (left : node @ immutable)
    (n : node @ immutable) (right : node @ immutable)
    (t : {t : node option Pref.token | present (Pref.own t) left
      && present (Pref.own t) n && present (Pref.own t) right
      && not (n === sentinel)
      && H.at (Pref.own t) left.next === Some (Some n)
      && H.at (Pref.own t) n.prev === Some (Some left)
      && H.at (Pref.own t) n.next === Some (Some right)
      && H.at (Pref.own t) right.prev === Some (Some n)} @ unique)
    : {r : node option Pref.token | let refine_ t = t in Pref.own r === removed (Pref.own t)
        left n right
      && Pref.own r === H.put (H.put
        (H.put (H.put (Pref.own t) left.next (Some right)) right.prev (Some
            left))
        n.next (Some n)) n.prev (Some n)}
      @ unique =
  let refine_ t = t in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let proof = ghost_ (
    let refine_ unfolded = present_def before left in
    let refine_ b = present_def before n in
    let refine_ c = present_def before right in
    let refine_ d = removed_def before left n right in
    let refine_ e = connected_def before left right in
    let u = () in
    let proof : {u : unit | H.mem before left.next && H.mem before right.prev
      && H.mem (connected before left right) n.next
      && H.mem (connected before left right) n.prev
      && removed before left n right ===
        connected (connected before left right) n n} = refine_ u in proof) in
  let refine_ proof = proof in
  let t : {t : node option Pref.token | H.mem (Pref.own t) left.next
    && H.mem (Pref.own t) right.prev} = refine_ t in
  let refine_ t = connect left right t in
  let t : {t : node option Pref.token | H.mem (Pref.own t) n.next
    && H.mem (Pref.own t) n.prev} = refine_ t in
  let refine_ t = connect n n t in
  refine_ t

let read_link : (p : node option Pref.t) @ immutable ->
    (expected : node) @ immutable ghost ->
    (t : {t : node option Pref.token | H.mem (Pref.own t) p
      && H.at (Pref.own t) p === Some (Some expected)}) @ local read ->
    {n : node | n === expected} @ immutable = fun p expected t ->
  let refine_ t = t in
  let b : {t : node option Pref.token | H.mem (Pref.own t) p} = refine_ t in
  let refine_ v = Pref.read p b in
  match v with
  | None -> failwith "unlinked node"
  | Some n -> refine_ n

let rec walk : (backward : bool) -> (cursor : node) @ immutable ->
    (stop : node) @ immutable -> (model : node list) @ immutable ghost ->
    (t : {t : node option Pref.token | stop.sentinel && cursor === head model stop
      && path (Pref.own t) backward model stop}) @ local read ->
    {ns : node list | ns === model} @ immutable =
  fun backward cursor stop model t ->
  let refine_ t = t in
  let h = ghost_ (Pref.own t) in
  let proof = ghost_ (
    let refine_ unfolded = path_def h backward model stop in
    let refine_ b = head_def model stop in
    let refine_ c = tail_def model in
    let u = () in
    let proof : {u : unit | (match model with
      | [] -> cursor === stop
      | n :: rest -> cursor === n && not cursor.sentinel
        && rest === tail model && present h cursor
        && H.at h (field backward cursor) === Some (Some (head rest stop))
        && path h backward rest stop)} = refine_ u in proof) in
  let refine_ proof = proof in
  if cursor.sentinel then
    let ns = [] in refine_ ns
  else
    let p = field backward cursor in
    let rest = ghost_ (tail model) in
    let expected = ghost_ (head rest stop) in
    let proof = ghost_ (
      let refine_ unfolded = present_def h cursor in
      let refine_ b = field_def backward cursor in
      let u = () in
      let proof : {u : unit | H.mem h p} = refine_ u in proof) in
    let refine_ proof = proof in
    let b : {t : node option Pref.token | H.mem (Pref.own t) p
      && H.at (Pref.own t) p === Some (Some expected)} = refine_ t in
    let refine_ next = read_link p expected b in
    let b : {t : node option Pref.token | stop.sentinel && next === head rest stop
      && path (Pref.own t) backward rest stop} = refine_ t in
    let refine_ ns = walk backward next stop rest b in
    let ns = cursor :: ns in refine_ ns

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
    let refine_ unfolded = owns_def h ns in
    let refine_ b = owns_def updated ns in
    match ns with
    | [] -> let u = () in refine_ u
    | n :: rest ->
      let refine_ c = present_def h n in
      let refine_ d = present_def updated n in
      let refine_ e = owns_put h rest p v in
      let u = () in refine_ u)

let rec reverse_nodes : (ns : node list) @ immutable ->
    (t : {t : node option Pref.token | owns (Pref.own t) ns}) @ unique ->
    {t' : node option Pref.token | let refine_ t = t in Pref.own t' === flipped_all
        (Pref.own t) ns}
      @ unique = fun ns t ->
  let refine_ t = t in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let refine_ unfolded = ghost_ (owns_def before ns) in
  let refine_ b = ghost_ (flipped_all_def before ns) in
  match ns with
  | [] -> refine_ t
  | n :: rest ->
    let p = n.prev in
    let q = n.next in
    let refine_ unfolded = ghost_ (present_def before n) in
    let prev : {v : node option | Some v === H.at before p} =
      let b = borrow_ t in
      let b : {t : node option Pref.token | H.mem (Pref.own t) p} = refine_ b in
      let refine_ prev = Pref.read p b in refine_ prev in
    let refine_ prev = prev in
    let next : {v : node option | Some v === H.at before q} =
      let b = borrow_ t in
      let b : {t : node option Pref.token | H.mem (Pref.own t) q} = refine_ b in
      let refine_ next = Pref.read q b in refine_ next in
    let refine_ next = next in
    let proof = ghost_ (
      let refine_ unfolded = value_def before p in
      let refine_ b = value_def before q in
      let refine_ c = flipped_def before n in
      let refine_ d = owns_put before rest p next in
      let h = H.put before p next in
      let refine_ e = owns_put h rest q prev in
      let u = () in
      let proof : {u : unit |
        H.put (H.put before p next) q prev === flipped before n
        && owns (H.put (H.put before p next) q prev) rest} = refine_ u in proof)
            in
    let refine_ proof = proof in
    let t : {t : node option Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let refine_ t = Pref.write p next t in
    let t : {t : node option Pref.token | H.mem (Pref.own t) q} = refine_ t in
    let refine_ t = Pref.write q prev t in
    let t : {t : node option Pref.token | owns (Pref.own t) rest} = refine_ t in
    let refine_ t = reverse_nodes rest t in
    refine_ t

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
  let refine_ a = Pref.alloc initial t in
  let prev = a.value in
  let t = a.state in
  let refine_ b = Pref.alloc initial t in
  let next = b.value in
  let t = b.state in
  let node = {value; sentinel; prev; next} in
  let v = Some node in
  let t : {t : node option Pref.token | H.mem (Pref.own t) prev} = refine_ t in
  let refine_ t = Pref.write prev v t in
  let t : {t : node option Pref.token | H.mem (Pref.own t) next} = refine_ t in
  let refine_ t = Pref.write next v t in
  let after = ghost_ (Pref.own (borrow_ t)) in
  let refine_ definition = ghost_ (present_def after node) in
  let result = {node; state = t} in
  refine_ result

let (mem_put @ total) (h : node option Pref.heap @ immutable)
    (p : node option Pref.t @ immutable) (v : node option @ immutable)
    (q : node option Pref.t @ immutable) :
    {u : unit | not (H.mem h q) || H.mem (H.put h p v) q} @ ghost =
  ghost_ (let u = () in refine_ u)

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
    : {r : node option Pref.token | let refine_ t = t in
      Pref.own r === H.put (H.put
        (H.put (H.put
          (H.put (H.put (Pref.own t) left.next (Some right)) right.prev (Some
              left))
          destination_left.next (Some first)) first.prev (Some
              destination_left))
        last.next (Some destination_right)) destination_right.prev (Some last)}
      @ unique =
  let refine_ t = t in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let u = () in
  let contents : {u : unit |
    H.mem before left.next && H.mem before right.prev
    && H.mem before destination_left.next && H.mem before first.prev
    && H.mem before last.next
      && H.mem before destination_right.prev} = refine_ u in
  let refine_ contents = contents in
  let p0 = left.next in
  let v0 = Some right in
  let p1 = right.prev in
  let v1 = Some left in
  let p2 = destination_left.next in
  let v2 = Some first in
  let p3 = first.prev in
  let v3 = Some destination_left in
  let p4 = last.next in
  let v4 = Some destination_right in
  let p5 = destination_right.prev in
  let v5 = Some last in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let refine_ preserved = ghost_ (mem_put h p0 v0 p1) in
  let refine_ preserved = ghost_ (mem_put h p0 v0 p2) in
  let refine_ preserved = ghost_ (mem_put h p0 v0 p3) in
  let refine_ preserved = ghost_ (mem_put h p0 v0 p4) in
  let refine_ preserved = ghost_ (mem_put h p0 v0 p5) in
  let t : {t : node option Pref.token | H.mem (Pref.own t) p0} = refine_ t in
  let refine_ t = Pref.write p0 v0 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let refine_ preserved = ghost_ (mem_put h p1 v1 p2) in
  let refine_ preserved = ghost_ (mem_put h p1 v1 p3) in
  let refine_ preserved = ghost_ (mem_put h p1 v1 p4) in
  let refine_ preserved = ghost_ (mem_put h p1 v1 p5) in
  let t : {t : node option Pref.token | H.mem (Pref.own t) p1} = refine_ t in
  let refine_ t = Pref.write p1 v1 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let refine_ preserved = ghost_ (mem_put h p2 v2 p3) in
  let refine_ preserved = ghost_ (mem_put h p2 v2 p4) in
  let refine_ preserved = ghost_ (mem_put h p2 v2 p5) in
  let t : {t : node option Pref.token | H.mem (Pref.own t) p2} = refine_ t in
  let refine_ t = Pref.write p2 v2 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let refine_ preserved = ghost_ (mem_put h p3 v3 p4) in
  let refine_ preserved = ghost_ (mem_put h p3 v3 p5) in
  let t : {t : node option Pref.token | H.mem (Pref.own t) p3} = refine_ t in
  let refine_ t = Pref.write p3 v3 t in
  let h = ghost_ (Pref.own (borrow_ t)) in
  let refine_ preserved = ghost_ (mem_put h p4 v4 p5) in
  let t : {t : node option Pref.token | H.mem (Pref.own t) p4} = refine_ t in
  let refine_ t = Pref.write p4 v4 t in
  let t : {t : node option Pref.token | H.mem (Pref.own t) p5} = refine_ t in
  let refine_ t = Pref.write p5 v5 t in

  refine_ t

let traverse : (backward : bool) -> (sentinel : node) @ immutable ->
    (expected : node list) @ immutable ghost ->
    (t : {t : node option Pref.token | sentinel.sentinel && present (Pref.own t) sentinel
      && H.at (Pref.own t) (field backward sentinel) ===
        Some (Some (head expected sentinel))
      && path (Pref.own t) backward expected sentinel}) @ local read ->
    {ns : node list | ns === expected} @ immutable =
  fun backward sentinel expected t ->
  let refine_ t = t in
  let h = ghost_ (Pref.own t) in
  let p = field backward sentinel in
  let start = ghost_ (head expected sentinel) in
  let proof = ghost_ (
    let refine_ unfolded = present_def h sentinel in
    let refine_ b = field_def backward sentinel in
    let u = () in
    let proof : {u : unit | H.mem h p} = refine_ u in proof) in
  let refine_ proof = proof in
  let b : {t : node option Pref.token | H.mem (Pref.own t) p
    && H.at (Pref.own t) p === Some (Some start)} = refine_ t in
  let refine_ first = read_link p start b in
  let b : {t : node option Pref.token | sentinel.sentinel
    && first === head expected sentinel
    && path (Pref.own t) backward expected sentinel} = refine_ t in
  let refine_ ns = walk backward first sentinel expected b in
  refine_ ns


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
    {r : node option Pref.partition | let refine_ t = t in
      Pref.own r.#left === H.restrict (Pref.own t)
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
  let refine_ t = t in
  let selection = ghost_ (H.put (H.put (H.empty ()) n.prev (Some n))
    n.next (Some n)) in
  let refine_ parts = Pref.split selection t in
  refine_ parts

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
  ghost_ (let u = () in refine_ u)

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
    let refine_ definition = flipped_def h n in
    let u = () in
    let _expanded : {u : unit | flipped h n ===
      H.put (H.put h n.prev (value h n.next)) n.next (value h n.prev)} = refine_
          u in
    refine_ u)

let (allocation_frame @ total) (n : node @ immutable)
    (other : node @ immutable)
    (h : {h : node option Pref.heap | H.mem h other.prev && H.mem h other.next
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
