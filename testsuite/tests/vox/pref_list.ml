(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_list.mli pref_list.ml pref_list_client.ml";
 { bytecode; }
 { native; }
*)

module H = Pref.Heap

type node : immutable_data = {
  value : int;
  next : node option Pref.t;
}

type model = Nil | Cons of node * model [@@inductive]

let[@def] (root @ total) (xs : model @ immutable) =
  match xs with Nil -> None | Cons (n, _) -> Some n

let[@def] (tail @ total) (xs : model @ immutable) =
  match xs with Nil -> Nil | Cons (_, xs) -> xs

let[@def] (link @ total) (n : node @ immutable)
    (next : node option @ immutable) =
  ghost_ (H.put (H.empty ()) n.next next)

let[@def] rec (heap @ total) (xs : model @ immutable) =
  ghost_ (match xs with
  | Nil -> H.empty ()
  | Cons (n, xs) -> H.union (link n (root xs)) (heap xs))

let[@def] rec (valid @ total) (xs : model @ immutable) =
  ghost_ (match xs with
  | Nil -> true
  | Cons (n, xs) -> valid xs && H.disjoint (link n (root xs)) (heap xs))

let[@def] rec (rev_append @ total) (xs : model @ immutable)
    (ys : model @ immutable) =
  match xs with
  | Nil -> ys
  | Cons (n, xs) -> rev_append xs (Cons (n, ys))

type result = { pointer : node option @@ aliased; state : Pref.token }

type built = {
  pointer : node option @@ aliased;
  model : model @@ aliased ghost;
  state : Pref.token;
}

let (unfold @ total) (xs : model @ immutable)
    : {u : unit | match xs with
      | Nil -> root xs === None && heap xs === H.empty () && valid xs
      | Cons (n, rest) -> root xs === Some n && tail xs === rest
        && heap xs === H.union (link n (root rest)) (heap rest)
        && valid xs = (valid rest
          && H.disjoint (link n (root rest)) (heap rest))} @ ghost =
  ghost_ (
    let _ = root_def xs in
    let _ = tail_def xs in
    let _ = heap_def xs in
    let _ = valid_def xs in
    ())

let rec reverse_into :
    (pointer : node option) @ immutable ->
    (acc : node option) @ immutable ->
    (xs : model) @ immutable ghost -> (ys : model) @ immutable ghost ->
    (t : {t : Pref.token | valid xs && root xs === pointer
      && Pref.own t === heap xs}) @ unique ->
    (a : {a : Pref.token | valid ys && root ys === acc
      && Pref.own a === heap ys}) @ unique ->
    {r : result | r.pointer === root (rev_append xs ys)
      && Pref.own r.state === heap (rev_append xs ys)
      && valid (rev_append xs ys)} @ unique =
  fun pointer acc xs ys t a ->
  let _ = ghost_ (unfold xs) in
  let _ = ghost_ (rev_append_def xs ys) in
  match pointer with
  | None ->
    let r = {pointer = acc; state = a} in
    r
  | Some n ->
    let rest = ghost_ (tail xs) in
    let next_model = ghost_ (root rest) in
    let selection = ghost_ (link n next_model) in
    let rest_heap = ghost_ (heap rest) in
    let _ = ghost_ (
      let _ = H.partition_law selection rest_heap in
      let _ = link_def n next_model in
      let u = () in
      let proof : {u : unit |
        H.restrict (heap xs) selection === selection
        && H.exclude (heap xs) selection === rest_heap
        && H.mem selection n.next
        && H.at selection n.next === Some next_model} = u in proof) in
    let parts = Pref.split selection t in
    let cell = parts.#left in
    let rest_token = parts.#right in
    let p = n.next in
    let next : {v : node option | v === root rest} =
      let b = borrow_ cell in
      let b : {b : Pref.token | H.mem (Pref.own b) p} = b in
      let next = Pref.read p b in
      next in
    let cell : {t : Pref.token | H.mem (Pref.own t) p} = cell in
    let cell = Pref.write p acc cell in
    let new_link = ghost_ (Pref.own (borrow_ cell)) in
    let a = Pref.join cell a in
    let extended = ghost_ (Cons (n, ys)) in
    let _ = ghost_ (
      let _ = unfold extended in
      let _ = link_def n next_model in
      let _ = link_def n acc in
      let e = H.empty () in
      let _ = H.put_law e p next_model acc in
      let u = () in
      let proof : {u : unit | new_link === link n acc
        && heap extended === H.union new_link (heap ys)
        && valid extended && root extended === Some n
        && rev_append xs ys === rev_append rest extended} = u in proof) in
    let rest_token : {t : Pref.token | valid rest && root rest === next
      && Pref.own t === heap rest} = rest_token in
    let pointer = Some n in
    let a : {a : Pref.token | valid extended && root extended === pointer
      && Pref.own a === heap extended} = a in
    let r = reverse_into next pointer rest extended rest_token a in
    r

let reverse : (pointer : node option) @ immutable ->
    (xs : model) @ immutable ghost ->
    (frame : Pref.heap) @ immutable ghost ->
    (t : {t : Pref.token | valid xs && root xs === pointer
      && H.disjoint (heap xs) frame
      && Pref.own t === H.union (heap xs) frame}) @ unique ->
    {r : result | r.pointer === root (rev_append xs Nil)
      && Pref.own r.state === H.union (heap (rev_append xs Nil)) frame
      && H.disjoint (heap (rev_append xs Nil)) frame
      && valid (rev_append xs Nil)} @ unique =
  fun pointer xs frame t ->
  let selection = ghost_ (heap xs) in
  let _ = ghost_ (H.partition_law selection frame) in
  let parts = Pref.split selection t in
  let list = parts.#left in
  let frame_token = parts.#right in
  let nil = ghost_ Nil in
  let _ = ghost_ (unfold nil) in
  let a = Pref.empty () in
  let acc : node option = None in
  let a : {a : Pref.token | valid nil && root nil === acc
    && Pref.own a === heap nil} = a in
  let list : {t : Pref.token | valid xs && root xs === pointer
    && Pref.own t === heap xs} = list in
  let r = reverse_into pointer acc xs nil list a in
  let pointer = r.pointer in
  let state = r.state in
  let state = Pref.join state frame_token in
  let result = {pointer; state} in
  result

let empty () : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model && b.model === Nil} @ unique =
  let model = ghost_ Nil in
  let _ = ghost_ (unfold model) in
  let state = Pref.empty () in
  let b = {pointer = None; model; state} in
  b

let cons (value : int)
    (b : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model} @ unique)
    : {r : built | r.pointer === root r.model && valid r.model
      && Pref.own r.state === heap r.model
      && (match r.model with Nil -> false | Cons (n, xs) ->
        n.value = value && xs === b.model)} @ unique =
  let pointer = b.pointer in
  let xs = b.model in
  let t = b.state in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let cell = Pref.alloc pointer t in
  let next = cell.value in
  let state = cell.state in
  let n = {value; next} in
  let model = ghost_ (Cons (n, xs)) in
  let _ = ghost_ (
    let _ = unfold model in
    let _ = link_def n pointer in
    let e = H.empty () in
    let _ = H.union_law before e e in
    let _ = H.put_union_law e before next pointer in
    let u = () in
    let _contents : {u : unit |
      H.put before next pointer === heap model} = u in
    let _separate : {u : unit | H.disjoint (link n pointer) before} =
      u in
    let proof : {u : unit | root model === Some n && valid model
      && H.put before next pointer === heap model} = u in proof) in
  let result = {pointer = Some n; model; state} in
  result

let[@def] rec (nodes @ total) (xs : model @ immutable) =
  match xs with Nil -> [] | Cons (n, xs) -> n :: nodes xs

type observation = { nodes : node list @@ aliased; state : Pref.token }

let rec observe : (pointer : node option) @ immutable ->
    (xs : model) @ immutable ghost ->
    (t : {t : Pref.token | valid xs && root xs === pointer
      && Pref.own t === heap xs}) @ unique ->
    {r : observation | r.nodes === nodes xs
      && Pref.own r.state === heap xs} @ unique = fun pointer xs t ->
  let _ = ghost_ (unfold xs) in
  let _ = ghost_ (nodes_def xs) in
  match pointer with
  | None -> let r = {nodes = []; state = t} in r
  | Some n ->
    let rest = ghost_ (tail xs) in
    let next_model = ghost_ (root rest) in
    let selection = ghost_ (link n next_model) in
    let rest_heap = ghost_ (heap rest) in
    let _ = ghost_ (
      let _ = H.partition_law selection rest_heap in
      let _ = link_def n next_model in
      let u = () in
      let proof : {u : unit |
        H.restrict (heap xs) selection === selection
        && H.exclude (heap xs) selection === rest_heap
        && H.mem selection n.next
        && H.at selection n.next === Some next_model} = u in proof) in
    let parts = Pref.split selection t in
    let cell = parts.#left in
    let rest_token = parts.#right in
    let p = n.next in
    let next : {v : node option | v === root rest} =
      let b = borrow_ cell in
      let b : {b : Pref.token | H.mem (Pref.own b) p} = b in
      let next = Pref.read p b in
      next in
    let rest_token : {t : Pref.token | valid rest && root rest === next
      && Pref.own t === heap rest} = rest_token in
    let r = observe next rest rest_token in
    let ns = r.nodes in
    let state = r.state in
    let state = Pref.join cell state in
    let r = {nodes = n :: ns; state} in
    r

let rec of_list : int list ->
    {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model} @ unique = fun values ->
  match values with
  | [] -> let b = empty () in b
  | value :: rest ->
    let b = of_list rest in
    let b : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model} = b in
    let b = cons value b in
    b
