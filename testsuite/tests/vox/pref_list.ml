(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_pref_semantics.mli vox_pref_semantics.ml pref_list.mli pref_list.ml pref_list_client.ml";
 { bytecode; }
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

type result = { pointer : node option @@ aliased; state : node option Pref.token }

type built = {
  pointer : node option @@ aliased;
  model : model @@ aliased ghost;
  state : node option Pref.token;
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

let (reverse_partition @ total) (xs : model @ immutable) (n : node @ immutable) :
    {u : unit | let rest = tail xs in let selection = link n (root rest) in
      if valid xs && root xs === Some n then
        H.restrict (heap xs) selection === selection &&
        H.exclude (heap xs) selection === heap rest &&
        H.mem selection n.next && H.at selection n.next === Some (root rest)
      else true} @ ghost = ghost_ (
  unfold xs;
  let rest = tail xs in
  let selection = link n (root rest) in
  H.partition_law selection (heap rest);
  link_def n (root rest);
  Vox_pref_semantics.put (H.empty ()) n.next (root rest) n.next; ())

let (reverse_rebuild @ total) (xs : model @ immutable) (ys : model @ immutable)
    (n : node @ immutable) (old_next : node option @ immutable)
    (acc : node option @ immutable) (new_link : node option Pref.heap @ immutable) :
    {u : unit | if valid xs && root xs === Some n && root (tail xs) === old_next &&
      valid ys && root ys === acc &&
      new_link === H.put (link n old_next) n.next acc &&
      H.disjoint new_link (heap ys) then
        new_link === link n acc &&
        heap (Cons (n, ys)) === H.union new_link (heap ys) &&
        valid (Cons (n, ys)) && root (Cons (n, ys)) === Some n &&
        rev_append xs ys === rev_append (tail xs) (Cons (n, ys))
      else true} @ ghost = ghost_ (
  unfold xs; rev_append_def xs ys; unfold (Cons (n, ys));
  link_def n old_next; link_def n acc;
  H.put_law (H.empty ()) n.next old_next acc; ())

let rec reverse_into :
    (pointer : node option) @ immutable ->
    (acc : node option) @ immutable ->
    (xs : model) @ immutable ghost -> (ys : model) @ immutable ghost ->
    (t : {t : node option Pref.token | valid xs && root xs === pointer
      && Pref.own t === heap xs}) @ unique ->
    (a : {a : node option Pref.token | valid ys && root ys === acc
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
    ghost_ (reverse_partition xs n);
    let parts = Pref.split selection t in
    let cell = parts.#left in
    let rest_token = parts.#right in
    let p = n.next in
    let next : {v : node option | v === root rest} =
      let b = borrow_ cell in
      let b : {b : node option Pref.token | H.mem (Pref.own b) p} = b in
      let next = Pref.read p b in
      next in
    let cell : {t : node option Pref.token | H.mem (Pref.own t) p} = cell in
    let cell = Pref.write p acc cell in
    let new_link = ghost_ (Pref.own (borrow_ cell)) in
    let a = Pref.join cell a in
    let extended = ghost_ (Cons (n, ys)) in
    ghost_ (reverse_rebuild xs ys n next_model acc new_link);
    let rest_token : {t : node option Pref.token | valid rest && root rest === next
      && Pref.own t === heap rest} = rest_token in
    let pointer = Some n in
    let a : {a : node option Pref.token | valid extended && root extended === pointer
      && Pref.own a === heap extended} = a in
    let r = reverse_into next pointer rest extended rest_token a in
    r

let reverse : (pointer : node option) @ immutable ->
    (xs : model) @ immutable ghost ->
    (frame : node option Pref.heap) @ immutable ghost ->
    (t : {t : node option Pref.token | valid xs && root xs === pointer
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
  let a : {a : node option Pref.token | valid nil && root nil === acc
    && Pref.own a === heap nil} = a in
  let list : {t : node option Pref.token | valid xs && root xs === pointer
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

type observation = { nodes : node list @@ aliased; state : node option Pref.token }

let rec observe_framed : (pointer : node option) @ immutable ->
    (xs : model) @ immutable ghost -> (frame : node option Pref.heap) @ immutable ghost ->
    (t : {t : node option Pref.token | valid xs && root xs === pointer
      && Pref.own t === H.union (heap xs) frame}) @ local read ->
    {result : node list | result === nodes xs} = fun pointer xs frame t ->
  ghost_ (unfold xs; nodes_def xs);
  match pointer with
  | None -> []
  | Some n ->
    let rest = ghost_ (tail xs) in
    let cell = ghost_ (link n (root rest)) in
    let rest_heap = ghost_ (heap rest) in
    let next_frame = ghost_ (H.union cell frame) in
    ghost_ (link_def n (root rest);
      H.union_law cell rest_heap frame;
      H.union_law rest_heap cell frame;
      Vox_pref_semantics.put (H.empty ()) n.next (root rest) n.next;
      Vox_pref_semantics.union cell rest_heap n.next;
      Vox_pref_semantics.union (heap xs) frame n.next);
    let p = n.next in
    let readable : {b : node option Pref.token | H.mem (Pref.own b) p} = t in
    let next : {v : node option | v === root rest} = Pref.read p readable in
    n :: observe_framed next rest next_frame t

let observe_read : (pointer : node option) @ immutable ->
    (xs : model) @ immutable ghost ->
    (t : {t : node option Pref.token | valid xs && root xs === pointer
      && Pref.own t === heap xs}) @ local read ->
    {result : node list | result === nodes xs} = fun pointer xs t ->
  let frame = ghost_ (H.empty ()) in
  ghost_ (H.union_law (heap xs) frame frame);
  observe_framed pointer xs frame t

let observe : (pointer : node option) @ immutable ->
    (xs : model) @ immutable ghost ->
    (t : {t : node option Pref.token | valid xs && root xs === pointer
      && Pref.own t === heap xs}) @ unique ->
    {r : observation | r.nodes === nodes xs
      && Pref.own r.state === heap xs} @ unique = fun pointer xs t ->
  let nodes = observe_read pointer xs (borrow_ t) in
  {nodes; state = t}

let[@def] rec (contents @ total) (model : model @ immutable) =
  match model with
  | Nil -> []
  | Cons (n, rest) -> n.value :: contents rest

let rec of_list : (values : int list) ->
    {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model
      && contents b.model === values} @ unique = fun values ->
  match values with
  | [] ->
    let b = empty () in
    ghost_ (let view = borrow_ b in contents_def view.model);
    b
  | value :: rest ->
    let b = of_list rest in
    let b : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model} = b in
    let b = cons value b in
    ghost_ (let view = borrow_ b in contents_def view.model);
    b

module Owned = struct
  type payload = #{pointer : node option @@ global;
    model : model @@ global ghost; state : node option Pref.token}
  type t = #{owned : {b : payload | b.#pointer === root b.#model &&
    valid b.#model && Pref.own b.#state === heap b.#model}}

  let[@def] model (state : t @ local immutable total ghost) = ghost_ state.#owned.#model

  let adopt : (b : {b : built | b.pointer === root b.model && valid b.model &&
      Pref.own b.state === heap b.model}) @ unique ->
      {state : t | model state === b.model} @ unique = fun b ->
    let owned = #{pointer = b.pointer; model = b.model; state = b.state} in
    let state : t = #{owned} in
    ghost_ (model_def (borrow_ state));
    state

  let release : (state : t) @ unique ->
      {b : built | b.pointer === root b.model && valid b.model &&
        Pref.own b.state === heap b.model && b.model === model state} @ unique =
    fun state ->
    ghost_ (model_def (borrow_ state));
    let b = state.#owned in
    {pointer = b.#pointer; model = b.#model; state = b.#state}

  let empty () : {state : t | model state === Nil} @ unique =
    adopt (empty ())

  let of_list (values : int list) :
      {state : t | contents (model state) === values} @ unique =
    adopt (of_list values)

  let reverse : (state : t) @ unique ->
      {next : t | model next === rev_append (model state) Nil} @ unique =
    fun state ->
    ghost_ (model_def (borrow_ state));
    let b = state.#owned in
    let pointer = b.#pointer in
    let before = ghost_ b.#model in
    let frame = ghost_ (H.empty ()) in
    ghost_ (H.union_law (heap before) frame frame);
    let reversed = reverse pointer before frame b.#state in
    let after = ghost_ (rev_append before Nil) in
    ghost_ (H.union_law (heap after) frame frame);
    let owned = #{pointer = reversed.pointer; model = after; state = reversed.state} in
    let next : t = #{owned} in
    ghost_ (model_def (borrow_ next));
    next

  let observe : (state : t) @ local read total forkable unyielding ->
      {result : node list | result === nodes (model state)} = fun state ->
    ghost_ (model_def (borrow_ state));
    let view = state.#owned in
    observe_read view.#pointer view.#model (borrow_ view.#state)
end
