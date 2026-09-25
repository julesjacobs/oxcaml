module H = Pref.Heap
type node = { value : int; next : node option Pref.t; }
type model = Nil | Cons of node * model
[@@inductive]
val root : model @ immutable -> node option @@ total
val root_def :
  (xs : model) @ immutable ->
  {u : unit
    | (root xs) === (match xs with | Nil -> None | Cons (n, _) -> Some n)} @@ total
val link : node @ immutable -> node option @ immutable -> Pref.heap @ ghost @@ total
val link_def :
  (n : node) @ immutable ->
  (next : node option) @ immutable ->
  {u : unit | (link n next) === (ghost_ (H.put (H.empty ()) n.next next))} @@ total
val heap : model @ immutable -> Pref.heap @ ghost @@ total
val heap_def :
  (xs' : model) @ immutable ->
  {u : unit
    | (heap xs') ===
        (ghost_
           (match xs' with
            | Nil -> H.empty ()
            | Cons (n, xs) -> H.union (link n (root xs)) (heap xs)))} @@ total
val valid : model @ immutable -> bool @ ghost @@ total
val valid_def :
  (xs' : model) @ immutable ->
  {u : unit
    | (valid xs') ===
        (ghost_
           (match xs' with
            | Nil -> true
            | Cons (n, xs) ->
                (valid xs) && (H.disjoint (link n (root xs)) (heap xs))))} @@ total
val rev_append : model @ immutable -> model @ immutable -> model @@ total
val rev_append_def :
  (xs' : model) @ immutable ->
  (ys : model) @ immutable ->
  {u : unit
    | (rev_append xs' ys) ===
        (match xs' with
         | Nil -> ys
         | Cons (n, xs) -> rev_append xs (Cons (n, ys)))} @@ total
type result = { pointer : node option @@ aliased; state : Pref.token; }
type built = {
  pointer : node option @@ aliased;
  model : model @@ ghost aliased;
  state : Pref.token;
}
val reverse : (pointer : node option) @ immutable ->
    (xs : model) @ immutable ghost ->
    (frame : Pref.heap) @ immutable ghost ->
    (t : {t : Pref.token | valid xs && root xs === pointer
      && H.disjoint (heap xs) frame
      && Pref.own t === H.union (heap xs) frame}) @ unique ->
    {r : result | r.pointer === root (rev_append xs Nil)
      && Pref.own r.state === H.union (heap (rev_append xs Nil)) frame
      && H.disjoint (heap (rev_append xs Nil)) frame
      && valid (rev_append xs Nil)} @ unique
val empty : unit ->
  {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model && b.model === Nil} @ unique
val cons : (value : int) ->
  (b : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model}) @ unique ->
  {r : built | let refine_ b = b in
      r.pointer === root r.model && valid r.model
      && Pref.own r.state === heap r.model
      && (match r.model with Nil -> false | Cons (n, xs) ->
        n.value = value && xs === b.model)} @ unique
val nodes : model @ immutable -> node list @@ total
val nodes_def :
  (xs' : model) @ immutable ->
  {u : unit
    | (nodes xs') ===
        (match xs' with | Nil -> [] | Cons (n, xs) -> n :: (nodes xs))} @@ total
type observation = { nodes : node list @@ aliased; state : Pref.token; }
val observe : (pointer : node option) @ immutable ->
    (xs : model) @ immutable ghost ->
    (t : {t : Pref.token | valid xs && root xs === pointer
      && Pref.own t === heap xs}) @ unique ->
    {r : observation | r.nodes === nodes xs
      && Pref.own r.state === heap xs} @ unique
val of_list : int list ->
    {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model} @ unique
