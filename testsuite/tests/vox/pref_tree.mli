module H = Pref.Heap
type node = {
  value : int;
  left : node option Pref.t;
  right : node option Pref.t;
}
type tree = Empty | Branch of node * tree * tree
[@@inductive]
val root : tree @ immutable -> node option @@ total
val root_def :
  (tree : tree) @ immutable ->
  {u : unit
    | (root tree) ===
        (match tree with | Empty -> None | Branch (n, _, _) -> Some n)} @@ total
val links :
  node @ immutable ->
  node option @ immutable -> node option @ immutable -> Pref.heap @ ghost @@ total
val links_def :
  (n : node) @ immutable ->
  (l : node option) @ immutable ->
  (r : node option) @ immutable ->
  {u : unit
    | (links n l r) ===
        (ghost_ (H.put (H.put (H.empty ()) n.left l) n.right r))} @@ total
val heap : tree @ immutable -> Pref.heap @ ghost @@ total
val heap_def :
  (tree : tree) @ immutable ->
  {u : unit
    | (heap tree) ===
        (ghost_
           (match tree with
            | Empty -> H.empty ()
            | Branch (n, l, r) ->
                H.union (links n (root l) (root r))
                  (H.union (heap l) (heap r))))} @@ total
val valid : tree @ immutable -> bool @ ghost @@ total
val valid_def :
  (tree : tree) @ immutable ->
  {u : unit
    | (valid tree) ===
        (ghost_
           (match tree with
            | Empty -> true
            | Branch (n, l, r) ->
                (valid l) &&
                  ((valid r) &&
                     ((not (n.left === n.right)) &&
                        ((H.disjoint (links n (root l) (root r))
                            (H.union (heap l) (heap r)))
                           && (H.disjoint (heap l) (heap r)))))))} @@ total
val flipped : tree @ immutable -> tree @@ total
val flipped_def :
  (tree : tree) @ immutable ->
  {u : unit
    | (flipped tree) ===
        (match tree with
         | Empty -> Empty
         | Branch (n, l, r) -> Branch (n, (flipped r), (flipped l)))} @@ total
val root_flipped :
  (tree : tree) @ immutable ->
  {u : unit | (root (flipped tree)) === (root tree)} @@ total
val mirror_with_frame :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (frame : Pref.heap) @ immutable ghost ->
    (t : {t : Pref.token | valid model && root model === pointer
      && H.disjoint (heap model) frame
      && Pref.own t === H.union (heap model) frame}) @ unique ->
    {t : Pref.token | Pref.own t === H.union (heap (flipped model)) frame
      && valid (flipped model)
      && H.disjoint (heap (flipped model)) frame}
      @ unique
type built = {
  pointer : node option @@ aliased;
  model : tree @@ ghost aliased;
  state : Pref.token;
}
val empty : unit ->
  {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model && b.model === Empty} @ unique
val branch : (value : int) ->
  (l : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model}) @ unique ->
  (r : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model}) @ unique ->
  {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model
      && (match b.model with Empty -> false | Branch (n, lm, rm) ->
        n.value = value && lm === l.model && rm === r.model)} @ unique
val leaf : (value : int) ->
  {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model} @ unique
type shape = Tip | Fork of int * shape * shape
val observe :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (t : {t : Pref.token | valid model && root model === pointer
      && Pref.own t === heap model}) @ unique -> shape
