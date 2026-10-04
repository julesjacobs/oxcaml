(** {1 Tree model} *)

module H = Pref.Heap

type node = {
  value : int;
  left : node option Pref.t;
  right : node option Pref.t;
}

type tree = Empty | Branch of node * tree * tree
[@@inductive]

type shape = Tip | Fork of int * shape * shape
[@@inductive]

val root : tree @ immutable -> node option @@ total
val root_def : (tree : tree) @ immutable ->
  {u : unit | root tree === (match tree with
    | Empty -> None | Branch (n, _, _) -> Some n)} @@ total

val shape_of : tree @ immutable -> shape @@ total
val shape_of_def : (model : tree) @ immutable ->
  {u : unit | shape_of model ===
    (match model with Empty -> Tip
    | Branch (n, l, r) -> Fork (n.value, shape_of l, shape_of r))} @@ total

val flipped : tree @ immutable -> tree @@ total
val flipped_def : (tree : tree) @ immutable ->
  {u : unit | flipped tree === (match tree with
    | Empty -> Empty
    | Branch (n, left, right) -> Branch (n, flipped right, flipped left))} @@ total

val mirror_shape : shape @ immutable -> shape @@ total
val mirror_shape_def : (s : shape) @ immutable ->
  {u : unit | mirror_shape s === (match s with Tip -> Tip
    | Fork (value, l, r) -> Fork (value, mirror_shape r, mirror_shape l))}
  @@ total

(** {1 Heap ownership} *)

val links :
  node @ immutable ->
  node option @ immutable -> node option @ immutable -> node option Pref.heap @ ghost @@ total
val links_def : (n : node) @ immutable ->
  (left : node option) @ immutable -> (right : node option) @ immutable ->
  {u : unit | links n left right ===
    ghost_ (H.put (H.put (H.empty ()) n.left left) n.right right)} @@ total

val heap : tree @ immutable -> node option Pref.heap @ ghost @@ total
val heap_def : (tree : tree) @ immutable ->
  {u : unit | heap tree === ghost_ (match tree with
    | Empty -> H.empty ()
    | Branch (n, left, right) ->
      H.union (links n (root left) (root right)) (H.union (heap left) (heap right)))}
  @@ total

val valid : tree @ immutable -> bool @ ghost @@ total
val valid_def : (tree : tree) @ immutable ->
  {u : unit | valid tree === ghost_ (match tree with
    | Empty -> true
    | Branch (n, left, right) ->
      valid left && valid right && not (n.left === n.right)
      && H.disjoint (links n (root left) (root right))
           (H.union (heap left) (heap right))
      && H.disjoint (heap left) (heap right))} @@ total

type built = {
  pointer : node option @@ aliased;
  model : tree @@ global ghost;
  state : node option Pref.token;
}

type observation = { shape : shape @@ aliased; state : node option Pref.token; }

(** {1 Owned trees} *)

module Owned : sig
  type t : value & void & void
  val model : t @ local immutable total ghost -> tree @ ghost @@ total
  val empty : unit -> {state : t | model state === Empty} @ unique
  val leaf : (value : int) ->
    {state : t | shape_of (model state) === Fork (value, Tip, Tip)} @ unique
  val branch : (value : int) -> (left : t) @ unique -> (right : t) @ unique ->
    {state : t | match model state with Empty -> false | Branch (n, l, r) ->
      n.value = value && l === model left && r === model right} @ unique
  val mirror : (state : t) @ unique ->
    {next : t | shape_of (model next) === mirror_shape (shape_of (model state))
    && model next === flipped (model state)} @ unique
  val observe : (state : t) @ local read total forkable unyielding ->
    {result : shape | result === shape_of (model state)}
  val adopt : (b : {b : built | b.pointer === root b.model && valid b.model &&
    Pref.own b.state === heap b.model}) @ unique ->
    {state : t | model state === b.model} @ unique
  val release : (state : t) @ unique ->
    {b : built | b.pointer === root b.model && valid b.model &&
      Pref.own b.state === heap b.model && b.model === model state} @ unique
end

(** {1 Operations with explicit ownership} *)

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
      && Pref.own b.state === heap b.model
      && shape_of b.model === Fork (value, Tip, Tip)} @ unique

val mirror_with_frame :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (frame : node option Pref.heap) @ immutable ghost ->
    (t : {t : node option Pref.token | valid model && root model === pointer
      && H.disjoint (heap model) frame
      && Pref.own t === H.union (heap model) frame}) @ unique ->
    {t : node option Pref.token | Pref.own t === H.union (heap (flipped model)) frame
      && valid (flipped model)
      && H.disjoint (heap (flipped model)) frame}
      @ unique

val observe_read :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (t : {t : node option Pref.token | valid model && root model === pointer
      && Pref.own t === heap model}) @ local read -> {result : shape | result === shape_of model}

val observe :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (t : {t : node option Pref.token | valid model && root model === pointer
      && Pref.own t === heap model}) @ unique ->
    {r : observation | r.shape === shape_of model
      && Pref.own r.state === heap model} @ unique

(** {1 Observation laws} *)

val root_flipped :
  (tree : tree) @ immutable ->
  {u : unit | (root (flipped tree)) === (root tree)} @@ total

val shape_flipped : (tree : tree) @ immutable ->
  {u : unit | shape_of (flipped tree) === mirror_shape (shape_of tree)}
  @ ghost @@ total
