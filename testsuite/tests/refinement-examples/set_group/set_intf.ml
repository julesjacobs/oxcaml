module type SET = sig
  type t

  val empty  : t @@ total
  val insert : int -> t @ logical -> t @@ total
  val member : int -> t @ local logical -> bool @@ total
  val equal  : t @ local logical -> t @ local logical -> bool @@ total

  (* The representation invariant the implementation maintains.  What it
     says is private to the implementation; a client learns only that
     [empty] satisfies it, that [insert] preserves it, and that the
     membership laws below hold for values that satisfy it.  [member]
     descends a single spine, so [insert_law] is false without it. *)
  val invariant : t @ local logical -> bool @@ total

  val empty_invariant : unit{ invariant empty = true } @@ total

  val insert_invariant :
    inserted:int ->
    tree:t @ logical ->
    well_formed:unit{ invariant tree = true } ->
    unit{ invariant (insert inserted tree) = true } @@ total

  val empty_law : query:int -> unit{ member query empty = false } @@ total

  val insert_law :
    inserted:int ->
    tree:t @ logical ->
    query:int ->
    well_formed:unit{ invariant tree = true } ->
    unit{
      member query (insert inserted tree)
      = ((query = inserted) || member query tree)
    } @@ total

  val equal_forward_law :
    t1:t @ logical ->
    t2:t @ logical ->
    equal_trees:unit{ equal t1 t2 = true } ->
    query:int ->
    unit{ member query t1 = member query t2 } @@ total

  val equal_backward_law :
    t1:t @ logical ->
    t2:t @ logical ->
    pointwise:(query:int -> unit{ member query t1 = member query t2 })
      @ total ->
    unit{ equal t1 t2 = true } @@ total
end
