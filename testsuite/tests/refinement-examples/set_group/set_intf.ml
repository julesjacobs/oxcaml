module type SET = sig
  type t

  val empty  : t @@ total
  val insert : int -> t @ logical -> t @@ total
  val member : int -> t @ local logical -> bool @@ total
  val equal  : t @ local logical -> t @ local logical -> bool @@ total

  (* The representation invariant the implementation maintains.  What it
     says is private to the implementation; a client learns only that
     [empty] satisfies it, that [insert] preserves it, and that the
     membership laws below hold for values that satisfy it.

     How much of an invariant these four operations force depends on the
     implementation, and it is worth being exact about it.  For the trees
     that rotate, [insert_law] really is false without ordering, so their
     ordering component is forced here.  For [bst] and [ulist] it is not,
     and for two different reasons that the same sentence used to run
     together: [bst]'s [member] and [insert] follow the same comparison
     spine, so [insert_law] holds whether or not the tree is ordered,
     while [ulist]'s [member] is a full scan, which makes it
     occurrence-exact and [insert_law] unconditional for that reason
     instead.  Either way an ill-formed value has the same membership
     behaviour as the well-formed value over the same observed member
     set, and nothing written in terms of exactly [empty], [insert],
     [member] and [equal] tells the two apart.

     Read that as a statement about these four operations, because it is
     one.  It is not an impossibility result, and it was once written as
     though it were: adding a fifth operation forces both components.
     [Bal_intf.LEAST_SET] adds the least element and forces [bst]'s
     ordering; [Bal_intf.REMOVING_SET] adds deletion and forces
     [ulist]'s uniqueness.  Neither exposes shape.

     One premise of the four-operation argument is load-bearing and easy
     to miss: [equal] is pinned to extensional equality because
     [equal_forward_law] and [equal_backward_law] below carry no
     [well_formed] hypothesis, so they are demanded of ill-formed values
     too and no implementation can choose an [equal] that separates.
     Guarding those two laws by the invariant would break the argument
     as much as adding an operation does.

     Forcing a property that the member set does not determine even on
     well-formed values --- balance --- needs a new observation rather
     than a new law.  [Bal_intf.BALANCED_SET] adds one. *)
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
