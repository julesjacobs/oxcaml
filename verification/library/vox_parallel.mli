@@ portable

(** [fork_join left right] runs [left] in a new domain and [right] in the
    calling domain, and returns both results once both have finished. If
    [right] raises, the new domain is joined first; if [left] raises, its
    exception is re-raised by the join.

    The type is ordinary polymorphism with no refinements: instantiating ['a]
    and ['b] at refined types, such as
    [{r : int Borrow.Owned_array.t | Spec.sorted (Borrow.Owned_array.contents r)}],
    carries each branch's proved result across the join. *)
val fork_join :
  (unit -> 'a @ unique) @ portable once ->
  (unit -> 'b @ unique) @ portable once ->
  'a * 'b @ unique
