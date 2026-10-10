@@ portable

module Spec = Vox_int_sequence

(** On normal return, the elements are nondecreasing and every integer
    occurs as often as before the call. The slice contract describes its
    contents when the borrow ends. Termination and cost are unspecified. *)
val sort : (s : int Borrow.Slice.t) @ local unique ->
  {u : unit | Spec.sorted (Borrow.Slice.final s)
    && Spec.permutation (Borrow.Slice.current s) (Borrow.Slice.final s)}

val sort_array : (a : int Borrow.Owned_array.t) @ unique ->
  {r : int Borrow.Owned_array.t | Spec.sorted (Borrow.Owned_array.contents r)
    && Spec.permutation (Borrow.Owned_array.contents a)
      (Borrow.Owned_array.contents r)} @ unique

val parallel_sort_array : ?max_domains:int -> ?cutoff:int ->
  (a : int Borrow.Owned_array.t) @ unique ->
  {r : int Borrow.Owned_array.t | Spec.sorted (Borrow.Owned_array.contents r)
    && Spec.permutation (Borrow.Owned_array.contents a) (Borrow.Owned_array.contents r)} @ unique
