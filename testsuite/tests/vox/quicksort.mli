@@ portable

module Spec = Vox_int_sequence

(* The sorts are not [total]. They lend the array or slice
   ([Borrow.Owned_array.with_mut], [Borrow.Slice.split3]) and end those loans
   ([Borrow.Slice.finish]); creating a loan chooses its final contents and
   ending it assumes them, so neither step is a function of its arguments.
   The recursion of [sort] still has a checked decreasing measure. *)
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
