@@ portable

module Spec = Vox_int_sequence

(* [sort] sorts a borrowed slice. It is not [total]: it lends parts of the
   slice ([Borrow.Slice.split3]) and ends those loans ([Borrow.Slice.finish]),
   and neither step is a function of its arguments. *)
val sort : (s : int Borrow.Slice.t) @ local unique ->
  {u : unit | Spec.sorted (Borrow.Slice.final s)
    && Spec.permutation (Borrow.Slice.current s) (Borrow.Slice.final s)}

(* [sort_array] sorts an owned array without borrowing it. Every operation it
   uses takes the array and returns it, so it is [total]. *)

val sort_array : (a : int Borrow.Owned_array.t) @ unique ->
  {r : int Borrow.Owned_array.t | Spec.sorted (Borrow.Owned_array.contents r)
    && Spec.permutation (Borrow.Owned_array.contents a)
      (Borrow.Owned_array.contents r)} @ unique @@ total

val parallel_sort_array : ?max_domains:int -> ?cutoff:int ->
  (a : int Borrow.Owned_array.t) @ unique ->
  {r : int Borrow.Owned_array.t | Spec.sorted (Borrow.Owned_array.contents r)
    && Spec.permutation (Borrow.Owned_array.contents a) (Borrow.Owned_array.contents r)} @ unique
