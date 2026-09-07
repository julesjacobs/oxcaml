@@ portable

module Spec = Quicksort_model

val sort : (s : int Borrow.Slice.t) @ local unique ->
  {u : unit | Spec.sorted (Borrow.Slice.final s)
    && Spec.permutation (Borrow.Slice.current s) (Borrow.Slice.final s)}

val parallel_sort : ?max_domains:int -> ?cutoff:int ->
  (s : int Borrow.Slice.t) @ local unique ->
  {u : unit | Spec.sorted (Borrow.Slice.final s)
    && Spec.permutation (Borrow.Slice.current s) (Borrow.Slice.final s)}

val sort_array : (a : int Borrow.Owned_array.t) @ unique ->
  {r : int Borrow.Owned_array.t | Spec.sorted (Borrow.Owned_array.contents r)
    && Spec.permutation (Borrow.Owned_array.contents a) (Borrow.Owned_array.contents r)} @ unique

val parallel_sort_array : ?max_domains:int -> ?cutoff:int ->
  (a : int Borrow.Owned_array.t) @ unique ->
  {r : int Borrow.Owned_array.t | Spec.sorted (Borrow.Owned_array.contents r)
    && Spec.permutation (Borrow.Owned_array.contents a) (Borrow.Owned_array.contents r)} @ unique
