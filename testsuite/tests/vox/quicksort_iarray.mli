@@ portable

module Spec = Vox_iarray.Int

val sort : (s : int Borrow_iarray.Slice.t) @ local unique ->
  {u : unit | Spec.sorted (Borrow_iarray.Slice.final s)
    && Spec.permutation (Borrow_iarray.Slice.current s) (Borrow_iarray.Slice.final s)} @@ total

val parallel_sort : ?max_domains:int -> ?cutoff:int ->
  (s : int Borrow_iarray.Slice.t) @ local unique ->
  {u : unit | Spec.sorted (Borrow_iarray.Slice.final s)
    && Spec.permutation (Borrow_iarray.Slice.current s) (Borrow_iarray.Slice.final s)}

val sort_array : (a : int Borrow_iarray.Owned_array.t) @ unique ->
  {r : int Borrow_iarray.Owned_array.t | Spec.sorted (Borrow_iarray.Owned_array.contents r)
    && Spec.permutation (Borrow_iarray.Owned_array.contents a)
      (Borrow_iarray.Owned_array.contents r)} @ unique @@ total

val parallel_sort_array : ?max_domains:int -> ?cutoff:int ->
  (a : int Borrow_iarray.Owned_array.t) @ unique ->
  {r : int Borrow_iarray.Owned_array.t | Spec.sorted (Borrow_iarray.Owned_array.contents r)
    && Spec.permutation (Borrow_iarray.Owned_array.contents a) (Borrow_iarray.Owned_array.contents r)} @ unique
