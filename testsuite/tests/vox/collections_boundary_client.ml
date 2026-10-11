(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml borrow.mli borrow.ml vox_parallel.mli vox_parallel.ml vox_int_sequence.mli vox_int_sequence.ml int_set_intf.mli avl_sets.mli avl_sets.ml quicksort_model.ml quicksort.mli quicksort.ml";
 { bytecode; }
 { native; }
*)

module Q = Quicksort
module S = Q.Spec
module B = Borrow
module M = Vox_sequence

let sort_count (parallel : bool) (input : int iarray) (value : int) :
    {output : int iarray | S.sorted (M.of_iarray output)
      && S.count (M.of_iarray output) value ===
        S.count (M.of_iarray input) value} =
  let array = B.Owned_array.of_iarray input in
  let array =
    if parallel then Q.parallel_sort_array ~max_domains:2 ~cutoff:2 array
    else Q.sort_array array in
  let output = B.Owned_array.into_iarray array in
  ghost_ (
    let before = M.of_iarray input in
    let after = M.of_iarray output in
    S.permutation_count before after value);
  output

let set_union (left : Avl_sets.t) (right : Avl_sets.t) (value : int) :
    {answer : bool | answer =
      (Avl_sets.lookup value left || Avl_sets.lookup value right)} =
  let combined = Avl_sets.union left right in
  let answer = Avl_sets.lookup value combined in
  ghost_ (Avl_sets.lookup_union value left right);
  answer

let (set_commutes @ total) (source : Avl_sets.t) (a : int) (b : int) :
    {u : unit | Avl_sets.equal (Avl_sets.add a (Avl_sets.add b source))
      (Avl_sets.add b (Avl_sets.add a source))} =
  let a_first = Avl_sets.add a source in
  let b_first = Avl_sets.add b source in
  let left = Avl_sets.add a b_first in
  let right = Avl_sets.add b a_first in
  let (members @ total) (query : int) :
      {u : unit | Avl_sets.lookup query left === Avl_sets.lookup query right} =
    Avl_sets.lookup_add query a b_first;
    Avl_sets.lookup_add query b a_first;
    Avl_sets.lookup_add query a source;
    Avl_sets.lookup_add query b source
  in
  Avl_sets.extensional left right members

let (set_duplicate_size @ total) (source : Avl_sets.t) (value : int) :
    {u : unit | Avl_sets.size (Avl_sets.add value (Avl_sets.add value source))
      === Avl_sets.size (Avl_sets.add value source)} =
  let once = Avl_sets.add value source in
  Avl_sets.lookup_add value value source;
  Avl_sets.size_add value once

let (set_pair_size @ total) : (first : int) -> (second : int) ->
    {u : unit | first <> second} @ ghost ->
    {u : unit | Avl_sets.size
      (Avl_sets.add second (Avl_sets.add first Avl_sets.empty)) === 2Z} =
  fun first second different ->
  different;
  let empty = Avl_sets.empty in
  Avl_sets.extensional empty empty (fun _element -> ());
  Avl_sets.size_zero empty;
  Avl_sets.lookup_empty first;
  Avl_sets.size_add first empty;
  let once = Avl_sets.add first empty in
  Avl_sets.lookup_empty second;
  Avl_sets.lookup_add second first empty;
  Avl_sets.size_add second once

let () =
  let input = [: 3; 1; 3; min_int; max_int; 3 :] in
  let sequential = sort_count false input 3 in
  let parallel = sort_count true input 3 in
  assert (Iarray.to_list sequential = [min_int; 1; 3; 3; 3; max_int]);
  assert (Iarray.to_list parallel = Iarray.to_list sequential);
  ghost_ (
    set_commutes Avl_sets.empty 1 2;
    set_duplicate_size Avl_sets.empty 1;
    set_pair_size 1 2 ());
  let present = set_union (Avl_sets.add 1 Avl_sets.empty)
    (Avl_sets.add 2 Avl_sets.empty) 2 in
  assert present;
  print_endline "public collection laws: multiplicity and extensional membership"
