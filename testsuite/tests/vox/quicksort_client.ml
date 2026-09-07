(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml borrow.mli borrow.ml quicksort_model.ml quicksort.mli quicksort.ml quicksort_client.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

open Borrow
module Spec = Quicksort.Spec

let verified_sort : (parallel : bool) -> (values : int iarray) ->
    {result : int iarray | Spec.sorted (Model.of_iarray result)
      && Spec.permutation (Model.of_iarray values) (Model.of_iarray result)} =
    fun parallel values ->
  let refine_ owned = Owned_array.of_iarray values in
  let refine_ sorted =
    if parallel then Quicksort.parallel_sort_array ~max_domains:4 ~cutoff:32 owned
    else Quicksort.sort_array owned in
  let refine_ result = Owned_array.into_iarray sorted in
  refine_ result

let check (values : int list) =
  let input = Iarray.of_list values in
  let expected = List.sort compare values in
  let sequential = verified_sort false input in
  let parallel = verified_sort true input in
  let refine_ sequential = sequential in
  let refine_ parallel = parallel in
  assert (Iarray.to_list sequential = expected);
  assert (Iarray.to_list parallel = expected);
  assert (Iarray.to_list input = values)

let () =
  List.iter check
    [[]; [0]; [2; 1]; [1; 1]; [min_int; 0; max_int; min_int; -1];
     [5; 1; 4; 2; 3; 2; 1; 5]];
  let random = Random.State.make [|0x51ce|] in
  for size = 0 to 128 do
    check (List.init size (fun _ -> Random.State.int random 17 - 8))
  done;
  check (List.init 8192 Fun.id);
  check (List.init 8192 (fun i -> 8191 - i));
  check (List.init 8192 (fun _ -> 7));
  check (List.init 8192 (fun i -> i mod 5));
  print_endline "verified quicksort: sequential and parallel, duplicates, extrema, ordered and random inputs"
