(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml borrow.mli borrow.ml vox_int_sequence.mli vox_int_sequence.ml int_set_intf.mli avl_sets.mli avl_sets.ml quicksort_model.ml quicksort.mli quicksort.ml collections_boundary_client.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
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
    Avl_sets.lookup_add query b source;
    let u = () in u
  in
  Avl_sets.extensional left right members;
  let u = () in u

let () =
  let input = [: 3; 1; 3; min_int; max_int; 3 :] in
  let sequential = sort_count false input 3 in
  let parallel = sort_count true input 3 in
  assert (Iarray.to_list sequential = [min_int; 1; 3; 3; 3; max_int]);
  assert (Iarray.to_list parallel = Iarray.to_list sequential);
  ghost_ (set_commutes Avl_sets.empty 1 2);
  let present = set_union (Avl_sets.add 1 Avl_sets.empty)
    (Avl_sets.add 2 Avl_sets.empty) 2 in
  assert present;
  print_endline "public collection laws: multiplicity and extensional membership"
