(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml quicksort_iarray_model.ml quicksort_iarray.mli quicksort_iarray.ml iarray_borrow.ml";
 { bytecode; }
 { native; }
*)

open Borrow_iarray

let unchanged (values : int iarray) :
    {result : int iarray | result === values} =
  let owned = Owned_array.of_iarray values in
  let result = Owned_array.into_iarray owned in
  result

let replace_first :
    (values : {a : int iarray | 0 < Iarray.length a}) ->
    {result : int iarray | let before = values in
      result === Vox_iarray.updated before 0 42} = fun values ->
  let before = values in
  let zero = 0 in
  let value = 42 in
  let owned = Owned_array.of_iarray before in
  let post = ghost_ (fun (_ : unit) (after : int iarray @ immutable) ->
    after === Vox_iarray.updated before zero value) in
  let result = Owned_array.with_mut owned post (fun loan ->
    let s = loan in
    let index : {i : int | 0 <= i && i < Iarray.length (Slice.current s)} =
      zero in
    let changed = Slice.set s index value in
    let snapshot = Slice.snapshot (borrow_ changed) in
    assert (Iarray.get snapshot 0 = 42);
    Slice.finish changed;
    ()) in
  let {state; _} = result in
  let output = Owned_array.into_iarray state in
  output

let () =
  let input = [: 1; 2; 3 :] in
  let output = unchanged input in
  assert (Iarray.to_list output = [1; 2; 3]);
  let input : {a : int iarray | 0 < Iarray.length a} = input in
  let changed = replace_first input in
  assert (Iarray.to_list changed = [42; 2; 3]);
  print_endline "direct iarray borrow specifications"

external same_list : ('a : immutable_data).
  'a list @ immutable -> 'a list @ immutable -> bool = "%equal"

let check_ints (values : int iarray @ immutable total) expected =
  assert (same_list (Vox_iarray.to_list values) expected)

let () =
  let values = [: 1; 2; 3 :] in
  check_ints values [1; 2; 3];
  check_ints (Vox_iarray.slice values max_int min_int) [];
  let changed = Vox_iarray.updated values 1 4 in
  check_ints changed [1; 4; 3];
  check_ints values [1; 2; 3];
  check_ints (Vox_iarray.slice changed 1 3) [4; 3];
  check_ints (Vox_iarray.slice changed (-1) max_int) [1; 4; 3];
  check_ints (Vox_iarray.updated changed max_int 0) [1; 4; 3];
  check_ints (Vox_iarray.swap changed 0 2) [3; 4; 1];
  let values = [: true; false :] in
  let owned = Owned_array.of_iarray values in
  let output = Owned_array.into_iarray owned in
  assert (same_list (Vox_iarray.to_list output) [true; false])
