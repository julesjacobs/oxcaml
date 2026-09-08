(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml borrow.mli borrow.ml vox_int_sequence.mli vox_int_sequence.ml borrow_validation.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

open Borrow
module Spec = Vox_int_sequence

let validate_slice : (s : int Slice.t) @ local unique ->
    {u : unit | Spec.sorted (Slice.final s)} = fun s ->
  let refine_ values = Slice.snapshot (borrow_ s) in
  let checked : {xs : int iarray | Spec.sorted (Model.of_iarray xs)} = assume_ values in
  let refine_ checked = checked in
  let refine_ closed = Slice.finish s in
  let u = () in refine_ u

let validate : (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)} @ unique = fun a ->
  let post = ghost_ (fun (_ : unit @ immutable) (after : int Model.t @ immutable) ->
        Spec.sorted after) in
  let refine_ result = Owned_array.with_mut a post (fun loan ->
    let refine_ s = loan in
    let refine_ u = validate_slice s in
    refine_ u) in
  let {state; _} = result in

  refine_ state

let check (values : int list) =
  let input = Iarray.of_list values in
  let refine_ a = Owned_array.of_iarray input in
  let refine_ a = validate a in
  let refine_ output = Owned_array.into_iarray a in
  assert (Iarray.to_list output = values)

let () =
  List.iter check [[]; [1]; [1; 1; 2; 3]; [min_int; 0; max_int]];
  let rejected = try check [2; 1]; false with Assert_failure _ -> true in
  assert rejected;
  print_endline "verified runtime validation: snapshot checked with assume_, unsorted input rejected"
