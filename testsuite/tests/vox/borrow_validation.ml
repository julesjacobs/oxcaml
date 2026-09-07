(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "borrow_model.mli borrow_model.ml borrow.mli borrow.ml quicksort_model.ml borrow_validation.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

open Borrow
module Spec = Quicksort_model

let validate_slice : (s : int Slice.t) @ local unique ->
    {u : unit | Spec.sorted (Slice.final s)} = fun s ->
  let refine_ snapshot = Slice.snapshot s in
  let {value = (values : int iarray); state} = snapshot in
  let checked : {xs : int iarray | Spec.sorted (Model.of_iarray xs)} = assume_ values in
  let refine_ checked = checked in
  let refine_ closed = Slice.finish state in
  let u = () in refine_ u

let validate : (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)} @ unique = fun a ->
  let[@def] (post @ total) (u : unit @ immutable) (after : int Model.t @ immutable) =
    ghost_ (Spec.sorted after) in
  let erased_post = ghost_ post in
  let refine_ result = Owned_array.with_mut a erased_post (fun loan ->
    let refine_ s = loan in
    let eventual = ghost_ (Slice.final (borrow_ s)) in
    let refine_ u = validate_slice s in
    let refine_ equation = ghost_ (post_def u eventual) in
    refine_ u) in
  let {value = u; state} = result in
  let after = ghost_ (Owned_array.contents (borrow_ state)) in
  let refine_ equation = ghost_ (post_def u after) in
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
