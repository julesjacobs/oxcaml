(* TEST
 has-z3;
 multicore;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml borrow.mli borrow.ml vox_int_sequence.mli vox_int_sequence.ml quicksort_model.ml quicksort.mli quicksort.ml quicksort_frame_client.ml";
 { bytecode; }
*)

open Borrow
module Spec = Quicksort.Spec

let sort_range : (parallel : bool) -> (s : int Slice.t) @ local unique ->
    (first : {i : int | 0 <= i
      && Bigint.of_int i <= Model.length (Slice.current s)}) ->
    (past : {j : int | let i = first in i <= j
      && Bigint.of_int j <= Model.length (Slice.current s)}) ->
    {result : int Slice.t | let i = first in let j = past in
      Spec.sorted (Model.sub (Slice.current result)
        (Bigint.of_int i) (Bigint.of_int j))
      && Spec.permutation
        (Model.sub (Slice.current s) (Bigint.of_int i) (Bigint.of_int j))
        (Model.sub (Slice.current result) (Bigint.of_int i) (Bigint.of_int j))
      && Slice.current result ===
        Model.append (Model.take (Bigint.of_int i) (Slice.current s))
          (Model.append
            (Model.sub (Slice.current result) (Bigint.of_int i) (Bigint.of_int j))
            (Model.drop (Bigint.of_int j) (Slice.current s)))} @ local unique =
  fun parallel s first past -> exclave_ (
  let i = first in
  let j = past in
  let before = ghost_ (Model.sub (Slice.current (borrow_ s))
    (Bigint.of_int i) (Bigint.of_int j)) in
  let[@def] (post @ total) (u : unit @ immutable)
      (values : int Model.t @ immutable) =
    ghost_ (Spec.sorted values && Spec.permutation before values) in
  let desired = ghost_ post in
  let step = Slice.with_range s first past desired (fun middle ->
    let slice = middle in
    let after = ghost_ (Slice.final (borrow_ slice)) in
    let done_ =
      if parallel then Quicksort.parallel_sort ~max_domains:2 ~cutoff:2 slice
      else Quicksort.sort slice in
    let u = () in
    ghost_ (post_def u after);
    u) in
  let {state; _} = step in
  let after = ghost_ (Model.sub (Slice.current (borrow_ state))
    (Bigint.of_int i) (Bigint.of_int j)) in
  let u = () in
  ghost_ (post_def u after);
  state)

let run_range parallel values first past =
  let array = Owned_array.of_iarray values in
  let[@def] (post @ total) (u : unit @ immutable)
      (values : int Model.t @ immutable) = ghost_ true in
  let desired = ghost_ post in
  let result = Owned_array.with_mut array desired (fun loan ->
    let s = loan in
    let eventual = ghost_ (Slice.final (borrow_ s)) in
    let size = Slice.length (borrow_ s) in
    let state =
      if 0 <= first && first <= past && past <= size then
        let i : {i : int | 0 <= i
          && Bigint.of_int i <= Model.length (Slice.current s)} =
          first in
        let j : {j : int | let i = i in i <= j
          && Bigint.of_int j <= Model.length (Slice.current s)} =
          past in
        let state = sort_range parallel s i j in state
      else s in
    Slice.finish state;
    let u = () in
    ghost_ (post_def u eventual);
    u) in
  let {state; _} = result in
  let output = Owned_array.into_iarray state in output

let () =
  let values = [: 99; 4; 2; 2; 1; -99 :] in
  List.iter (fun parallel ->
    let output = run_range parallel values 1 5 in
    assert (Iarray.to_list output = [99; 1; 2; 2; 4; -99]);
    assert (Iarray.to_list values = [99; 4; 2; 2; 1; -99]);
    assert (Iarray.to_list (run_range parallel values 2 2) =
      Iarray.to_list values)) [false; true];
  print_endline "quicksort slice: exact framing, duplicate counts, empty range"
