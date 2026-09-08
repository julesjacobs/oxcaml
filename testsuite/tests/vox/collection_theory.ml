(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml collection_theory.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

module Model = Vox_sequence
module Spec = Vox_int_sequence

let (rotate_at @ total) : (values : int list) ->
    (position : {i : Bigint.t | Bigint.compare 0Z i <= 0
      && Bigint.compare i (Model.length values) <= 0}) ->
    {result : int list | Spec.permutation values result
      && Model.length result === Model.length values} = fun values position ->
  let refine_ position = position in
  let left = Model.take position values in
  let right = Model.drop position values in
  let result = Model.append right left in
  ghost_ (Model.cut values position);
  ghost_ (Model.append_length right left);
  ghost_ (Spec.permutation_rotate left right);
  refine_ result

let (rotate_count @ total) : (before : int list) -> (after : int list) ->
    (target : int) ->
    {u : unit | Spec.permutation before after} @ ghost ->
    {u : unit | Spec.count before target === Spec.count after target} =
    fun before after target premise ->
  premise;
  Spec.permutation_count before after target;
  let u = () in refine_ u

let () =
  let values = [1; 2; 1; 3] in
  let position = 2Z in
  ghost_ (Model.length_def values);
  let tail = [2; 1; 3] in
  ghost_ (Model.length_def tail);
  let position : {i : Bigint.t | Bigint.compare 0Z i <= 0
    && Bigint.compare i (Model.length values) <= 0} = refine_ position in
  let refine_ result = rotate_at values position in
  assert (result = [1; 3; 1; 2]);
  let one = 1 in
  ghost_ (Spec.permutation_count values result one);
  let model = ghost_ (Spec.bag result) in
  ghost_ (Spec.bag_multiplicity result one);
  let _u : {u : unit | Spec.multiplicity model one === Spec.count values one} =
    let u = () in refine_ u in
  print_endline "verified rotation: length, permutation, and multiplicity"

let check values cut =
  let position = Bigint.of_int cut in
  let checked : {i : Bigint.t | Bigint.compare 0Z i <= 0
    && Bigint.compare i (Model.length values) <= 0} = assume_ position in
  let refine_ output = rotate_at values checked in
  let size = List.length values in
  let expected = List.init size (fun i -> List.nth values ((i + cut) mod size)) in
  assert (output = expected)

let () =
  for size = 0 to 16 do
    let values = List.init size (fun i -> i mod 5 - 2) in
    for cut = 0 to size do check values cut done
  done
