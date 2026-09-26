(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml sorted_array_proofs.ml sorted_array.mli sorted_array.ml sorted_array_client.ml";
 { bytecode; }
*)

open Sorted_array

let (empty_observations @ total) (value : int) :
    {u : unit | not (occurs empty value)
      && range_spec empty value 0 0
      && edited empty empty 0 value true
      && at empty (-1) = 0} =
  let array = empty in
  let zero = 0 in
  let negative = -1 in
  let inserting = true in
  occurs_equation array value;
  occurs_between_equation array value zero zero;
  range_equation array value zero zero;
  edited_equation array array zero value inserting;
  edit_suffix_equation array array zero value inserting zero;
  at_outside array negative;
  let u = () in u

let round_trip : (source : t) -> (value : int) -> (index : int) ->
    {u : unit | 0 < length source + 2
      && 0 <= index && index < length source} @ ghost ->
    {result : t | length result = length source
      && at result index = at source index} =
  fun source value index premise ->
  premise;
  let u = () in
  let pair = insert source value (u) in
  let (position : int), (inserted : t) = pair in
  let result = remove_at inserted position (u) in
  ghost_ (
    let zero = 0 in
    let insertion = true in
    let removal = false in
    let original = if index < position then index else index + 1 in
    edited_at inserted result position zero
      removal index (u);
    edited_at source inserted position value
      insertion original (u);
    (u : {u : unit | at result index = at source index}));
  result

let () =
  let initial = empty in
  let u = () in
  let value = 7 in
  let pair = insert initial value (u) in
  let (position : int), (one : t) = pair in
  let found = mem one value in
  let proof : {u : unit | found && length one = 1} = u in
  let _proof = proof in
  let smaller = 3 in
  let pair = insert one smaller (u) in
  let _, two = pair in
  let left = 0 in
  let right = 1 in
  ghost_ (ordered two left right (u));
  let proof : {u : unit | at two left <= at two right} = u in
  let _proof = proof in
  let result = round_trip one value position (u) in
  let removed = remove_at result position (u) in
  let proof : {u : unit | length removed = 0} = u in
  proof;
  Format.printf "abstract sorted array: found=%b; restored length=%d@."
    found (length removed)

let check values =
  let initial = empty in
  let (array : t) = List.fold_left (fun (source : t) (value : int) ->
    let u = () in
    let capacity : {u : unit | 0 < length source + 2} = assume_ u in
    let pair = insert source value capacity in
    let _, result = pair in result) initial values in
  let expected = List.sort Int.compare values in
  let actual = List.init (length array) (at array) in
  assert (actual = expected);
  List.iter (fun value ->
    let found = mem array value in
    assert (found = List.mem value expected);
    let bounds = equal_range array value in
    let first, past = bounds in
    let lower = List.length (List.filter (fun x -> x < value) expected) in
    let upper = List.length (List.filter (fun x -> x <= value) expected) in
    assert (first = lower && past = upper);
    let first_match = find_first array value in
    let last_match = find_last array value in
    assert (first_match = if lower = upper then None else Some lower);
    assert (last_match = if lower = upper then None else Some (upper - 1));
    let removed = remove_one array value in
    match removed with
    | None -> assert (not found)
    | Some (position, result) ->
      assert (position = lower);
      let remaining = List.filteri (fun i _ -> i <> position) expected in
      assert (List.init (length result) (at result) = remaining))
    [-1; 0; 1; 2; 3];
  List.iteri (fun index _ ->
    let u = () in
    let bounds : {u : unit | 0 <= index && index < length array} = assume_ u in
    let result = remove_at array index bounds in
    let remaining = List.filteri (fun i _ -> i <> index) expected in
    assert (List.init (length result) (at result) = remaining)) expected

let () =
  let rec sequences length =
    if length = 0 then [[]]
    else List.concat_map (fun tail ->
      List.map (fun head -> head :: tail) [0; 1; 2]) (sequences (length - 1)) in
  for length = 0 to 4 do List.iter check (sequences length) done;
  Format.printf "abstract sorted-array oracle: 121 input sequences@."

let observe_sequence : (array : t) -> (index : int) ->
    {u : unit | 0 <= index && index < length array} @ ghost ->
    {u : unit | Vox_sequence.at (contents array) (Bigint.of_int index)
      === Some (at array index)} @ ghost = fun array index premise ->
  premise;
  let bounded : {i : int | 0 <= i && i < length array} = index in
  ghost_ (
    contents_at array bounded;
    let u = () in u)
