(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml sorted_array_proofs.ml sorted_array_model.ml sorted_array.mli sorted_array.ml";
 { bytecode; }
 { native; }
*)

open Sorted_array

let (empty_observations @ total) (value : int) :
    {u : unit | not (occurs empty value)
      && range_spec empty value 0 0
      && at empty (-1) = 0} =
  let array = empty in
  let zero = 0 in
  let negative = -1 in
  occurs_equation array value;
  occurs_between_equation array value zero zero;
  range_equation array value zero zero;
  at_outside array negative;
  ()

let round_trip : (source : t) -> (value : int) -> (index : int) ->
    {u : unit | 0 <= index && index < length source} @ ghost ->
    {result : t | contents result === contents source
      && length result = length source && at result index = at source index} =
  fun source value index premise ->
  premise;
  let pair = insert source value in
  let (position : int), (inserted : t) = pair in
  ghost_ (length_bounds source);
  let result = remove_at inserted position () in
  ghost_ (
    inserted_def source inserted position value;
    removed_def inserted result position;
    contents_length source;
    let sequence = contents source in
    let p = Bigint.of_int position in
    let prefix = Vox_sequence.take p sequence in
    let suffix = Vox_sequence.drop p sequence in
    Sorted_array_model.insert_def sequence p value;
    Vox_sequence.cut sequence p;
    Vox_sequence.append_split prefix (value :: suffix);
    Sorted_array_model.remove_def (contents inserted) p;
    Vox_sequence.drop_add (contents inserted) p 1Z;
    Vox_sequence.drop_def 1Z (value :: suffix);
    Vox_sequence.drop_def 0Z suffix;
    let original = if index < position then index else index + 1 in
    removed_at inserted result position index ();
    inserted_at source inserted position value original ();
    (() : {u : unit | at result index = at source index}));
  result

let () =
  let initial = empty in
  let value = 7 in
  let pair = insert initial value in
  let (position : int), (one : t) = pair in
  let found = mem one value in
  (() : {u : unit | found && length one = 1});
  let smaller = 3 in
  let pair = insert one smaller in
  let _, two = pair in
  let left = 0 in
  let right = 1 in
  ghost_ (ordered two left right ());
  (() : {u : unit | at two left <= at two right});
  let result = round_trip one value position () in
  let removed = remove_at result position () in
  (() : {u : unit | length removed = 0});
  Format.printf "abstract sorted array: found=%b; restored length=%d@."
    found (length removed)

let insert_twice : (source : t) -> (first : int) -> (second : int) ->
    {result : t | length result = length source + 2
      && occurs result second} =
  fun source first second ->
  let pair = insert source first in
  let _, middle = pair in
  let pair = insert middle second in
  let _, result = pair in
  result

let () =
  let initial = empty in
  let pair = insert initial 5 in
  let _, source = pair in
  let result = insert_twice source 1 9 in
  let found = mem result 9 in
  let proof : {u : unit | found && length result = 3} = () in
  proof;
  Format.printf "abstract sorted array: inserted twice; found=%b; length=%d@."
    found (length result)

let check values =
  let initial = empty in
  let (array : t) = List.fold_left (fun (source : t) (value : int) ->
    let pair = insert source value in
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
    if 0 <= index && index < length array then (
      let result = remove_at array index () in
      let remaining = List.filteri (fun i _ -> i <> index) expected in
      assert (List.init (length result) (at result) = remaining))
    else assert false) expected

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
      === Some (at array index)} @ ghost = fun array index premise -> ghost_ (
  premise;
  let bounded : {i : int | 0 <= i && i < length array} = index in
  contents_at array bounded;
  ())

let () =
  let _, one = insert empty 7 in
  let _, duplicates = insert one 7 in
  assert (length duplicates = 2);
  let restored = remove_at duplicates 0 () in
  assert (length restored = 1 && at restored 0 = 7);
  assert (length duplicates = 2 && at duplicates 0 = 7 && at duplicates 1 = 7)
