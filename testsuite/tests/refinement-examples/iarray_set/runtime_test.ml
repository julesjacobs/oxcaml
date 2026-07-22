let expect condition message =
  if not condition then failwith message

let model_insert inserted values =
  List.sort_uniq Int.compare (inserted :: values)

external runtime_view : Iarray_set.t -> int list
  = "vox_sorted_iarray_view"

let check_set expected set =
  expect (runtime_view set = expected) "view mismatch";
  List.iter
    (fun query ->
      expect
        (Iarray_set.member query set = List.mem query expected)
        "membership mismatch")
    [ min_int; -1001; -17; -1; 0; 1; 2; 3; 17; 1001; max_int ]

let () =
  let set = ref Iarray_set.empty in
  let model = ref [] in
  check_set !model !set;
  List.iter
    (fun inserted ->
      set := Iarray_set.insert inserted !set;
      model := model_insert inserted !model;
      check_set !model !set)
    [ 3; 1; 2; 3; -1; max_int; min_int; 0; max_int; min_int ];

  let ascending =
    List.fold_left
      (fun set value -> Iarray_set.insert value set)
      Iarray_set.empty
      [ min_int; -1; 0; 1; max_int ]
  in
  let descending =
    List.fold_left
      (fun set value -> Iarray_set.insert value set)
      Iarray_set.empty
      [ max_int; 1; 0; -1; min_int ]
  in
  expect (Iarray_set.equal ascending descending) "extensional equality";
  expect
    (not (Iarray_set.equal ascending (Iarray_set.insert 2 descending)))
    "non-extensional equality";

  let evens = ref Iarray_set.empty in
  for value = 0 to 999 do
    evens := Iarray_set.insert (2 * value) !evens
  done;
  for value = 0 to 1999 do
    expect
      (Iarray_set.member value !evens = (value mod 2 = 0))
      "binary-search boundary mismatch"
  done;
  expect (not (Iarray_set.member (-1) !evens)) "lower miss";
  expect (not (Iarray_set.member 2000 !evens)) "upper miss";
  print_endline "sorted iarray set runtime: ok"
