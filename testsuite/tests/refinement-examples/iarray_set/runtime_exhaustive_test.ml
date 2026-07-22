let require condition =
  if not condition then failwith "sorted-iarray differential mismatch"

let values = [ min_int; -2; -1; 0; 1; 2; max_int ]

let model_insert value model =
  List.sort_uniq Int.compare (value :: model)

external runtime_view : Iarray_set.t -> int list
  = "vox_sorted_iarray_view"

let check model set =
  require (runtime_view set = model);
  List.iter
    (fun query ->
      require (Iarray_set.member query set = List.mem query model))
    values

let rec visit depth model set =
  check model set;
  if depth > 0 then
    List.iter
      (fun value ->
        visit (depth - 1) (model_insert value model)
          (Iarray_set.insert value set))
      values

let () =
  visit 6 [] Iarray_set.empty;
  let dense = ref Iarray_set.empty in
  for value = -4096 to 4096 do
    dense := Iarray_set.insert value !dense
  done;
  for query = -4100 to 4100 do
    require
      (Iarray_set.member query !dense
       = (query >= -4096 && query <= 4096))
  done;
  require (not (Iarray_set.member min_int !dense));
  require (not (Iarray_set.member max_int !dense));
  print_endline "sorted-iarray exhaustive differential: ok"
