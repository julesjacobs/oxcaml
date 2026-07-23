(* TEST
 readonly_files = "vslice_model.mli vslice_model.ml vslice.mli \
                   vslice_runtime_impl.ml fork_join.mli fork_join.ml \
                   quicksort.mli quicksort.ml \
                   runtime_quicksort_test.reference";
 flags = "-extension-universe alpha -vox-backend z3";
 {
   setup-ocamlc.byte-build-env;
   script = "cp vslice_runtime_impl.ml vslice.ml";
   script;
   all_modules = "vslice_model.mli vslice_model.ml vslice.mli vslice.ml \
                  fork_join.mli fork_join.ml quicksort.mli quicksort.ml \
                  runtime_quicksort_test.ml";
   ocamlc.byte;
   output = "${test_build_directory}/program-output";
   stdout = "${output}";
   run;
   check-program-output;
   script = "env OCAMLRUNPARAM=s=1k ${program}";
   script;
   check-program-output;
   script = "env OCAMLRUNPARAM=s=512,o=10 ${program}";
   script;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   script = "cp vslice_runtime_impl.ml vslice.ml";
   script;
   all_modules = "vslice_model.mli vslice_model.ml vslice.mli vslice.ml \
                  fork_join.mli fork_join.ml quicksort.mli quicksort.ml \
                  runtime_quicksort_test.ml";
   ocamlopt.byte;
   output = "${test_build_directory}/program-output";
   stdout = "${output}";
   run;
   check-program-output;
   script = "env OCAMLRUNPARAM=s=1k ${program}";
   script;
   check-program-output;
   script = "env OCAMLRUNPARAM=s=512,o=10 ${program}";
   script;
   check-program-output;
 }
*)

let set_preserving_length
    (loan : Vslice.slice @ local unique)
    (index : int{
       0 <= _ && _ < Vslice_model.len (Vslice.current loan)
     })
    value
    : Vslice.slice{
        Vslice_model.len (Vslice.current _)
        = Vslice_model.len (Vslice.current loan)
        && Vslice.final _ = Vslice.final loan
      } @ local unique =
  exclave_
    (let snapshot, loan = Vslice.snapshot ~loan in
     let values = Vslice.take_snapshot_values ~snapshot in
     let loan = Vslice.slice_set ~loan ~index ~value in
     Vslice_model.len_update_law ~values ~index ~value;
     loan)

let array_of_eight a b c d e f g h =
  let array = Vslice.make ~n:8 ~value:a in
  let prophecy = Vslice.new_prophecy () in
  let array, () =
    Vslice.borrow ~prophecy ~array (fun ~loan ->
      let loan = set_preserving_length loan 1 b in
      let loan = set_preserving_length loan 2 c in
      let loan = set_preserving_length loan 3 d in
      let loan = set_preserving_length loan 4 e in
      let loan = set_preserving_length loan 5 f in
      let loan = set_preserving_length loan 6 g in
      let loan = set_preserving_length loan 7 h in
      let _ = Vslice.current loan in
      ())
  in
  array

let check parallel array values =
  let sorted =
    if parallel
    then Quicksort.parallel_sort_array ~array
    else Quicksort.sort_array ~array
  in
  let actual = Vslice.contents sorted in
  let expected = List.sort Int.compare values in
  assert (actual = expected)

let check_cases parallel =
  let empty = Vslice.make ~n:0 ~value:0 in
  check parallel empty [];
  let singleton = Vslice.make ~n:1 ~value:1 in
  check parallel singleton [1];
  let equal = array_of_eight 5 5 5 5 5 5 5 5 in
  check parallel
    equal
    [5; 5; 5; 5; 5; 5; 5; 5];
  let shuffled = array_of_eight 8 3 7 4 2 6 5 1 in
  check parallel
    shuffled
    [8; 3; 7; 4; 2; 6; 5; 1];
  let duplicates = array_of_eight 0 (-3) 7 (-3) 2 2 1 min_int in
  check parallel
    duplicates
    [0; -3; 7; -3; 2; 2; 1; min_int];
  let extremes =
    array_of_eight max_int 0 (-1) 1 min_int 42 42 (-7)
  in
  check parallel
    extremes
    [max_int; 0; -1; 1; min_int; 42; 42; -7]

exception First
exception Second

let check_exception_join fork_join =
  let first_completed = Atomic.make false in
  let saw_second =
    match
      fork_join
        (fun () -> Atomic.set first_completed true)
        (fun () -> raise Second)
    with
    | _ -> false
    | exception Second -> true
  in
  assert saw_second;
  assert (Atomic.get first_completed);
  let second_completed = Atomic.make false in
  let saw_first =
    match
      fork_join
        (fun () -> raise First)
        (fun () -> Atomic.set second_completed true)
    with
    | _ -> false
    | exception First -> true
  in
  assert saw_first;
  assert (Atomic.get second_completed);
  let first_wins =
    match
      fork_join
        (fun () -> raise First)
        (fun () -> raise Second)
    with
    | _ -> false
    | exception First -> true
  in
  assert first_wins

let () =
  check_cases false;
  Fork_join.reset_counters_for_test ();
  check_cases true;
  if Fork_join.multidomain_capable_for_test ()
  then begin
    assert (0 < Fork_join.spawned_children_for_test ());
    assert (0 < Fork_join.transferred_tasks_for_test ())
  end
  else begin
    assert (Fork_join.spawned_children_for_test () = 0);
    assert (Fork_join.transferred_tasks_for_test () = 0)
  end;
  check_exception_join Fork_join.fork_join2;
  check_exception_join Fork_join.fork_join2_sequential_for_test;
  print_endline "quicksort-runtime-ok"
