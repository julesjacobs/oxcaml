(* TEST
 readonly_files = "vslice_model.mli vslice_model.ml vslice.mli \
                   vslice_runtime_impl.ml fork_join.mli fork_join.ml \
                   runtime_test.reference";
 flags = "-vox-backend z3";
 {
   setup-ocamlc.byte-build-env;
   script = "cp vslice_runtime_impl.ml vslice.ml";
   script;
   all_modules = "vslice_model.mli vslice_model.ml vslice.mli vslice.ml \
                  fork_join.mli fork_join.ml runtime_test.ml";
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
                  fork_join.mli fork_join.ml runtime_test.ml";
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

let check_array array expected =
  let length, array = Vslice.length ~array in
  assert (length = Array.length expected);
  let projected = Vslice.contents array in
  assert (projected = Array.to_list expected)

let test_borrow_and_mutation () =
  let array = Vslice.make ~n:4 ~value:1 in
  let first, array = Vslice.get ~array ~index:0 in
  assert (first = 1);
  let prophecy = Vslice.new_prophecy () in
  let array, () =
    Vslice.borrow ~prophecy ~array (fun ~loan ->
      let length, loan = Vslice.slice_length ~loan in
      assert (length = 4);
      let before_snapshot, loan = Vslice.snapshot ~loan in
      let before =
        Vslice.take_snapshot_values ~snapshot:before_snapshot
      in
      let value, loan = Vslice.slice_get ~loan ~index:1 in
      assert (value = 1);
      let loan =
        Vslice.slice_set ~loan ~index:1 ~value:7
      in
      assert (before = [1; 1; 1; 1]);
      let after_snapshot, loan = Vslice.snapshot ~loan in
      let after = Vslice.snapshot_values after_snapshot in
      assert (after = [1; 7; 1; 1]);
      assert (Vslice.current loan = [1; 7; 1; 1]))
  in
  ignore (check_array array [| 1; 7; 1; 1 |])

let test_split_and_disjoint_visibility () =
  let array = Vslice.make ~n:6 ~value:0 in
  let root_prophecy = Vslice.new_prophecy () in
  let array, () =
    Vslice.borrow ~prophecy:root_prophecy ~array (fun ~loan ->
      let root_snapshot, loan = Vslice.snapshot ~loan in
      let original =
        Vslice.take_snapshot_values ~snapshot:root_snapshot
      in
      let first_prophecy = Vslice.new_prophecy () in
      let middle_prophecy = Vslice.new_prophecy () in
      let last_prophecy = Vslice.new_prophecy () in
      let loan, () =
        Vslice.split3
          ~first_prophecy ~middle_prophecy ~last_prophecy
          ~loan ~first:2 ~last:4
          (fun ~first_loan ~middle_loan ~last_loan ->
            let (), () =
              Fork_join.fork_join2
                (fun () ->
                  Vslice_model.len_take_law
                    ~count:2 ~values:original ~bounded:();
                  let first_loan =
                    Vslice.slice_set ~loan:first_loan ~index:0 ~value:11
                  in
                  Vslice_model.len_update_law
                    ~values:(Vslice_model.take 2 original)
                    ~index:0 ~value:11;
                  let first_loan =
                    Vslice.slice_set ~loan:first_loan ~index:1 ~value:12
                  in
                  let snapshot, first_loan =
                    Vslice.snapshot ~loan:first_loan
                  in
                  let values =
                    Vslice.take_snapshot_values ~snapshot
                  in
                  assert (values = [11; 12]);
                  let () = Vslice.close ~loan:first_loan in
                  ())
                (fun () ->
                  Vslice_model.len_drop_law
                    ~count:4 ~values:original ~bounded:();
                  let last_loan =
                    Vslice.slice_set ~loan:last_loan ~index:0 ~value:31
                  in
                  Vslice_model.len_update_law
                    ~values:(Vslice_model.drop 4 original)
                    ~index:0 ~value:31;
                  let last_loan =
                    Vslice.slice_set ~loan:last_loan ~index:1 ~value:32
                  in
                  let snapshot, last_loan =
                    Vslice.snapshot ~loan:last_loan
                  in
                  let values =
                    Vslice.take_snapshot_values ~snapshot
                  in
                  assert (values = [31; 32]);
                  let () = Vslice.close ~loan:last_loan in
                  ())
            in
            Vslice_model.len_segment_law
              ~values:original ~first:2 ~last:4;
            let middle_loan =
              Vslice.slice_set ~loan:middle_loan ~index:0 ~value:21
            in
            Vslice_model.len_update_law
              ~values:(Vslice_model.segment 2 4 original)
              ~index:0 ~value:21;
            let middle_loan =
              Vslice.slice_set ~loan:middle_loan ~index:1 ~value:22
            in
            let snapshot, middle_loan =
              Vslice.snapshot ~loan:middle_loan
            in
            let values = Vslice.take_snapshot_values ~snapshot in
            assert (values = [21; 22]);
            let () = Vslice.close ~loan:middle_loan in
            ())
      in
      let () = Vslice.close ~loan in
      ())
  in
  ignore (check_array array [| 11; 12; 21; 22; 31; 32 |])

let test_split_boundaries () =
  let run (first : int{ 0 <= _ })
      (last : int{ first <= _ && _ <= 3 }) expected_lengths =
    let array = Vslice.make ~n:3 ~value:5 in
    let root_prophecy = Vslice.new_prophecy () in
    let array, () =
      Vslice.borrow ~prophecy:root_prophecy ~array (fun ~loan ->
        let first_prophecy = Vslice.new_prophecy () in
        let middle_prophecy = Vslice.new_prophecy () in
        let last_prophecy = Vslice.new_prophecy () in
        let loan, () =
          Vslice.split3
            ~first_prophecy ~middle_prophecy ~last_prophecy
            ~loan ~first ~last
            (fun ~first_loan ~middle_loan ~last_loan ->
              let first_length, first_loan =
                Vslice.slice_length ~loan:first_loan
              in
              let middle_length, middle_loan =
                Vslice.slice_length ~loan:middle_loan
              in
              let last_length, last_loan =
                Vslice.slice_length ~loan:last_loan
              in
              assert
                ((first_length, middle_length, last_length)
                 = expected_lengths);
              let () = Vslice.close ~loan:first_loan in
              let () = Vslice.close ~loan:middle_loan in
              let () = Vslice.close ~loan:last_loan in
              ())
        in
        let () = Vslice.close ~loan in
        ())
    in
    ignore (check_array array [| 5; 5; 5 |])
  in
  run 0 0 (0, 0, 3);
  run 0 3 (0, 3, 0);
  run 3 3 (3, 0, 0)

let test_empty_snapshot () =
  let array = Vslice.make ~n:0 ~value:9 in
  let prophecy = Vslice.new_prophecy () in
  let array, () =
    Vslice.borrow ~prophecy ~array (fun ~loan ->
      let snapshot, loan = Vslice.snapshot ~loan in
      let values =
        Vslice.take_snapshot_values ~snapshot
      in
      assert (values = []);
      let () = Vslice.close ~loan in
      ())
  in
  ignore (check_array array [||])

exception First
exception Second

let test_join_on_exception_with fork_join =
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

let test_join_on_exception () =
  test_join_on_exception_with Fork_join.fork_join2;
  test_join_on_exception_with
    Fork_join.fork_join2_sequential_for_test

let test_borrow_exception () =
  let borrow_raised =
    let array = Vslice.make ~n:1 ~value:0 in
    let prophecy = Vslice.new_prophecy () in
    match
      Vslice.borrow ~prophecy ~array
        (fun ~loan:_ -> raise Exit)
    with
    | _ -> false
    | exception Exit -> true
  in
  assert borrow_raised

let test_split_exception () =
  let split_raised =
    let array = Vslice.make ~n:1 ~value:0 in
    let root_prophecy = Vslice.new_prophecy () in
    match
      Vslice.borrow ~prophecy:root_prophecy ~array
        (fun ~loan ->
          let first_prophecy = Vslice.new_prophecy () in
          let middle_prophecy = Vslice.new_prophecy () in
          let last_prophecy = Vslice.new_prophecy () in
          let loan, () =
            Vslice.split3
              ~first_prophecy ~middle_prophecy ~last_prophecy
              ~loan ~first:0 ~last:1
              (fun ~first_loan:_ ~middle_loan:_ ~last_loan:_ ->
                raise Exit)
          in
          let () = Vslice.close ~loan in
          ())
    with
    | _ -> false
    | exception Exit -> true
  in
  assert split_raised

let stress_value case =
  match case mod 8 with
  | 0 -> min_int
  | 1 -> max_int
  | 2 | 3 -> 0
  | 4 -> -1
  | 5 -> 1
  | _ -> case - 150

let test_repeated_projection_case case =
  let initial = stress_value case in
  let replacement = stress_value (case + 3) in
  let array = Vslice.make ~n:4 ~value:initial in
  let first, array = Vslice.get ~array ~index:0 in
  assert (first = initial);
  let prophecy = Vslice.new_prophecy () in
  let array, () =
    Vslice.borrow ~prophecy ~array (fun ~loan ->
      let length, loan = Vslice.slice_length ~loan in
      assert (length = 4);
      let before_snapshot, loan = Vslice.snapshot ~loan in
      let before =
        Vslice.take_snapshot_values ~snapshot:before_snapshot
      in
      assert (before = [initial; initial; initial; initial]);
      let loan =
        Vslice.slice_set ~loan ~index:1 ~value:replacement
      in
      let after_snapshot, loan = Vslice.snapshot ~loan in
      let after = Vslice.snapshot_values after_snapshot in
      let expected = [initial; replacement; initial; initial] in
      assert (after = expected);
      assert (Vslice.current loan = expected))
  in
  assert
    (Vslice.contents array
     = [initial; replacement; initial; initial])

let test_repeated_split_case case =
  let value = stress_value case in
  let array = Vslice.make ~n:6 ~value in
  let root_prophecy = Vslice.new_prophecy () in
  let array, () =
    Vslice.borrow ~prophecy:root_prophecy ~array (fun ~loan ->
      let first_prophecy = Vslice.new_prophecy () in
      let middle_prophecy = Vslice.new_prophecy () in
      let last_prophecy = Vslice.new_prophecy () in
      let loan, () =
        Vslice.split3
          ~first_prophecy ~middle_prophecy ~last_prophecy
          ~loan ~first:2 ~last:4
          (fun ~first_loan ~middle_loan ~last_loan ->
            let check loan =
              let snapshot, loan = Vslice.snapshot ~loan in
              let values =
                Vslice.take_snapshot_values ~snapshot
              in
              assert (values = [value; value]);
              let () = Vslice.close ~loan in
              ()
            in
            let (), () =
              Fork_join.fork_join2
                (fun () -> check first_loan)
                (fun () -> check last_loan)
            in
            check middle_loan)
      in
      let snapshot, loan = Vslice.snapshot ~loan in
      let values = Vslice.take_snapshot_values ~snapshot in
      assert
        (values = [value; value; value; value; value; value]);
      let () = Vslice.close ~loan in
      ())
  in
  assert
    (Vslice.contents array
     = [value; value; value; value; value; value])

let test_repeated_cases () =
  for case = 0 to 299 do
    test_repeated_projection_case case;
    test_repeated_split_case case;
    test_join_on_exception ()
  done

let test_child_domain_and_cap () =
  Fork_join.reset_counters_for_test ();
  let caller = Domain.self () in
  let child_was_distinct, sibling_stayed_local =
    Fork_join.fork_join2
      (fun () -> Domain.self () <> caller)
      (fun () -> Domain.self () = caller)
  in
  let limit = Fork_join.child_limit_for_test () in
  if 0 < limit
  then begin
    assert child_was_distinct;
    assert (Fork_join.spawned_children_for_test () = 1)
  end
  else assert (Fork_join.spawned_children_for_test () = 0);
  assert sibling_stayed_local;
  assert (Fork_join.peak_reserved_children_for_test () <= limit)

let test_forced_tokens_without_multidomain () =
  Fork_join.reset_counters_for_test ();
  if not (Fork_join.multidomain_capable_for_test ())
  then begin
    Fork_join.force_available_children_for_test 3;
    let first, second =
      Fork_join.fork_join2 (fun () -> 17) (fun () -> 29)
    in
    assert (first = 17 && second = 29);
    assert (Fork_join.transferred_tasks_for_test () = 0);
    assert (Fork_join.spawned_children_for_test () = 0);
    assert (Fork_join.peak_reserved_children_for_test () = 0);
    Fork_join.reset_counters_for_test ()
  end

let swap array left right =
  let temporary = array.(left) in
  array.(left) <- array.(right);
  array.(right) <- temporary

let partition array first last =
  let pivot = array.(last) in
  let lower = ref first in
  for scan = first to last - 1 do
    if array.(scan) <= pivot
    then begin
      swap array !lower scan;
      incr lower
    end
  done;
  swap array !lower last;
  !lower

let rec parallel_quicksort depth array first last =
  if first < last
  then begin
    let pivot = partition array first last in
    if 0 < depth
    then begin
      let (), () =
        Fork_join.fork_join2
          (fun () ->
            parallel_quicksort (depth - 1) array first (pivot - 1))
          (fun () ->
            parallel_quicksort (depth - 1) array (pivot + 1) last)
      in
      ()
    end
    else begin
      parallel_quicksort 0 array first (pivot - 1);
      parallel_quicksort 0 array (pivot + 1) last
    end
  end

let test_nested_parallel_quicksort () =
  Fork_join.reset_counters_for_test ();
  for case = 0 to 63 do
    let values =
      Array.init 64 (fun index -> stress_value (case * 17 + index))
    in
    let expected = Array.to_list values |> List.sort Int.compare in
    parallel_quicksort 5 values 0 (Array.length values - 1);
    assert (Array.to_list values = expected)
  done;
  assert
    (Fork_join.peak_reserved_children_for_test ()
     <= Fork_join.child_limit_for_test ());
  if 0 < Fork_join.child_limit_for_test ()
  then assert (0 < Fork_join.spawned_children_for_test ())

let () =
  test_borrow_and_mutation ();
  test_split_and_disjoint_visibility ();
  test_split_boundaries ();
  test_empty_snapshot ();
  test_join_on_exception ();
  test_borrow_exception ();
  test_split_exception ();
  test_repeated_cases ();
  test_child_domain_and_cap ();
  test_forced_tokens_without_multidomain ();
  test_nested_parallel_quicksort ();
  print_endline "runtime-ok"
