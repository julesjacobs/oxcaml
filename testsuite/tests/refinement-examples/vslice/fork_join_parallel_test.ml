(* TEST
 modules = "fork_join.mli fork_join.ml";
 multicore;
 { bytecode; }
 { native; }
*)

exception First
exception Second

let stress_value case =
  match case mod 8 with
  | 0 -> min_int
  | 1 -> max_int
  | 2 | 3 -> 0
  | 4 -> -1
  | 5 -> 1
  | _ -> case - 150

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

let check_exception_join () =
  let first_completed = Atomic.make false in
  let second_won =
    match
      Fork_join.fork_join2
        (fun () -> Atomic.set first_completed true)
        (fun () -> raise Second)
    with
    | _ -> false
    | exception Second -> true
  in
  assert second_won;
  assert (Atomic.get first_completed);
  let second_completed = Atomic.make false in
  let first_won =
    match
      Fork_join.fork_join2
        (fun () -> raise First)
        (fun () -> Atomic.set second_completed true)
    with
    | _ -> false
    | exception First -> true
  in
  assert first_won;
  assert (Atomic.get second_completed);
  let both_raise_with_first_priority =
    match
      Fork_join.fork_join2
        (fun () -> raise First)
        (fun () -> raise Second)
    with
    | _ -> false
    | exception First -> true
  in
  assert both_raise_with_first_priority

let () =
  Fork_join.reset_counters_for_test ();
  let caller = Domain.self () in
  let child_was_distinct, sibling_stayed_local =
    Fork_join.fork_join2
      (fun () -> Domain.self () <> caller)
      (fun () -> Domain.self () = caller)
  in
  if Fork_join.multidomain_capable_for_test ()
  then assert child_was_distinct
  else assert (not child_was_distinct);
  assert sibling_stayed_local;
  for case = 0 to 299 do
    let values =
      Array.init 96 (fun index -> stress_value (case * 17 + index))
    in
    let expected = Array.to_list values |> List.sort Int.compare in
    parallel_quicksort 6 values 0 (Array.length values - 1);
    assert (Array.to_list values = expected);
    check_exception_join ()
  done;
  if Fork_join.multidomain_capable_for_test ()
  then begin
    assert (0 < Fork_join.spawned_children_for_test ());
    assert (0 < Fork_join.transferred_tasks_for_test ())
  end
  else begin
    assert (Fork_join.spawned_children_for_test () = 0);
    assert (Fork_join.transferred_tasks_for_test () = 0)
  end;
  assert
    (Fork_join.peak_reserved_children_for_test ()
     <= Fork_join.child_limit_for_test ());
  print_endline "parallel-runtime-ok"
