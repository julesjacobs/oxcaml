(* TEST
 has-z3;
 multicore;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml borrow.mli borrow.ml borrow_parallel.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

open Borrow

let[@def] (anything @ total) (values : int Model.t @ immutable) = ghost_ true
let[@def] (root_post @ total) (u : unit @ immutable) (values : int Model.t @ immutable) =
  ghost_ true
let[@def] (split_post @ total) (u : unit @ immutable)
    (left : int Model.t @ immutable) (right : int Model.t @ immutable) = ghost_ true

let write_first : (s : int Slice.t) @ local unique -> (value : int) ->
    {u : unit | anything (Slice.final s)} = fun s value ->
  let eventual = ghost_ (Slice.final (borrow_ s)) in
  let n = Slice.length (borrow_ s) in
  let state =
    if n > 0 then
      let zero = 0 in
      let index : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
        zero in
      let state = Slice.set s index value in
      state
    else s in
  Slice.finish state;
  ghost_ (anything_def eventual);
  ()

let run_pair spawn a left_body right_body =
  let root = ghost_ root_post in
  let split = ghost_ split_post in
  let left_post = ghost_ anything in
  let right_post = ghost_ anything in
  let result = Owned_array.with_mut a root (fun loan ->
    let s = loan in
    let eventual = ghost_ (Slice.final (borrow_ s)) in
    let n = Slice.length (borrow_ s) in
    let middle = n / 2 in
    let cut : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) <= 0} =
      middle in
    let result = Slice.split_at s cut split (fun l r ->
      let left = l in
      let right = r in
      let left_end = ghost_ (Slice.final (borrow_ left)) in
      let right_end = ghost_ (Slice.final (borrow_ right)) in
      let completed = Slice.parallel spawn left right left_post right_post
        (fun loan ->
          let s = loan in
          left_body ();
          let u = write_first s (Domain.self () :> int) in
          u)
        (fun loan ->
          let s = loan in
          right_body ();
          let u = write_first s (Domain.self () :> int) in
          u) in
      let u = () in
      ghost_ (split_post_def u left_end right_end);
      u) in
    let {state; _} = result in
    Slice.finish state;
    let u = () in
    ghost_ (root_post_def u eventual);
    u) in
  let {state; _} = result in
  let values = Owned_array.into_iarray state in
  values

let check () =
  let values = [: -1; -1; -1; -1 :] in
  let a = Owned_array.of_iarray values in
  let result = run_pair true a (fun () -> ()) (fun () -> ()) in
  assert (Iarray.get result 0 <> Iarray.get result 2);
  assert (Iarray.get result 2 = (Domain.self () :> int));
  let a = Owned_array.of_iarray values in
  let result = run_pair false a (fun () -> ()) (fun () -> ()) in
  assert (Iarray.get result 0 = Iarray.get result 2);
  let right_failed = Atomic.make false in
  let left_finished = Atomic.make false in
  let a = Owned_array.of_iarray values in
  (try
     ignore (run_pair true a
       (fun () ->
         while not (Atomic.get right_failed) do Domain.cpu_relax () done;
         Atomic.set left_finished true)
       (fun () -> Atomic.set right_failed true; failwith "right callback"));
     assert false
   with Failure message -> assert (message = "right callback"));
  assert (Atomic.get left_finished);
  let expect_failure expected left_body right_body =
    let a = Owned_array.of_iarray values in
    let message =
      try ignore (run_pair true a left_body right_body); None
      with Failure message -> Some message in
    assert (message = Some expected) in
  expect_failure "left callback"
    (fun () -> failwith "left callback") (fun () -> ());
  expect_failure "right callback"
    (fun () -> failwith "left callback") (fun () -> failwith "right callback");
  print_endline "parallel slices: separate domains, sequential fallback, exception join"

let () = check ()
