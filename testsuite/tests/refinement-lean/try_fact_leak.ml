(* TEST
 flags = "-vox-backend lean";
 readonly_files = "try_fact_leak_lib.ml try_fact_leak_lib.mli";
 setup-ocamlc.byte-build-env;
 module = "try_fact_leak_lib.mli";
 ocamlc.byte;
 module = "try_fact_leak_lib.ml";
 ocamlc.byte;
 flags += " -I ocamlc.byte ocamlc.byte/try_fact_leak_lib.cmo";
 expect;
*)

(* [%raise] never completes normally, so this trusted primitive contract and
   the wrapper's resulting false postcondition are sound. *)
external raise_false : exn -> int{ false } = "%raise"
let impossible () : int{ false } = raise_false Exit
[%%expect {|
external raise_false : exn -> int{ false } = "%raise"
val impossible : unit -> int{ false } = <fun>
|}]

let false_alone =
  (0 : int{ false })
[%%expect {|
Line 2, characters 2-20:
2 |   (0 : int{ false })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* A caught exception makes the continuation reachable.  The postcondition of
   [impossible] holds only when the call returns normally, so it must not escape
   the [try] and prove false here. *)
let try_leak =
  let () = try ignore (impossible ()) with _ -> () in
  (0 : int{ false })
[%%expect {|
Line 3, characters 2-20:
3 |   (0 : int{ false })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* The restoration must also survive the enclosing let when the postcondition
   came from a separately compiled interface. *)
let imported_try_leak =
  let () =
    try ignore (Try_fact_leak_lib.impossible ()) with _ -> ()
  in
  (0 : int{ false })
[%%expect {|
Line 5, characters 2-20:
5 |   (0 : int{ false })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

(* Without a handler, the continuation after [impossible] is reachable only if
   the call returns normally.  Its false postcondition therefore soundly proves
   any obligation in that dead continuation. *)
let dead_continuation () =
  let _ = Try_fact_leak_lib.impossible () in
  (0 : int{ false })
[%%expect {|
val dead_continuation : unit -> int{ false } = <fun>
|}]

(* An ordinary caught-exception continuation still verifies when its result is
   justified without a fact from the interrupted computation. *)
let caught_continuation =
  let () = try raise Exit with _ -> () in
  (0 : int{ _ = 0 })
[%%expect {|
val caught_continuation : int{ _ = 0 } = 0
|}]

(* For [try] inside [match], the inner [try] owns the handler-path restore. *)
let try_inside_match_reject () =
  match () with
  | () ->
    let () = try ignore (impossible ()) with _ -> () in
    (0 : int{ false })
[%%expect {|
Line 5, characters 4-22:
5 |     (0 : int{ false })
        ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let try_inside_match_accept () =
  match () with
  | () ->
    let () = try ignore (impossible ()) with _ -> () in
    (0 : int{ _ = 0 })
[%%expect {|
val try_inside_match_accept : unit -> int{ _ = 0 } = <fun>
|}]

(* For [match] inside [try], the outer [try] owns the handler-path restore. *)
let match_inside_try_reject () =
  let () =
    try
      match () with
      | () -> ignore (impossible ())
    with _ -> ()
  in
  (0 : int{ false })
[%%expect {|
Line 8, characters 2-20:
8 |   (0 : int{ false })
      ^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (disproved)
|}]

let match_inside_try_accept () =
  let () =
    try
      match () with
      | () -> ignore (impossible ())
    with _ -> ()
  in
  (0 : int{ _ = 0 })
[%%expect {|
val match_inside_try_accept : unit -> int{ _ = 0 } = <fun>
|}]
