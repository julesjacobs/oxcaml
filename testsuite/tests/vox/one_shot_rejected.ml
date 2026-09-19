(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml verified_atomic.mli verified_atomic.ml unique_cell.mli unique_cell.ml one_shot.mli one_shot.ml";
 readonly_files = "one_shot_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt;
   run-expectnat;
   check-program-output;
 }
*)
module C = One_shot;;
[%%expect{|
module C = One_shot
|}]

let twice_send () =
  let tx, _ = C.create () in
  C.send tx 1;
  C.send tx 2;;
[%%expect{|
Line 4, characters 9-11:
4 |   C.send tx 2;;
             ^^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 9-11:
3 |   C.send tx 1;
             ^^

|}]

let twice_recv (rx : int C.recv @ unique) =
  let x = C.recv rx in
  x + C.recv rx;;
[%%expect{|
Line 3, characters 13-15:
3 |   x + C.recv rx;;
                 ^^
Error: This value is used here, but it has already been used as unique at:
Line 2, characters 17-19:
2 |   let x = C.recv rx in
                     ^^

|}]

let wrong_end (rx : int C.recv @ unique) = C.send rx 1;;
[%%expect{|
Line 1, characters 50-52:
1 | let wrong_end (rx : int C.recv @ unique) = C.send rx 1;;
                                                      ^^
Error: The value "rx" has type "int C.recv" = "int One_shot.recv"
       but an expression was expected of type "'a C.send" = "'a One_shot.send"
|}]

let bad_value () =
  let (tx, _ : {n : int | n >= 0} C.send * {n : int | n >= 0} C.recv) =
    C.create () in
  C.send tx (-1);;
[%%expect{|
Line 4, characters 12-16:
4 |   C.send tx (-1);;
                ^^^^
Error: Refinement could not be proved (counterexample)
|}]

let forged_result (rx : int C.recv @ unique) : {n : int | n = 42} =
  C.recv rx;;
[%%expect{|
Line 2, characters 2-11:
2 |   C.recv rx;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let reuse_transferred () =
  let tx, _ = C.create () in
  let (reply, _ : int C.send * int C.recv) = C.create () in
  C.send tx (reply : int C.send);
  C.send reply 42;;
[%%expect{|
Line 5, characters 9-14:
5 |   C.send reply 42;;
             ^^^^^
Error: This value is used here, but it has already been used as unique at:
Line 4, characters 13-18:
4 |   C.send tx (reply : int C.send);
                 ^^^^^

|}]

let unsafe_payload (x : int ref) =
  let tx, _ = C.create () in C.send tx x;;
[%%expect{|
Line 2, characters 39-40:
2 |   let tx, _ = C.create () in C.send tx x;;
                                           ^
Error: The value "x" has type "int ref" but an expression was expected of type
         "('a : value mod portable contended)"
       The kind of int ref is
           mutable_data with int @@ forkable unyielding many.
       But the kind of int ref must be a subkind of
           value mod portable contended.

       The first mode-crosses less than the second along:
         contention: mod uncontended ≰ mod contended
         portability: mod portable with int ≰ mod portable
|}]

let expose (x : int C.recv) = x.C.cell;;
[%%expect{|
Line 1, characters 32-38:
1 | let expose (x : int C.recv) = x.C.cell;;
                                    ^^^^^^
Error: Unbound record field "C.cell"
|}]

let empty_take () =
  let r = Unique_cell.Slot.empty () (Ghost_pref.empty ()) in
  Unique_cell.Slot.take r.value r.state;;
[%%expect{|
Line 3, characters 32-39:
3 |   Unique_cell.Slot.take r.value r.state;;
                                    ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let double_put () =
  let r = Unique_cell.Slot.empty () (Ghost_pref.empty ()) in
  let t = Unique_cell.Slot.put r.value 1 r.state in
  Unique_cell.Slot.put r.value 2 t;;
[%%expect{|
Line 4, characters 33-34:
4 |   Unique_cell.Slot.put r.value 2 t;;
                                     ^
Error: Refinement could not be proved (counterexample)
|}]
