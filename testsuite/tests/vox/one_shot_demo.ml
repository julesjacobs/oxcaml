(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml verified_atomic.mli verified_atomic.ml unique_cell.mli unique_cell.ml one_shot.mli one_shot.ml one_shot_demo.ml";
 { bytecode; }
 { native; }
*)

module C = One_shot

let () =
  let tx, rx = C.create () in
  C.send tx 42;
  assert (C.recv rx = 42)

type natural = {n : int | n >= 0}

let () =
  let (tx, rx : natural C.send * natural C.recv) = C.create () in
  C.send tx 42;
  let n : natural = C.recv rx in
  assert (n = 42)

let echo (rx : (int * int C.send) C.recv @ unique) =
  let n, reply = C.recv rx in
  C.send reply n

let () =
  let tx, rx = C.create () in
  let (reply, answer : int C.send * int C.recv) = C.create () in
  C.send tx ((42, reply) : int * int C.send);
  echo rx;
  assert (C.recv answer = 42)

let produce (tx : (int * int C.recv) C.send @ unique) =
  let (next_tx, next_rx : int C.send * int C.recv) = C.create () in
  C.send tx ((20, next_rx) : int * int C.recv);
  C.send next_tx 22

let () =
  let tx, rx = C.create () in
  produce tx;
  let first, rest = C.recv rx in
  assert (first + C.recv rest = 42)

let refined (bound : int) =
  let (tx, rx : {n : int | n >= bound} C.send *
      {n : int | n >= bound} C.recv) = C.create () in
  C.send tx bound;
  let n : {n : int | n >= bound} = C.recv rx in
  n

let () = assert (refined 42 = 42)

type choice =
  | Add of int * int * int C.send
  | Stop of unit C.send

let select (rx : choice C.recv @ unique) =
  match C.recv rx with
  | Add (x, y, reply) -> C.send reply (x + y)
  | Stop reply -> C.send reply ()

let () =
  let tx, rx = C.create () in
  let (reply, answer : int C.send * int C.recv) = C.create () in
  C.send tx (Add (20, 22, reply));
  select rx;
  assert (C.recv answer = 42);
  let tx, rx = C.create () in
  let (reply, answer : unit C.send * unit C.recv) = C.create () in
  C.send tx (Stop reply);
  select rx;
  C.recv answer
