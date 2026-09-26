(* TEST
 has-z3;
 multicore;
 flags = "-extension refinement_types -alert -do_not_spawn_domains";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml verified_atomic.mli verified_atomic.ml unique_cell.mli unique_cell.ml one_shot.mli one_shot.ml one_shot_parallel.ml";
 { bytecode; }
*)

module C = One_shot

let echo (rx : (int * int C.send) C.recv @ unique) =
  let value, reply = C.recv rx in
  C.send reply value

let () =
  for i = 1 to 200 do
    let tx, rx = C.create () in
    let worker = Domain.Safe.spawn (fun () -> echo rx) in
    let (reply, answer : int C.send * int C.recv) = C.create () in
    if i mod 10 = 0 then Gc.full_major ();
    C.send tx ((i, reply) : int * int C.send);
    assert (C.recv answer = i);
    Domain.join worker
  done

let () =
  let tx, rx = C.create () in
  let worker = Domain.Safe.spawn (fun () ->
    let (next_tx, next_rx : int C.send * int C.recv) = C.create () in
    C.send tx ((20, next_rx) : int * int C.recv);
    Gc.full_major ();
    C.send next_tx 22) in
  let first, rest = C.recv rx in
  assert (first + C.recv rest = 42);
  Domain.join worker

let refined (bound : int) =
  let (tx, rx : {n : int | n >= bound} C.send *
      {n : int | n >= bound} C.recv) = C.create () in
  let worker = Domain.Safe.spawn (fun () -> C.send tx bound) in
  let n : {n : int | n >= bound} = C.recv rx in
  Domain.join worker;
  n

let () = assert (refined 42 = 42)

let echo_at : (n : int) ->
    ({v : int | v = n} * {v : int | v = n} C.send) C.recv @ unique ->
    unit = fun _n rx ->
  let value, reply = C.recv rx in
  C.send reply value

let exact_reply : (n : int) -> {v : int | v = n} = fun n ->
  let tx, rx = C.create () in
  let worker = Domain.Safe.spawn (fun () -> echo_at n rx) in
  let (reply, answer : {v : int | v = n} C.send *
      {v : int | v = n} C.recv) = C.create () in
  C.send tx ((n, reply) : {v : int | v = n} * {v : int | v = n} C.send);
  let result : {v : int | v = n} = C.recv answer in
  Domain.join worker;
  result

let () = assert (exact_reply 42 = 42)

module P = Ghost_pref

type owned_cell = {
  cell : int P.t @@ aliased;
  permission : int P.token @@ ghost;
}
type input = {r : owned_cell |
  P.Heap.mem (P.own r.permission) r.cell &&
  P.Heap.at (P.own r.permission) r.cell === Some 41}
type output = {r : owned_cell |
  P.Heap.mem (P.own r.permission) r.cell &&
  P.Heap.at (P.own r.permission) r.cell === Some 42}

let increment (input : input @ unique) : output @ unique =
  let { cell; permission } = input in
  let value = P.read cell (borrow_ permission) in
  let permission = P.write cell (value + 1) permission in
  { cell; permission }

let () =
  let (tx, rx : (input * output C.send) C.send *
      (input * output C.send) C.recv) = C.create () in
  let worker = Domain.Safe.spawn (fun () ->
    let input, reply = C.recv rx in
    C.send reply (increment input)) in
  let (reply, answer : output C.send * output C.recv) = C.create () in
  let allocation = P.alloc 41 (P.empty ()) in
  let input : input = {
    cell = allocation.P.value; permission = allocation.P.state;
  } in
  C.send tx ((input, reply) : input * output C.send);
  let output = C.recv answer in
  let { cell; permission } = output in
  let value : {n : int | n = 42} = P.read cell (borrow_ permission) in
  assert (value = 42);
  Domain.join worker
