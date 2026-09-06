(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "functional_queue.mli functional_queue.ml queue_client.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

let () =
  let open Functional_queue in
  let refine_ q0 = empty in
  let first = 10 in
  let second = 20 in
  let refine_ q1 = enqueue q0 first in
  let nil = [] in
  let one = [first] in
  let two = [second] in
  let refine_ equation = ghost_ (append_def nil one) in
  let refine_ q2 = enqueue q1 second in
  let refine_ equation = ghost_ (append_def one two) in
  let refine_ equation = ghost_ (append_def nil two) in
  let nonempty : {q : t | (contents q === []) === false} = refine_ q2 in
  let refine_ first_result = dequeue nonempty in
  let (a : int), q3 = first_result in
  let nonempty : {q : t | (contents q === []) === false} = refine_ q3 in
  let refine_ second_result = dequeue nonempty in
  let (b : int), q4 = second_result in
  let proof : {u : unit | a = 10 && b = 20 && contents q4 === []} =
    let u = () in
    refine_ u
  in
  let refine_ proof = proof in
  Format.printf "FIFO: %d %d; empty=%b@." a b (contents q4 = [])
