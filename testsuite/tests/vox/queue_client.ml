(* TEST
 has-z3;
 flags = "-extension refinement_types";
 all_modules = "functional_queue.mli functional_queue.ml queue_client.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

open Functional_queue

let (fifo @ total) :
    (first : ('a : immutable_data)) @ immutable ->
    (second : 'a) @ immutable ->
    {r : 'a * 'a * 'a t |
      match r with a, b, q ->
        a === first && b === second && contents q === []} @ immutable total =
  fun first second ->
  let refine_ q0 = empty in
  let refine_ q1 = enqueue q0 first in
  let nil = [] in
  let one = [first] in
  let two = [second] in
  let refine_ equation = ghost_ (append_def nil one) in
  let refine_ q2 = enqueue q1 second in
  let refine_ equation = ghost_ (append_def one two) in
  let refine_ equation = ghost_ (append_def nil two) in
  let nonempty : {q : 'a t | (contents q === []) === false} = refine_ q2 in
  let refine_ first_result = dequeue nonempty in
  let (a : 'a), q3 = first_result in
  let nonempty : {q : 'a t | (contents q === []) === false} = refine_ q3 in
  let refine_ second_result = dequeue nonempty in
  let (b : 'a), q4 = second_result in
  let result = a, b, q4 in
  refine_ result

type item = {key : int; weight : int}
type numbers : immutable_data = int list

let () =
  let first = 10 in
  let second = 20 in
  let refine_ result = fifo first second in
  let (a : int), (b : int), q = result in
  Format.printf "FIFO: %d %d; empty=%b@." a b
    (match contents q with [] -> true | _ -> false);
  let first = {key = 1; weight = 10} in
  let second = {key = 2; weight = 20} in
  let refine_ result = fifo first second in
  let (a : item), (b : item), _ = result in
  let proof : {u : unit | a.key = 1 && b.key = 2} =
    let u = () in refine_ u
  in
  let refine_ proof = proof in
  Format.printf "records: %d %d@." a.weight b.weight;
  let first : numbers = [1; 2] in
  let second : numbers = [3] in
  let refine_ result = fifo first second in
  let (a : int list), (b : int list), _ = result in
  let proof : {u : unit | a === first && b === second} =
    let u = () in refine_ u
  in
  let refine_ proof = proof in
  Format.printf "lists: %b@."
    (match a, b with [1; 2], [3] -> true | _ -> false)
