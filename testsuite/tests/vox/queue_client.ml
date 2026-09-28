(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml functional_queue.mli functional_queue.ml";
 { bytecode; }
 { native; }
*)

open Vox_sequence
open Functional_queue

let (fifo @ total) :
    (first : ('a : immutable_data)) @ immutable ->
    (second : 'a) @ immutable ->
    {r : 'a * 'a * 'a t |
      match r with a, b, q ->
        a === first && b === second && contents q === []} @ immutable total =
  fun first second ->
  let q0 = empty in
  let q1 = enqueue q0 first in
  ghost_ (append_def [] [first]);
  let q2 = enqueue q1 second in
  ghost_ (append_def [first] [second]);
  ghost_ (append_def [] [second]);
  let a, q3 = dequeue q2 in
  let b, q4 = dequeue q3 in
  a, b, q4

type item = {key : int; weight : int}
type numbers : immutable_data = int list

let () =
  let a, b, q = fifo 10 20 in
  Format.printf "FIFO: %d %d; empty=%b@." a b
    (match contents q with [] -> true | _ -> false);
  let a, b, _ = fifo {key = 1; weight = 10} {key = 2; weight = 20} in
  (() : {u : unit | a.key = 1 && b.key = 2});
  Format.printf "records: %d %d@." a.weight b.weight;
  let first : numbers = [1; 2] in
  let second : numbers = [3] in
  let a, b, _ = fifo first second in
  (() : {u : unit | a === first && b === second});
  Format.printf "lists: %b@."
    (match a, b with [1; 2], [3] -> true | _ -> false)
