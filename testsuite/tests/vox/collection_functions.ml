(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml vox_traversal.mli vox_traversal.ml hof_candidates.ml collection_functions.ml hof_challenges.ml hof_array_clients.ml";
 { bytecode; }
 { native; }
*)

open Vox_sequence
open Vox_traversal
open Hof_candidates

let[@def] grows : int @ immutable total ->
    (int @ immutable -> bool) @ total = fun x y -> y >= x

let partial_clamp : (x : int) @ immutable ->
    {y : int | grows x y} @ immutable total = fun x ->
  if x = 13 then failwith "clamp"
  else
    let y = if x < 0 then 0 else x in
    ghost_ (grows_def x y);
    refine_ y

let clamp (xs : int list) :
    {ys : int list | map_rel grows xs ys && length xs === length ys} =
  let f = partial_clamp in
  let refine_ ys = map grows f xs in
  ghost_ (map_length grows xs ys);
  refine_ ys

let[@def] count_relation : int list @ immutable total ->
    (Bigint.t @ immutable -> bool) @ total =
    fun xs n -> n >= length xs && n <= length xs

let count_step : (x : int) @ immutable -> (xs : int list) @ immutable ghost ->
    (n : Bigint.t) @ immutable -> {u : unit | count_relation xs n} @ ghost ->
    {result : Bigint.t | count_relation (x :: xs) result}
      @ immutable total =
    fun x xs n premise ->
  premise;
  if x = 13 then failwith "count"
  else
    let result = Bigint.add n 1Z in
    ghost_ (
      let inputs = x :: xs in
      count_relation_def xs n;
      count_relation_def inputs result;
      length_def inputs;
      let u = () in
      (refine_ u : {u : unit | count_relation (x :: xs) result}));
    refine_ result

let count (xs : int list) : {n : Bigint.t | n === length xs} =
  let initial = 0Z in
  let u = () in
  ghost_ (
    let empty : int list = [] in
    count_relation_def empty initial;
    length_def empty;
    (refine_ u : {u : unit | count_relation [] initial}));
  let step = count_step in
  let refine_ n = fold_right_ih count_relation step xs initial (refine_ u) in
  ghost_ (count_relation_def xs n);
  refine_ n

let () =
  let inputs = [-2; 3; 0] in
  let refine_ ys = clamp inputs in
  assert (ys = [0; 3; 0]);
  let inputs = [1; 2; 3] in
  let refine_ n = count inputs in
  assert (Bigint.equal n 3Z);
  let inputs = [13] in
  (match clamp inputs with
   | exception Failure message -> assert (message = "clamp")
   | _ -> assert false);
  (match count inputs with
   | exception Failure message -> assert (message = "count")
   | _ -> assert false)


let[@def] same_length : int list @ immutable total ->
    (int list @ immutable -> bool) @ total =
    fun xs ys -> length xs >= length ys && length xs <= length ys

let ih_clamp_step : (x : int) @ immutable ->
    (xs : int list) @ immutable ghost ->
    (ys : int list) @ immutable ghost ->
    {u : unit | same_length xs ys} @ ghost ->
    {y : int | same_length (x :: xs) (y :: ys)} @ immutable total =
    fun x xs ys premise ->
  premise;
  let refine_ y = partial_clamp x in
  ghost_ (
    let inputs = x :: xs in
    let outputs = y :: ys in
    same_length_def xs ys;
    same_length_def inputs outputs;
    length_def inputs;
    length_def outputs;
    let u = () in
    (refine_ u : {u : unit | same_length (x :: xs) (y :: ys)}));
  refine_ y

let ih_clamp (xs : int list) :
    {ys : int list | length xs === length ys} =
  let u = () in
  ghost_ (
    let empty : int list = [] in
    same_length_def empty empty;
    (refine_ u : {u : unit | same_length [] []}));
  let step = ih_clamp_step in
  let refine_ ys = map_ih same_length step xs (refine_ u) in
  ghost_ (same_length_def xs ys);
  refine_ ys

let[@def] shifted : int @ immutable total ->
    (int @ immutable total -> (int @ immutable -> bool) @ total) @ total =
    fun delta x y -> y = x + delta

let shift (delta : int) (xs : int list) :
    {ys : int list | length xs === length ys} =
  let r = shifted delta in
  let f : (x : int) @ immutable ->
      {y : int | r x y} @ immutable total = fun x ->
    if x = 13 then failwith "shift"
    else
      let y = x + delta in
      ghost_ (shifted_def delta x y);
      refine_ y in
  let refine_ ys = map r f xs in
  ghost_ (map_length r xs ys);
  (refine_ ys : {ys : int list | length xs === length ys})

let () =
  let xs = [-2; 3; 0] in
  let refine_ ys = ih_clamp xs in
  assert (ys = [0; 3; 0]);
  let delta = 7 in
  let refine_ ys = shift delta xs in
  assert (ys = [5; 10; 7]);
  let xs = [13] in
  (match ih_clamp xs with
   | exception Failure message -> assert (message = "clamp")
   | _ -> assert false);
  (match shift delta xs with
   | exception Failure message -> assert (message = "shift")
   | _ -> assert false)


let[@def] increment_relation : int @ immutable total ->
    (Bigint.t @ immutable total ->
      (Bigint.t @ immutable -> bool) @ total) @ total =
    fun _x acc result -> Bigint.equal result (Bigint.add acc 1Z)

let increment : (x : int) @ immutable -> (acc : Bigint.t) @ immutable ->
    {result : Bigint.t | increment_relation x acc result} @ immutable total =
    fun x acc ->
  if x = 13 then failwith "increment"
  else
    let result = Bigint.add acc 1Z in
    ghost_ (increment_relation_def x acc result);
    refine_ result

let (count_preserved @ total) :
    (x : int) @ immutable -> (tail : int list) @ immutable ->
    (acc : Bigint.t) @ immutable -> (result : Bigint.t) @ immutable ->
    {u : unit | count_relation tail acc && increment_relation x acc result} ->
    {u : unit | count_relation (x :: tail) result} =
    fun x tail acc result premise ->
  premise;
  let inputs = x :: tail in
  count_relation_def tail acc;
  increment_relation_def x acc result;
  count_relation_def inputs result;
  length_def inputs;
  let u = () in refine_ u

let count_separate (xs : int list) : {n : Bigint.t | n === length xs} =
  let u = () in
  let initial = 0Z in
  ghost_ (
    let empty : int list = [] in
    count_relation_def empty initial;
    length_def empty;
    (refine_ u : {u : unit | count_relation [] initial}));
  let preserve = ghost_ count_preserved in
  let step = increment in
  let refine_ n =
    fold_right increment_relation count_relation step preserve xs initial
      (refine_ u) in
  ghost_ (count_relation_def xs n);
  refine_ n

let () =
  let xs = [1; 2; 3] in
  let refine_ n = count_separate xs in
  assert (Bigint.equal n 3Z);
  let xs = [13] in
  match count_separate xs with
  | exception Failure message -> assert (message = "increment")
  | _ -> assert false

let[@def] next_count : int @ immutable total ->
    (Bigint.t @ immutable -> Bigint.t @ immutable total) @ total =
    fun _x acc -> Bigint.add acc 1Z

let count_exact : (x : int) @ immutable -> (acc : Bigint.t) @ immutable ->
    {result : Bigint.t | result === next_count x acc} @ immutable total =
    fun x acc ->
  let refine_ result = increment x acc in
  ghost_ (increment_relation_def x acc result);
  ghost_ (next_count_def x acc);
  refine_ result

let (count_model_preserved @ total) :
    (x : int) @ immutable -> (tail : int list) @ immutable ->
    (acc : Bigint.t) @ immutable -> {u : unit | count_relation tail acc} ->
    {u : unit | count_relation (x :: tail) (next_count x acc)} =
    fun x tail acc premise ->
  premise;
  let result = next_count x acc in
  next_count_def x acc;
  increment_relation_def x acc result;
  let u = () in
  count_preserved x tail acc result (refine_ u);
  refine_ u

let count_by_model (xs : int list) : {n : Bigint.t | n === length xs} =
  let initial = 0Z in
  let step = count_exact in
  let refine_ n = fold_model next_count step xs initial in
  ghost_ (
    let empty : int list = [] in
    count_relation_def empty initial;
    length_def empty;
    let u = () in
    let preserve = count_model_preserved in
    model_invariant next_count count_relation preserve xs initial
        (refine_ u);
    count_relation_def xs n;
    (refine_ u : {u : unit | n === length xs}));
  refine_ n

let count_by_trace (xs : int list) : {n : Bigint.t | n === length xs} =
  let initial = 0Z in
  let step = increment in
  let refine_ result = fold_trace increment_relation step xs initial in
  let n = result.value in
  ghost_ (
    let empty : int list = [] in
    count_relation_def empty initial;
    length_def empty;
    let preserve = count_preserved in
    let history = result.trace in
    let u = () in
    trace_invariant increment_relation count_relation
      preserve xs initial n history (refine_ u);
    count_relation_def xs n;
    (refine_ u : {u : unit | n === length xs}));
  refine_ n

let () =
  List.iter (fun xs ->
    let refine_ a = count xs in
    let refine_ b = count_separate xs in
    let refine_ c = count_by_model xs in
    let refine_ d = count_by_trace xs in
    assert (Bigint.equal a b && Bigint.equal b c && Bigint.equal c d))
    [[]; [1]; [1; 2; 3]; [-2; 0; 7]];
  List.iter (fun f ->
    let xs = [13] in
    match f xs with
    | exception Failure message -> assert (message = "increment")
    | _ -> failwith "expected partial callback")
    [(fun xs -> let refine_ n = count_by_model xs in n);
     (fun xs -> let refine_ n = count_by_trace xs in n)]
