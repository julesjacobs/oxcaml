(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml proof_adaptation_runtime.ml";
 { bytecode; }{ native; }
*)

type 'a evidence = { proof : 'a }

let (transport @ total) :
    (before : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (model : ((p : int Pref.t) @ immutable ->
      {u : unit | Pref.Heap.mem before p})) @ total ->
    {u : unit | before === after} ->
    ((p : int Pref.t) @ immutable -> {u : unit | Pref.Heap.mem after p}) @ total ghost =
  fun before after model equality -> ghost_ (
    let refine_ equality = equality in
    let evidence = {proof = model} in
    refine_ evidence.proof)

let calls = ref 0
let source x : {r : int | r > 0} =
  incr calls;
  let r = 1 in refine_ r
let adapted : int -> {r : int | r >= 0} = refine_ source
let () =
  assert (!calls = 0);
  let refine_ r = adapted 7 in
  assert (r = 1 && !calls = 1);
  let refine_ r = adapted 8 in
  assert (r = 1 && !calls = 2)

let identity : (x : int) -> {r : int | r = x} = fun x -> refine_ x
let () =
  let refine_ a = identity 42 in
  let refine_ b = identity 43 in
  assert (a = 42 && b = 43)

type witnessed = #{ value : int; witness : int @@ ghost }
let witnessed : (x : int) -> {r : witnessed | r.#witness = x} =
  fun x -> let r = #{ value = x; witness = ghost_ x } in refine_ r
let () =
  let refine_ r = witnessed 42 in
  let proof : {u : unit | r.#witness = 42} = let u = () in refine_ u in
  let refine_ proof = proof in
  assert (r.#value = 42)

let stages = ref 0
let staged : int -> int -> {r : int | r > 0} = fun x ->
  incr stages;
  fun y -> incr stages; let r = 1 in refine_ r
let adapted_stages : int -> int -> {r : int | r >= 0} = refine_ staged
let () =
  assert (!stages = 0);
  let partial = adapted_stages 1 in
  assert (!stages = 1);
  let refine_ result = partial 2 in
  assert (result = 1 && !stages = 2)

type callbacks = { callback : int -> {r : int | r > 0} }
let constructed = ref 0
let callback_calls = ref 0
let make_callbacks () =
  incr constructed;
  { callback = fun _ -> incr callback_calls; let r = 1 in refine_ r }
let projected : int -> {r : int | r >= 0} =
  refine_ (make_callbacks ()).callback
let () =
  assert (!constructed = 1 && !callback_calls = 0);
  let refine_ a = projected 3 in
  let refine_ b = projected 4 in
  assert (a = 1 && b = 1 && !constructed = 1 && !callback_calls = 2)

let expression_calls = ref 0
let value : {n : int | n >= 0} = refine_ (incr expression_calls; 42)
let () = let refine_ value = value in assert (value = 42 && !expression_calls = 1)

let adapted_stages_expr : int -> int -> {r : int | r >= 0} =
  refine_ (incr constructed; staged)
let () =
  assert (!constructed = 2 && !stages = 2);
  let partial = adapted_stages_expr 1 in
  assert (!constructed = 2 && !stages = 3);
  let refine_ result = partial 2 in
  assert (result = 1 && !constructed = 2 && !stages = 4)
