(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml proof_adaptation_runtime.ml";
 { bytecode; }{ native; }
*)

let (transport @ total) :
    (before : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable ->
    (model : ((p : int Pref.t) @ immutable ->
      {u : unit | Pref.Heap.mem before p})) @ total ->
    {u : unit | before === after} ->
    ((p : int Pref.t) @ immutable -> {u : unit | Pref.Heap.mem after p}) @ total ghost =
  fun before after model equality -> ghost_ (
    let refine_ equality = equality in
    refine_ model)

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
