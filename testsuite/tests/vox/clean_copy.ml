open Copy_spec
open Copy_heap_proofs
open Generalize_spec
open Pooled_spec
open Copy_cleanup_spec
open Copy_cleanup_proofs

type instance = Copy_cleanup_spec.instance

let instantiate : (saved : (Pref.heap) Ghost.t) @ immutable ->
    (clean : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t) @ total ->
    (scope : (((p : node Pref.t) @ immutable ->
      {u : unit | if H.mem saved.Ghost.ghost p then source_ok saved.Ghost.ghost p
        else H.at saved.Ghost.ghost p === None})) Ghost.t) @ total ->
    (base : pool) @ immutable -> (depth : {n : int | n >= 0}) ->
    (p : {p : node Pref.t | H.mem saved.Ghost.ghost p}) @ immutable ->
    (state : {t : Pref.token | Pref.own t === saved.Ghost.ghost
      && pool_scoped saved.Ghost.ghost base}) @ unique ->
    {r : instance | valid saved.Ghost.ghost r.#epoch depth r.#history
      && Pref.own r.#state ===
        swept (heap saved.Ghost.ghost r.#epoch depth r.#history) (touched r.#history)
      && r.#pool === registered base r.#epoch r.#history
      && target_for saved.Ghost.ghost r.#history p r.#value} @ unique = fun saved clean scope base depth p state ->
    let saved_witness1 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (saved.Ghost.ghost)} in
    let clean_witness2 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved_witness1.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t = {Ghost.ghost = ghost_ (refine_ clean.Ghost.ghost)} in
    let scope_witness3 : (((p : node Pref.t) @ immutable -> {u : unit | if H.mem saved_witness1.Ghost.ghost p then source_ok saved_witness1.Ghost.ghost p else H.at saved_witness1.Ghost.ghost p === None})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
    let copy_source1 : {p : node Pref.t | H.mem saved_witness1.Ghost.ghost p} =
      p in
    let out = Clean_pooled_copy.instantiate saved_witness1 clean_witness2 scope_witness3 base depth copy_source1 (state) in
    let epoch = ghost_ out.#epoch in let history = ghost_ out.#history in
    let raw = ghost_ (heap saved.Ghost.ghost epoch depth history) in
    let trail = out.#trail in
    let members : ((x : node Pref.t) @ immutable ->
      {u : unit | not (listed trail x) || H.mem raw x}) @ total ghost = ghost_ (fun x ->
      touched_saved saved.Ghost.ghost epoch depth history x ();
      history_grows saved.Ghost.ghost epoch depth history x (); ()) in
    let state = out.#state in
    let state : {t : Pref.token | Pref.own t === raw} = state in
    let heap_witness : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (raw)} in
    let members_witness : (((x : node Pref.t) @ immutable ->
      {u : unit | not (listed trail x) || H.mem heap_witness.Ghost.ghost x})) Ghost.t =
      {Ghost.ghost = ghost_ (members)} in
    let state = Copy_cleanup.clear heap_witness trail members_witness (state) in
    let result = #{value = out.#value; state; pool = out.#pool; epoch; history} in
    result

let (result_at @ total) : (saved : Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d} ->
    {u : unit | swept_at (heap saved epoch depth d)
      (swept (heap saved epoch depth d) (touched d)) (touched d) x} @ ghost =
  fun saved epoch depth d x premise -> ghost_ (
    let h = heap saved epoch depth d in let trail = touched d in
    let members : ((y : node Pref.t) @ immutable ->
      {u : unit | not (listed trail y) || H.mem h y}) @ total = fun y ->
      touched_saved saved epoch depth d y ();
      history_grows saved epoch depth d y (); () in
    let () = sweep_at h trail members x in ())

let (memo_released @ total) : (saved : Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d} ->
    {u : unit | match mapping d x with None -> true | Some _ ->
      match H.at (swept (heap saved epoch depth d) (touched d)) x with
      | None -> false | Some v -> v.memo === Empty_memo} @ ghost =
  fun saved epoch depth d x premise -> ghost_ (
    result_at saved epoch depth d x ();
    touched_mapping d x; touched_saved saved epoch depth d x ();
    history_at saved epoch depth d x ();
    let h = heap saved epoch depth d in let trail = touched d in
    let after = swept h trail in swept_at_def h after trail x;
    ())

let (model_equivalence @ total) : (saved : Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d} ->
    {u : unit | equation (heap saved epoch depth d) rho x ===
      equation (swept (heap saved epoch depth d) (touched d)) rho x} @ ghost =
  fun saved epoch depth d rho x premise -> ghost_ (
    result_at saved epoch depth d x ();
    let h = heap saved epoch depth d in let trail = touched d in
    let after = swept h trail in
    let () = sweep_model h after trail rho x () in
    ())

let (clean_result @ total) : (saved : Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d &&
      match H.at saved x with None -> true | Some v -> v.memo === Empty_memo} ->
    {u : unit | match H.at (swept (heap saved epoch depth d) (touched d)) x with
      None -> true | Some v -> v.memo === Empty_memo} @ ghost =
  fun saved epoch depth d x premise -> ghost_ (
    history_clean saved epoch depth d x ();
    result_at saved epoch depth d x ();
    let h = heap saved epoch depth d in let trail = touched d in
    let after = swept h trail in swept_at_def h after trail x;
    ())
