open Copy_spec
open Copy_heap_proofs
open Generalize_spec
open Pooled_spec
open Copy_cleanup_spec
open Copy_cleanup_proofs

type instance = #{value : node Pref.t @@ aliased; state : node Pref.token;
  pool : pool @@ aliased; epoch : node Pref.t @@ ghost;
  history : history @@ ghost}

let instantiate : (saved : node Pref.heap) @ immutable ghost ->
    (scope : ((p : node Pref.t) @ immutable ->
      {u : unit | if H.mem saved p then source_ok saved p
        else H.at saved p === None})) @ total ghost ->
    (base : pool) @ immutable -> (depth : {n : int | n >= 0}) ->
    (p : {p : node Pref.t | H.mem saved p}) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === saved
      && pool_scoped saved base}) @ unique ->
    {r : instance | let refine_ p = p in let refine_ depth = depth in
      valid saved r.#epoch depth r.#history
      && Pref.own r.#state ===
        swept (heap saved r.#epoch depth r.#history) (touched r.#history)
      && r.#pool === registered base r.#epoch r.#history
      && target_for saved r.#history p r.#value} @ unique =
  fun saved scope base depth p state ->
    let refine_ out = Pooled_copy.instantiate saved scope base depth p state in
    let refine_ depth = depth in let refine_ p = p in
    let epoch = ghost_ out.#epoch in let history = ghost_ out.#history in
    let raw = ghost_ (heap saved epoch depth history) in
    let trail = out.#trail in
    let members : ((x : node Pref.t) @ immutable ->
      {u : unit | not (listed trail x) || H.mem raw x}) @ total ghost = ghost_ (fun x ->
      let u = () in touched_saved saved epoch depth history x (refine_ u);
      history_grows saved epoch depth history x (refine_ u); refine_ u) in
    let state = out.#state in
    let state : {t : node Pref.token | Pref.own t === raw} = refine_ state in
    let refine_ state = Copy_cleanup.clear raw trail members state in
    let result = #{value = out.#value; state; pool = out.#pool; epoch; history} in
    refine_ result

let (result_at @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d} ->
    {u : unit | swept_at (heap saved epoch depth d)
      (swept (heap saved epoch depth d) (touched d)) (touched d) x} @ ghost =
  fun saved epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in
    let h = heap saved epoch depth d in let trail = touched d in
    let members : ((y : node Pref.t) @ immutable ->
      {u : unit | not (listed trail y) || H.mem h y}) @ total = fun y ->
      let u = () in touched_saved saved epoch depth d y (refine_ u);
      history_grows saved epoch depth d y (refine_ u); refine_ u in
    let refine_ u = sweep_at h trail members x in refine_ u)

let (memo_released @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d} ->
    {u : unit | match mapping d x with None -> true | Some _ ->
      match H.at (swept (heap saved epoch depth d) (touched d)) x with
      | None -> false | Some v -> v.memo === Empty_memo} @ ghost =
  fun saved epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    result_at saved epoch depth d x (refine_ u);
    touched_mapping d x; touched_saved saved epoch depth d x (refine_ u);
    history_at saved epoch depth d x (refine_ u);
    let h = heap saved epoch depth d in let trail = touched d in
    let after = swept h trail in swept_at_def h after trail x;
    let u = () in refine_ u)

let (model_equivalence @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d} ->
    {u : unit | equation (heap saved epoch depth d) rho x ===
      equation (swept (heap saved epoch depth d) (touched d)) rho x} @ ghost =
  fun saved epoch depth d rho x premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    result_at saved epoch depth d x (refine_ u);
    let h = heap saved epoch depth d in let trail = touched d in
    let after = swept h trail in
    let refine_ u = sweep_model h after trail rho x (refine_ u) in
    refine_ u)
