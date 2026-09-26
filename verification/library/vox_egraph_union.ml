module I = Vox_iarray
module S = Vox_egraph_union_spec

type t = {parents : int iarray; count : int}

let[@def] valid (state : t @ immutable) = ghost_ (
  Iarray.length state.parents = 512 &&
  0 <= state.count && state.count <= 512 &&
  S.ordered state.parents state.count)

let[@def] (smaller @ total) (a : int) (b : int) = if a < b then a else b
let[@def] (larger @ total) (a : int) (b : int) = if a < b then b else a

type add_result = #{value : int option; state : t}
type union_result = #{merged : bool; winner : int; state : t}

let create : unit -> {state : t | valid state && state.count = 0}
    @ immutable = fun () ->
  let parents = Iarray.init 512 (fun id -> id) in
  let state = {parents; count = 0} in
  ghost_ (S.ordered_def parents 0; valid_def state);
  state

let add : (state : {s : t | valid s}) @ immutable ->
    {r : add_result | valid r.#state &&
      (if state.count = 512 then
        r.#value === None && r.#state === state
      else r.#value === Some state.count &&
        r.#state.count = state.count + 1 &&
        r.#state.parents ===
          I.updated state.parents state.count state.count &&
        S.root r.#state.parents state.count = state.count)}
    @ immutable = fun state ->
  ghost_ (valid_def state);
  if state.count = 512 then #{value = None; state}
  else
    let parents = I.updated state.parents state.count state.count in
    ghost_ (
      S.append_preserves state.parents state.count ();
      I.updated_length state.parents state.count state.count);
    let next = {parents; count = state.count + 1} in
    ghost_ (
      valid_def next;
      S.parent_def parents state.count;
      I.updated_read state.parents state.count state.count state.count;
      S.root_def parents state.count);
    ghost_ (
      let _ : {u : unit | valid next} = () in
      let _ : {u : unit | S.root parents state.count = state.count} =
        () in
      ());
    #{value = Some state.count; state = next}

let find : (state : {s : t | valid s}) @ immutable ->
    (id : {i : int | 0 <= i && i < state.count}) ->
    {root : int | root = S.root state.parents id &&
      0 <= root && root <= id &&
      S.parent state.parents root = root} @ immutable = fun state id ->
  ghost_ (valid_def state; S.root_spec state.parents state.count id ());
  S.root state.parents id

let union : (state : {s : t | valid s}) @ immutable ->
    (a : {i : int | 0 <= i && i < state.count}) ->
    (b : {i : int | 0 <= i && i < state.count}) ->
    {r : union_result | valid r.#state &&
      (if S.root state.parents a = S.root state.parents b then
        not r.#merged && r.#state === state
      else
        r.#merged && r.#state.count = state.count &&
        r.#winner = smaller (S.root state.parents a) (S.root state.parents b)
        && r.#state.parents === I.updated state.parents
          (larger (S.root state.parents a) (S.root state.parents b))
          r.#winner)} @ immutable = fun state a b ->
  ghost_ (valid_def state);
  let ra = find state a in
  let rb = find state b in
  if ra = rb then #{merged = false; winner = ra; state}
  else
    let loser = larger ra rb in
    let winner = smaller ra rb in
    let parents = I.updated state.parents loser winner in
    ghost_ (
      smaller_def ra rb;
      larger_def ra rb;
      S.link_frame state.parents state.count loser winner ();
      I.updated_length state.parents loser winner;
      I.updated_read state.parents loser winner loser);
    let next = {parents; count = state.count} in
    ghost_ (valid_def next);
    #{merged = true; winner; state = next}

let (union_semantics @ total) :
    (before : t) @ immutable ->
    (after : union_result) @ immutable ->
    (a : int) -> (b : int) -> (query : int) ->
    {u : unit | valid before && 0 <= a && a < before.count &&
      0 <= b && b < before.count && 0 <= query && query < before.count &&
      after.#state.count = before.count &&
      (if S.root before.parents a = S.root before.parents b then
        after.#state === before
      else
        after.#state.parents === I.updated before.parents
          (larger (S.root before.parents a) (S.root before.parents b))
          (smaller (S.root before.parents a) (S.root before.parents b)))} ->
    {u : unit | S.root after.#state.parents query =
      (let ra = S.root before.parents a in
       let rb = S.root before.parents b in
       let rq = S.root before.parents query in
       if ra = rb then rq else
       if rq = larger ra rb then smaller ra rb else rq)} @ ghost =
  fun before after a b query premise -> ghost_ (
    valid_def before;
    S.root_spec before.parents before.count a ();
    S.root_spec before.parents before.count b ();
    let ra = S.root before.parents a in
    let rb = S.root before.parents b in
    if ra <> rb then (
      let loser = larger ra rb in
      let winner = smaller ra rb in
      larger_def ra rb;
      smaller_def ra rb;
      S.root_after_link before.parents before.count loser winner query ());
    ())
