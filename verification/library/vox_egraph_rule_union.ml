module I = Vox_iarray
module L = Vox_egraph_language_spec
module LP = Vox_egraph_language_proof
module E = Vox_egraph_derivation_spec
module EP = Vox_egraph_derivation
module U = Vox_egraph_union
module S = Vox_egraph_rule_semantics
module M = Vox_egraph_union_spec

type t = {
  rules : Vox_egraph_rule_spec.t @@ ghost;
  union : U.t @@ total;
  origins : L.expr iarray @@ ghost;
  edges : E.evidence option iarray @@ ghost;
}

let[@def] valid (state : t @ immutable) = ghost_ (
  U.valid state.union &&
  Iarray.length state.origins = 512 &&
  Iarray.length state.edges = 512 &&
  S.valid_edges state.rules state.union.parents state.origins state.edges
    state.union.count)

type add_result = #{value : int option; state : t}
type merge_result = #{merged : bool; state : t}

let create : (rules : Vox_egraph_rule_spec.t) @ immutable ghost ->
    {state : t | valid state && state.union.count = 0 &&
      state.rules === rules}
    @ immutable = fun rules ->
  let union = U.create () in
  let origins = Vox_egraph_ghost_arrays.origins () in
  let edges = Vox_egraph_ghost_arrays.edges () in
  let state = {rules; union; origins; edges} in
  ghost_ (
    S.valid_edges_def rules union.parents origins edges 0;
    valid_def state);
  state

let add : (state : {s : t | valid s}) @ immutable ->
    (expr : L.expr) @ immutable ghost ->
    {r : add_result | valid r.#state && r.#state.rules === state.rules &&
      (if state.union.count = 512 then
        r.#value === None && r.#state === state
      else r.#value === Some state.union.count &&
        r.#state.union.count = state.union.count + 1 &&
        r.#state.union.parents ===
          I.updated state.union.parents state.union.count
            state.union.count &&
        r.#state.origins ===
          I.updated state.origins state.union.count expr &&
        r.#state.edges ===
          I.updated state.edges state.union.count None &&
        S.origin r.#state.origins state.union.count === expr)}
    @ immutable = fun state expr ->
  ghost_ (valid_def state; U.valid_def state.union);
  let #{U.value; state = union} = U.add state.union in
  match value with
  | None -> #{value = None; state}
  | Some id ->
    let origins = ghost_ (I.updated state.origins id expr) in
    let edges = ghost_ (I.updated state.edges id None) in
    ghost_ (
      S.append_preserves state.rules state.union.parents state.origins state.edges
        id expr ();
      I.updated_length state.origins id expr;
      I.updated_length state.edges id None;
      I.updated_read state.origins id expr id;
      S.origin_def origins id);
    let next = {rules = state.rules; union; origins; edges} in
    ghost_ (valid_def next);
    #{value = Some id; state = next}

let (root_proof @ total) :
    (state : t) @ immutable ->
    (a : int) -> (b : int) ->
    (proof : E.evidence) @ immutable ->
    {u : unit | valid state &&
      0 <= a && a < state.union.count &&
      0 <= b && b < state.union.count &&
      not (L.sort (S.origin state.origins a) === None) &&
      not (L.sort (S.origin state.origins b) === None) &&
      E.valid state.rules proof &&
      E.left proof === S.origin state.origins a &&
      E.right proof === S.origin state.origins b} ->
    {result : E.evidence | E.valid state.rules result &&
      E.left result === S.origin state.origins
        (U.larger (M.root state.union.parents a)
          (M.root state.union.parents b)) &&
      E.right result === S.origin state.origins
        (U.smaller (M.root state.union.parents a)
          (M.root state.union.parents b))} @ ghost =
  fun state a b proof premise -> ghost_ (
    valid_def state;
    U.valid_def state.union;
    let from_a = S.explain state.rules state.union.parents state.origins
      state.edges state.union.count a () in
    let from_b = S.explain state.rules state.union.parents state.origins
      state.edges state.union.count b () in
    let ra = M.root state.union.parents a in
    let rb = M.root state.union.parents b in
    U.larger_def ra rb;
    U.smaller_def ra rb;
    if ra < rb then (
      let first = EP.symmetric state.rules from_b in
      let middle = EP.symmetric state.rules proof in
      let left = EP.transitive state.rules first middle () in
      EP.transitive state.rules left from_a ())
    else (
      let first = EP.symmetric state.rules from_a in
      let left = EP.transitive state.rules first proof () in
      EP.transitive state.rules left from_b ()))

let merge : (state : {s : t | valid s}) @ immutable ->
    (a : {i : int | 0 <= i && i < state.union.count}) ->
    (b : {i : int | 0 <= i && i < state.union.count}) ->
    (proof : {p : E.evidence |
      E.valid state.rules p &&
      E.left p === S.origin state.origins
        (U.larger (M.root state.union.parents a)
          (M.root state.union.parents b)) &&
      E.right p === S.origin state.origins
        (U.smaller (M.root state.union.parents a)
          (M.root state.union.parents b))}) @ immutable ghost ->
    {r : merge_result | valid r.#state && r.#state.rules === state.rules &&
      r.#state.union.count = state.union.count &&
      r.#state.origins === state.origins &&
      (if M.root state.union.parents a = M.root state.union.parents b then
        not r.#merged && r.#state === state
      else r.#merged &&
        r.#state.union.parents === I.updated state.union.parents
          (U.larger (M.root state.union.parents a)
            (M.root state.union.parents b))
          (U.smaller (M.root state.union.parents a)
            (M.root state.union.parents b)) &&
        M.root r.#state.union.parents a =
          M.root r.#state.union.parents b)}
    @ immutable = fun state a b proof ->
  ghost_ (valid_def state; U.valid_def state.union);
  let #{U.merged; winner = _; state = union} = U.union state.union a b in
  if not merged then #{merged = false; state}
  else
    let ra = M.root state.union.parents a in
    let rb = M.root state.union.parents b in
    let loser = U.larger ra rb in
    let winner = U.smaller ra rb in
    let edges = ghost_ (I.updated state.edges loser (Some proof)) in
    ghost_ (
      M.root_spec state.union.parents state.union.count a ();
      M.root_spec state.union.parents state.union.count b ();
      U.larger_def ra rb;
      U.smaller_def ra rb;
      M.root_after_link state.union.parents state.union.count loser winner a ();
      M.root_after_link state.union.parents state.union.count loser winner b ();
      S.link_preserves state.rules state.union.parents state.origins state.edges
        state.union.count loser winner proof ();
      I.updated_length state.edges loser (Some proof));
    let next = {rules = state.rules; union; origins = state.origins; edges} in
    ghost_ (valid_def next);
    #{merged = true; state = next}

let merge_nodes : (state : {s : t | valid s}) @ immutable ->
    (a : {i : int | 0 <= i && i < state.union.count}) ->
    (b : {i : int | 0 <= i && i < state.union.count}) ->
    (proof : {p : E.evidence | E.valid state.rules p &&
      E.left p === S.origin state.origins a &&
      E.right p === S.origin state.origins b}) @ immutable ghost ->
    {r : merge_result | valid r.#state && r.#state.rules === state.rules &&
      r.#state.union.count = state.union.count &&
      r.#state.origins === state.origins &&
      (if r.#merged then true else r.#state === state) &&
      M.root r.#state.union.parents a =
        M.root r.#state.union.parents b} @ immutable =
  fun state a b proof ->
    ghost_ (
      valid_def state;
      U.valid_def state.union;
      EP.sort_sound state.rules proof ();
      EP.well_sorted_def proof);
    let roots = ghost_ (root_proof state a b proof ()) in
    merge state a b roots

let same : (state : {s : t | valid s}) @ immutable ->
    (a : {i : int | 0 <= i && i < state.union.count}) ->
    (b : {i : int | 0 <= i && i < state.union.count}) ->
    {equal : bool |
      equal = (M.root state.union.parents a = M.root state.union.parents b)}
    @ immutable = fun state a b ->
  let left = U.find state.union a in
  let right = U.find state.union b in
  left = right

let (same_evidence @ total) :
    (state : t) @ immutable -> (a : int) -> (b : int) ->
    {u : unit | valid state && 0 <= a && a < state.union.count &&
      0 <= b && b < state.union.count &&
      not (L.sort (S.origin state.origins a) === None) &&
      not (L.sort (S.origin state.origins b) === None) &&
      M.root state.union.parents a = M.root state.union.parents b} ->
    {proof : E.evidence | E.valid state.rules proof &&
      E.left proof === S.origin state.origins a &&
      E.right proof === S.origin state.origins b} @ ghost =
  fun state a b premise -> ghost_ (
    valid_def state;
    U.valid_def state.union;
    S.same_class_evidence state.rules state.union.parents state.origins
      state.edges state.union.count a b ())
