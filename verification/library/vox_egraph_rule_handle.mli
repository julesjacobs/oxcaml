module Preserves = Vox_egraph_preservation_spec
module R = Vox_egraph_rule_spec
module Q = Vox_egraph_match_spec
module L = Vox_egraph_language_spec
module Snapshot = Vox_egraph_snapshot_spec

type t

val model : t @ local immutable -> Q.graph @ immutable ghost @@ total
val rules : t @ local immutable -> R.t @ immutable ghost @@ total

val create : (input_rules : {rs : R.t | R.valid rs}) @ immutable ->
    {state : t | (model state).count = 0 && rules state === input_rules} @ unique

type admit_result = #{id : int option @@ aliased; state : t}

val admit : (state : t) @ unique -> (expr : L.expr) @ immutable ->
    {r : admit_result | rules r.#state === rules state &&
      Preserves.extends (model state) (model r.#state) &&
      (model r.#state).count >= (model state).count &&
      (match r.#id with
       | None -> L.sort expr === None || (model r.#state).count = 512
       | Some id -> 0 <= id && id < (model r.#state).count &&
         Snapshot.origin (model r.#state) id === Some expr)} @ unique

module E = Vox_egraph_derivation_spec
module Fixed = Vox_egraph_fixedpoint_spec

type equality = Equal | Not_proved | Invalid_input | Node_limit
type query_result = #{status : equality; state : t; proof : E.evidence option @@ ghost}

val query : (state : t) @ unique -> (left : L.expr) @ immutable -> (right : L.expr) @ immutable ->
    {r : query_result | rules r.#state === rules state &&
      Preserves.extends (model state) (model r.#state) &&
      (match r.#status, r.#proof with
       | Equal, Some proof -> E.valid (rules state) proof &&
         E.left proof === left && E.right proof === right
       | Not_proved, None -> true
       | Invalid_input, None -> L.sort left === None || L.sort right === None
       | Node_limit, None -> (model r.#state).count = 512
       | _ -> false)} @ unique

type saturation_status = Fixed_point | Saturation_node_limit | Search_limit | Rebuild_limit | Round_limit
type saturation_result = #{status : saturation_status; fuel : int; state : t}

val saturate : (state : t) @ unique -> (rounds : int) -> (rebuild_passes : int) ->
    (fuel : {f : int | 0 <= f && f <= 4611686018427387903}) ->
    {r : saturation_result | rules r.#state === rules state &&
      Preserves.extends (model state) (model r.#state) &&
      0 <= r.#fuel && r.#fuel <= fuel &&
      (match r.#status with
       | Fixed_point -> Fixed.fixed (model r.#state) (rules state)
       | Saturation_node_limit -> (model r.#state).count = 512
       | Search_limit -> r.#fuel = 0
       | Rebuild_limit | Round_limit -> true)} @ unique

val preserved_origin : (before : Q.graph) @ immutable -> (after : Q.graph) @ immutable -> (id : int) ->
    {u : unit | Preserves.extends before after && 0 <= id && id < before.count} ->
    {u : unit | Snapshot.origin before id === Snapshot.origin after id} @ ghost @@ total

type class_result = #{equal : bool; state : t; proof : E.evidence option @@ ghost}

val same_class : (state : t) @ unique ->
    (a : {i : int | 0 <= i && i < (model state).count}) ->
    (b : {i : int | 0 <= i && i < (model state).count}) ->
    {r : class_result | model r.#state === model state && rules r.#state === rules state &&
      r.#equal = Q.same (model state) a b &&
      (match r.#proof with
       | None -> not r.#equal
       | Some proof -> r.#equal && E.valid (rules state) proof &&
         Snapshot.origin (model state) a === Some (E.left proof) &&
         Snapshot.origin (model state) b === Some (E.right proof))} @ unique @@ total
