(* The public interface of the e-graph. A graph [t] is created from a list
   of valid rewrite rules and is unique: each operation consumes it and
   returns it. [model] and [rules] are ghost observations used in the
   contracts: [model] is a [Q.graph] (nodes and a class label per id) and
   [rules] the rules given to [create]. Every operation keeps [rules] and,
   through [Preserves.extends], the expression ([Snapshot.origin]) of
   every existing id.

   - [admit] adds an expression and returns an id that stands for exactly
     it. It returns [None] only for an ill-sorted expression or when the
     graph has 512 nodes.
   - [query] admits both expressions and compares their classes, without
     saturating. [Equal] comes with an erased derivation, valid for the
     rules, from [left] to [right]; [Not_proved] says nothing.
   - [saturate state rounds rebuild_passes fuel] applies the rules and
     restores congruence until a fixed point or a limit. [Fixed_point]
     means [Fixed.fixed] of the returned graph; the other statuses say only
     which limit was reached.
   - [same_class] says whether two ids have the same class label and, if
     they do, gives a derivation between their origins. It does not change
     the graph.
   - [preserved_origin] reads one id's origin out of [Preserves.extends].

   [Vox_egraph_interpret_wrapping.sound] turns a derivation into equal
   evaluations for rules that preserve evaluation. vox_egraph_rule_handle.md
   lists the specification files in reading order and describes the limits
   and the search fuel. *)

module Preserves = Vox_egraph_preservation_spec
module R = Vox_egraph_rule_spec
module Q = Vox_egraph_match_spec
module L = Vox_egraph_language_spec
module Snapshot = Vox_egraph_snapshot_spec

type t [@@total_matchable]

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
type query_result = #{status : equality; state : t; proof : E.evidence option @@ ghost aliased}

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

type class_result = #{equal : bool; state : t; proof : E.evidence option @@ ghost aliased}

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
