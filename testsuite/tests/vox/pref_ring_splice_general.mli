open Pref_ring
open Pref_ring_general

val last : node @ immutable -> node list @ immutable -> node @ ghost @@ total
val last_def : (fallback : node) @ immutable -> (ns : node list) @ immutable ->
  {u : unit | last fallback ns === (ghost_ (match ns with
    | [] -> fallback | n :: rest -> last n rest))} @@ total

val spliced : node option Pref.heap @ immutable -> node @ immutable -> node @ immutable ->
  node @ immutable -> node @ immutable -> node @ immutable -> node @ immutable -> node option Pref.heap @ ghost @@ total
val spliced_def : (h : node option Pref.heap) @ immutable -> (left : node) @ immutable ->
  (first : node) @ immutable -> (final : node) @ immutable -> (right : node) @ immutable ->
  (destination_left : node) @ immutable -> (destination_right : node) @ immutable ->
  {u : unit | spliced h left first final right destination_left destination_right ===
    (ghost_ (connected (connected (connected h left right) destination_left first)
      final destination_right))} @@ total

val splice : (s : node) @ immutable ghost -> (t : node) @ immutable ghost ->
    (prefix : node list) @ immutable ghost -> (first : node) @ immutable ->
    (rest : node list) @ immutable ghost ->
    (final : {n : node | n === last first rest}) @ immutable ->
    (suffix : node list) @ immutable ghost ->
    (destination_prefix : node list) @ immutable ghost ->
    (destination_left : {n : node | n === last t destination_prefix}) @ immutable ->
    (destination_suffix : node list) @ immutable ghost ->
    (state : {state : node option Pref.token |
      ring (Pref.own state) s (append prefix (append (first :: rest) suffix)) &&
      ring (Pref.own state) t (append destination_prefix destination_suffix) &&
      separated (append (s :: append prefix (append (first :: rest) suffix))
        (t :: append destination_prefix destination_suffix))}) @ unique ->
    {result : node option Pref.token |
      Pref.own result === spliced (Pref.own state) (last s prefix) first final
        (head suffix s) destination_left (head destination_suffix t) &&
      ring (Pref.own result) s (append prefix suffix) &&
      ring (Pref.own result) t (append destination_prefix (append (first :: rest) destination_suffix)) &&
      separated (append (s :: append prefix suffix)
        (t :: append destination_prefix (append (first :: rest) destination_suffix)))} @ unique

type paired = #{source : node @@ aliased; destination : node @@ aliased;
  source_nodes : node list @@ aliased ghost; destination_nodes : node list @@ aliased ghost;
  state : node option Pref.token}

module Owned : sig
  type t : value & value & void & void & void
  val source : t @ local immutable total ghost -> node @ ghost @@ total
  val destination : t @ local immutable total ghost -> node @ ghost @@ total
  val source_model : t @ local immutable total ghost -> node list @ ghost @@ total
  val destination_model : t @ local immutable total ghost -> node list @ ghost @@ total
  val heap : t @ local immutable total ghost -> node option Pref.heap @ ghost @@ total

  val adopt : (b : {b : paired | ring (Pref.own b.#state) b.#source b.#source_nodes &&
      ring (Pref.own b.#state) b.#destination b.#destination_nodes &&
      separated (append (b.#source :: b.#source_nodes) (b.#destination :: b.#destination_nodes))}) @ unique ->
      {state : t | source state === b.#source && destination state === b.#destination &&
        source_model state === b.#source_nodes && destination_model state === b.#destination_nodes &&
        heap state === Pref.own b.#state} @ unique

  val release : (state : t) @ unique ->
      {b : paired | ring (Pref.own b.#state) b.#source b.#source_nodes &&
        ring (Pref.own b.#state) b.#destination b.#destination_nodes &&
        separated (append (b.#source :: b.#source_nodes) (b.#destination :: b.#destination_nodes)) &&
        b.#source === source state && b.#destination === destination state &&
        b.#source_nodes === source_model state && b.#destination_nodes === destination_model state &&
        Pref.own b.#state === heap state} @ unique

  val splice : (prefix : node list) @ immutable ghost -> (first : node) @ immutable ->
      (rest : node list) @ immutable ghost ->
      (final : {n : node | n === last first rest}) @ immutable ->
      (suffix : node list) @ immutable ghost ->
      (destination_prefix : node list) @ immutable ghost -> (destination_left : node) @ immutable ->
      (destination_suffix : node list) @ immutable ghost ->
      (state : {state : t | source_model state === append prefix (append (first :: rest) suffix) &&
        destination_model state === append destination_prefix destination_suffix &&
        destination_left === last (destination state) destination_prefix}) @ unique ->
      {next : t | source next === source state && destination next === destination state &&
        source_model next === append prefix suffix &&
        destination_model next === append destination_prefix (append (first :: rest) destination_suffix) &&
        heap next === spliced (heap state) (last (source state) prefix) first final
          (head suffix (source state)) destination_left (head destination_suffix (destination state))} @ unique

  val observe_source : (state : t) @ local read total forkable unyielding ->
      {nodes : node list | nodes === source_model state} @ immutable

  val observe_destination : (state : t) @ local read total forkable unyielding ->
      {nodes : node list | nodes === destination_model state} @ immutable

  val swap : (state : t) @ unique ->
    {next : t | source next === destination state && destination next === source state &&
      source_model next === destination_model state && destination_model next === source_model state &&
      heap next === heap state} @ unique
end
