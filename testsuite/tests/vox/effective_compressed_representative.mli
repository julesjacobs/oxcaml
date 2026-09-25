module E := Effective_level
open Copy_spec
open Level_spec
open Level_unifier_spec
open Compression_spec
open Effective_compression_spec

val walk :
  (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u
    : unit | not (H.mem h.Ghost.ghost x) || source_ok h.Ghost.ghost x})) Ghost.t) @ total
    ->
    (p : node Pref.t) @ immutable  -> (root : node Pref.t) @ immutable  ->(path :
      (resolution) Ghost.t) @ immutable  ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p &&
      active h.Ghost.ghost root && resolves h.Ghost.ghost p root path.Ghost.ghost}) @
      unique  ->
    {r : result | effective_rewritten h.Ghost.ghost (Pref.own r.#state) r.#edits &&
      r.#value === root
      && resolves h.Ghost.ghost p root r.#path && active (Pref.own r.#state) root
      && H.mem (Pref.own r.#state) root && terminal (Pref.own r.#state) root} @ unique

val resolved_active :
  (h : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (p : node Pref.t) @ immutable ->
    (root : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    {u : unit | H.mem h p && E.valid_head h heads p && resolves h p root path
      && (match E.level h heads p with Generic -> false | Finite n -> n >= 0)} ->
    {u : unit | active h root} @ ghost
  @@ total

val representative :
  (h : Pref.heap Ghost.t) @ immutable ->
    (heads : E.heads Ghost.t) @ immutable ->
    (valid : (((x : node Pref.t) @ immutable ->
      {u : unit | E.valid_head h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost x) || source_ok h.Ghost.ghost x})) Ghost.t) @
        total ->
    (p : node Pref.t) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p
      && (match E.level h.Ghost.ghost heads.Ghost.ghost p with Generic -> false | Finite n
        -> n >= 0)}) @ unique ->
    {r : result | effective_rewritten h.Ghost.ghost (Pref.own r.#state) r.#edits
      && resolves h.Ghost.ghost p r.#value r.#path
      && active (Pref.own r.#state) r.#value && H.mem (Pref.own r.#state) r.#value
      && terminal (Pref.own r.#state) r.#value} @ unique
