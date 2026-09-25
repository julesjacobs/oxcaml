open Copy_spec
open Effective_copy_spec
open Generalize_spec
open Pooled_spec
module E := Effective_level

type context = Effective_copy_spec.context

val instantiate :
  (c : context) @ immutable -> (heads : E.heads Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem c.saved x) || source_ok c.saved x})) Ghost.t) @ total ->
    (clean : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at c.saved x with None -> true | Some v -> v.memo ===
        Empty_memo})) Ghost.t) @ total ->
    (witness : (((x : node Pref.t) @ immutable ->
      {u : unit | E.valid_head c.saved heads.Ghost.ghost x})) Ghost.t) @ total ->
    (depth : {n : int | n === c.depth && n >= 0}) ->
    (pool : pool) @ immutable -> (p : node Pref.t) @ immutable ->
    (state : {t : Pref.token | Pref.own t === c.saved && pool === c.base
      && H.mem c.saved p && H.mem c.saved c.epoch}) @ unique ->
    {r : Copy_cleanup_spec.instance | effective_valid c.saved heads.Ghost.ghost c.epoch
      c.depth r.#history
      && clean_session r.#history && r.#epoch === c.epoch
      && Pref.own r.#state === Copy_cleanup_spec.swept
        (heap c.saved c.epoch c.depth r.#history) (touched r.#history)
      && r.#pool === registered c.base c.epoch r.#history
      && effective_target_for c.saved heads.Ghost.ghost r.#history p r.#value} @ unique
