open Copy_spec
open Level_spec
open Level_unifier_spec

val occurs :
  (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost x) || source_ok h.Ghost.ghost x})) Ghost.t) @
        total  ->(unmarked : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h.Ghost.ghost x with
        None -> true | Some v -> not v.visited})) Ghost.t) @ total  ->
    (needle : node Pref.t) @ immutable  -> (p : node Pref.t) @ immutable  ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p}) @
      unique  ->
    {r : checked | marks_valid h.Ghost.ghost needle r.#marks
      && searched h.Ghost.ghost needle p r.#found r.#search
      && Pref.own r.#state ===
        reset_heap (marked_heap h.Ghost.ghost r.#marks) (mark_trail r.#marks)} @ unique
