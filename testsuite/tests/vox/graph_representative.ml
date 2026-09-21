open Marked_occurs_proofs
open Copy_spec
open Level_unifier_spec
open Level_unifier_proofs
open Level_spec
open Level_proofs
open Level_unifier_metadata

let rec representative_loop :
    (start : (node Pref.t) Ghost.t) @ immutable ->
    (h : (node Pref.heap) Ghost.t) @ immutable  ->(scope : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost q) || source_ok h.Ghost.ghost q})) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  ->
    (t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p}) @ local read  ->
    (lift : (((value : node Pref.t) @ immutable ->
      (path : {d : resolution | resolves h.Ghost.ghost p value d}) @ immutable ->
      {d : resolution | resolves h.Ghost.ghost start.Ghost.ghost value d} @ immutable)) Ghost.t) @ total ->
    {r : resolved | H.mem h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost start.Ghost.ghost r.#value r.#path} @ immutable = fun start h scope p t lift ->
    let refine_ t = t in
    ghost_ (scope.Ghost.ghost p);
    ghost_ (source_ok_def h.Ghost.ghost p; observe_def h.Ghost.ghost p);
    let t : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let refine_ old = Pref.read p t in
    ghost_ (observe_def h.Ghost.ghost p);
    let n = old.desc in
    match n with
    | Link q ->
      let refine_ t = t in
      let t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost q} = refine_ t in
      let h_witness5 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let scope_witness6 : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness5.Ghost.ghost q) || source_ok h_witness5.Ghost.ghost q})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
      let refine_ state_argument8 = t in
      let next_lift : (((value : node Pref.t) @ immutable ->
        (path : {d : resolution | resolves h_witness5.Ghost.ghost q value d}) @ immutable ->
        {d : resolution | resolves h_witness5.Ghost.ghost start.Ghost.ghost value d} @ immutable)) Ghost.t =
        {Ghost.ghost = ghost_ (fun value path ->
          let refine_ path = path in
          let joined = Via (q, path) in
          resolves_def h.Ghost.ghost p value joined;
          let joined : {d : resolution | resolves h.Ghost.ghost p value d} = refine_ joined in
          let refine_ result = lift.Ghost.ghost value joined in refine_ result)} in
      let refine_ out = representative_loop start h_witness5 scope_witness6 q (refine_ state_argument8) next_lift in refine_ out

    | Var | Bool | Arrow _ ->
      let path = ghost_ Here in
      ghost_ (terminal_def h.Ghost.ghost p);
      ghost_ (resolves_def h.Ghost.ghost p p path);
      let path : {d : resolution | resolves h.Ghost.ghost start.Ghost.ghost p d} @ immutable ghost = ghost_ (
        let path : {d : resolution | resolves h.Ghost.ghost p p d} = refine_ path in
        let refine_ out = lift.Ghost.ghost p path in refine_ out) in
      let refine_ path = path in
      let result = #{value = p; path = path} in refine_ result

let representative :
    (h : (node Pref.heap) Ghost.t) @ immutable  ->(scope : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost q) || source_ok h.Ghost.ghost q})) Ghost.t) @ total  ->
    (p : {p : node Pref.t | H.mem h.Ghost.ghost p}) @ immutable  ->
    (t : {t : node Pref.token | Pref.own t === h.Ghost.ghost}) @ local read  ->
    {r : resolved | let refine_ p = p in H.mem h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost p r.#value r.#path} @ immutable = fun h scope p t ->
    let refine_ p = p in let refine_ t = t in
    let start = {Ghost.ghost = ghost_ p} in
    let lift : (((value : node Pref.t) @ immutable ->
      (path : {d : resolution | resolves h.Ghost.ghost p value d}) @ immutable ->
      {d : resolution | resolves h.Ghost.ghost start.Ghost.ghost value d} @ immutable)) Ghost.t =
      {Ghost.ghost = ghost_ (fun value path -> let refine_ path = path in refine_ path)} in
    let t : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p} = refine_ t in
    let refine_ result = representative_loop start h scope p t lift in refine_ result

