open Copy_spec
open Level_spec
open Level_unifier_spec
open Compression_spec
open Effective_compression_spec

type walk_goal = {heap : Pref.heap @@ ghost; start : node Pref.t @@ ghost;
  path : resolution @@ ghost}

let rec walk_loop : (goal : walk_goal) @ immutable -> (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || source_ok h.Ghost.ghost x})) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (root : node Pref.t) @ immutable  ->(path : (resolution) Ghost.t) @ immutable  ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && active h.Ghost.ghost root && resolves h.Ghost.ghost p root path.Ghost.ghost && resolves goal.heap goal.start root goal.path}) @ unique  ->
(lift : (((after : Pref.heap) @ immutable ->
    (edits : {d : edits | effective_rewritten h.Ghost.ghost after d}) @ immutable ->
    {d : edits | effective_rewritten goal.heap after d} @ immutable)) Ghost.t) @ total ->
{r : result | effective_rewritten goal.heap (Pref.own r.#state) r.#edits && r.#value === root
      && resolves goal.heap goal.start root r.#path && active (Pref.own r.#state) root
      && H.mem (Pref.own r.#state) root && terminal (Pref.own r.#state) root} @ unique = fun goal h scope p root path state lift ->
    let refine_ state = state in
    ghost_ (let u = () in Compression_path_proofs.resolution_terminal h.Ghost.ghost p root path.Ghost.ghost (refine_ u);
 ());
    let refine_ equal = Pref.equal p root in
    if equal then (
      let edits = ghost_ Done in ghost_ (effective_rewritten_def h.Ghost.ghost h.Ghost.ghost edits);
      let edits : {d : edits | effective_rewritten goal.heap h.Ghost.ghost d} @ immutable ghost = ghost_ (let edits : {d : edits | effective_rewritten h.Ghost.ghost h.Ghost.ghost d} = refine_ edits in
        let refine_ out = lift.Ghost.ghost h.Ghost.ghost edits in refine_ out) in
      let refine_ edits = edits in
      let r = #{value = root; state; edits; path = goal.path} in refine_ r)
    else (
      ghost_ (resolves_def h.Ghost.ghost p root path.Ghost.ghost);
      let state : {t : Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ old = Pref.read p (borrow_ state) in let refine_ state = state in
      ghost_ (observe_def h.Ghost.ghost p);
      match old.desc with
      | Link q ->
        let refine_ direct = Pref.equal q root in
        if direct then (
          let edits = ghost_ Done in ghost_ (effective_rewritten_def h.Ghost.ghost h.Ghost.ghost edits);
          let edits : {d : edits | effective_rewritten goal.heap h.Ghost.ghost d} @ immutable ghost = ghost_ (let edits : {d : edits | effective_rewritten h.Ghost.ghost h.Ghost.ghost d} = refine_ edits in
        let refine_ out = lift.Ghost.ghost h.Ghost.ghost edits in refine_ out) in
      let refine_ edits = edits in
      let r = #{value = root; state; edits; path = goal.path} in refine_ r
        ) else (
        let rest = ghost_ (Compression_path_proofs.tail path.Ghost.ghost) in
        ghost_ (let u = () in Compression_path_proofs.tail_resolves h.Ghost.ghost p q root path.Ghost.ghost (refine_ u);
          scope.Ghost.ghost p; source_ok_def h.Ghost.ghost p; ());
        let next_path : {d : resolution | resolves (H.put h.Ghost.ghost p (redirect h.Ghost.ghost p root)) q root d} @ immutable ghost = ghost_ (let u = () in
          let refine_ d = Compression_path_proofs.redirect_resolution h.Ghost.ghost p root q rest (refine_ u) in refine_ d) in
        let link = {old with desc = Link root} in ghost_ (redirect_def h.Ghost.ghost p root);
        let state : {t : Pref.token | H.mem (Pref.own t) p} = refine_ state in
        let refine_ state = Pref.write p link state in
        let middle = ghost_ (Pref.own (borrow_ state)) in
        let next_scope : ((x : node Pref.t) @ immutable ->
          {u : unit | not (H.mem middle x) || source_ok middle x}) @ total ghost = ghost_ (fun x ->
            scope.Ghost.ghost x; let u = () in Effective_compression_proofs.redirect_source h.Ghost.ghost scope.Ghost.ghost p root x (refine_ u); refine_ u) in
        ghost_ (let u = () in Effective_compression_proofs.redirect_levels h.Ghost.ghost p root root (refine_ u); active_def h.Ghost.ghost root; active_def middle root; Copy_heap_proofs.put_frame h.Ghost.ghost p link q);
        let refine_ next_path = next_path in
        let state : {t : Pref.token | Pref.own t === middle && H.mem middle q && active middle root && resolves middle q root next_path} = refine_ state in
        let h_witness5 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (middle)} in
        let scope_witness6 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness5.Ghost.ghost x) || source_ok h_witness5.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ next_scope)} in
        let path_witness7 : (resolution) Ghost.t = {Ghost.ghost = ghost_ (next_path)} in
        let refine_ state_argument8 = state in
        let next_lift : (((after : Pref.heap) @ immutable ->
          (edits : {d : edits | effective_rewritten h_witness5.Ghost.ghost after d}) @ immutable ->
          {d : edits | effective_rewritten goal.heap after d} @ immutable)) Ghost.t =
          {Ghost.ghost = ghost_ (fun after edits ->
            let refine_ edits = edits in
            let joined = Write (p, q, root, path.Ghost.ghost, edits) in
            effective_rewritten_def h.Ghost.ghost after joined;
            let joined : {d : edits | effective_rewritten h.Ghost.ghost after d} = refine_ joined in
            let refine_ result = lift.Ghost.ghost after joined in refine_ result)} in
        let refine_ out = walk_loop goal h_witness5 scope_witness6 q root path_witness7 (refine_ state_argument8) next_lift in refine_ out

      )
      | Var | Bool | Arrow _ ->
        ghost_ (let impossible : {u : unit | false} = refine_ () in let refine_ impossible = impossible in ()); assert false)

let walk : (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || source_ok h.Ghost.ghost x})) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (root : node Pref.t) @ immutable  ->(path : (resolution) Ghost.t) @ immutable  ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && active h.Ghost.ghost root && resolves h.Ghost.ghost p root path.Ghost.ghost}) @ unique  ->
    {r : result | effective_rewritten h.Ghost.ghost (Pref.own r.#state) r.#edits && r.#value === root
      && resolves h.Ghost.ghost p root r.#path && active (Pref.own r.#state) root
      && H.mem (Pref.own r.#state) root && terminal (Pref.own r.#state) root} @ unique = fun h scope p root path state ->
  let refine_ state = state in
  let goal = {heap = h.Ghost.ghost; start = p; path = path.Ghost.ghost} in
  let lift : (((after : Pref.heap) @ immutable ->
    (edits : {d : edits | effective_rewritten h.Ghost.ghost after d}) @ immutable ->
    {d : edits | effective_rewritten goal.heap after d} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun after edits -> let refine_ edits = edits in refine_ edits)} in
  let refine_ out = walk_loop goal h scope p root path (refine_ state) lift in refine_ out

module E = Effective_level
module R = Representative_level

let (resolved_active @ total) : (h : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (p : node Pref.t) @ immutable ->
    (root : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    {u : unit | H.mem h p && E.valid_head h heads p && resolves h p root path
      && (match E.level h heads p with Generic -> false | Finite n -> n >= 0)} ->
    {u : unit | active h root} @ ghost = fun h heads p root path premise -> ghost_ (
      let refine_ premise = premise in E.valid_head_def h heads p;
      E.level_def h heads p; let expected = heads p in let u = () in
      R.unique h p root path expected.root expected.path (refine_ u);
      Compression_path_proofs.resolution_terminal h p root path (refine_ u);
      active_def h root; refine_ u)

let representative :
    (h : Pref.heap Ghost.t) @ immutable ->
    (heads : E.heads Ghost.t) @ immutable ->
    (valid : (((x : node Pref.t) @ immutable ->
      {u : unit | E.valid_head h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost x) || source_ok h.Ghost.ghost x})) Ghost.t) @ total ->
    (p : node Pref.t) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p
      && (match E.level h.Ghost.ghost heads.Ghost.ghost p with Generic -> false | Finite n -> n >= 0)}) @ unique ->
    {r : result | effective_rewritten h.Ghost.ghost (Pref.own r.#state) r.#edits
      && resolves h.Ghost.ghost p r.#value r.#path
      && active (Pref.own r.#state) r.#value && H.mem (Pref.own r.#state) r.#value
      && terminal (Pref.own r.#state) r.#value} @ unique =
  fun h heads valid scope p state ->
    let refine_ state = state in
    let input : {p : node Pref.t | H.mem h.Ghost.ghost p} = refine_ p in
    let found : {r : resolved | H.mem h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value && resolves h.Ghost.ghost p r.#value r.#path} =
      let borrowed = borrow_ state in
      let borrowed : {t : Pref.token | Pref.own t === h.Ghost.ghost} = refine_ borrowed in
      let refine_ found = Graph_representative.representative h scope input borrowed in
      let refine_ input = input in refine_ found in
    let refine_ found = found in
    let refine_ input = input in
    let root = found.#value in let path = ghost_ found.#path in
    ghost_ (
      valid.Ghost.ghost p; let u = () in
      resolved_active h.Ghost.ghost heads.Ghost.ghost p root path (refine_ u); ());
    let path = {Ghost.ghost = ghost_ path} in
    let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && active h.Ghost.ghost root && resolves h.Ghost.ghost p root path.Ghost.ghost} = refine_ state in
    let refine_ state_argument = state in
    let refine_ out = walk h scope p root path (refine_ state_argument) in refine_ out
