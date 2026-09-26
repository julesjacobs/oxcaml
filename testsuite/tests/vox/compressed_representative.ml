open Copy_spec
open Level_spec
open Level_unifier_spec
open Compression_spec

type walk_goal = {heap : node Pref.heap @@ ghost; start : node Pref.t @@ ghost;
  path : resolution @@ ghost}

let rec walk_loop : (goal : walk_goal) @ immutable -> (h : (node Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (root : node Pref.t) @ immutable  ->(path : (resolution) Ghost.t) @ immutable  ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && active h.Ghost.ghost p && resolves h.Ghost.ghost p root path.Ghost.ghost && resolves goal.heap goal.start root goal.path}) @ unique  ->
(lift : (((after : node Pref.heap) @ immutable ->
    (edits : {d : edits | rewritten h.Ghost.ghost after d}) @ immutable ->
    {d : edits | rewritten goal.heap after d} @ immutable)) Ghost.t) @ total ->
{r : result | rewritten goal.heap (Pref.own r.#state) r.#edits && r.#value === root
      && resolves goal.heap goal.start root r.#path && active (Pref.own r.#state) root
      && H.mem (Pref.own r.#state) root && terminal (Pref.own r.#state) root} @ unique = fun goal h scope p root path state lift ->
    let refine_ state = state in
    ghost_ (let u = () in Compression_path_proofs.resolution_terminal h.Ghost.ghost p root path.Ghost.ghost (refine_ u);
      Level_unifier_metadata.resolution_active h.Ghost.ghost scope.Ghost.ghost p root path.Ghost.ghost (refine_ u); ());
    let refine_ equal = Pref.equal p root in
    if equal then (
      let edits = ghost_ Done in ghost_ (rewritten_def h.Ghost.ghost h.Ghost.ghost edits);
      let edits : {d : edits | rewritten goal.heap h.Ghost.ghost d} @ immutable ghost = ghost_ (let edits : {d : edits | rewritten h.Ghost.ghost h.Ghost.ghost d} = refine_ edits in
        let refine_ out = lift.Ghost.ghost h.Ghost.ghost edits in refine_ out) in
      let refine_ edits = edits in
      let r = #{value = root; state; edits; path = goal.path} in refine_ r)
    else (
      ghost_ (resolves_def h.Ghost.ghost p root path.Ghost.ghost);
      let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ old = Pref.read p (borrow_ state) in let refine_ state = state in
      ghost_ (observe_def h.Ghost.ghost p);
      match old.desc with
      | Link q ->
        let refine_ direct = Pref.equal q root in
        if direct then (
          let edits = ghost_ Done in ghost_ (rewritten_def h.Ghost.ghost h.Ghost.ghost edits);
          let edits : {d : edits | rewritten goal.heap h.Ghost.ghost d} @ immutable ghost = ghost_ (let edits : {d : edits | rewritten h.Ghost.ghost h.Ghost.ghost d} = refine_ edits in
        let refine_ out = lift.Ghost.ghost h.Ghost.ghost edits in refine_ out) in
      let refine_ edits = edits in
      let r = #{value = root; state; edits; path = goal.path} in refine_ r
        ) else (
        let rest = ghost_ (Compression_path_proofs.tail path.Ghost.ghost) in
        ghost_ (let u = () in Compression_path_proofs.tail_resolves h.Ghost.ghost p q root path.Ghost.ghost (refine_ u);
          scope.Ghost.ghost p; finite_scope_def h.Ghost.ghost p; source_ok_def h.Ghost.ghost p; ());
        let next_path : {d : resolution | resolves (H.put h.Ghost.ghost p (redirect h.Ghost.ghost p root)) q root d} @ immutable ghost = ghost_ (let u = () in
          let refine_ d = Compression_path_proofs.redirect_resolution h.Ghost.ghost p root q rest (refine_ u) in refine_ d) in
        let link = {old with desc = Link root} in ghost_ (redirect_def h.Ghost.ghost p root);
        let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
        let refine_ state = Pref.write p link state in
        let middle = ghost_ (Pref.own (borrow_ state)) in
        let next_scope : ((x : node Pref.t) @ immutable ->
          {u : unit | not (H.mem middle x) || finite_scope middle x}) @ total ghost = ghost_ (fun x ->
            scope.Ghost.ghost x; let u = () in Level_unifier_metadata.redirect_scope h.Ghost.ghost p root x (refine_ u); refine_ u) in
        ghost_ (let u = () in Level_unifier_metadata.redirect_active h.Ghost.ghost p root q (refine_ u));
        let refine_ next_path = next_path in
        let state : {t : node Pref.token | Pref.own t === middle && active middle q && resolves middle q root next_path} = refine_ state in
        let h_witness5 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (middle)} in
        let scope_witness6 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness5.Ghost.ghost x) || finite_scope h_witness5.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ next_scope)} in
        let path_witness7 : (resolution) Ghost.t = {Ghost.ghost = ghost_ (next_path)} in
        let refine_ state_argument8 = state in
        let next_lift : (((after : node Pref.heap) @ immutable ->
          (edits : {d : edits | rewritten h_witness5.Ghost.ghost after d}) @ immutable ->
          {d : edits | rewritten goal.heap after d} @ immutable)) Ghost.t =
          {Ghost.ghost = ghost_ (fun after edits ->
            let refine_ edits = edits in
            let joined = Write (p, q, root, path.Ghost.ghost, edits) in
            rewritten_def h.Ghost.ghost after joined;
            let joined : {d : edits | rewritten h.Ghost.ghost after d} = refine_ joined in
            let refine_ result = lift.Ghost.ghost after joined in refine_ result)} in
        let refine_ out = walk_loop goal h_witness5 scope_witness6 q root path_witness7 (refine_ state_argument8) next_lift in refine_ out

      )
      | Var | Bool | Word | List _ | Arrow _ ->
        ghost_ (let impossible : {u : unit | false} = refine_ () in let refine_ impossible = impossible in ()); assert false)

let walk : (h : (node Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (root : node Pref.t) @ immutable  ->(path : (resolution) Ghost.t) @ immutable  ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && active h.Ghost.ghost p && resolves h.Ghost.ghost p root path.Ghost.ghost}) @ unique  ->
    {r : result | rewritten h.Ghost.ghost (Pref.own r.#state) r.#edits && r.#value === root
      && resolves h.Ghost.ghost p root r.#path && active (Pref.own r.#state) root
      && H.mem (Pref.own r.#state) root && terminal (Pref.own r.#state) root} @ unique = fun h scope p root path state ->
  let refine_ state = state in
  let goal = {heap = h.Ghost.ghost; start = p; path = path.Ghost.ghost} in
  let lift : (((after : node Pref.heap) @ immutable ->
    (edits : {d : edits | rewritten h.Ghost.ghost after d}) @ immutable ->
    {d : edits | rewritten goal.heap after d} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun after edits -> let refine_ edits = edits in refine_ edits)} in
  let refine_ out = walk_loop goal h scope p root path (refine_ state) lift in refine_ out

let representative : (h : (node Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && active h.Ghost.ghost p}) @ unique  ->
    {r : result | rewritten h.Ghost.ghost (Pref.own r.#state) r.#edits && resolves h.Ghost.ghost p r.#value r.#path
      && active (Pref.own r.#state) r.#value && H.mem (Pref.own r.#state) r.#value
      && terminal (Pref.own r.#state) r.#value} @ unique = fun h scope p state  ->
    let refine_ state = state in
    let found : {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost p r.#value r.#path} =
      let input : {p : node Pref.t | H.mem h.Ghost.ghost p && active h.Ghost.ghost p} = refine_ p in
      let borrowed = borrow_ state in
      let borrowed : {t : node Pref.token | Pref.own t === h.Ghost.ghost} = refine_ borrowed in
      let h_witness1 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let scope_witness2 : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness1.Ghost.ghost q) || finite_scope h_witness1.Ghost.ghost q})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
      let p_argument3 : {p : node Pref.t | H.mem h_witness1.Ghost.ghost p && active h_witness1.Ghost.ghost p} = let refine_ argument = input in refine_ argument in
      let refine_ state_argument4 = borrowed in
      let refine_ found = Level_unifier.representative h_witness1 scope_witness2 p_argument3 (refine_ state_argument4) in
      let refine_ input = input in refine_ found in
    let refine_ found = found in
    let path = ghost_ found.#path in let root = found.#value in
    let state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && active h.Ghost.ghost p && resolves h.Ghost.ghost p root path} = refine_ state in
    let h_witness9 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
    let scope_witness10 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness9.Ghost.ghost x) || finite_scope h_witness9.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
    let path_witness11 : (resolution) Ghost.t = {Ghost.ghost = ghost_ (path)} in
    let refine_ state_argument12 = state in
    let refine_ out = walk h_witness9 scope_witness10 p root path_witness11 (refine_ state_argument12) in refine_ out
