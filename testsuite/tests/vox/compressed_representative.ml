open Copy_spec
open Level_spec
open Level_unifier_spec
open Compression_spec

type walk_goal = {heap : Pref.heap @@ ghost; start : node Pref.t @@ ghost;
  path : resolution @@ ghost}

let rec walk_loop : (goal : walk_goal) @ immutable -> (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (root : node Pref.t) @ immutable  ->(path : (resolution) Ghost.t) @ immutable  ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && active h.Ghost.ghost p && resolves h.Ghost.ghost p root path.Ghost.ghost && resolves goal.heap goal.start root goal.path}) @ unique  ->
(lift : (((after : Pref.heap) @ immutable ->
    (edits : {d : edits | rewritten h.Ghost.ghost after d}) @ immutable ->
    {d : edits | rewritten goal.heap after d} @ immutable)) Ghost.t) @ total ->
{r : result | rewritten goal.heap (Pref.own r.#state) r.#edits && r.#value === root
      && resolves goal.heap goal.start root r.#path && active (Pref.own r.#state) root
      && H.mem (Pref.own r.#state) root && terminal (Pref.own r.#state) root} @ unique = fun goal h scope p root path state lift ->
    ghost_ (let u = () in Compression_path_proofs.resolution_terminal h.Ghost.ghost p root path.Ghost.ghost (u);
      Level_unifier_metadata.resolution_active h.Ghost.ghost scope.Ghost.ghost p root path.Ghost.ghost (u); ());
    let equal = Pref.equal p root in
    if equal then (
      let edits = ghost_ Done in ghost_ (rewritten_def h.Ghost.ghost h.Ghost.ghost edits);
      let edits : {d : edits | rewritten goal.heap h.Ghost.ghost d} @ immutable ghost = ghost_ (let edits : {d : edits | rewritten h.Ghost.ghost h.Ghost.ghost d} = edits in
        let out = lift.Ghost.ghost h.Ghost.ghost edits in out) in
      let r = #{value = root; state; edits; path = goal.path} in r)
    else (
      ghost_ (resolves_def h.Ghost.ghost p root path.Ghost.ghost);
      let state : {t : Pref.token | H.mem (Pref.own t) p} = state in
      let old = Pref.read p (borrow_ state) in ghost_ (observe_def h.Ghost.ghost p);
      match old.desc with
      | Link q ->
        let direct = Pref.equal q root in
        if direct then (
          let edits = ghost_ Done in ghost_ (rewritten_def h.Ghost.ghost h.Ghost.ghost edits);
          let edits : {d : edits | rewritten goal.heap h.Ghost.ghost d} @ immutable ghost = ghost_ (let edits : {d : edits | rewritten h.Ghost.ghost h.Ghost.ghost d} = edits in
        let out = lift.Ghost.ghost h.Ghost.ghost edits in out) in
      let r = #{value = root; state; edits; path = goal.path} in r
        ) else (
        let rest = ghost_ (Compression_path_proofs.tail path.Ghost.ghost) in
        ghost_ (let u = () in Compression_path_proofs.tail_resolves h.Ghost.ghost p q root path.Ghost.ghost (u);
          scope.Ghost.ghost p; finite_scope_def h.Ghost.ghost p; source_ok_def h.Ghost.ghost p; ());
        let next_path : {d : resolution | resolves (H.put h.Ghost.ghost p (redirect h.Ghost.ghost p root)) q root d} @ immutable ghost = ghost_ (let u = () in
          let d = Compression_path_proofs.redirect_resolution h.Ghost.ghost p root q rest (u) in d) in
        let link = {old with desc = Link root} in ghost_ (redirect_def h.Ghost.ghost p root);
        let state : {t : Pref.token | H.mem (Pref.own t) p} = state in
        let state = Pref.write p link state in
        let middle = ghost_ (Pref.own (borrow_ state)) in
        let next_scope : ((x : node Pref.t) @ immutable ->
          {u : unit | not (H.mem middle x) || finite_scope middle x}) @ total ghost = ghost_ (fun x ->
            scope.Ghost.ghost x; let u = () in Level_unifier_metadata.redirect_scope h.Ghost.ghost p root x (u); u) in
        ghost_ (let u = () in Level_unifier_metadata.redirect_active h.Ghost.ghost p root q (u));
        let state : {t : Pref.token | Pref.own t === middle && active middle q && resolves middle q root next_path} = state in
        let h_witness5 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (middle)} in
        let scope_witness6 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness5.Ghost.ghost x) || finite_scope h_witness5.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (next_scope)} in
        let path_witness7 : (resolution) Ghost.t = {Ghost.ghost = ghost_ (next_path)} in
        let state_argument8 = state in
        let next_lift : (((after : Pref.heap) @ immutable ->
          (edits : {d : edits | rewritten h_witness5.Ghost.ghost after d}) @ immutable ->
          {d : edits | rewritten goal.heap after d} @ immutable)) Ghost.t =
          {Ghost.ghost = ghost_ (fun after edits ->
            let joined = Write (p, q, root, path.Ghost.ghost, edits) in
            rewritten_def h.Ghost.ghost after joined;
            let joined : {d : edits | rewritten h.Ghost.ghost after d} = joined in
            let result = lift.Ghost.ghost after joined in result)} in
        let out = walk_loop goal h_witness5 scope_witness6 q root path_witness7 (state_argument8) next_lift in out

      )
      | Var | Bool | Arrow _ ->
        ghost_ (let _impossible : {u : unit | false} = () in ()); assert false)

let walk : (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (root : node Pref.t) @ immutable  ->(path : (resolution) Ghost.t) @ immutable  ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && active h.Ghost.ghost p && resolves h.Ghost.ghost p root path.Ghost.ghost}) @ unique  ->
    {r : result | rewritten h.Ghost.ghost (Pref.own r.#state) r.#edits && r.#value === root
      && resolves h.Ghost.ghost p root r.#path && active (Pref.own r.#state) root
      && H.mem (Pref.own r.#state) root && terminal (Pref.own r.#state) root} @ unique = fun h scope p root path state ->
  let goal = {heap = h.Ghost.ghost; start = p; path = path.Ghost.ghost} in
  let lift : (((after : Pref.heap) @ immutable ->
    (edits : {d : edits | rewritten h.Ghost.ghost after d}) @ immutable ->
    {d : edits | rewritten goal.heap after d} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun after edits -> edits)} in
  let out = walk_loop goal h scope p root path (state) lift in out

let representative : (h : (Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || finite_scope h.Ghost.ghost x})) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && active h.Ghost.ghost p}) @ unique  ->
    {r : result | rewritten h.Ghost.ghost (Pref.own r.#state) r.#edits && resolves h.Ghost.ghost p r.#value r.#path
      && active (Pref.own r.#state) r.#value && H.mem (Pref.own r.#state) r.#value
      && terminal (Pref.own r.#state) r.#value} @ unique = fun h scope p state  ->
    let found : {r : resolved | H.mem h.Ghost.ghost r.#value && active h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value
      && resolves h.Ghost.ghost p r.#value r.#path} =
      let input : {p : node Pref.t | H.mem h.Ghost.ghost p && active h.Ghost.ghost p} = p in
      let borrowed = borrow_ state in
      let borrowed : {t : Pref.token | Pref.own t === h.Ghost.ghost} = borrowed in
      let h_witness1 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
      let scope_witness2 : (((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h_witness1.Ghost.ghost q) || finite_scope h_witness1.Ghost.ghost q})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
      let p_argument3 : {p : node Pref.t | H.mem h_witness1.Ghost.ghost p && active h_witness1.Ghost.ghost p} = let argument = input in argument in
      let state_argument4 = borrowed in
      let found = Level_unifier.representative h_witness1 scope_witness2 p_argument3 (state_argument4) in
      found in
    let path = ghost_ found.#path in let root = found.#value in
    let state : {t : Pref.token | Pref.own t === h.Ghost.ghost && active h.Ghost.ghost p && resolves h.Ghost.ghost p root path} = state in
    let h_witness9 : (Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (h.Ghost.ghost)} in
    let scope_witness10 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness9.Ghost.ghost x) || finite_scope h_witness9.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ scope.Ghost.ghost)} in
    let path_witness11 : (resolution) Ghost.t = {Ghost.ghost = ghost_ (path)} in
    let state_argument12 = state in
    let out = walk h_witness9 scope_witness10 p root path_witness11 (state_argument12) in out
