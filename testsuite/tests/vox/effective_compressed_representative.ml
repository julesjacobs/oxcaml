open Copy_spec
open Level_spec
open Level_unifier_spec
open Compression_spec
open Effective_compression_spec

type walk_goal = {heap : node Pref.heap @@ ghost; start : node Pref.t @@ ghost;
  path : resolution @@ ghost}

let rec walk_loop : (goal : walk_goal) @ immutable -> (h : (node Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || source_ok h.Ghost.ghost x})) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (root : node Pref.t) @ immutable  ->(path : (resolution) Ghost.t) @ immutable  ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && active h.Ghost.ghost root && resolves h.Ghost.ghost p root path.Ghost.ghost && resolves goal.heap goal.start root goal.path}) @ unique  ->
(lift : (((after : node Pref.heap) @ immutable ->
    (edits : {d : edits | effective_rewritten h.Ghost.ghost after d}) @ immutable ->
    {d : edits | effective_rewritten goal.heap after d} @ immutable)) Ghost.t) @ total ->
{r : result | effective_rewritten goal.heap (Pref.own r.#state) r.#edits && r.#value === root
      && resolves goal.heap goal.start root r.#path && active (Pref.own r.#state) root
      && H.mem (Pref.own r.#state) root && terminal (Pref.own r.#state) root} @ unique = fun goal h scope p root path state lift ->
    ghost_ (Compression_path_proofs.resolution_terminal h.Ghost.ghost p root path.Ghost.ghost ();
 ());
    let equal = Pref.equal p root in
    if equal then (
      let edits = ghost_ Done in ghost_ (effective_rewritten_def h.Ghost.ghost h.Ghost.ghost edits);
      let edits : {d : edits | effective_rewritten goal.heap h.Ghost.ghost d} @ immutable ghost = ghost_ (let edits : {d : edits | effective_rewritten h.Ghost.ghost h.Ghost.ghost d} = edits in
        let out = lift.Ghost.ghost h.Ghost.ghost edits in out) in
      let r = #{value = root; state; edits; path = goal.path} in r)
    else (
      ghost_ (resolves_def h.Ghost.ghost p root path.Ghost.ghost);
      let old = Pref.read p (borrow_ state) in ghost_ (observe_def h.Ghost.ghost p);
      match old.desc with
      | Link q ->
        let direct = Pref.equal q root in
        if direct then (
          let edits = ghost_ Done in ghost_ (effective_rewritten_def h.Ghost.ghost h.Ghost.ghost edits);
          let edits : {d : edits | effective_rewritten goal.heap h.Ghost.ghost d} @ immutable ghost = ghost_ (let edits : {d : edits | effective_rewritten h.Ghost.ghost h.Ghost.ghost d} = edits in
        let out = lift.Ghost.ghost h.Ghost.ghost edits in out) in
      let r = #{value = root; state; edits; path = goal.path} in r
        ) else (
        let rest = ghost_ (Compression_path_proofs.tail path.Ghost.ghost) in
        ghost_ (Compression_path_proofs.tail_resolves h.Ghost.ghost p q root path.Ghost.ghost ();
          scope.Ghost.ghost p; source_ok_def h.Ghost.ghost p; ());
        let next_path : {d : resolution | resolves (H.put h.Ghost.ghost p (redirect h.Ghost.ghost p root)) q root d} @ immutable ghost = ghost_ (let d = Compression_path_proofs.redirect_resolution h.Ghost.ghost p root q rest () in d) in
        let link = {old with desc = Link root} in ghost_ (redirect_def h.Ghost.ghost p root);
        let state = Pref.write p link state in
        let middle = ghost_ (Pref.own (borrow_ state)) in
        let next_scope : ((x : node Pref.t) @ immutable ->
          {u : unit | not (H.mem middle x) || source_ok middle x}) @ total ghost = ghost_ (fun x ->
            scope.Ghost.ghost x; Effective_compression_proofs.redirect_source h.Ghost.ghost scope.Ghost.ghost p root x (); ()) in
        ghost_ (Effective_compression_proofs.redirect_levels h.Ghost.ghost p root root (); active_def h.Ghost.ghost root; active_def middle root; Copy_heap_proofs.put_frame h.Ghost.ghost p link q);
        let state : {t : node Pref.token | Pref.own t === middle && H.mem middle q && active middle root && resolves middle q root next_path} = state in
        let h_witness5 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (middle)} in
        let scope_witness6 : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h_witness5.Ghost.ghost x) || source_ok h_witness5.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (next_scope)} in
        let path_witness7 : (resolution) Ghost.t = {Ghost.ghost = ghost_ (next_path)} in
        let state_argument8 = state in
        let next_lift : (((after : node Pref.heap) @ immutable ->
          (edits : {d : edits | effective_rewritten h_witness5.Ghost.ghost after d}) @ immutable ->
          {d : edits | effective_rewritten goal.heap after d} @ immutable)) Ghost.t =
          {Ghost.ghost = ghost_ (fun after edits ->
            let joined = Write (p, q, root, path.Ghost.ghost, edits) in
            effective_rewritten_def h.Ghost.ghost after joined;
            let joined : {d : edits | effective_rewritten h.Ghost.ghost after d} = joined in
            let result = lift.Ghost.ghost after joined in result)} in
        let out = walk_loop goal h_witness5 scope_witness6 q root path_witness7 (state_argument8) next_lift in out

      )
      | Var | Bool | Arrow _ ->
        ghost_ (let _ : {u : unit | false} = () in ()); assert false)

let walk : (h : (node Pref.heap) Ghost.t) @ immutable  ->(scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h.Ghost.ghost x) || source_ok h.Ghost.ghost x})) Ghost.t) @ total  ->
    (p : node Pref.t) @ immutable  -> (root : node Pref.t) @ immutable  ->(path : (resolution) Ghost.t) @ immutable  ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && active h.Ghost.ghost root && resolves h.Ghost.ghost p root path.Ghost.ghost}) @ unique  ->
    {r : result | effective_rewritten h.Ghost.ghost (Pref.own r.#state) r.#edits && r.#value === root
      && resolves h.Ghost.ghost p root r.#path && active (Pref.own r.#state) root
      && H.mem (Pref.own r.#state) root && terminal (Pref.own r.#state) root} @ unique = fun h scope p root path state ->
  let goal = {heap = h.Ghost.ghost; start = p; path = path.Ghost.ghost} in
  let lift : (((after : node Pref.heap) @ immutable ->
    (edits : {d : edits | effective_rewritten h.Ghost.ghost after d}) @ immutable ->
    {d : edits | effective_rewritten goal.heap after d} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun after edits -> edits)} in
  let out = walk_loop goal h scope p root path (state) lift in out

module E = Effective_level
module R = Representative_level

let (resolved_active @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (p : node Pref.t) @ immutable ->
    (root : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    {u : unit | H.mem h p && E.valid_head h heads p && resolves h p root path
      && (match E.level h heads p with Generic -> false | Finite n -> n >= 0)} ->
    {u : unit | active h root} @ ghost = fun h heads p root path premise -> ghost_ (
      E.valid_head_def h heads p;
      E.level_def h heads p; let expected = heads p in R.unique h p root path expected.root expected.path ();
      Compression_path_proofs.resolution_terminal h p root path ();
      active_def h root; ())

let representative :
    (h : node Pref.heap Ghost.t) @ immutable ->
    (heads : E.heads Ghost.t) @ immutable ->
    (valid : (((x : node Pref.t) @ immutable ->
      {u : unit | E.valid_head h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t) @ total ->
    (scope : (((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h.Ghost.ghost x) || source_ok h.Ghost.ghost x})) Ghost.t) @ total ->
    (p : node Pref.t) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p
      && (match E.level h.Ghost.ghost heads.Ghost.ghost p with Generic -> false | Finite n -> n >= 0)}) @ unique ->
    {r : result | effective_rewritten h.Ghost.ghost (Pref.own r.#state) r.#edits
      && resolves h.Ghost.ghost p r.#value r.#path
      && active (Pref.own r.#state) r.#value && H.mem (Pref.own r.#state) r.#value
      && terminal (Pref.own r.#state) r.#value} @ unique =
  fun h heads valid scope p state ->
    let input : {p : node Pref.t | H.mem h.Ghost.ghost p} = p in
    let found : {r : resolved | H.mem h.Ghost.ghost r.#value && terminal h.Ghost.ghost r.#value && resolves h.Ghost.ghost p r.#value r.#path} =
      let borrowed = borrow_ state in
      let borrowed : {t : node Pref.token | Pref.own t === h.Ghost.ghost} = borrowed in
      let found = Graph_representative.representative h scope input borrowed in
      found in
    let root = found.#value in let path = ghost_ found.#path in
    ghost_ (
      valid.Ghost.ghost p; resolved_active h.Ghost.ghost heads.Ghost.ghost p root path (); ());
    let path = {Ghost.ghost = ghost_ path} in
    let state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p && active h.Ghost.ghost root && resolves h.Ghost.ghost p root path.Ghost.ghost} = state in
    let state_argument = state in
    let out = walk h scope p root path (state_argument) in out
