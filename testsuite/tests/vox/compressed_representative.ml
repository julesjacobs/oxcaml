open Copy_spec
open Level_spec
open Level_unifier_spec
open Compression_spec

let rec walk : (h : node Pref.heap) @ immutable ghost ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ghost ->
    (p : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable -> (path : resolution) @ immutable ghost ->
    (state : {t : node Pref.token | Pref.own t === h && active h p && resolves h p root path}) @ unique ->
    {r : result | rewritten h (Pref.own r.#state) r.#edits && r.#value === root
      && resolves h p root r.#path && active (Pref.own r.#state) root
      && H.mem (Pref.own r.#state) root && terminal (Pref.own r.#state) root} @ unique =
  fun h scope p root path state ->
    let refine_ state = state in
    ghost_ (let u = () in Compression_path_proofs.resolution_terminal h p root path (refine_ u);
      Level_unifier_metadata.resolution_active h scope p root path (refine_ u); ());
    let refine_ equal = Pref.equal p root in
    if equal then (
      let edits = ghost_ Done in ghost_ (rewritten_def h h edits);
      let r = #{value = root; state; edits; path} in refine_ r)
    else (
      ghost_ (resolves_def h p root path);
      let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ old = Pref.read p (borrow_ state) in let refine_ state = state in
      ghost_ (observe_def h p);
      match old.desc with
      | Link q ->
        let refine_ direct = Pref.equal q root in
        if direct then (
          let edits = ghost_ Done in ghost_ (rewritten_def h h edits);
          let r = #{value = root; state; edits; path} in refine_ r
        ) else (
        let rest = ghost_ (Compression_path_proofs.tail path) in
        ghost_ (let u = () in Compression_path_proofs.tail_resolves h p q root path (refine_ u);
          scope p; finite_scope_def h p; source_ok_def h p; ());
        let next_path : {d : resolution | resolves (H.put h p (redirect h p root)) q root d} @ immutable ghost = ghost_ (let u = () in
          let refine_ d = Compression_path_proofs.redirect_resolution h p root q rest (refine_ u) in refine_ d) in
        let link = {old with desc = Link root} in ghost_ (redirect_def h p root);
        let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
        let refine_ state = Pref.write p link state in
        let middle = ghost_ (Pref.own (borrow_ state)) in
        let next_scope : ((x : node Pref.t) @ immutable ->
          {u : unit | not (H.mem middle x) || finite_scope middle x}) @ total ghost = ghost_ (fun x ->
            scope x; let u = () in Level_unifier_metadata.redirect_scope h p root x (refine_ u); refine_ u) in
        ghost_ (let u = () in Level_unifier_metadata.redirect_active h p root q (refine_ u));
        let refine_ next_path = next_path in
        let state : {t : node Pref.token | Pref.own t === middle && active middle q && resolves middle q root next_path} = refine_ state in
        let refine_ out = walk middle next_scope q root next_path state in
        let after = ghost_ (Pref.own (borrow_ out.#state)) in
        let edits = ghost_ (Write (p, q, root, path, out.#edits)) in
        ghost_ (rewritten_def h after edits);
        let r = #{value = root; state = out.#state; edits; path} in refine_ r
      )
      | Var | Bool | Arrow _ ->
        ghost_ (let impossible : {u : unit | false} = refine_ () in let refine_ impossible = impossible in ()); assert false)

let representative : (h : node Pref.heap) @ immutable ghost ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ghost ->
    (p : node Pref.t) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h && H.mem h p && active h p}) @ unique ->
    {r : result | rewritten h (Pref.own r.#state) r.#edits && resolves h p r.#value r.#path
      && active (Pref.own r.#state) r.#value && H.mem (Pref.own r.#state) r.#value
      && terminal (Pref.own r.#state) r.#value} @ unique = fun h scope p state ->
    let refine_ state = state in
    let found : {r : resolved | H.mem h r.#value && active h r.#value && terminal h r.#value
      && resolves h p r.#value r.#path} =
      let input : {p : node Pref.t | H.mem h p && active h p} = refine_ p in
      let borrowed = borrow_ state in
      let borrowed : {t : node Pref.token | Pref.own t === h} = refine_ borrowed in
      let refine_ found = Level_unifier.representative h scope input borrowed in
      let refine_ input = input in refine_ found in
    let refine_ found = found in
    let path = ghost_ found.#path in let root = found.#value in
    let state : {t : node Pref.token | Pref.own t === h && active h p && resolves h p root path} = refine_ state in
    let refine_ out = walk h scope p root path state in refine_ out
