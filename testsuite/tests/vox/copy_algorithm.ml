open Copy_spec
open Copy_heap_proofs

let finish : (saved : node Pref.heap) @ immutable ghost -> (epoch : node Pref.t) @ immutable ->
    (depth : int) -> (d : history) @ immutable ghost ->
    (p : {p : node Pref.t | H.mem saved p && source_ok saved p}) @ immutable ->
    (dest : destination) @ immutable ->
    (t : {t : node Pref.token | let refine_ p = p in valid saved epoch depth d && Pref.own t === heap saved epoch depth d
      && match H.at saved p with None -> false | Some v ->
        v.level === Generic && prepared saved d v.desc dest}) @ unique ->
    {r : copied | let refine_ p = p in valid saved epoch depth r.#history
      && Pref.own r.#state === heap saved epoch depth r.#history
      && extends d r.#history && target_for saved r.#history p r.#value} @ unique =
  fun saved epoch depth d p dest t ->
    let refine_ p = p in let refine_ t = t in
    let h = ghost_ (heap saved epoch depth d) in
    ghost_ (let u = () in history_at saved epoch depth d p (refine_ u));
    let t : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let refine_ old = Pref.read p (borrow_ t) in let refine_ t = t in
    ghost_ (let u = () in memo_lookup saved epoch depth d p old (refine_ u));
    let hit = match old.memo with Empty_memo -> None | Memo (stamp, q) ->
      let refine_ equal = Pref.equal stamp epoch in if equal then Some q else None in
    match hit with
    | Some value ->
      ghost_ (target_for_def saved d p value; extends_def d d);
      let r = #{value; state = t; history = d} in refine_ r
    | None ->
      ghost_ (prepared_def saved d old.desc dest);
      (match dest with
      | Allocate desc ->
        let v = cell desc depth in let refine_ step = Pref.alloc v t in
        let value = step.value in let t = step.state in
        ghost_ (put_frame h value v p);
        let t : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ t in
        let w = mark old epoch value in let refine_ t = Pref.write p w t in
        let history = ghost_ (Fresh (d, p, value, old, desc)) in
        ghost_ (valid_def saved epoch depth history; heap_def saved epoch depth history;
          mapping_def history p; target_for_def saved history p value; extends_def d history; extends_def d d);
        let r = #{value; state = t; history} in refine_ r
      | Share value ->
        let t : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ t in
        let w = mark old epoch value in let refine_ t = Pref.write p w t in
        let history = ghost_ (Alias (d, p, value, old)) in
        ghost_ (valid_def saved epoch depth history; heap_def saved epoch depth history;
          mapping_def history p; target_for_def saved history p value; extends_def d history; extends_def d d);
        let r = #{value; state = t; history} in refine_ r)

let rec copy : (saved : node Pref.heap) @ immutable ghost ->
    (scope : ((p : node Pref.t) @ immutable -> {u : unit | if H.mem saved p then source_ok saved p else H.at saved p === None})) @ total ghost ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ghost ->
    (p : {p : node Pref.t | H.mem saved p}) @ immutable ->
    (t : {t : node Pref.token | let refine_ p = p in valid saved epoch depth d && Pref.own t === heap saved epoch depth d}) @ unique ->
    {r : copied | let refine_ p = p in valid saved epoch depth r.#history
      && Pref.own r.#state === heap saved epoch depth r.#history
      && extends d r.#history && target_for saved r.#history p r.#value} @ unique =
  fun saved scope epoch depth d p t ->
    let refine_ p = p in let refine_ t = t in
    ghost_ (scope p; let u = () in history_at saved epoch depth d p (refine_ u));
    let t : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let refine_ old = Pref.read p (borrow_ t) in let refine_ t = t in
    match old.level with
    | Finite _ ->
      ghost_ (target_for_def saved d p p; extends_def d d);
      let r = #{value = p; state = t; history = d} in refine_ r
    | Generic ->
      ghost_ (let u = () in memo_lookup saved epoch depth d p old (refine_ u));
      let hit = match old.memo with Empty_memo -> None | Memo (stamp, q) ->
        let refine_ equal = Pref.equal stamp epoch in if equal then Some q else None in
      match hit with
      | Some value ->
        ghost_ (target_for_def saved d p value; extends_def d d);
        let r = #{value; state = t; history = d} in refine_ r
      | None ->
        ghost_ (source_ok_def saved p; payload_scoped_def saved old);
        let p : {p : node Pref.t | H.mem saved p && source_ok saved p} = refine_ p in
        match old.desc with
        | Var | Bool ->
          let dest = Allocate old.desc in
          ghost_ (prepared_def saved d old.desc dest; ready_def saved d old.desc old.desc);
          let t : {t : node Pref.token | let refine_ p = p in valid saved epoch depth d && Pref.own t === heap saved epoch depth d
            && match H.at saved p with None -> false | Some v ->
              v.level === Generic && prepared saved d v.desc dest} = refine_ t in
          let refine_ r = finish saved epoch depth d p dest t in refine_ r
        | Link child ->
          let child : {p : node Pref.t | H.mem saved p} = refine_ child in
          let t : {t : node Pref.token | let refine_ p = child in valid saved epoch depth d && Pref.own t === heap saved epoch depth d} = refine_ t in
          let refine_ r = copy saved scope epoch depth d child t in
          let history = ghost_ r.#history in let dest = Share r.#value in
          ghost_ (prepared_def saved history old.desc dest);
          let t = r.#state in
          let t : {t : node Pref.token | let refine_ p = p in valid saved epoch depth history && Pref.own t === heap saved epoch depth history
            && match H.at saved p with None -> false | Some v ->
              v.level === Generic && prepared saved history v.desc dest} = refine_ t in
          let refine_ out = finish saved epoch depth history p dest t in
          ghost_ (let u = () in extension_trans d history out.#history (refine_ u));
          let r = #{value = out.#value; state = out.#state; history = out.#history} in refine_ r
        | Arrow (a, b) ->
          let a : {p : node Pref.t | H.mem saved p} = refine_ a in
          let t : {t : node Pref.token | let refine_ p = a in valid saved epoch depth d && Pref.own t === heap saved epoch depth d} = refine_ t in
          let refine_ left = copy saved scope epoch depth d a t in
          let d1 = ghost_ left.#history in let b : {p : node Pref.t | H.mem saved p} = refine_ b in
          let t = left.#state in
          let t : {t : node Pref.token | let refine_ p = b in valid saved epoch depth d1 && Pref.own t === heap saved epoch depth d1} = refine_ t in
          let refine_ right = copy saved scope epoch depth d1 b t in
          let d2 = ghost_ right.#history in let desc = Arrow (left.#value, right.#value) in let dest = Allocate desc in
          ghost_ (let refine_ a = a in let refine_ b = b in let u = () in
            target_preserved saved epoch depth d1 d2 a left.#value (refine_ u);
            ready_def saved d2 old.desc desc; prepared_def saved d2 old.desc dest;
            let proof : {u : unit | prepared saved d2 old.desc dest} = refine_ u in proof);
          let t = right.#state in
          let t : {t : node Pref.token | let refine_ p = p in valid saved epoch depth d2 && Pref.own t === heap saved epoch depth d2
            && match H.at saved p with None -> false | Some v ->
              v.level === Generic && prepared saved d2 v.desc dest} = refine_ t in
          let refine_ out = finish saved epoch depth d2 p dest t in
          ghost_ (let u = () in extension_trans d d1 d2 (refine_ u);
            extension_trans d d2 out.#history (refine_ u));
          let r = #{value = out.#value; state = out.#state; history = out.#history} in refine_ r

let instantiate : (saved : node Pref.heap) @ immutable ghost ->
    (scope : ((p : node Pref.t) @ immutable -> {u : unit | if H.mem saved p then source_ok saved p else H.at saved p === None})) @ total ghost ->
    (depth : {depth : int | depth >= 0}) -> (p : {p : node Pref.t | H.mem saved p}) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === saved}) @ unique ->
    {r : instance | let refine_ p = p in let refine_ depth = depth in valid saved r.#epoch depth r.#history
      && Pref.own r.#state === heap saved r.#epoch depth r.#history
      && target_for saved r.#history p r.#value} @ unique = fun saved scope depth p t ->
  let refine_ depth = depth in let refine_ t = t in
  let v = cell Bool depth in let refine_ step = Pref.alloc v t in
  let epoch = step.value in let d = ghost_ Start in
  ghost_ (valid_def saved epoch depth d; heap_def saved epoch depth d);
  let t = step.state in
  let t : {t : node Pref.token | let refine_ p = p in valid saved epoch depth d && Pref.own t === heap saved epoch depth d} = refine_ t in
  let refine_ r = copy saved scope epoch depth d p t in let refine_ p = p in
  let out = #{value = r.#value; state = r.#state; epoch; history = r.#history} in refine_ out
