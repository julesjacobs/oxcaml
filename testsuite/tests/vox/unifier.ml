open Unifier_spec
open Unifier_proofs

let rec representative :
    (h : node Pref.heap) @ immutable ghost ->
    (scope : ((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h q) || scoped h q})) @ total ghost ->
    (p : {p : node Pref.t | H.mem h p}) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h}) @ local read ->
    {r : resolved | let refine_ p = p in H.mem h r.#value && terminal h r.#value
      && resolves h p r.#value r.#path.ghost} @ immutable =
  fun h scope p t ->
    let refine_ p = p in
    let refine_ t = t in
    ghost_ (scope p);
    ghost_ (scoped_def h p);
    let t : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let refine_ n = Pref.read p t in
    match n with
    | Link q ->
      let q : {q : node Pref.t | H.mem h q} = refine_ q in
      let refine_ t = t in
      let t : {t : node Pref.token | Pref.own t === h} = refine_ t in
      let refine_ r = representative h scope q t in
      let refine_ q = q in
      let path = ghost_ (Via (q, r.#path.ghost)) in
      let value = r.#value in
      ghost_ (resolves_def h p value path);
      let result = #{value = r.#value; path = {Ghost.ghost = path}} in refine_ result
    | Var | Bool | Arrow _ ->
      let path = ghost_ Here in
      ghost_ (terminal_def h p);
      ghost_ (resolves_def h p p path);
      let result = #{value = p; path = {Ghost.ghost = path}} in refine_ result

let rec occurs :
    (h : node Pref.heap) @ immutable ghost ->
    (scope : ((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h q) || scoped h q})) @ total ghost ->
    (needle : node Pref.t) @ immutable ->
    (p : node Pref.t) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h && H.mem h p}) @ local read ->
    {r : searched_result | searched h needle p r.#found r.#search.ghost} @ immutable =
  fun h scope needle p t ->
  let refine_ t = t in
  let equal = Pref.equal p needle in
  let refine_ equal = equal in
  let result : {r : searched_result | searched h needle p r.#found r.#search.ghost} = if equal then
    let trace = ghost_ Hit in
    let flag = true in
    ghost_ (searched_def h needle p flag trace);
    let r = #{found = true; search = {Ghost.ghost = trace}} in refine_ r
  else begin
    ghost_ (scope p);
    ghost_ (scoped_def h p);
    let n : {n : node | Some n === H.at h p} =
      let b : {b : node Pref.token | H.mem (Pref.own b) p} = refine_ t in
      let refine_ n = Pref.read p b in refine_ n in
    let refine_ n = n in
    match n with
    | Var | Bool ->
      let trace = ghost_ Leaf in
      let flag = false in
      ghost_ (searched_def h needle p flag trace);
      let r = #{found = false; search = {Ghost.ghost = trace}} in refine_ r
    | Link q ->
      let t : {t : node Pref.token | Pref.own t === h && H.mem h q} = refine_ t in
      let refine_ r = occurs h scope needle q t in
      let trace = ghost_ (Follow (q, r.#search.ghost)) in
      let found = r.#found in
      ghost_ (searched_def h needle p found trace);
      let r = #{found = r.#found; search = {Ghost.ghost = trace}} in refine_ r
    | Arrow (a, b) ->
      let t : {t : node Pref.token | Pref.own t === h && H.mem h a} = refine_ t in
      let refine_ left = occurs h scope needle a t in
      let refine_ t = t in
      if left.#found then
        let trace = ghost_ (Left (a, b, left.#search.ghost)) in
        let flag = true in
        ghost_ (searched_def h needle p flag trace);
        let r = #{found = true; search = {Ghost.ghost = trace}} in refine_ r
      else
        let t : {t : node Pref.token | Pref.own t === h && H.mem h b} = refine_ t in
        let refine_ right = occurs h scope needle b t in
        let trace = ghost_ (Both (a, b, left.#search.ghost, right.#search.ghost)) in
        let found = right.#found in
        ghost_ (searched_def h needle p found trace);
        let r = #{found = right.#found; search = {Ghost.ghost = trace}} in refine_ r
  end in
  result

let bind :
    (h : node Pref.heap) @ immutable ghost ->
    (scope : ((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h q) || scoped h q})) @ total ghost ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h && H.mem h p && H.mem h q
      && H.at h p === Some Var && terminal h q && not (p === q)}) @ unique ->
    {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation.ghost} @ unique =
  fun h scope p q t ->
  let refine_ t = t in
  let checked : {r : searched_result | searched h p q r.#found r.#search.ghost} =
    let b = borrow_ t in
    let b : {b : node Pref.token | Pref.own b === h && H.mem h q} = refine_ b in
    let refine_ checked = occurs h scope p q b in refine_ checked in
  let refine_ checked = checked in
  let search = ghost_ checked.#search.ghost in
  if checked.#found then
    let d = ghost_ (Occurs_left search) in
    let ok = false in
    ghost_ (unified_def h p q ok h d);
    let r = #{ok; state = t; derivation = {Ghost.ghost = d}} in
    (refine_ r : {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation.ghost})
  else
    let t : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ t in
    let link = Link q in
    let refine_ t = Pref.write p link t in
    let after = ghost_ (Pref.own (borrow_ t)) in
    let d = ghost_ (Bind_left search) in
    let ok = true in
    ghost_ (unified_def h p q ok after d);
    let r = #{ok; state = t; derivation = {Ghost.ghost = d}} in
    (refine_ r : {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation.ghost})

let rec unify :
    (h : node Pref.heap) @ immutable ghost ->
    (scope : ((q : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h q) || scoped h q})) @ total ghost ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (t : {t : node Pref.token | Pref.own t === h && H.mem h p && H.mem h q}) @ unique ->
    {r : result | unified h p q r.#ok (Pref.own r.#state) r.#derivation.ghost} @ unique =
  fun h scope p q t ->
  let refine_ t = t in
  let rp : {r : resolved | H.mem h r.#value && terminal h r.#value
    && resolves h p r.#value r.#path.ghost} =
    let p : {p : node Pref.t | H.mem h p} = refine_ p in
    let b = borrow_ t in
    let b : {b : node Pref.token | Pref.own b === h} = refine_ b in
    let refine_ r = representative h scope p b in refine_ r in
  let refine_ rp = rp in
  let sq : {r : resolved | H.mem h r.#value && terminal h r.#value
    && resolves h q r.#value r.#path.ghost} =
    let q : {q : node Pref.t | H.mem h q} = refine_ q in
    let b = borrow_ t in
    let b : {b : node Pref.token | Pref.own b === h} = refine_ b in
    let refine_ r = representative h scope q b in refine_ r in
  let refine_ sq = sq in
  let r = rp.#value in
  let s = sq.#value in
  let result : {answer : result |
      unified h r s answer.#ok (Pref.own answer.#state) answer.#derivation.ghost} =
    let refine_ equal = Pref.equal r s in
    let result = if equal then
      let d = ghost_ Same in
      let ok = true in
      ghost_ (unified_def h r s ok h d);
      #{ok; state = t; derivation = {Ghost.ghost = d}}
    else
      let n : {n : node | Some n === H.at h r} =
        let b = borrow_ t in
        let b : {b : node Pref.token | H.mem (Pref.own b) r} = refine_ b in
        let refine_ n = Pref.read r b in refine_ n in
      let refine_ n = n in
      let m : {m : node | Some m === H.at h s} =
        let b = borrow_ t in
        let b : {b : node Pref.token | H.mem (Pref.own b) s} = refine_ b in
        let refine_ m = Pref.read s b in refine_ m in
      let refine_ m = m in
      ghost_ (terminal_def h r);
      ghost_ (terminal_def h s);
      match n, m with
      | Var, _ ->
        let t : {t : node Pref.token | Pref.own t === h && H.mem h r && H.mem h s
          && H.at h r === Some Var && terminal h s && not (r === s)} = refine_ t in
        let refine_ answer = bind h scope r s t in answer
      | _, Var ->
        let t : {t : node Pref.token | Pref.own t === h && H.mem h s && H.mem h r
          && H.at h s === Some Var && terminal h r && not (s === r)} = refine_ t in
        let refine_ answer = bind h scope s r t in
        let ok = answer.#ok in
        let t = answer.#state in
        let after = ghost_ (Pref.own (borrow_ t)) in
        let d = ghost_ (Swap answer.#derivation.ghost) in
        ghost_ (unified_def h r s ok after d);
        #{ok; state = t; derivation = {Ghost.ghost = d}}
      | Bool, Bool ->
        let ok = true in
        let d = ghost_ Constants in
        ghost_ (unified_def h r s ok h d);
        #{ok; state = t; derivation = {Ghost.ghost = d}}
      | Arrow (a, b), Arrow (c, e) ->
        ghost_ (scope r);
        ghost_ (scope s);
        ghost_ (scoped_def h r);
        ghost_ (scoped_def h s);
        let t : {t : node Pref.token | Pref.own t === h && H.mem h a && H.mem h c} = refine_ t in
        let refine_ left = unify h scope a c t in
        let left_ok = left.#ok in
        let ld = ghost_ left.#derivation.ghost in
        let t = left.#state in
        let middle = ghost_ (Pref.own (borrow_ t)) in
        if left_ok then
          let scope_middle : ((x : node Pref.t) @ immutable ->
              {u : unit | not (H.mem middle x) || scoped middle x}) @ total ghost =
            ghost_ (fun x ->
              scope x;
              let u = () in
              unified_frame h a c left_ok middle ld x (refine_ u);
              refine_ u) in
          let proof = ghost_ (
            let u = () in
            unified_frame h a c left_ok middle ld b (refine_ u);
            unified_frame h a c left_ok middle ld e (refine_ u);
            let proof : {u : unit | H.mem middle b && H.mem middle e} = refine_ u in proof) in
          let refine_ proof = proof in
          let t : {t : node Pref.token | Pref.own t === middle && H.mem middle b && H.mem middle e} = refine_ t in
          let refine_ right = unify middle scope_middle b e t in
          let ok = right.#ok in
          let t = right.#state in
          let after = ghost_ (Pref.own (borrow_ t)) in
          let d = ghost_ (Children (a, b, c, e, middle, left_ok, ld, right.#derivation.ghost)) in
          ghost_ (unified_def h r s ok after d);
          #{ok; state = t; derivation = {Ghost.ghost = d}}
        else
          let ok = false in
          let d = ghost_ (Children (a, b, c, e, middle, left_ok, ld, Same)) in
          ghost_ (unified_def h r s ok middle d);
          #{ok; state = t; derivation = {Ghost.ghost = d}}
      | Bool, Arrow _ | Arrow _, Bool ->
        let ok = false in
        let d = ghost_ Clash in
        ghost_ (unified_def h r s ok h d);
        #{ok; state = t; derivation = {Ghost.ghost = d}}
      | Link _, _ | _, Link _ ->
        let proof = ghost_ (let u = () in (refine_ u : {u : unit | false})) in
        let refine_ proof = proof in assert false
    in refine_ result in
  let refine_ result = result in
  let ok = result.#ok in
  let t = result.#state in
  let after = ghost_ (Pref.own (borrow_ t)) in
  let d = ghost_ (Resolve (r, s, rp.#path.ghost, sq.#path.ghost, result.#derivation.ghost)) in
  ghost_ (unified_def h p q ok after d);
  let answer = #{ok; state = t; derivation = {Ghost.ghost = d}} in
  refine_ answer
