open Copy_spec
open Copy_heap_proofs
open Generalize_spec
open Copy_cleanup_spec

let rec clear : (h : node Pref.heap) @ immutable ghost ->
    (trail : pool) @ immutable ->
    (members : ((x : node Pref.t) @ immutable ->
      {u : unit | not (listed trail x) || H.mem h x})) @ total ghost ->
    (state : {t : node Pref.token | Pref.own t === h}) @ unique ->
    {t : node Pref.token | Pref.own t === swept h trail} @ unique =
  fun h trail members state ->
    let refine_ state = state in
    ghost_ (swept_def h trail);
    match trail with
    | Empty -> refine_ state
    | Entry (p, rest) ->
      ghost_ (listed_def trail p; members p);
      let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ old = Pref.read p (borrow_ state) in
      let refine_ state = state in
      let v = clear_memo old in
      let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ state = Pref.write p v state in
      let mid = ghost_ (H.put h p v) in
      let tail : ((x : node Pref.t) @ immutable ->
        {u : unit | not (listed rest x) || H.mem mid x}) @ total ghost = ghost_ (fun x ->
        listed_def trail x; members x; put_frame h p v x;
        let u = () in refine_ u) in
      let state : {t : node Pref.token | Pref.own t === mid} = refine_ state in
      let refine_ state = clear mid rest tail state in refine_ state
