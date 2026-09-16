open Copy_spec
open Copy_heap_proofs
open Generalize_spec
open Copy_cleanup_spec

let rec clear : (h : Pref.heap Ghost.t) @ immutable ->
    (trail : pool) @ immutable ->
    (members : (((x : node Pref.t) @ immutable ->
      {u : unit | not (listed trail x) || H.mem h.Ghost.ghost x})) Ghost.t) @ total ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost}) @ unique ->
    {t : Pref.token | Pref.own t === swept h.Ghost.ghost trail} @ unique =
  fun h trail members state ->
    let members = ghost_ members.Ghost.ghost in
    let refine_ state = state in
    ghost_ (swept_def h.Ghost.ghost trail);
    match trail with
    | Empty -> refine_ state
    | Entry (p, rest) ->
      ghost_ (listed_def trail p; members p);
      let state : {t : Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ old = Pref.read p (borrow_ state) in
      let refine_ state = state in
      let v = clear_memo old in
      let state : {t : Pref.token | H.mem (Pref.own t) p} = refine_ state in
      let refine_ state = Pref.write p v state in
      let mid = ghost_ (H.put h.Ghost.ghost p v) in
      let tail : ((x : node Pref.t) @ immutable ->
        {u : unit | not (listed rest x) || H.mem mid x}) @ total ghost = ghost_ (fun x ->
        listed_def trail x; members x; put_frame h.Ghost.ghost p v x;
        let u = () in refine_ u) in
      let state : {t : Pref.token | Pref.own t === mid} = refine_ state in
      let heap_witness : Pref.heap Ghost.t = {Ghost.ghost = mid} in
      let members_witness : (((x : node Pref.t) @ immutable ->
        {u : unit | not (listed rest x) || H.mem heap_witness.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (refine_ tail)} in
      let refine_ state = state in
      let refine_ state = clear heap_witness rest members_witness (refine_ state) in refine_ state
