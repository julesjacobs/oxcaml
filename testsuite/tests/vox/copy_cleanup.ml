open Copy_spec
open Copy_heap_proofs
open Generalize_spec
open Copy_cleanup_spec

let rec clear : (h : node Pref.heap Ghost.t) @ immutable ->
    (trail : pool) @ immutable ->
    (members : (((x : node Pref.t) @ immutable ->
      {u : unit | not (listed trail x) || H.mem h.Ghost.ghost x})) Ghost.t) @ total ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost}) @ unique ->
    {t : node Pref.token | Pref.own t === swept h.Ghost.ghost trail} @ unique =
  fun h trail members state ->
    let members = ghost_ members.Ghost.ghost in
    ghost_ (swept_def h.Ghost.ghost trail);
    match trail with
    | Empty -> state
    | Entry (p, rest) ->
      ghost_ (listed_def trail p; members p);
      let old = Pref.read p (borrow_ state) in
      let v = clear_memo old in
      let state = Pref.write p v state in
      let mid = ghost_ (H.put h.Ghost.ghost p v) in
      let tail : ((x : node Pref.t) @ immutable ->
        {u : unit | not (listed rest x) || H.mem mid x}) @ total ghost = ghost_ (fun x ->
        listed_def trail x; members x; ()) in
      let heap_witness : node Pref.heap Ghost.t = {Ghost.ghost = mid} in
      let members_witness : (((x : node Pref.t) @ immutable ->
        {u : unit | not (listed rest x) || H.mem heap_witness.Ghost.ghost x})) Ghost.t =
        {Ghost.ghost = ghost_ (tail)} in
      clear heap_witness rest members_witness state
