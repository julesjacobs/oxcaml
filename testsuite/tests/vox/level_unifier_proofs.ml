open Marked_occurs_proofs
open Copy_spec
open Level_unifier_spec
open Level_spec
open Level_proofs

let (observe_write @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | observe (H.put h p v) x === (if x === p then Some v.desc else observe h x)} @ ghost =
  fun h p v x -> ghost_ (let after = H.put h p v in observe_def after x; observe_def h x; let u = () in refine_ u)

let (redirect_desc @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> {u : unit | (redirect h p q).desc === Link q} @ ghost =
  fun h p q -> ghost_ (redirect_def h p q; let desc = Link q in cell_def desc 0; let u = () in refine_ u)

let (lower_observe @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | lower_valid h bound d} ->
    {u : unit | observe h x === observe (lower_heap h bound d) x
      && H.mem h x === H.mem (lower_heap h bound d) x} @ ghost = fun h bound d x premise -> ghost_ (
  let refine_ premise = premise in let after = lower_heap h bound d in
  let u = () in lowering_at h bound d x (refine_ u); lower_frame_def h after x;
  observe_def h x; observe_def after x; refine_ u)
let (lower_scoped @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | lower_valid h bound d} ->
    {u : unit | scoped h x === scoped (lower_heap h bound d) x} @ ghost = fun h bound d x premise -> ghost_ (
  let refine_ premise = premise in let after = lower_heap h bound d in
  let u = () in lower_observe h bound d x (refine_ u); scoped_def h x; scoped_def after x;
  (match observe h x with Some (Link q) -> lower_observe h bound d q (refine_ u); ()
    | Some (Arrow (a, b)) -> lower_observe h bound d a (refine_ u); lower_observe h bound d b (refine_ u); () | _ -> ()); refine_ u)
let (lower_equation @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | lower_valid h bound d} ->
    {u : unit | node_equation h rho x === node_equation (lower_heap h bound d) rho x} @ ghost = fun h bound d rho x premise -> ghost_ (
  let refine_ premise = premise in let after = lower_heap h bound d in let u = () in
  lower_observe h bound d x (refine_ u); node_equation_def h rho x; node_equation_def after rho x; refine_ u)

let rec (resolution_model @ total) :
    (h : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((p : node Pref.t) @ immutable ->
      {u : unit | node_equation h rho p})) @ total ->
    (p : node Pref.t) @ immutable -> (r : node Pref.t) @ immutable ->
    (path : resolution) @ immutable ->
    {u : unit | resolves h p r path} ->
    {u : unit | rho p === rho r} @ ghost =
  fun h rho model p r path premise ->
  ghost_ (
    let refine_ premise = premise in
    resolves_def h p r path;
    match path with
    | Here -> let u = () in refine_ u
    | Via (q, rest) ->
      model p;
      node_equation_def h rho p;
      let u = () in
      resolution_model h rho model q r rest (refine_ u);
      refine_ u)

let (bind_scope @ total) :
    (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && H.mem h q && (not (H.mem h x) || scoped h x)} ->
    {u : unit | not (H.mem (H.put h p (redirect h p q)) x)
      || scoped (H.put h p (redirect h p q)) x} @ ghost =
  fun h p q x premise ->
  ghost_ (
    let refine_ premise = premise in
    scoped_def h x;
    let changed = H.put h p (redirect h p q) in
    let v = redirect h p q in redirect_desc h p q; observe_write h p v x;
    scoped_def changed x;
    let u = () in refine_ u)

let (bind_model_backward @ total) :
    (h : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation h rho x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | rho p === rho q} ->
    {u : unit | node_equation (H.put h p (redirect h p q)) rho x} @ ghost =
  fun h rho p q model x premise ->
  ghost_ (
    let refine_ premise = premise in
    let changed = H.put h p (redirect h p q) in
    let v = redirect h p q in redirect_desc h p q; observe_write h p v x;
    model x;
    node_equation_def h rho x;
    node_equation_def changed rho x;
    let u = () in refine_ u)

let rec (unified_frame @ total) :
    (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} ->
    {u : unit | H.mem after x = H.mem h x
      && (not (H.mem h x) || not (scoped h x) || scoped after x)} @ ghost =
  fun h p q ok after d x premise ->
  ghost_ (
    let refine_ premise = premise in
    unified_def h p q ok after d;
    match d with
    | Same | Constants | Occurs_left _ | Occurs_right _ | Clash ->
      let u = () in refine_ u
    | Bind_left search ->
      let flag = false in
      searched_def h p q flag search;
      if H.mem h x && scoped h x then (
        let u = () in bind_scope h p q x (refine_ u); refine_ u)
      else let u = () in refine_ u
    | Bind_right search ->
      let flag = false in
      searched_def h q p flag search;
      if H.mem h x && scoped h x then (
        let u = () in bind_scope h q p x (refine_ u); refine_ u)
      else let u = () in refine_ u
    | Scanned (needle, marks, rest) ->
      let u = () in let mid = scan_heap h marks in
      scan_observe h needle marks x (refine_ u); scan_scoped h needle marks x (refine_ u);
      unified_frame mid p q ok after rest x (refine_ u); refine_ u
    | Lowering (bound, edits, _, rest) ->
      let u = () in let mid = lower_heap h bound edits in
      lower_observe h bound edits x (refine_ u); lower_scoped h bound edits x (refine_ u);
      unified_frame mid p q ok after rest x (refine_ u); refine_ u
    | Swap rest ->
      let u = () in
      unified_frame h q p ok after rest x (refine_ u); refine_ u
    | Resolve (r, s, rp, sq, rest) ->
      let u = () in
      unified_frame h r s ok after rest x (refine_ u);
      refine_ u
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let u = () in
      unified_frame h a c left_ok middle left x (refine_ u);
      if left_ok then (
        unified_frame middle b e ok after right x (refine_ u);
        refine_ u)
      else refine_ u)

let rec (unified_edits @ total) :
    (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : derivation) @ immutable ->
    {u : unit | unified h p q ok after d} ->
    {u : unit | after === apply_edits h (writes p q d)
      && valid_edits h (writes p q d)} @ ghost =
  fun h p q ok after d premise -> ghost_ (
    let refine_ premise = premise in
    unified_def h p q ok after d;
    writes_def p q d;
    let edits = writes p q d in
    apply_edits_def h edits;
    valid_edits_def h edits;
    let u = () in
    match d with
    | Same | Constants | Bind_left _ | Bind_right _
    | Occurs_left _ | Occurs_right _ | Clash -> refine_ u
    | Scanned (needle, marks, rest) ->
      let mid = scan_heap h marks in
      unified_edits mid p q ok after rest (refine_ u);
      let first = Scanned_edit (needle, marks) in
      apply_edits_def h first; valid_edits_def h first; refine_ u
    | Lowering (bound, levels, _, rest) ->
      let mid = lower_heap h bound levels in
      unified_edits mid p q ok after rest (refine_ u);
      let first = Lowered (bound, levels) in apply_edits_def h first; valid_edits_def h first; refine_ u
    | Swap rest ->
      unified_edits h q p ok after rest (refine_ u); refine_ u
    | Resolve (r, s, _, _, rest) ->
      unified_edits h r s ok after rest (refine_ u); refine_ u
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      unified_edits h a c left_ok middle left (refine_ u);
      if left_ok then (
        unified_edits middle b e ok after right (refine_ u); refine_ u)
      else refine_ u)

let rec (success_backward_at @ total) :
    (h : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q true after d && rho p === rho q} ->
    {u : unit | node_equation after rho x} @ ghost =
  fun h rho model p q after d x premise -> ghost_ (
    let refine_ premise = premise in
    let ok = true in
    unified_def h p q ok after d;
    let u = () in
    match d with
    | Same | Constants | Occurs_left _ | Occurs_right _ | Clash ->
      model x; refine_ u
    | Bind_left _ ->
      bind_model_backward h rho p q model x (refine_ u); refine_ u
    | Bind_right _ ->
      bind_model_backward h rho q p model x (refine_ u); refine_ u
    | Scanned (needle, marks, rest) ->
      let mid = scan_heap h marks in
      let model_mid : ((x : node Pref.t) @ immutable -> {u : unit | node_equation mid rho x}) @ total =
        fun x -> model x; let u = () in scan_equation h needle marks rho x (refine_ u); refine_ u in
      success_backward_at mid rho model_mid p q after rest x (refine_ u); refine_ u
    | Lowering (bound, edits, _, rest) ->
      let mid = lower_heap h bound edits in
      let model_mid : ((x : node Pref.t) @ immutable -> {u : unit | node_equation mid rho x}) @ total =
        fun x -> model x; let u = () in lower_equation h bound edits rho x (refine_ u); refine_ u in
      success_backward_at mid rho model_mid p q after rest x (refine_ u); refine_ u
    | Swap rest ->
      success_backward_at h rho model q p after rest x (refine_ u); refine_ u
    | Resolve (r, s, rp, sq, rest) ->
      resolution_model h rho model p r rp (refine_ u);
      resolution_model h rho model q s sq (refine_ u);
      success_backward_at h rho model r s after rest x (refine_ u); refine_ u
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      model p; model q;
      node_equation_def h rho p; node_equation_def h rho q;
      let middle_model : (x : node Pref.t) @ immutable ->
          {u : unit | node_equation middle rho x} @ total = fun x ->
        let u = () in
        let refine_ proof =
          success_backward_at h rho model a c middle left x (refine_ u) in
        refine_ u in
      success_backward_at middle rho middle_model b e after right x (refine_ u);
      refine_ u)

let rec (success_forward_at @ total) :
    (h : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation after rho x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q true after d} ->
    {u : unit | node_equation h rho x && rho p === rho q} @ ghost =
  fun h rho p q after d model x premise -> ghost_ (
    let refine_ premise = premise in
    let ok = true in
    unified_def h p q ok after d;
    let u = () in
    match d with
    | Same | Occurs_left _ | Occurs_right _ | Clash -> model x; refine_ u
    | Constants ->
      model x; model p; model q;
      node_equation_def h rho p; node_equation_def h rho q;
      refine_ u
    | Bind_left _ ->
      let v = redirect h p q in redirect_desc h p q; observe_write h p v x; observe_write h p v p;
      model x; model p;
      node_equation_def after rho x; node_equation_def after rho p;
      node_equation_def h rho x;
      refine_ u
    | Bind_right _ ->
      let v = redirect h q p in redirect_desc h q p; observe_write h q v x; observe_write h q v q;
      model x; model q;
      node_equation_def after rho x; node_equation_def after rho q;
      node_equation_def h rho x;
      refine_ u
    | Scanned (needle, marks, rest) ->
      let mid = scan_heap h marks in
      success_forward_at mid rho p q after rest model x (refine_ u);
      scan_equation h needle marks rho x (refine_ u); refine_ u
    | Lowering (bound, edits, _, rest) ->
      let mid = lower_heap h bound edits in
      success_forward_at mid rho p q after rest model x (refine_ u);
      lower_equation h bound edits rho x (refine_ u); refine_ u
    | Swap rest ->
      success_forward_at h rho q p after rest model x (refine_ u); refine_ u
    | Resolve (r, s, rp, sq, rest) ->
      let before_model : (x : node Pref.t) @ immutable ->
          {u : unit | node_equation h rho x} @ total = fun x ->
        let u = () in
        let refine_ proof =
          success_forward_at h rho r s after rest model x (refine_ u) in
        refine_ u in
      resolution_model h rho before_model p r rp (refine_ u);
      resolution_model h rho before_model q s sq (refine_ u);
      success_forward_at h rho r s after rest model x (refine_ u);
      refine_ u
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let middle_model : (x : node Pref.t) @ immutable ->
          {u : unit | node_equation middle rho x} @ total = fun x ->
        let u = () in
        let refine_ proof =
          success_forward_at middle rho b e after right model x (refine_ u) in
        refine_ u in
      success_forward_at h rho a c middle left middle_model p (refine_ u);
      success_forward_at h rho a c middle left middle_model q (refine_ u);
      node_equation_def h rho p; node_equation_def h rho q;
      success_forward_at middle rho b e after right model x (refine_ u);
      success_forward_at h rho a c middle left middle_model x (refine_ u);
      refine_ u)

let rec (weight_positive @ total) : (t : ty) @ immutable ->
    {u : unit | weight t > Bigint.zero} @ ghost = fun t -> ghost_ (
  weight_def t;
  match t with
  | Variable _ | Boolean -> let u = () in refine_ u
  | Function (a, b) ->
    let refine_ pa = weight_positive a in
    let refine_ pb = weight_positive b in
    let u = () in refine_ u)

let rec (search_bound @ total) :
    (h : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation h rho x})) @ total ->
    (needle : node Pref.t) @ immutable -> (p : node Pref.t) @ immutable ->
    (trace : search) @ immutable ->
    {u : unit | searched h needle p true trace} ->
    {u : unit | weight (rho needle) <= weight (rho p)
      && (not (terminal h p) || p === needle
        || weight (rho needle) < weight (rho p))} @ ghost =
  fun h rho model needle p trace premise -> ghost_ (
    let refine_ premise = premise in
    let found = true in
    searched_def h needle p found trace;
    terminal_def h p;
    model p;
    node_equation_def h rho p;
    let u = () in
    match trace with
    | Hit | Leaf -> refine_ u
    | Follow (q, rest) ->
      search_bound h rho model needle q rest (refine_ u);
      refine_ u
    | Left (a, b, left) ->
      search_bound h rho model needle a left (refine_ u);
      let ta = rho a in let tb = rho b in
      let refine_ pa = weight_positive ta in
      let refine_ pb = weight_positive tb in
      let arrow = Function (ta, tb) in weight_def arrow;
      refine_ u
    | Both (a, b, left, right) ->
      search_bound h rho model needle b right (refine_ u);
      let ta = rho a in let tb = rho b in
      let refine_ pa = weight_positive ta in
      let refine_ pb = weight_positive tb in
      let arrow = Function (ta, tb) in weight_def arrow;
      refine_ u)

let rec (failure_refutes @ total) :
    (h : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    {u : unit | unified h p q false after d && rho p === rho q} ->
    {u : unit | false} @ ghost =
  fun h rho model p q after d premise -> ghost_ (
    let refine_ premise = premise in
    let ok = false in
    unified_def h p q ok after d;
    let u = () in
    match d with
    | Same | Constants | Bind_left _ | Bind_right _ -> refine_ u
    | Occurs_left search ->
      search_bound h rho model p q search (refine_ u); refine_ u
    | Occurs_right search ->
      search_bound h rho model q p search (refine_ u); refine_ u
    | Clash ->
      model p; model q;
      node_equation_def h rho p; node_equation_def h rho q;
      refine_ u
    | Scanned (needle, marks, rest) ->
      let mid = scan_heap h marks in
      let model_mid : ((x : node Pref.t) @ immutable -> {u : unit | node_equation mid rho x}) @ total =
        fun x -> model x; let u = () in scan_equation h needle marks rho x (refine_ u); refine_ u in
      failure_refutes mid rho model_mid p q after rest (refine_ u); refine_ u
    | Lowering (bound, edits, _, rest) ->
      let mid = lower_heap h bound edits in
      let model_mid : ((x : node Pref.t) @ immutable -> {u : unit | node_equation mid rho x}) @ total =
        fun x -> model x; let u = () in lower_equation h bound edits rho x (refine_ u); refine_ u in
      failure_refutes mid rho model_mid p q after rest (refine_ u); refine_ u
    | Swap rest ->
      failure_refutes h rho model q p after rest (refine_ u); refine_ u
    | Resolve (r, s, rp, sq, rest) ->
      resolution_model h rho model p r rp (refine_ u);
      resolution_model h rho model q s sq (refine_ u);
      failure_refutes h rho model r s after rest (refine_ u); refine_ u
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      model p; model q;
      node_equation_def h rho p; node_equation_def h rho q;
      if left_ok then (
        let middle_model : (x : node Pref.t) @ immutable ->
            {u : unit | node_equation middle rho x} @ total = fun x ->
          model x;
          let u = () in
          let refine_ proof =
            success_backward_at h rho model a c middle left x (refine_ u) in
          refine_ u in
        failure_refutes middle rho middle_model b e after right (refine_ u);
        refine_ u)
      else (
        failure_refutes h rho model a c middle left (refine_ u);
        refine_ u))
