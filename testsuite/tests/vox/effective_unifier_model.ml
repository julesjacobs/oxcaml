open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_unifier_proofs
open Level_finite_spec

let (lower_equation @ total) : (h : Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | Terminal_lower_spec.terminal_valid h bound d} ->
    {u : unit | node_equation h rho x === node_equation (lower_heap h bound d) rho x} @ ghost =
  fun h bound d rho x premise -> ghost_ (
    let refine_ premise = premise in let after = lower_heap h bound d in let u = () in
    Terminal_lower_proofs.lowering_at h bound d x (refine_ u);
    lower_frame_def h after x; observe_def h x; observe_def after x;
    node_equation_def h rho x; node_equation_def after rho x; refine_ u)

open Effective_unifier_spec
let rec (success_backward_at @ total) :
    (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q true after d && rho p === rho q} ->
    {u : unit | node_equation after rho x} @ ghost =
  fun h rho model p q after d x premise -> ghost_ (
    let refine_ premise = premise in
    let ok = true in
    unified_def h p q ok after d;
    let u = () in
    match d with
    | Swap rest -> success_backward_at h rho model q p after rest x (refine_ u); refine_ u
    | Scanned (needle, marks, rest) ->  let middle = scan_heap h marks in
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        model y; let u = () in Marked_occurs_proofs.scan_equation h needle marks rho y (refine_ u); refine_ u in
      success_backward_at middle rho next p q after rest x (refine_ u); refine_ u
    | Terminal_lower (bound, edits, tree, rest) -> let middle = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q middle edits tree;
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        model y; let u = () in lower_equation h bound edits rho y (refine_ u); refine_ u in
      success_backward_at middle rho next p q after rest x (refine_ u); refine_ u
    | Base old -> Level_unifier_proofs.success_backward_at h rho model p q after old x (refine_ u); refine_ u
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
      refine_ u
    | Post_link (middle, rest, source, target) ->
      Structure_spec.linkable_def middle source target;
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let u = () in let refine_ u = success_backward_at h rho model p q middle rest y (refine_ u) in refine_ u in
      Structure_model_proofs.forward middle source target rho next x (refine_ u); refine_ u
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let u = () in let refine_ u = Effective_compression_proofs.forward h middle edits rho model y (refine_ u) in refine_ u in
      success_backward_at middle rho next p q after rest x (refine_ u); refine_ u)

let rec (success_forward_at @ total) :
    (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
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
    | Swap rest -> success_forward_at h rho q p after rest model x (refine_ u); refine_ u
    | Scanned (needle, marks, rest) ->  let middle = scan_heap h marks in
      success_forward_at middle rho p q after rest model x (refine_ u);
      Marked_occurs_proofs.scan_equation h needle marks rho x (refine_ u); refine_ u
    | Terminal_lower (bound, edits, tree, rest) -> let middle = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q middle edits tree;
      success_forward_at middle rho p q after rest model x (refine_ u);
      lower_equation h bound edits rho x (refine_ u); refine_ u
    | Base old -> Level_unifier_proofs.success_forward_at h rho p q after old model x (refine_ u); refine_ u
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
      refine_ u
    | Post_link (middle, rest, source, target) ->
      Structure_spec.linkable_def middle source target;
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let u = () in let refine_ u = Structure_model_proofs.backward middle source target rho (refine_ model) y (refine_ u) in refine_ u in
      success_forward_at h rho p q middle rest next x (refine_ u); refine_ u
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let u = () in let refine_ u = success_forward_at middle rho p q after rest model y (refine_ u) in refine_ u in
      success_forward_at middle rho p q after rest model x (refine_ u);
      Effective_compression_proofs.backward h middle edits rho next x (refine_ u); refine_ u)

let rec (failure_refutes @ total) :
    (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    {u : unit | unified h p q false after d && rho p === rho q} ->
    {u : unit | false} @ ghost =
  fun h rho model p q after d premise -> ghost_ (
    let refine_ premise = premise in
    let ok = false in
    unified_def h p q ok after d;
    let u = () in
    match d with
    | Swap rest -> failure_refutes h rho model q p after rest (refine_ u); refine_ u
    | Scanned (needle, marks, rest) ->  let middle = scan_heap h marks in
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        model y; let u = () in Marked_occurs_proofs.scan_equation h needle marks rho y (refine_ u); refine_ u in
      failure_refutes middle rho next p q after rest (refine_ u); refine_ u
    | Terminal_lower (bound, edits, tree, rest) -> let middle = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q middle edits tree;
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        model y; let u = () in lower_equation h bound edits rho y (refine_ u); refine_ u in
      failure_refutes middle rho next p q after rest (refine_ u); refine_ u
    | Base old -> Level_unifier_proofs.failure_refutes h rho model p q after old (refine_ u); refine_ u
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
        refine_ u)
    | Post_link _ -> refine_ u
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let u = () in let refine_ u = Effective_compression_proofs.forward h middle edits rho model y (refine_ u) in refine_ u in
      failure_refutes middle rho next p q after rest (refine_ u); refine_ u)


let rec (unify_restrict @ total) : (h : Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after rho x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | unified h p q ok after d} ->
    {u : unit | node_equation h rho x} @ ghost = fun h rho p q ok after d model x premise -> ghost_ (
    let refine_ premise = premise in unified_def h p q ok after d;
    let u = () in match d with
    | Swap rest -> unify_restrict h rho q p ok after rest model x (refine_ u); refine_ u
    | Scanned (needle, marks, rest) ->  let middle = scan_heap h marks in
      unify_restrict middle rho p q ok after rest model x (refine_ u);
      Marked_occurs_proofs.scan_equation h needle marks rho x (refine_ u); refine_ u
    | Terminal_lower (bound, edits, tree, rest) -> let middle = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q middle edits tree;
      unify_restrict middle rho p q ok after rest model x (refine_ u);
      lower_equation h bound edits rho x (refine_ u); refine_ u
    | Base old -> Optimized_model_proofs.legacy_restrict h rho p q ok after old model x (refine_ u); refine_ u
    | Resolve (r, s, _, _, rest) -> unify_restrict h rho r s ok after rest model x (refine_ u); refine_ u
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      if left_ok then (
        let mid_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
          let u = () in let refine_ u = unify_restrict middle rho b e ok after right model x (refine_ u) in refine_ u in
        unify_restrict h rho a c left_ok middle left mid_model x (refine_ u); refine_ u)
      else (unify_restrict h rho a c left_ok middle left (refine_ model) x (refine_ u); refine_ u)
    | Post_link (middle, rest, source, target) ->
      Structure_spec.linkable_def middle source target;
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let u = () in let refine_ u = Structure_model_proofs.backward middle source target rho (refine_ model) y (refine_ u) in refine_ u in
      unify_restrict h rho p q true middle rest next x (refine_ u); refine_ u
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let u = () in let refine_ u = unify_restrict middle rho p q ok after rest model y (refine_ u) in refine_ u in
      Effective_compression_proofs.backward h middle edits rho next x (refine_ u); refine_ u)
