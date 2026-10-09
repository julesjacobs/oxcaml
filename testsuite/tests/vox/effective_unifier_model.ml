open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_unifier_proofs
open Level_finite_spec

let (lower_equation @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (d : lowering) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | Terminal_lower_spec.terminal_valid h bound d} ->
    {u : unit | node_equation h rho x === node_equation (lower_heap h bound d) rho x} @ ghost =
  fun h bound d rho x premise -> ghost_ (
    let after = lower_heap h bound d in Terminal_lower_proofs.lowering_at h bound d x ();
    lower_frame_def h after x; observe_def h x; observe_def after x;
    node_equation_def h rho x; node_equation_def after rho x; ())

open Effective_unifier_spec
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
    let ok = true in
    unified_def h p q ok after d;
    match d with
    | Swap rest -> success_backward_at h rho model q p after rest x (); ()
    | Scanned (needle, marks, rest) ->  let middle = scan_heap h marks in
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        model y; Marked_occurs_proofs.scan_equation h needle marks rho y (); () in
      success_backward_at middle rho next p q after rest x (); ()
    | Terminal_lower (bound, edits, tree, rest) -> let middle = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q middle edits tree;
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        model y; lower_equation h bound edits rho y (); () in
      success_backward_at middle rho next p q after rest x (); ()
    | Base old -> Level_unifier_proofs.success_backward_at h rho model p q after old x (); ()
    | Resolve (r, s, rp, sq, rest) ->
      resolution_model h rho model p r rp ();
      resolution_model h rho model q s sq ();
      success_backward_at h rho model r s after rest x (); ()
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      model p; model q;
      node_equation_def h rho p; node_equation_def h rho q;
      let middle_model : (x : node Pref.t) @ immutable ->
          {u : unit | node_equation middle rho x} @ total = fun x ->
        let refine_ proof =
          success_backward_at h rho model a c middle left x () in
        () in
      success_backward_at middle rho middle_model b e after right x ();
      ()
    | Post_link (middle, rest, source, target) ->
      Structure_spec.linkable_def middle source target;
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let () = success_backward_at h rho model p q middle rest y () in () in
      Structure_model_proofs.forward middle source target rho next x (); ()
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let () = Effective_compression_proofs.forward h middle edits rho model y () in () in
      success_backward_at middle rho next p q after rest x (); ())

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
    let ok = true in
    unified_def h p q ok after d;
    match d with
    | Swap rest -> success_forward_at h rho q p after rest model x (); ()
    | Scanned (needle, marks, rest) ->  let middle = scan_heap h marks in
      success_forward_at middle rho p q after rest model x ();
      Marked_occurs_proofs.scan_equation h needle marks rho x (); ()
    | Terminal_lower (bound, edits, tree, rest) -> let middle = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q middle edits tree;
      success_forward_at middle rho p q after rest model x ();
      lower_equation h bound edits rho x (); ()
    | Base old -> Level_unifier_proofs.success_forward_at h rho p q after old model x (); ()
    | Resolve (r, s, rp, sq, rest) ->
      let before_model : (x : node Pref.t) @ immutable ->
          {u : unit | node_equation h rho x} @ total = fun x ->
        let refine_ proof =
          success_forward_at h rho r s after rest model x () in
        () in
      resolution_model h rho before_model p r rp ();
      resolution_model h rho before_model q s sq ();
      success_forward_at h rho r s after rest model x ();
      ()
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let middle_model : (x : node Pref.t) @ immutable ->
          {u : unit | node_equation middle rho x} @ total = fun x ->
        let refine_ proof =
          success_forward_at middle rho b e after right model x () in
        () in
      success_forward_at h rho a c middle left middle_model p ();
      success_forward_at h rho a c middle left middle_model q ();
      node_equation_def h rho p; node_equation_def h rho q;
      success_forward_at middle rho b e after right model x ();
      success_forward_at h rho a c middle left middle_model x ();
      ()
    | Post_link (middle, rest, source, target) ->
      Structure_spec.linkable_def middle source target;
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let () = Structure_model_proofs.backward middle source target rho (refine_ model) y () in () in
      success_forward_at h rho p q middle rest next x (); ()
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let () = success_forward_at middle rho p q after rest model y () in () in
      success_forward_at middle rho p q after rest model x ();
      Effective_compression_proofs.backward h middle edits rho next x (); ())

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
    let ok = false in
    unified_def h p q ok after d;
    match d with
    | Swap rest -> failure_refutes h rho model q p after rest (); ()
    | Scanned (needle, marks, rest) ->  let middle = scan_heap h marks in
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        model y; Marked_occurs_proofs.scan_equation h needle marks rho y (); () in
      failure_refutes middle rho next p q after rest (); ()
    | Terminal_lower (bound, edits, tree, rest) -> let middle = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q middle edits tree;
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        model y; lower_equation h bound edits rho y (); () in
      failure_refutes middle rho next p q after rest (); ()
    | Base old -> Level_unifier_proofs.failure_refutes h rho model p q after old (); ()
    | Resolve (r, s, rp, sq, rest) ->
      resolution_model h rho model p r rp ();
      resolution_model h rho model q s sq ();
      failure_refutes h rho model r s after rest (); ()
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      model p; model q;
      node_equation_def h rho p; node_equation_def h rho q;
      if left_ok then (
        let middle_model : (x : node Pref.t) @ immutable ->
            {u : unit | node_equation middle rho x} @ total = fun x ->
          model x;
          let refine_ proof =
            success_backward_at h rho model a c middle left x () in
          () in
        failure_refutes middle rho middle_model b e after right ();
        ())
      else (
        failure_refutes h rho model a c middle left ();
        ())
    | Post_link _ -> ()
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let () = Effective_compression_proofs.forward h middle edits rho model y () in () in
      failure_refutes middle rho next p q after rest (); ())


let rec (unify_restrict @ total) : (h : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (ok : bool) ->
    (after : node Pref.heap) @ immutable -> (d : derivation) @ immutable ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after rho x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | unified h p q ok after d} ->
    {u : unit | node_equation h rho x} @ ghost = fun h rho p q ok after d model x premise -> ghost_ (
    unified_def h p q ok after d;
    match d with
    | Swap rest -> unify_restrict h rho q p ok after rest model x (); ()
    | Scanned (needle, marks, rest) ->  let middle = scan_heap h marks in
      unify_restrict middle rho p q ok after rest model x ();
      Marked_occurs_proofs.scan_equation h needle marks rho x (); ()
    | Terminal_lower (bound, edits, tree, rest) -> let middle = lower_heap h bound edits in Terminal_lower_spec.completed_def h bound q middle edits tree;
      unify_restrict middle rho p q ok after rest model x ();
      lower_equation h bound edits rho x (); ()
    | Base old -> Optimized_model_proofs.legacy_restrict h rho p q ok after old model x (); ()
    | Resolve (r, s, _, _, rest) -> unify_restrict h rho r s ok after rest model x (); ()
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      if left_ok then (
        let mid_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
          let () = unify_restrict middle rho b e ok after right model x () in () in
        unify_restrict h rho a c left_ok middle left mid_model x (); ())
      else (unify_restrict h rho a c left_ok middle left (refine_ model) x (); ())
    | Post_link (middle, rest, source, target) ->
      Structure_spec.linkable_def middle source target;
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let () = Structure_model_proofs.backward middle source target rho (refine_ model) y () in () in
      unify_restrict h rho p q true middle rest next x (); ()
    | Pre_compress (middle, edits, rest) ->
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let () = unify_restrict middle rho p q ok after rest model y () in () in
      Effective_compression_proofs.backward h middle edits rho next x (); ())
