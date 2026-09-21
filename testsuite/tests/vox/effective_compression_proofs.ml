open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Compression_spec
open Effective_compression_spec

let rec (forward @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable -> (d : edits) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | effective_rewritten h after d} ->
    {u : unit | node_equation after rho x} @ ghost = fun h after d rho model x premise -> ghost_ (
    let refine_ premise = premise in effective_rewritten_def h after d; let u = () in match d with
    | Done -> model x; refine_ u
    | Write (p, q, r, path, rest) -> let middle = H.put h p (redirect h p r) in
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let u = () in let refine_ u = Compression_model_proofs.forward h p r path rho model y (refine_ u) in refine_ u in
      let refine_ u = forward middle after rest rho next x (refine_ u) in refine_ u)

let rec (backward @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable -> (d : edits) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after rho x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | effective_rewritten h after d} ->
    {u : unit | node_equation h rho x} @ ghost = fun h after d rho model x premise -> ghost_ (
    let refine_ premise = premise in effective_rewritten_def h after d; let u = () in match d with
    | Done -> model x; refine_ u
    | Write (p, q, r, path, rest) -> let middle = H.put h p (redirect h p r) in
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let u = () in let refine_ u = backward middle after rest rho model y (refine_ u) in refine_ u in
      let refine_ u = Compression_model_proofs.backward h p q r path rho (refine_ next) x (refine_ u) in refine_ u)

let rec (finite @ total) : (h : node Pref.heap) @ immutable -> (after : node Pref.heap) @ immutable -> (d : edits) @ immutable ->
    (tree : tree) @ immutable -> {u : unit | effective_rewritten h after d && Level_finite_spec.finite h tree} ->
    {t : tree | Level_finite_spec.finite after t && tree_root t === tree_root tree && readback t === readback tree} @ immutable ghost =
  fun h after d tree premise -> ghost_ (
    let refine_ premise = premise in effective_rewritten_def h after d; let u = () in match d with
    | Done -> refine_ tree
    | Write (p, q, r, path, rest) -> let middle = H.put h p (redirect h p r) in
      Compression_finite_proofs.finite_compress h p q r path tree (refine_ u);
      Compression_finite_proofs.compress_root p tree; Compression_finite_proofs.compress_readback p tree;
      let changed = Compression_finite_proofs.compress p tree in
      let refine_ out = finite middle after rest changed (refine_ u) in refine_ out)


let (redirect_source @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || source_ok h x})) @ total ->
    (p : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && H.mem h root} ->
    {u : unit | let after = H.put h p (redirect h p root) in not (H.mem after x) || source_ok after x} @ ghost =
  fun h scope p root x premise -> ghost_ (
    let refine_ premise = premise in let v = redirect h p root in let after = H.put h p v in
    scope p; scope x; source_ok_def h p; source_ok_def h x; source_ok_def after x; redirect_def h p root;
    Copy_heap_proofs.put_frame h p v x;
    (match H.at h x with None -> () | Some old ->
      (match old.desc with Var | Bool -> ()
       | Link q -> Copy_heap_proofs.put_frame h p v q; ()
       | Arrow (a, b) -> Copy_heap_proofs.put_frame h p v a; Copy_heap_proofs.put_frame h p v b; ());
      match old.memo with Empty_memo | Forward _ -> ()
      | Memo (stamp, _) -> Copy_heap_proofs.put_frame h p v stamp; ());
    (match H.at h p with None -> () | Some old -> match old.memo with Empty_memo | Forward _ -> ()
      | Memo (stamp, _) -> Copy_heap_proofs.put_frame h p v stamp; ());
    Copy_heap_proofs.put_frame h p v root;
    let u = () in refine_ u)

let (redirect_levels @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | H.mem h p && (match H.at h p with None -> false | Some _ -> true)} ->
    {u : unit | let after = H.put h p (redirect h p root) in
      H.mem h x === H.mem after x && at_level h x === at_level after x} @ ghost =
  fun h p root x premise -> ghost_ (
    let refine_ premise = premise in let v = redirect h p root in let after = H.put h p v in
    redirect_def h p root; Copy_heap_proofs.put_frame h p v x;
    at_level_def h x; at_level_def after x; let u = () in refine_ u)

let rec (resolution @ total) : (h : node Pref.heap) @ immutable ->
    (after : node Pref.heap) @ immutable -> (edits : edits) @ immutable ->
    (p : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable ->
    (path : resolution) @ immutable ->
    {u : unit | effective_rewritten h after edits && resolves h p root path} ->
    {d : resolution | resolves after p root d} @ immutable ghost =
  fun h after edits p root path premise -> ghost_ (
    let refine_ premise = premise in effective_rewritten_def h after edits;
    let u = () in match edits with
    | Done -> refine_ path
    | Write (x, _, r, original, rest) ->

      Compression_path_proofs.resolution_terminal h x r original (refine_ u); terminal_def h r;
      let middle = H.put h x (redirect h x r) in
      let refine_ next = Compression_path_proofs.shortcut_resolution h x r original p root path (refine_ u) in
      let refine_ out = resolution middle after rest p root next (refine_ u) in refine_ out)

let rec (frame @ total) : (h : node Pref.heap) @ immutable ->
    (after : node Pref.heap) @ immutable -> (edits : edits) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | effective_rewritten h after edits} ->
    {u : unit | H.mem h x === H.mem after x && at_level h x === at_level after x
      && (match H.at h x, H.at after x with
        | Some old, Some next -> old.memo === next.memo && old.visited === next.visited
          && (match old.desc with Link _ -> (match next.desc with Link _ -> true | _ -> false) | _ -> old === next)
        | None, None -> true | _ -> false)} @ ghost =
  fun h after edits x premise -> ghost_ (
    let refine_ premise = premise in effective_rewritten_def h after edits;
    let u = () in match edits with
    | Done -> refine_ u
    | Write (p, _, root, _, rest) ->
      let v = redirect h p root in let middle = H.put h p v in
      observe_def h p; redirect_levels h p root x (refine_ u);
      redirect_def h p root; Copy_heap_proofs.put_frame h p v x;
      frame middle after rest x (refine_ u); refine_ u)

let rec (source @ total) : (h : node Pref.heap) @ immutable ->
    (after : node Pref.heap) @ immutable -> (edits : edits) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || source_ok h x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | effective_rewritten h after edits} ->
    {u : unit | not (H.mem after x) || source_ok after x} @ ghost =
  fun h after edits scope x premise -> ghost_ (
    let refine_ premise = premise in effective_rewritten_def h after edits;
    let u = () in match edits with
    | Done -> scope x; refine_ u
    | Write (p, _, root, path, rest) ->
      let middle = H.put h p (redirect h p root) in
      Compression_path_proofs.resolution_terminal h p root path (refine_ u);
      let next : ((y : node Pref.t) @ immutable ->
        {u : unit | not (H.mem middle y) || source_ok middle y}) @ total = fun y ->
          let u = () in let refine_ out = redirect_source h scope p root y (refine_ u) in refine_ out in
      let refine_ out = source middle after rest next x (refine_ u) in refine_ out)

let rec (generic @ total) : (h : node Pref.heap) @ immutable ->
    (after : node Pref.heap) @ immutable -> (edits : edits) @ immutable ->
    (x : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable ->
    (path : resolution) @ immutable ->
    {u : unit | effective_rewritten h after edits && resolves h x root path
      && at_level h root === Generic} ->
    {u : unit | H.at h x === H.at after x} @ ghost =
  fun h after edits x root path premise -> ghost_ (
    let refine_ premise = premise in effective_rewritten_def h after edits;
    let u = () in match edits with
    | Done -> refine_ u
    | Write (p, _, r, original, rest) ->
      active_def h r;
      if x === p then (Compression_path_proofs.unique_root h p root r path original (refine_ u); ()) else ();
      let v = redirect h p r in let middle = H.put h p v in
      observe_def h p; redirect_levels h p r root (refine_ u);
      Compression_path_proofs.resolution_terminal h p r original (refine_ u); terminal_def h r;
      let refine_ next = Compression_path_proofs.shortcut_resolution h p r original x root path (refine_ u) in
      Copy_heap_proofs.put_frame h p v x;
      generic middle after rest x root next (refine_ u); refine_ u)
