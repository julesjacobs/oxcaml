open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Compression_spec

let rec (forward @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable -> (d : edits) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | rewritten h after d} ->
    {u : unit | node_equation after rho x} @ ghost = fun h after d rho model x premise -> ghost_ (
    rewritten_def h after d; match d with
    | Done -> model x; ()
    | Write (p, q, r, path, rest) -> let middle = H.put h p (redirect h p r) in
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let () = Compression_model_proofs.forward h p r path rho model y () in () in
      let () = forward middle after rest rho next x () in ())

let rec (backward @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable -> (d : edits) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after rho x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | rewritten h after d} ->
    {u : unit | node_equation h rho x} @ ghost = fun h after d rho model x premise -> ghost_ (
    rewritten_def h after d; match d with
    | Done -> model x; ()
    | Write (p, q, r, path, rest) -> let middle = H.put h p (redirect h p r) in
      let next : ((y : node Pref.t) @ immutable -> {u : unit | node_equation middle rho y}) @ total = fun y ->
        let () = backward middle after rest rho model y () in () in
      let () = Compression_model_proofs.backward h p q r path rho (refine_ next) x () in ())

let rec (finite @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable -> (d : edits) @ immutable ->
    (tree : tree) @ immutable -> {u : unit | rewritten h after d && Level_finite_spec.finite h tree} ->
    {t : tree | Level_finite_spec.finite after t && tree_root t === tree_root tree && readback t === readback tree} @ immutable ghost =
  fun h after d tree premise -> ghost_ (
    rewritten_def h after d; match d with
    | Done -> refine_ tree
    | Write (p, q, r, path, rest) -> let middle = H.put h p (redirect h p r) in
      Compression_finite_proofs.finite_compress h p q r path tree ();
      Compression_finite_proofs.compress_root p tree; Compression_finite_proofs.compress_readback p tree;
      let changed = Compression_finite_proofs.compress p tree in
      let refine_ out = finite middle after rest changed () in refine_ out)

let rec (frame @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable -> (d : edits) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | rewritten h after d} ->
    {u : unit | H.mem h x === H.mem after x && at_level h x === at_level after x
      && Level_unifier_metadata.scratch_frame h after x} @ ghost = fun h after d x premise -> ghost_ (
    rewritten_def h after d; match d with
    | Done -> Level_unifier_metadata.scratch_frame_def h after x;
      (match H.at h x with None -> () | Some v -> decreases_def v.level v.level; ()); ()
    | Write (p, _, r, _, rest) -> let v = redirect h p r in let middle = H.put h p v in
      frame middle after rest x ();
      Level_unifier_metadata.redirect_scratch h p r x ();
      Level_unifier_metadata.scratch_trans h middle after x ();
      active_def h p; at_level_def h p; redirect_def h p r;
      at_level_def h x; at_level_def middle x; ())

let rec (resolution_below @ total) : (h : Pref.heap) @ immutable ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x})) @ total ->
    (p : node Pref.t) @ immutable -> (r : node Pref.t) @ immutable -> (path : resolution) @ immutable -> (bound : int) ->
    {u : unit | resolves h p r path && below h p bound} -> {u : unit | below h r bound} @ ghost =
  fun h order p r path bound premise -> ghost_ (
    resolves_def h p r path; match path with Here -> ()
    | Via (q, rest) -> order p; ordered_def h p; below_def h p bound; at_level_def h p; observe_def h p;
      (match H.at h p with None -> () | Some v -> match v.level with Generic -> () | Finite n ->
        children_below_def h v.desc n; below_def h q n; at_level_def h q; ());
      below_def h q bound; let () = resolution_below h order q r rest bound () in ())

let rec (ordered @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable -> (d : edits) @ immutable ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | Level_spec.ordered h x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | rewritten h after d} ->
    {u : unit | Level_spec.ordered after x} @ ghost = fun h after d order x premise -> ghost_ (
    rewritten_def h after d; match d with Done -> order x; ()
    | Write (p, _, r, path, rest) -> let middle = H.put h p (redirect h p r) in
      order p; ordered_def h p; active_def h p; at_level_def h p;
      (match at_level h p with Generic -> () | Finite n -> below_def h p n; resolution_below h order p r path n (); ());
      let next : ((y : node Pref.t) @ immutable -> {u : unit | Level_spec.ordered middle y}) @ total = fun y ->
        order y; let () = Level_unifier_metadata.redirect_ordered h p r y () in () in
      let () = ordered middle after rest next x () in ())

let rec (scope @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable -> (d : edits) @ immutable ->
    (before : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | rewritten h after d} ->
    {u : unit | not (H.mem after x) || finite_scope after x} @ ghost = fun h after d before x premise -> ghost_ (
    rewritten_def h after d; match d with Done -> before x; ()
    | Write (p, _, r, path, rest) -> let middle = H.put h p (redirect h p r) in
      Level_unifier_metadata.resolution_active h before p r path ();
      let next : ((y : node Pref.t) @ immutable -> {u : unit | not (H.mem middle y) || finite_scope middle y}) @ total = fun y ->
        before y; let () = Level_unifier_metadata.redirect_scope h p r y () in () in
      let () = scope middle after rest next x () in ())

let (below @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable -> (d : edits) @ immutable ->
    (x : node Pref.t) @ immutable -> (cut : int) -> {u : unit | rewritten h after d} ->
    {u : unit | Level_spec.below h x cut === Level_spec.below after x cut} @ ghost = fun h after d x cut premise -> ghost_ (
    frame h after d x ();
    below_def h x cut; below_def after x cut; ())

let (protected @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable -> (d : edits) @ immutable ->
    (x : node Pref.t) @ immutable -> (cut : int) -> {u : unit | rewritten h after d} ->
    {u : unit | Hm_environment_spec.protected_at h after cut x} @ ghost = fun h after d x cut premise -> ghost_ (
    frame h after d x (); below h after d x cut ();
    Hm_environment_spec.protected_at_def h after cut x; Level_unifier_metadata.scratch_frame_def h after x;
    ())

let (covered @ total) : (h : Pref.heap) @ immutable -> (after : Pref.heap) @ immutable -> (d : edits) @ immutable ->
    (pool : Generalize_spec.pool) @ immutable -> (x : node Pref.t) @ immutable -> (cut : int) ->
    {u : unit | rewritten h after d && Generalize_spec.covered h cut pool x} ->
    {u : unit | Generalize_spec.covered after cut pool x} @ ghost = fun h after d pool x cut premise -> ghost_ (
    frame h after d x ();
    Generalize_spec.covered_def h cut pool x; Generalize_spec.covered_def after cut pool x; ())

let (determined @ total) : (saved : Pref.heap) @ immutable -> (h : Pref.heap) @ immutable ->
    (after : Pref.heap) @ immutable -> (d : edits) @ immutable -> (cut : int) ->
    (prior : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (rho_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
      (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h eta x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable ->
        {u : unit | not (Level_spec.below saved x cut) || rho x === eta x})) @ total ->
      (x : node Pref.t) @ immutable -> {u : unit | Level_spec.below h x cut} ->
      {u : unit | rho x === eta x})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after rho x})) @ total ->
    (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after eta x})) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (Level_spec.below saved x cut) || rho x === eta x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | rewritten h after d && Level_spec.below after x cut} ->
    {u : unit | rho x === eta x} @ ghost = fun saved h after d cut prior rho rho_model eta eta_model equal x premise -> ghost_ (
    below h after d x cut ();
    let old_rho : ((y : node Pref.t) @ immutable -> {u : unit | equation h rho y}) @ total = fun y ->
      backward h after d rho rho_model y ();
      node_equation_def h rho y; observe_def h y; equation_def h rho y; () in
    let old_eta : ((y : node Pref.t) @ immutable -> {u : unit | equation h eta y}) @ total = fun y ->
      backward h after d eta eta_model y ();
      node_equation_def h eta y; observe_def h y; equation_def h eta y; () in
    let () = prior rho old_rho eta old_eta equal x () in ())

let rec (resolution @ total) : (h : Pref.heap) @ immutable ->
    (after : Pref.heap) @ immutable -> (edits : edits) @ immutable ->
    (p : node Pref.t) @ immutable -> (root : node Pref.t) @ immutable ->
    (path : resolution) @ immutable ->
    {u : unit | rewritten h after edits && resolves h p root path} ->
    {d : resolution | resolves after p root d} @ immutable ghost =
  fun h after edits p root path premise -> ghost_ (
    rewritten_def h after edits;
    match edits with
    | Done -> refine_ path
    | Write (x, _, r, original, rest) ->
      active_def h x;
      Compression_path_proofs.resolution_terminal h x r original (); terminal_def h r;
      let middle = H.put h x (redirect h x r) in
      let refine_ next = Compression_path_proofs.shortcut_resolution h x r original p root path () in
      let refine_ out = resolution middle after rest p root next () in refine_ out)
