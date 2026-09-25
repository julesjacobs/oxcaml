open Copy_spec
open Level_unifier_spec
open Level_unifier_proofs

let rec (resolution_after @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (r : node Pref.t) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation (H.put h p (redirect h p r)) rho x})) @ total ->
    (q : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    {u : unit | resolves h q r path} -> {u : unit | rho q === rho r} @ ghost =
  fun h p r rho model q path premise -> ghost_ (
    let v = redirect h p r in let after = H.put h p v in
    model q; node_equation_def after rho q; observe_write h p v q; redirect_desc h p r;
    if q === p then () else (
      resolves_def h q r path;
      match path with Here -> () | Via (next, rest) ->
        resolution_after h p r rho model next rest (); ()))

let (forward @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (r : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | resolves h p r path} ->
    {u : unit | node_equation (H.put h p (redirect h p r)) rho x} @ ghost =
  fun h p r path rho model x premise -> ghost_ (
    resolution_model h rho model p r path ();
    model x; node_equation_def h rho x; let v = redirect h p r in let after = H.put h p v in
    node_equation_def after rho x; observe_write h p v x; redirect_desc h p r; ())

let (backward @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (r : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation (H.put h p (redirect h p r)) rho x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | observe h p === Some (Link q) && resolves h p r path} ->
    {u : unit | node_equation h rho x} @ ghost = fun h p q r path rho model x premise -> ghost_ (
    let v = redirect h p r in let after = H.put h p v in
    resolves_def h p r path;
    (match path with Here -> terminal_def h p; ()
    | Via (next, rest) -> resolution_after h p r rho model next rest (); ());
    model x; node_equation_def h rho x; node_equation_def after rho x;
    observe_write h p v x; redirect_desc h p r; ())

let (determined_agreement @ total) : (saved : Pref.heap) @ immutable -> (h : Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (rho_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
      (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h eta x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable ->
        {u : unit | not (Level_spec.below saved x cut) || rho x === eta x})) @ total ->
      (x : node Pref.t) @ immutable -> {u : unit | Level_spec.below h x cut} ->
      {u : unit | rho x === eta x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (r : node Pref.t) @ immutable ->
    (path : resolution) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho_model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation (H.put h p (redirect h p r)) rho x})) @ total ->
    (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta_model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation (H.put h p (redirect h p r)) eta x})) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (Level_spec.below saved x cut) || rho x === eta x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Level_spec.active h p && observe h p === Some (Link q) && resolves h p r path
      && Level_spec.below (H.put h p (redirect h p r)) x cut} ->
    {u : unit | rho x === eta x} @ ghost =
  fun saved h cut prior p q r path rho rho_model eta eta_model equal x premise -> ghost_ (
    Level_unifier_metadata.redirect_below h p r x cut ();
    let old_rho : ((y : node Pref.t) @ immutable -> {u : unit | equation h rho y}) @ total = fun y ->
      backward h p q r path rho rho_model y ();
      node_equation_def h rho y; observe_def h y; equation_def h rho y; () in
    let old_eta : ((y : node Pref.t) @ immutable -> {u : unit | equation h eta y}) @ total = fun y ->
      backward h p q r path eta eta_model y ();
      node_equation_def h eta y; observe_def h y; equation_def h eta y; () in
    let () = prior rho old_rho eta old_eta equal x () in ())

let (origin_agreement @ total) : (saved : Pref.heap) @ immutable -> (h : Pref.heap) @ immutable -> (cut : int) ->
    (prior : ((x : node Pref.t) @ immutable ->
      {o : Provenance_spec.origin | not (Level_spec.below h x cut) || Provenance_spec.originates saved h cut x o} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (r : node Pref.t) @ immutable ->
    (path : resolution) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (rho_model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation (H.put h p (redirect h p r)) rho x})) @ total ->
    (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (eta_model : ((x : node Pref.t) @ immutable ->
      {u : unit | node_equation (H.put h p (redirect h p r)) eta x})) @ total ->
    (equal : ((x : node Pref.t) @ immutable ->
      {u : unit | not (Level_spec.below saved x cut) || rho x === eta x})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | Level_spec.active h p && observe h p === Some (Link q) && resolves h p r path
      && Level_spec.below (H.put h p (redirect h p r)) x cut} ->
    {u : unit | rho x === eta x} @ ghost =
  fun saved h cut prior p q r path rho rho_model eta eta_model equal x premise -> ghost_ (
    let base : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (rho_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
      (eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (eta_model : ((x : node Pref.t) @ immutable -> {u : unit | equation h eta x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable ->
        {u : unit | not (Level_spec.below saved x cut) || rho x === eta x})) @ total ->
      (x : node Pref.t) @ immutable -> {u : unit | Level_spec.below h x cut} ->
      {u : unit | rho x === eta x}) @ total = fun a am b bm equal x low ->
        let origin = prior x in let () = Relative_generalization.origin_agreement saved h cut a am b bm equal x origin () in () in
    let () = determined_agreement saved h cut base p q r path rho rho_model eta eta_model equal x () in ())
