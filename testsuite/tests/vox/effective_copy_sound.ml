open Copy_spec
open Copy_heap_proofs
open Effective_copy_spec
open Effective_copy_heap_proofs

let (target_image @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total -> (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | effective_target_for saved heads d p q} ->
    {u : unit | effective_available saved heads d p && effective_image saved heads d rho p === rho q} @ ghost = fun saved heads d rho p q premise -> ghost_ (
  effective_target_for_def saved heads d p q; effective_available_def saved heads d p; effective_image_def saved heads d rho p;
  ())

let rec (mapped_instance @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (final : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth final) rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (want : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (images : ((x : node Pref.t) @ immutable -> {u : unit | want x === effective_image saved heads final rho x})) @ total ->
    {u : unit | effective_valid saved heads epoch depth d && effective_valid saved heads epoch depth final && extends d final
      && mapping d p === Some q} ->
    {u : unit | effective_instance_at saved heads rho want p && match H.at saved p with None -> false
      | Some v -> Effective_level.level saved heads p === Generic && effective_children_available saved heads final v.desc
        && (match v.desc with Link _ -> true | _ -> not (H.mem saved q))} @ ghost =
  fun saved heads epoch depth d final rho model p q want images premise -> ghost_ (
    effective_valid_def saved heads epoch depth d; mapping_def d p;
    match d with
    | Start | Clean -> ()
    | Fresh (rest, x, y, old, desc) ->
      if p === x then (
        history_at saved heads epoch depth rest p ();
        history_grows saved heads epoch depth rest p ();
        history_grows saved heads epoch depth rest q ();
        heap_def saved epoch depth d; let h = heap saved epoch depth rest in
        let v = cell desc depth in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; cell_def desc depth;
        let h1 = H.put h q v in put_frame h1 p w q;
        fresh_frame saved heads epoch depth d final q ();
        mapping_preserved saved heads epoch depth d final p q ();
        effective_image_def saved heads final rho p; images p;
        let after = heap saved epoch depth final in model q; equation_def after rho q;
        effective_instance_at_def saved heads rho want p; effective_children_available_def saved heads final old.desc;
        effective_ready_def saved heads rest old.desc desc;
        let _ : {u : unit | extends rest final} =
          extends_def rest d; extends_def rest rest;
          extension_trans rest d final (); () in
        match old.desc, desc with
        | List a, List c ->
          target_preserved saved heads epoch depth rest final a c ();
          target_image saved heads final rho a c (); images a; ()
        | Arrow (a, b), Arrow (c, e) ->
          target_preserved saved heads epoch depth rest final a c ();
          target_preserved saved heads epoch depth rest final b e ();
          target_image saved heads final rho a c (); target_image saved heads final rho b e ();
          images a; images b; ()
        | _ -> ())
      else (
        extends_def rest d; extends_def rest rest; extension_trans rest d final ();
        let () = mapped_instance saved heads epoch depth rest final rho model p q want images () in ())
    | Alias (rest, x, y, old) ->
      if p === x then (
        history_at saved heads epoch depth rest p ();
        mapping_preserved saved heads epoch depth d final p q ();
        effective_image_def saved heads final rho p; images p;
        effective_instance_at_def saved heads rho want p; effective_children_available_def saved heads final old.desc;
        extends_def rest d; extends_def rest rest; extension_trans rest d final ();
        match old.desc with
        | Link child -> target_preserved saved heads epoch depth rest final child q ();
          target_image saved heads final rho child q (); images child; ()
        | _ -> ())
      else (
        extends_def rest d; extends_def rest rest; extension_trans rest d final ();
        let () = mapped_instance saved heads epoch depth rest final rho model p q want images () in ()))

let rec (template_sound @ total) : (saved : node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) rho x})) @ total ->
    (want : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (images : ((x : node Pref.t) @ immutable -> {u : unit | want x === effective_image saved heads d rho x})) @ total ->
    (t : template) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && Effective_template.valid_template saved heads t && effective_available saved heads d (root t)} ->
    {u : unit | interpret rho want t === want (root t)} @ ghost =
  fun saved heads epoch depth d rho model want images t premise -> ghost_ (
    Effective_template.valid_template_def saved heads t; root_def t; interpret_def rho want t;
    let p = root t in Effective_template.head saved heads t ();
    Effective_template.finite_def saved heads p; Effective_template.generic_def saved heads p;
    Level_unifier_spec.observe_def saved p; head_desc_def t; head_generic_def t;
    let p = root t in effective_available_def saved heads d p; images p; effective_image_def saved heads d rho p;
    (match mapping d p with None -> () | Some q ->
      extends_def d d; mapped_instance saved heads epoch depth d d rho model p q want images (); ());
    effective_instance_at_def saved heads rho want p;
    match t with
    | Boundary _ | Parameter _ | Constant _ | Word_constant _ -> ()
    | Product (_, a, b) ->
      let desc = Arrow (root a, root b) in effective_children_available_def saved heads d desc;
      template_sound saved heads epoch depth d rho model want images a ();
      template_sound saved heads epoch depth d rho model want images b (); ()
    | List_template (_, child) ->
      let desc = List (root child) in effective_children_available_def saved heads d desc;
      template_sound saved heads epoch depth d rho model want images child (); ()
    | Indirect (_, child) ->
      let desc = Link (root child) in effective_children_available_def saved heads d desc;
      template_sound saved heads epoch depth d rho model want images child (); ())
