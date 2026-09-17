open Copy_spec
open Copy_heap_proofs
open Effective_copy_spec
open Effective_copy_heap_proofs

let (target_image @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total -> (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | effective_target_for saved heads d p q} ->
    {u : unit | effective_available saved heads d p && effective_image saved heads d rho p === rho q} @ ghost = fun saved heads d rho p q premise -> ghost_ (
  let refine_ premise = premise in effective_target_for_def saved heads d p q; effective_available_def saved heads d p; effective_image_def saved heads d rho p;
  let u = () in refine_ u)

let rec (mapped_instance @ total) : (saved : Pref.heap) @ immutable ->
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
    let refine_ premise = premise in effective_valid_def saved heads epoch depth d; mapping_def d p;
    let u = () in match d with
    | Start | Clean -> refine_ u
    | Fresh (rest, x, y, old, desc) ->
      if p === x then (
        history_at saved heads epoch depth rest p (refine_ u);
        history_grows saved heads epoch depth rest p (refine_ u);
        history_grows saved heads epoch depth rest q (refine_ u);
        heap_def saved epoch depth d; let h = heap saved epoch depth rest in
        let v = cell desc depth in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; cell_def desc depth;
        put_frame h q v q; let h1 = H.put h q v in put_frame h1 p w q;
        fresh_frame saved heads epoch depth d final q (refine_ u);
        mapping_preserved saved heads epoch depth d final p q (refine_ u);
        effective_image_def saved heads final rho p; images p;
        let after = heap saved epoch depth final in model q; equation_def after rho q;
        effective_instance_at_def saved heads rho want p; effective_children_available_def saved heads final old.desc;
        effective_ready_def saved heads rest old.desc desc;
        let ext : {u : unit | extends rest final} =
          extends_def rest d; extends_def rest rest;
          extension_trans rest d final (refine_ u); refine_ u in
        let refine_ ext = ext in
        match old.desc, desc with
        | Arrow (a, b), Arrow (c, e) ->
          target_preserved saved heads epoch depth rest final a c (refine_ u);
          target_preserved saved heads epoch depth rest final b e (refine_ u);
          target_image saved heads final rho a c (refine_ u); target_image saved heads final rho b e (refine_ u);
          images a; images b; refine_ u
        | _ -> refine_ u)
      else (
        extends_def rest d; extends_def rest rest; extension_trans rest d final (refine_ u);
        let refine_ u = mapped_instance saved heads epoch depth rest final rho model p q want images (refine_ u) in refine_ u)
    | Alias (rest, x, y, old) ->
      if p === x then (
        history_at saved heads epoch depth rest p (refine_ u);
        mapping_preserved saved heads epoch depth d final p q (refine_ u);
        effective_image_def saved heads final rho p; images p;
        effective_instance_at_def saved heads rho want p; effective_children_available_def saved heads final old.desc;
        extends_def rest d; extends_def rest rest; extension_trans rest d final (refine_ u);
        match old.desc with
        | Link child -> target_preserved saved heads epoch depth rest final child q (refine_ u);
          target_image saved heads final rho child q (refine_ u); images child; refine_ u
        | _ -> refine_ u)
      else (
        extends_def rest d; extends_def rest rest; extension_trans rest d final (refine_ u);
        let refine_ u = mapped_instance saved heads epoch depth rest final rho model p q want images (refine_ u) in refine_ u))

let rec (template_sound @ total) : (saved : Pref.heap) @ immutable ->
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
    let refine_ premise = premise in Effective_template.valid_template_def saved heads t; root_def t; interpret_def rho want t;
    let u = () in let p = root t in Effective_template.head saved heads t (refine_ u);
    Effective_template.finite_def saved heads p; Effective_template.generic_def saved heads p;
    Level_unifier_spec.observe_def saved p; head_desc_def t; head_generic_def t;
    let p = root t in effective_available_def saved heads d p; images p; effective_image_def saved heads d rho p;
    let u = () in
    (match mapping d p with None -> () | Some q ->
      extends_def d d; mapped_instance saved heads epoch depth d d rho model p q want images (refine_ u); ());
    effective_instance_at_def saved heads rho want p;
    match t with
    | Boundary _ | Parameter _ | Constant _ -> refine_ u
    | Product (_, a, b) ->
      let desc = Arrow (root a, root b) in effective_children_available_def saved heads d desc;
      template_sound saved heads epoch depth d rho model want images a (refine_ u);
      template_sound saved heads epoch depth d rho model want images b (refine_ u); refine_ u
    | Indirect (_, child) ->
      let desc = Link (root child) in effective_children_available_def saved heads d desc;
      template_sound saved heads epoch depth d rho model want images child (refine_ u); refine_ u)
