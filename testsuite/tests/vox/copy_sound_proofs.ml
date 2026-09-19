open Copy_spec
open Copy_heap_proofs

let (target_image @ total) : (saved : node Pref.heap) @ immutable -> (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | target_for saved d p q} ->
    {u : unit | available saved d p && image saved d rho p === rho q} @ ghost = fun saved d rho p q premise -> ghost_ (
  target_for_def saved d p q; available_def saved d p; image_def saved d rho p;
  ())

let rec (mapped_instance @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (final : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth final) rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (want : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (images : ((x : node Pref.t) @ immutable -> {u : unit | want x === image saved final rho x})) @ total ->
    {u : unit | valid saved epoch depth d && valid saved epoch depth final && extends d final
      && mapping d p === Some q} ->
    {u : unit | instance_at saved rho want p && match H.at saved p with None -> false
      | Some v -> v.level === Generic && children_available saved final v.desc
        && (match v.desc with Link _ -> true | _ -> not (H.mem saved q))} @ ghost =
  fun saved epoch depth d final rho model p q want images premise -> ghost_ (
    valid_def saved epoch depth d; mapping_def d p;
    match d with
    | Start | Clean -> ()
    | Fresh (rest, x, y, old, desc) ->
      if p === x then (
        history_at saved epoch depth rest p ();
        history_grows saved epoch depth rest p ();
        history_grows saved epoch depth rest q ();
        heap_def saved epoch depth d; let h = heap saved epoch depth rest in
        let v = cell desc depth in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; cell_def desc depth;
        let h1 = H.put h q v in put_frame h1 p w q;
        fresh_frame saved epoch depth d final q ();
        mapping_preserved saved epoch depth d final p q ();
        image_def saved final rho p; images p;
        let after = heap saved epoch depth final in model q; equation_def after rho q;
        instance_at_def saved rho want p; children_available_def saved final old.desc;
        ready_def saved rest old.desc desc;
        let _ : {u : unit | extends rest final} =
          extends_def rest d; extends_def rest rest;
          extension_trans rest d final (); () in
        match old.desc, desc with
        | Arrow (a, b), Arrow (c, e) ->
          target_preserved saved epoch depth rest final a c ();
          target_preserved saved epoch depth rest final b e ();
          target_image saved final rho a c (); target_image saved final rho b e ();
          images a; images b; ()
        | _ -> ())
      else (
        extends_def rest d; extends_def rest rest; extension_trans rest d final ();
        let () = mapped_instance saved epoch depth rest final rho model p q want images () in ())
    | Alias (rest, x, y, old) ->
      if p === x then (
        history_at saved epoch depth rest p ();
        mapping_preserved saved epoch depth d final p q ();
        image_def saved final rho p; images p;
        instance_at_def saved rho want p; children_available_def saved final old.desc;
        extends_def rest d; extends_def rest rest; extension_trans rest d final ();
        match old.desc with
        | Link child -> target_preserved saved epoch depth rest final child q ();
          target_image saved final rho child q (); images child; ()
        | _ -> ())
      else (
        extends_def rest d; extends_def rest rest; extension_trans rest d final ();
        let () = mapped_instance saved epoch depth rest final rho model p q want images () in ()))

let rec (template_sound @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) rho x})) @ total ->
    (want : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (images : ((x : node Pref.t) @ immutable -> {u : unit | want x === image saved d rho x})) @ total ->
    (t : template) @ immutable ->
    {u : unit | valid saved epoch depth d && template saved t && available saved d (root t)} ->
    {u : unit | interpret rho want t === want (root t)} @ ghost =
  fun saved epoch depth d rho model want images t premise -> ghost_ (
    template_def saved t; root_def t; interpret_def rho want t;
    let p = root t in template_head saved t p (); head_desc_def t; head_generic_def t;
    let p = root t in available_def saved d p; images p; image_def saved d rho p;
    (match mapping d p with None -> () | Some q ->
      extends_def d d; mapped_instance saved epoch depth d d rho model p q want images (); ());
    instance_at_def saved rho want p;
    match t with
    | Boundary _ | Parameter _ | Constant _ -> ()
    | Product (_, a, b) ->
      let desc = Arrow (root a, root b) in children_available_def saved d desc;
      template_sound saved epoch depth d rho model want images a ();
      template_sound saved epoch depth d rho model want images b (); ()
    | Indirect (_, child) ->
      let desc = Link (root child) in children_available_def saved d desc;
      template_sound saved epoch depth d rho model want images child (); ())
