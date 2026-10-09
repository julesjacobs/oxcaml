open Copy_spec
open Copy_heap_proofs
open Copy_complete_proofs
open Copy_sound_proofs

let rec (template_unique @ total) : (saved : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (a : template) @ immutable -> (b : template) @ immutable ->
    {u : unit | template saved a && template saved b && root a === root b} ->
    {u : unit | interpret rho choices a === interpret rho choices b} @ ghost =
  fun saved rho choices a b premise -> ghost_ (
    template_def saved a; template_def saved b; root_def a; root_def b;
    interpret_def rho choices a; interpret_def rho choices b;
    let pa = root a in let pb = root b in
    template_head saved a pa (); template_head saved b pb ();
    head_desc_def a; head_desc_def b; head_generic_def a; head_generic_def b;
    match a with
    | Product (_, a1, a2) -> (match b with Product (_, b1, b2) ->
      template_unique saved rho choices a1 b1 (); template_unique saved rho choices a2 b2 (); ()
      | _ -> ())
    | Indirect (_, a) -> (match b with Indirect (_, b) -> template_unique saved rho choices a b (); ()
      | _ -> ())
    | _ -> ())

let (forest_eval @ total) : (saved : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : template | not (H.mem saved x) ||
      (root t === x && template saved t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (want : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((x : node Pref.t) @ immutable -> {u : unit |
      let refine_ t = trees x in want x === interpret rho choices t})) @ total ->
    (t : template) @ immutable -> {u : unit | template saved t} ->
    {u : unit | want (root t) === interpret rho choices t} @ ghost =
  fun saved trees rho choices want values t premise -> ghost_ (
    template_def saved t; let p = root t in
    let actual = trees p in values p; template_unique saved rho choices actual t (); ())

let (forest_instance @ total) : (saved : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (trees : ((x : node Pref.t) @ immutable -> {t : template | not (H.mem saved x) ||
      (root t === x && template saved t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (want : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((x : node Pref.t) @ immutable -> {u : unit |
      let refine_ t = trees x in want x === interpret rho choices t})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | instance_at saved rho want x} @ ghost =
  fun saved scope trees rho choices want values x -> ghost_ (
    scope x; instance_at_def saved rho want x;
    if H.mem saved x then (
      let t = trees x in values x; template_def saved t; root_def t; interpret_def rho choices t;
      template_head saved t x (); head_desc_def t; head_generic_def t;
      match t with
      | Boundary _ | Parameter _ | Constant _ -> ()
      | Product (_, a, b) -> forest_eval saved trees rho choices want values a ();
        forest_eval saved trees rho choices want values b (); ()
      | Indirect (_, child) -> forest_eval saved trees rho choices want values child (); ())
    else ())

let (with_scheme_instance @ total) : (saved : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (trees : ((x : node Pref.t) @ immutable -> {t : template | not (H.mem saved x) ||
      (root t === x && template saved t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation saved rho x})) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (t : template) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d && template saved t && target_for saved d (root t) q} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x})) @ total ->
      {u : unit | tau q === interpret rho choices t} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun saved scope trees rho model choices epoch depth d t q premise claim use -> ghost_ (
    let[@def] want : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
      let t = trees x in interpret rho choices t in
    let values : (x : node Pref.t) @ immutable -> {u : unit | let refine_ t = trees x in want x === interpret rho choices t}
        @ total = fun x -> want_def x; () in
    let wanted : (x : node Pref.t) @ immutable -> {u : unit | instance_at saved rho want x}
        @ total = fun x -> let () = forest_instance saved scope trees rho choices want values x in () in
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x})) @ total ->
        (assigned : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
          {u : unit | not (target_for saved d p q) || tau q === want p})) @ total -> {u : unit | claim}) @ total =
      fun tau next equal assigned -> let p = root t in assigned p q;
        forest_eval saved trees rho choices want values t ();
        let () = use tau next equal () in () in
    let () = with_copy_model saved scope rho model want wanted epoch depth d () claim consume in ())

let (with_instance_choices @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) rho x})) @ total ->
    (t : template) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d && template saved t && target_for saved d (root t) q} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | rho q === interpret rho choices t} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun saved epoch depth d rho model t q premise claim use -> ghost_ (
    let[@def] choices : node Pref.t @ immutable total -> ty @ immutable total = fun x -> image saved d rho x in
    let images : (x : node Pref.t) @ immutable -> {u : unit | choices x === image saved d rho x}
        @ total = fun x -> choices_def x; () in
    let p = root t in target_image saved d rho p q (); choices_def p;
    template_sound saved epoch depth d rho model choices images t ();
    let () = use choices () in ())
