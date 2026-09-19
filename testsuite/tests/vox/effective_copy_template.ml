open Copy_spec
open Copy_heap_proofs
open Effective_copy_spec
open Effective_copy_heap_proofs
open Effective_copy_complete
open Effective_copy_sound
let (forest_eval @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (trees : ((x : node Pref.t) @ immutable -> {t : template | not (H.mem saved x) ||
      (root t === x && Effective_template.valid_template saved heads t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (want : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((x : node Pref.t) @ immutable -> {u : unit |
      let refine_ t = trees x in want x === interpret rho choices t})) @ total ->
    (t : template) @ immutable -> {u : unit | Effective_template.valid_template saved heads t} ->
    {u : unit | want (root t) === interpret rho choices t} @ ghost =
  fun saved heads trees rho choices want values t premise -> ghost_ (
    Effective_template.valid_template_def saved heads t; let p = root t in
    let refine_ actual = trees p in values p; Effective_template.unique saved heads rho choices actual t (); ())

let (forest_instance @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (trees : ((x : node Pref.t) @ immutable -> {t : template | not (H.mem saved x) ||
      (root t === x && Effective_template.valid_template saved heads t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (want : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (values : ((x : node Pref.t) @ immutable -> {u : unit |
      let refine_ t = trees x in want x === interpret rho choices t})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | effective_instance_at saved heads rho want x} @ ghost =
  fun saved heads scope trees rho choices want values x -> ghost_ (
    scope x; effective_instance_at_def saved heads rho want x;
    if H.mem saved x then (
      let refine_ t = trees x in values x; Effective_template.valid_template_def saved heads t; root_def t; interpret_def rho choices t;
      Effective_template.head saved heads t ();
      Effective_template.finite_def saved heads x; Effective_template.generic_def saved heads x;
      Level_unifier_spec.observe_def saved x; head_desc_def t; head_generic_def t;
      match t with
      | Boundary _ | Parameter _ | Constant _ -> ()
      | Product (_, a, b) -> forest_eval saved heads trees rho choices want values a ();
        forest_eval saved heads trees rho choices want values b (); ()
      | Indirect (_, child) -> forest_eval saved heads trees rho choices want values child (); ())
    else ())

let (with_scheme_instance @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (trees : ((x : node Pref.t) @ immutable -> {t : template | not (H.mem saved x) ||
      (root t === x && Effective_template.valid_template saved heads t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation saved rho x})) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (t : template) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && Effective_template.valid_template saved heads t && effective_target_for saved heads d (root t) q} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x})) @ total ->
      {u : unit | tau q === interpret rho choices t} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun saved heads scope trees rho model choices epoch depth d t q premise claim use -> ghost_ (
    let[@def] want : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
      let refine_ t = trees x in interpret rho choices t in
    let values : (x : node Pref.t) @ immutable -> {u : unit | let refine_ t = trees x in want x === interpret rho choices t}
        @ total = fun x -> want_def x; () in
    let wanted : (x : node Pref.t) @ immutable -> {u : unit | effective_instance_at saved heads rho want x}
        @ total = fun x -> let () = forest_instance saved heads scope trees rho choices want values x in () in
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x})) @ total ->
        (assigned : ((p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
          {u : unit | not (effective_target_for saved heads d p q) || tau q === want p})) @ total -> {u : unit | claim}) @ total =
      fun tau next equal assigned -> let p = root t in assigned p q;
        forest_eval saved heads trees rho choices want values t ();
        let () = use tau next equal () in () in
    let () = with_copy_model saved heads scope rho model want wanted epoch depth d () claim consume in ())

let (with_instance_choices @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) rho x})) @ total ->
    (t : template) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && Effective_template.valid_template saved heads t && effective_target_for saved heads d (root t) q} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | rho q === interpret rho choices t} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun saved heads epoch depth d rho model t q premise claim use -> ghost_ (
    let[@def] choices : node Pref.t @ immutable total -> ty @ immutable total = fun x -> effective_image saved heads d rho x in
    let images : (x : node Pref.t) @ immutable -> {u : unit | choices x === effective_image saved heads d rho x}
        @ total = fun x -> choices_def x; () in
    let p = root t in target_image saved heads d rho p q (); choices_def p;
    template_sound saved heads epoch depth d rho model choices images t ();
    let () = use choices () in ())

let (with_clean_scheme_instance @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (trees : ((x : node Pref.t) @ immutable -> {t : template | not (H.mem saved x) ||
      (root t === x && Effective_template.valid_template saved heads t)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation saved rho x})) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (t : template) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && Effective_template.valid_template saved heads t && effective_target_for saved heads d (root t) q} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (Copy_cleanup_spec.swept (heap saved epoch depth d) (Pooled_spec.touched d)) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x})) @ total ->
      {u : unit | tau q === interpret rho choices t} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun saved heads scope trees rho model choices epoch depth d t q premise claim use -> ghost_ (
    let raw = heap saved epoch depth d in let trail = Pooled_spec.touched d in
    let after = Copy_cleanup_spec.swept raw trail in
    let consume : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (model_raw : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || tau x === rho x})) @ total ->
        {u : unit | tau q === interpret rho choices t} -> {u : unit | claim}) @ total = fun tau model_raw equal fit ->
      let next : ((x : node Pref.t) @ immutable -> {u : unit | equation after tau x}) @ total = fun x ->
        model_raw x; Effective_copy_metadata.model_equivalence saved heads epoch depth d tau x (); () in
      let () = use tau (refine_ next) equal () in () in
    let () = with_scheme_instance saved heads scope trees rho model choices epoch depth d t q () claim (refine_ consume) in ())

let (with_clean_instance_choices @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation (Copy_cleanup_spec.swept (heap saved epoch depth d) (Pooled_spec.touched d)) rho x})) @ total ->
    (t : template) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && Effective_template.valid_template saved heads t && effective_target_for saved heads d (root t) q} -> (claim : bool) ->
    (use : ((choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      {u : unit | rho q === interpret rho choices t} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun saved heads epoch depth d rho model t q premise claim use -> ghost_ (
    let raw = heap saved epoch depth d in
    let model_raw : ((x : node Pref.t) @ immutable -> {u : unit | equation raw rho x}) @ total = fun x ->
      model x; Effective_copy_metadata.model_equivalence saved heads epoch depth d rho x (); () in
    let () = with_instance_choices saved heads epoch depth d rho (refine_ model_raw) t q () claim use in ())
