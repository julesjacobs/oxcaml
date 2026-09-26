module Ty = Copy_spec
module D = Hm_declarative
module G = Hm_generalization
module I = Hm_generalization_instances
module E = Hm_elaboration
module A = Hm_abstraction
module F = Hm_freshness_proofs
module P = Hm_template_instance_proofs

let rec (boundary_fixed @ total) : (names : A.names) @ immutable ->
    (scope : E.scope) @ immutable -> (selected : G.variables) @ immutable ->
    (replacement : (Ty.node Pref.t @ immutable total -> D.mono @ immutable total)) @ total ->
    (fixed : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (A.position names p === None) || replacement p === D.Free p})) @ total ->
    (ty : Ty.ty) @ immutable -> {u : unit | A.avoids names (D.embed ty)} ->
    {u : unit | I.substitute selected replacement (E.interpret scope ty) === E.interpret scope ty} @ ghost =
  fun names scope selected replacement fixed ty premise -> ghost_ (
    D.embed_def ty; A.avoids_def names (D.embed ty); E.interpret_def scope ty;
    I.substitute_def selected replacement (E.interpret scope ty);
    match ty with
    | Ty.Variable p -> fixed p;
      (match E.parameter p scope with
      | None -> I.substitute_def selected replacement (D.Free p)
      | Some index -> I.substitute_def selected replacement (D.Parameter index))
    | Ty.List_type element -> boundary_fixed names scope selected replacement fixed element ()
    | Ty.Function (argument, result) ->
      boundary_fixed names scope selected replacement fixed argument ();
      boundary_fixed names scope selected replacement fixed result ()
    | Ty.Boolean | Ty.Word64 -> ())

let rec (substitute_template @ total) : (names : A.names) @ immutable ->
    (scope : E.scope) @ immutable -> (selected : G.variables) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (replacement : (Ty.node Pref.t @ immutable total -> D.mono @ immutable total)) @ total ->
    (agrees : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | replacement p === (match A.position names p with
        None -> D.Free p | Some _ -> E.interpret scope (choices p))})) @ total ->
    (selected_names : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | A.position names p === None ||
        (not (G.find p selected === None) && E.parameter p scope === None)})) @ total ->
    (schema : Ty.template) @ immutable ->
    {u : unit | P.parameters_in names schema && P.boundaries_avoid names rho schema} ->
    {u : unit | I.substitute selected replacement
        (E.interpret scope (Ty.interpret rho F.variable_choice schema))
      === E.interpret scope (Ty.interpret rho choices schema)} @ ghost =
  fun names scope selected rho choices replacement agrees selected_names schema premise -> ghost_ (
    P.parameters_in_def names schema; P.boundaries_avoid_def names rho schema;
    Ty.interpret_def rho F.variable_choice schema; Ty.interpret_def rho choices schema;
    match schema with
    | Ty.Boundary p ->
      let fixed : ((q : Ty.node Pref.t) @ immutable ->
        {u : unit | not (A.position names q === None) || replacement q === D.Free q}) @ total = fun q ->
        agrees q; () in
      boundary_fixed names scope selected replacement fixed (rho p) ()
    | Ty.Parameter p -> selected_names p; agrees p; F.variable_choice_def p;
      E.interpret_def scope (Ty.Variable p); I.substitute_def selected replacement (D.Free p)
    | Ty.Constant _ ->
      E.interpret_def scope Ty.Boolean; I.substitute_def selected replacement D.Boolean
    | Ty.Word_constant _ ->
      E.interpret_def scope Ty.Word64; I.substitute_def selected replacement D.Word64
    | Ty.Indirect (_, child) ->
      substitute_template names scope selected rho choices replacement agrees selected_names child ()
    | Ty.List_template (_, child) ->
      E.interpret_def scope (Ty.List_type (Ty.interpret rho F.variable_choice child));
      E.interpret_def scope (Ty.List_type (Ty.interpret rho choices child));
      I.substitute_def selected replacement (D.List_type (E.interpret scope (Ty.interpret rho F.variable_choice child)));
      substitute_template names scope selected rho choices replacement agrees selected_names child ()
    | Ty.Product (_, left, right) ->
      E.interpret_def scope (Ty.Function (Ty.interpret rho F.variable_choice left, Ty.interpret rho F.variable_choice right));
      E.interpret_def scope (Ty.Function (Ty.interpret rho choices left, Ty.interpret rho choices right));
      I.substitute_def selected replacement (D.Function (
        E.interpret scope (Ty.interpret rho F.variable_choice left), E.interpret scope (Ty.interpret rho F.variable_choice right)));
      substitute_template names scope selected rho choices replacement agrees selected_names left ();
      substitute_template names scope selected rho choices replacement agrees selected_names right ())

let (complete @ total) : (scope : E.scope) @ immutable -> (selected : G.variables) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (schema : Ty.template) @ immutable ->
    (selected_names : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | A.position (F.template_names schema) p === None ||
        (not (G.find p selected === None) && E.parameter p scope === None)})) @ total ->
    {u : unit | P.boundaries_avoid (F.template_names schema) rho schema} ->
    {u : unit | match Hm_instantiation.infer
        (D.Forall (G.count selected, G.abstract (G.count selected) selected
          (E.interpret scope (Ty.interpret rho F.variable_choice schema))))
        (E.interpret scope (Ty.interpret rho choices schema)) with None -> false | Some _ -> true} @ ghost =
  fun scope selected rho choices schema selected_names premise -> ghost_ (
    let names = F.template_names schema in
    P.parameters_subset schema names (fun _p -> ());
    let[@def] replacement : Ty.node Pref.t @ immutable total -> D.mono @ immutable total = fun p ->
      match A.position names p with None -> D.Free p | Some _ -> E.interpret scope (choices p) in
    let agrees : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | replacement p === (match A.position names p with
        None -> D.Free p | Some _ -> E.interpret scope (choices p))}) @ total = fun p -> replacement_def p in
    substitute_template names scope selected rho choices replacement agrees (fun p -> selected_names p) schema ();
    I.complete selected replacement (E.interpret scope (Ty.interpret rho F.variable_choice schema)))

let (copy_complete @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (valid : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | Effective_level.valid_head heap heads p})) @ total ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (depth : int) -> (epoch : Ty.node Pref.t) @ immutable -> (history : Ty.history) @ immutable ->
    (schema : Ty.template) @ immutable -> (target : Ty.node Pref.t) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (model : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | Level_unifier_spec.node_equation (Hm_execution_spec.copy_heap heap epoch depth history) rho p})) @ total ->
    (scope : E.scope) @ immutable -> (selected : G.variables) @ immutable ->
    (selected_names : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | A.position (F.template_names schema) p === None ||
        (not (G.find p selected === None) && E.parameter p scope === None)})) @ total ->
    {u : unit | Copy_certificate_spec.certifies heap certificate epoch depth history (Ty.root schema) target
      && Effective_template.valid_template heap heads schema
      && P.boundaries_avoid (F.template_names schema) rho schema} ->
    {u : unit | match Hm_instantiation.infer
        (D.Forall (G.count selected, G.abstract (G.count selected) selected
          (E.interpret scope (Ty.interpret rho F.variable_choice schema))))
        (E.interpret scope (rho target)) with None -> false | Some _ -> true} @ ghost =
  fun heap heads valid certificate depth epoch history schema target rho model scope selected selected_names premise -> ghost_ (
    let original = Ty.root schema in
    Copy_certificate_proofs.replay heap certificate heads valid epoch depth history original target ();
    Hm_execution_spec.copy_heap_def heap epoch depth history;
    let after = Hm_execution_spec.copy_heap heap epoch depth history in
    let raw = Ty.heap heap epoch depth history in
    let trail = Pooled_spec.touched history in
    let clean_model : ((p : Ty.node Pref.t) @ immutable -> {u : unit | Ty.equation after rho p}) @ total = fun p ->
      model p; Level_unifier_spec.node_equation_def after rho p;
      Level_unifier_spec.observe_def after p; Ty.equation_def after rho p in
    let raw_model : ((p : Ty.node Pref.t) @ immutable -> {u : unit | Ty.equation raw rho p}) @ total = fun p ->
      clean_model p; Effective_copy_metadata.result_at heap heads epoch depth history p ();
      Copy_cleanup_proofs.sweep_model raw after trail rho p () in
    let[@def] choices : Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total = fun p ->
      Effective_copy_spec.effective_image heap heads history rho p in
    let images : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | choices p === Effective_copy_spec.effective_image heap heads history rho p}) @ total = fun p -> choices_def p in
    Effective_copy_sound.target_image heap heads history rho original target (); choices_def original;
    Effective_copy_sound.template_sound heap heads epoch depth history rho
      (fun p -> raw_model p) choices images schema ();
    complete scope selected rho choices schema selected_names ())

let rec (parameter_occurs @ total) : (scope : E.scope) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (schema : Ty.template) @ immutable -> (p : Ty.node Pref.t) @ immutable ->
    {u : unit | not (A.position (F.template_names schema) p === None) && E.parameter p scope === None} ->
    {u : unit | G.occurs p (E.interpret scope (Ty.interpret rho F.variable_choice schema))} @ ghost =
  fun scope rho schema p premise -> ghost_ (
    F.template_names_def schema; Ty.interpret_def rho F.variable_choice schema;
    match schema with
    | Ty.Boundary _ | Ty.Constant _ | Ty.Word_constant _ -> A.position_def A.No_names p
    | Ty.Parameter q ->
      A.position_def (A.Name (q, A.No_names)) p; A.position_def A.No_names p;
      F.variable_choice_def q; E.interpret_def scope (Ty.Variable q);
      G.occurs_def p (D.Free q); let _same = Pref.equal p q in ()
    | Ty.Indirect (_, child) -> parameter_occurs scope rho child p ()
    | Ty.List_template (_, child) ->
      E.interpret_def scope (Ty.List_type (Ty.interpret rho F.variable_choice child));
      G.occurs_def p (D.List_type (E.interpret scope (Ty.interpret rho F.variable_choice child)));
      parameter_occurs scope rho child p ()
    | Ty.Product (_, left, right) ->
      F.join_position (F.template_names left) (F.template_names right) p;
      E.interpret_def scope (Ty.Function (Ty.interpret rho F.variable_choice left, Ty.interpret rho F.variable_choice right));
      G.occurs_def p (D.Function (E.interpret scope (Ty.interpret rho F.variable_choice left),
        E.interpret scope (Ty.interpret rho F.variable_choice right)));
      match A.position (F.template_names left) p with
      | None -> parameter_occurs scope rho right p ()
      | Some _ -> parameter_occurs scope rho left p ())

let (collected_name @ total) : (scope : E.scope) @ immutable -> (context : D.context) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (schema : Ty.template) @ immutable -> (p : Ty.node Pref.t) @ immutable ->
    {u : unit | not (A.position (F.template_names schema) p === None)
      && E.parameter p scope === None && not (G.in_context p context)} ->
    {u : unit | not (G.find p (G.collect context
      (E.interpret scope (Ty.interpret rho F.variable_choice schema)) G.Empty) === None)} @ ghost =
  fun scope context rho schema p premise -> ghost_ (
    parameter_occurs scope rho schema p ();
    G.collect_member p context (E.interpret scope (Ty.interpret rho F.variable_choice schema)) G.Empty;
    G.find_def p G.Empty)

let (generalized_complete @ total) : (scope : E.scope) @ immutable -> (context : D.context) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (schema : Ty.template) @ immutable ->
    (unbound : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | A.position (F.template_names schema) p === None ||
        (not (G.in_context p context) && E.parameter p scope === None)})) @ total ->
    {u : unit | P.boundaries_avoid (F.template_names schema) rho schema} ->
    {u : unit | match Hm_instantiation.infer
        (G.generalize context (E.interpret scope (Ty.interpret rho F.variable_choice schema))).G.scheme
        (E.interpret scope (Ty.interpret rho choices schema)) with None -> false | Some _ -> true} @ ghost =
  fun scope context rho choices schema unbound premise -> ghost_ (
    let value = E.interpret scope (Ty.interpret rho F.variable_choice schema) in
    let generalized = G.generalize context value in
    let selected_names : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | A.position (F.template_names schema) p === None ||
        (not (G.find p generalized.G.variables === None) && E.parameter p scope === None)}) @ total = fun p ->
      unbound p; match A.position (F.template_names schema) p with
      | None -> () | Some _ -> collected_name scope context rho schema p () in
    complete scope generalized.G.variables rho choices schema selected_names ())

module Tree = Level_finite_spec
module U = Level_unifier_spec

let rec (canonical_after @ total) : (heap : Ty.node Pref.heap) @ immutable -> (heads : Effective_level.heads) @ total ->
    (after : Ty.node Pref.heap) @ immutable ->
    (preserve : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (Effective_template.generic heap heads p) ||
        (Ty.H.mem after p && U.observe after p === U.observe heap p)})) @ total ->
    (trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : Tree.tree | Tree.tree_root t === p &&
        (if Ty.H.mem after p then Tree.finite after t else U.observe after p === None)} @ immutable)) @ total ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (values : ((p : Ty.node Pref.t) @ immutable -> {u : unit | rho p === Tree.readback (trees p)})) @ total ->
    (schema : Ty.template) @ immutable -> {u : unit | Effective_template.valid_template heap heads schema} ->
    {u : unit | Ty.interpret rho F.variable_choice schema === rho (Ty.root schema)} @ ghost =
  fun heap heads after preserve trees rho values schema premise -> ghost_ (
    Effective_template.valid_template_def heap heads schema;
    Ty.root_def schema; Ty.interpret_def rho F.variable_choice schema;
    let p = Ty.root schema in
    (match schema with
    | Ty.Boundary _ -> ()
    | _ -> preserve p; Effective_template.generic_def heap heads p;
      Level_finite_proofs.readback_model_at after trees rho (fun q -> values q) p;
      U.node_equation_def after rho p);
    match schema with
    | Ty.Boundary _ | Ty.Constant _ | Ty.Word_constant _ -> ()
    | Ty.Parameter p ->
      F.variable_choice_def p; Tree.tree_root_def (Tree.Free p);
      Tree.finite_def after (Tree.Free p); Tree.readback_def (Tree.Free p);
      let actual = trees p in values p;
      Level_finite_proofs.finite_unique after (Tree.Free p) actual ()
    | Ty.Indirect (_, child) | Ty.List_template (_, child) -> canonical_after heap heads after preserve trees rho values child ()
    | Ty.Product (_, left, right) ->
      canonical_after heap heads after preserve trees rho values left (); canonical_after heap heads after preserve trees rho values right ())

let (canonical @ total) : (heap : Ty.node Pref.heap) @ immutable -> (heads : Effective_level.heads) @ total ->
    (trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : Tree.tree | Tree.tree_root t === p &&
        (if Ty.H.mem heap p then Tree.finite heap t else U.observe heap p === None)} @ immutable)) @ total ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (values : ((p : Ty.node Pref.t) @ immutable -> {u : unit | rho p === Tree.readback (trees p)})) @ total ->
    (schema : Ty.template) @ immutable -> {u : unit | Effective_template.valid_template heap heads schema} ->
    {u : unit | Ty.interpret rho F.variable_choice schema === rho (Ty.root schema)} @ ghost =
  fun heap heads trees rho values schema premise -> ghost_ (
    let preserve : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (Effective_template.generic heap heads p) ||
        (Ty.H.mem heap p && U.observe heap p === U.observe heap p)}) @ total = fun p ->
      Effective_template.generic_def heap heads p in
    canonical_after heap heads heap preserve trees rho values schema ())

let (canonical_run @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total -> (next : Effective_level.heads) @ total ->
    (valid : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | Effective_level.valid_head heap heads p})) @ total ->
    (depth : int) -> (pool : Generalize_spec.pool) @ immutable ->
    (env : Hm_environment_spec.env) @ immutable ->
    (execution : Hm_effective_execution_spec.execution) @ immutable ->
    (after : Ty.node Pref.heap) @ immutable -> (final_pool : Generalize_spec.pool) @ immutable ->
    (next_valid : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | Effective_level.valid_head after next p})) @ total ->
    (trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : Tree.tree | Tree.tree_root t === p &&
        (if Ty.H.mem after p then Tree.finite after t else U.observe after p === None)} @ immutable)) @ total ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (values : ((p : Ty.node Pref.t) @ immutable -> {u : unit | rho p === Tree.readback (trees p)})) @ total ->
    (schema : Ty.template) @ immutable ->
    {u : unit | Hm_effective_execution_spec.ran heap depth pool env execution after final_pool
      && Effective_template.valid_template heap heads schema} ->
    {u : unit | Ty.interpret rho F.variable_choice schema === rho (Ty.root schema)} @ ghost =
  fun heap heads next valid depth pool env execution after final_pool next_valid trees rho values schema premise -> ghost_ (
    let preserve : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (Effective_template.generic heap heads p) ||
        (Ty.H.mem after p && U.observe after p === U.observe heap p)}) @ total = fun p ->
      valid p; next_valid p;
      Hm_effective_generic.run_protected heap heads next depth pool env execution after final_pool p depth ();
      Effective_template.protected_def heap heads after next depth p;
      Effective_template.generic_def after next p in
    canonical_after heap heads after preserve trees rho values schema ())

let (canonical_close @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total -> (cut : int) ->
    (pool : Generalize_spec.pool) @ immutable -> (after : Ty.node Pref.heap) @ immutable ->
    (trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : Tree.tree | Tree.tree_root t === p &&
        (if Ty.H.mem after p then Tree.finite after t else U.observe after p === None)} @ immutable)) @ total ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (values : ((p : Ty.node Pref.t) @ immutable -> {u : unit | rho p === Tree.readback (trees p)})) @ total ->
    (schema : Ty.template) @ immutable ->
    {u : unit | Generalize_spec.pool_scoped heap pool
      && after === Representative_pool_spec.close_heap heap cut pool
      && Effective_template.valid_template heap heads schema} ->
    {u : unit | Ty.interpret rho F.variable_choice schema === rho (Ty.root schema)} @ ghost =
  fun heap heads cut pool after trees rho values schema premise -> ghost_ (
    Representative_pool_spec.close_heap_def heap cut pool;
    let filtered = Representative_level.representatives heap pool in
    Representative_level.representatives_scoped heap pool ();
    let preserve : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (Effective_template.generic heap heads p) ||
        (Ty.H.mem after p && U.observe after p === U.observe heap p)}) @ total = fun p ->
      Effective_template.generic_def heap heads p;
      Generalize_proofs.closed_observe heap cut filtered p ();
      Generalize_spec.closed_at_def heap after cut filtered p;
      U.observe_def heap p; U.observe_def after p in
    canonical_after heap heads after preserve trees rho values schema ())
