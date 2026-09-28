module Ty = Copy_spec
module H = Pref.Heap
module U = Level_unifier_spec
module F = Level_finite_spec
module R = Representative_level
module E = Effective_level
module T = Effective_template
module Env = Hm_environment_spec
module Fresh = Hm_elaboration_freshness
module Generic = Hm_effective_generic
module Scope = Hm_elaboration
module D = Hm_declarative
module G = Hm_generalization
module A = Hm_abstraction
module Names = Hm_freshness_proofs
module Templates = Hm_template_instance_proofs

let (generalized @ total) : (heap : Ty.node Pref.heap) @ immutable -> (heads : E.heads) @ total ->
    (safe : ((p : Ty.node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe heap heads p})) @ total ->
    (after : Ty.node Pref.heap) @ immutable ->
    (owned : ((p : Ty.node Pref.t) @ immutable -> {u : unit | not (H.mem heap p) || H.mem after p})) @ total ->
    (future : ((protected : (Ty.node Pref.t @ immutable total -> bool)) @ total ->
      (closed : ((p : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at heap protected p})) @ total ->
      (generic : ((p : Ty.node Pref.t) @ immutable ->
        {r : R.representative | not (protected p) || Fresh.generic_at heap p r} @ immutable)) @ total ->
      (p : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at after protected p})) @ total ->
    (preserve : ((p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
      (desc : Ty.desc option) @ immutable -> {u : unit | Generic.generic_path heap p r desc} ->
      {s : R.representative | Generic.generic_path after p s desc} @ immutable)) @ total ->
    (trees : ((p : Ty.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem after p then F.finite after t else U.observe after p === None)} @ immutable)) @ total ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (values : ((p : Ty.node Pref.t) @ immutable -> {u : unit | rho p === F.readback (trees p)})) @ total ->
    (depth : int) -> (env : Env.env) @ immutable -> (schemas : Env.templates) @ immutable ->
    (scope : Scope.scope) @ immutable -> (context : D.context) @ immutable ->
    (support : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (G.in_context p context) || G.in_context p (Templates.context rho schemas)})) @ total ->
    (scope_fresh : ((p : Ty.node Pref.t) @ immutable -> (r : R.representative) @ immutable ->
      {u : unit | Fresh.generic_at heap p r} -> {u : unit | Scope.parameter p scope === None})) @ total ->
    (schema : Ty.template) @ immutable ->
    (choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    {u : unit | Hm_effective_environment.effective_env heap heads depth env schemas
      && T.valid_template heap heads schema && D.context_wf (Scope.depth scope) context} ->
    {d : D.typing | D.typed (Scope.depth scope)
      (D.Binding ((G.generalize context (Scope.interpret scope (rho (Ty.root schema)))).G.scheme, context))
      (D.Bound D.Z) (Scope.interpret scope (Ty.interpret rho choices schema)) d} @ immutable ghost =
  fun heap heads safe after owned future preserve trees rho values depth env schemas scope context support scope_fresh schema choices premise -> ghost_ (
    let[@def] protected : Ty.node Pref.t @ immutable total -> bool = fun p -> T.generic heap heads p in
    let agrees : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | protected p = T.generic heap heads p}) @ total = fun p -> protected_def p in
    let valid : ((p : Ty.node Pref.t) @ immutable -> {u : unit | E.valid_head heap heads p}) @ total = fun p ->
      safe p; Hm_effective_runtime.safe_def heap heads p in
    let closed : ((p : Ty.node Pref.t) @ immutable -> {u : unit | Fresh.closed_at heap protected p}) @ total = fun p ->
      safe p; Hm_effective_runtime.safe_def heap heads p;
      Fresh.initial heap heads valid protected agrees p () in
    let generic : ((p : Ty.node Pref.t) @ immutable ->
      {r : R.representative | not (protected p) || Fresh.generic_at heap p r} @ immutable) @ total = fun p ->
      let r = heads p in
      protected_def p; T.generic_def heap heads p;
      valid p; E.valid_head_def heap heads p; E.level_def heap heads p;
      Fresh.generic_at_def heap p r; r in
    let final_closed : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | Fresh.closed_at after protected p}) @ total = fun p ->
      future protected closed generic p in
    let selected : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | A.position (Names.template_names schema) p === None || protected p}) @ total = fun p ->
      Fresh.parameters_protected heap heads protected agrees schema p () in
    Fresh.boundaries_after heap heads after protected agrees owned final_closed trees rho values
      (Names.template_names schema) (fun p -> selected p) schema ();
    let unbound : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | A.position (Names.template_names schema) p === None
        || (not (G.in_context p context) && Scope.parameter p scope === None)}) @ total = fun p ->
      selected p;
      if protected p then (
        Fresh.context_after heap heads after protected agrees owned final_closed trees rho values depth env schemas p ();
        support p; let r = generic p in scope_fresh p r ());
      () in
    let canonical : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | not (T.generic heap heads p) || (H.mem after p && U.observe after p === U.observe heap p)}) @ total = fun p ->
      protected_def p;
      if protected p then (
        let r = generic p in Fresh.generic_at_def heap p r;
        Generic.generic_path_def heap p r (U.observe heap p);
        let next = preserve p r (U.observe heap p) () in
        Generic.generic_path_def after p next (U.observe heap p);
        U.resolves_def after p next.R.root next.R.path);
      () in
    Hm_template_generalization.canonical_after heap heads after canonical trees rho values schema ();
    Hm_elaboration_instance_scope.generalized_binding scope context rho choices schema (fun p -> unbound p) ())

let (copied @ total) : (heap : Ty.node Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (valid : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | Effective_level.valid_head heap heads p})) @ total ->
    (certificate : Representative_certificate.certificate) @ immutable ->
    (depth : int) -> (epoch : Ty.node Pref.t) @ immutable -> (history : Ty.history) @ immutable ->
    (schema : Ty.template) @ immutable -> (target : Ty.node Pref.t) @ immutable ->
    (rho : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
    (model : ((p : Ty.node Pref.t) @ immutable ->
      {u : unit | Level_unifier_spec.node_equation (Hm_execution_spec.copy_heap heap epoch depth history) rho p})) @ total ->
    (scope : Scope.scope) @ immutable -> (context : D.context) @ immutable ->
    (index : D.index) @ immutable ->
    (instances : ((choices : (Ty.node Pref.t @ immutable total -> Ty.ty @ immutable total)) @ total ->
      {d : D.typing | D.typed (Scope.depth scope) context (D.Bound index)
        (Scope.interpret scope (Ty.interpret rho choices schema)) d} @ immutable)) @ total ->
    {u : unit | Copy_certificate_spec.certifies heap certificate epoch depth history (Ty.root schema) target
      && Effective_template.valid_template heap heads schema} ->
    {d : D.typing | D.typed (Scope.depth scope) context (D.Bound index)
      (Scope.interpret scope (rho target)) d} @ immutable ghost =
  fun heap heads valid certificate depth epoch history schema target rho model scope context index instances premise -> ghost_ (
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
    instances choices)
