module D = Hm_declarative
module S = Hm_interpreter_substitution
module P = Hm_interpreter_substitution_proofs
module W = Hmc_parameter_typing
module C = Hmc_parameter_closed
module G = Hmc_ground_type
module A = Hmc_ground_arguments
module N = Hmc_no_free
module I = Hmc_instance
module T = Hmc_templates

let rec (ground_wf @ total) : (ty : D.mono) @ immutable -> {u : unit | G.ground ty} ->
    {u : unit | D.mono_wf D.Z ty && N.mono ty} @ ghost = fun ty premise -> ghost_ (
    G.ground_def ty; D.mono_wf_def D.Z ty; N.mono_def ty;
    match ty with
    | D.Boolean | D.Word64 | D.Free _ | D.Parameter _ -> ()
    | D.List_type a -> ground_wf a ()
    | D.Function (a, b) -> ground_wf a (); ground_wf b ())

let rec (ground_arguments @ total) : (args : D.arguments) @ immutable -> {u : unit | A.ground args} ->
    {u : unit | D.arguments_wf D.Z args && N.arguments args} @ ghost = fun args premise -> ghost_ (
    A.ground_def args; D.arguments_wf_def D.Z args; N.arguments_def args;
    match args with D.No_arguments -> () | D.Argument (ty, rest) -> ground_wf ty (); ground_arguments rest ())

let[@def] (substitution @ total) (instance : I.instance @ immutable) =
  {S.front = A.declarative instance.I.key.A.arguments; tail = D.Z}

type payload = {origin : I.instance; derivation : D.typing}

let[@def] (valid @ total) (body : payload @ immutable) = ghost_ (
  D.typed D.Z (T.context body.origin.I.earlier) body.origin.I.definition.T.source
    (G.mono body.origin.I.ty) body.derivation
  && Hmc_ground_annotations.typing body.derivation
  && N.typing body.derivation && Hmc_admission.local body.origin.I.definition.T.source body.derivation
  && body.derivation === S.act_typing (substitution body.origin) body.origin.I.definition.T.derivation)

type t = {p : payload | valid p}

let (instantiate @ total) : (instance : I.instance) @ immutable ->
    {body : t | body.origin === instance} @ immutable = fun instance ->
  let definition = instance.I.definition in
  let args = A.declarative instance.I.key.A.arguments in
  let action = substitution instance in
  let proof = S.act_typing action definition.T.derivation in
  ghost_ (
    I.valid_def instance; T.definition_valid_def instance.I.earlier definition;
    substitution_def instance; A.represented instance.I.key.A.arguments;
    ground_arguments args ();
    C.typing_no_free action definition.T.derivation ();
    C.local_shape action definition.T.source definition.T.derivation ();
    C.catalog_context instance.I.earlier ();
    match definition.T.scheme with D.Forall (k, ty) ->
      D.arity_def definition.T.scheme; D.open_scheme_def definition.T.scheme args;
      Hm_abstraction_proofs.add_zero k;
      let source = D.add k D.Z in let target = D.Z in
      let mapping : ((i : D.index) @ immutable ->
        {u : unit | not (D.present source i) || D.mono_wf target (S.at action.S.front action.S.tail i)}) @ total =
        fun i ->
          if D.present source i then (
            A.parameter_ground args i (); P.at_open args i; ground_wf (D.open_index args i) (); ()) else () in
      let context = D.weaken_context k (T.context instance.I.earlier) in
      W.typing_action action source target mapping context definition.T.source ty definition.T.derivation ();
      C.closed_context action k (T.context instance.I.earlier) ();
      P.action_open args ty;
      Hmc_ground_annotations.typing_ground (T.context instance.I.earlier) definition.T.source
        (G.mono instance.I.ty) proof ());
  let out = {origin = instance; derivation = proof} in
  ghost_ (valid_def out);
  let out : t = refine_ out in out
