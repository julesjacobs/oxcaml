module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module H = Hmc_monomorphic_typing
module B = Hmc_specialized_body
module I = Hmc_instance
module T = Hmc_templates
module G = Hmc_ground_type

let[@def] (callable @ total) (code : C.term @ immutable) = match code with
  | C.Lambda _ | C.Recursive _ -> true | _ -> false

let (fetch @ total) : (program : C.program) @ immutable ->
    (id : {i : D.index | D.present (M.size (C.manifest program.C.definitions)) i}) @ immutable ->
    {d : C.definition | callable d.C.code
      && M.lookup (C.manifest program.C.definitions) id === Some {M.body = d.C.body; dependencies = C.links d.C.code}
      && H.typed (C.manifest program.C.definitions) D.Empty_context d.C.code
        (G.mono d.C.body.B.origin.I.ty) d.C.body.B.derivation} @ immutable = fun program id ->
  ghost_ (C.ready_def program);
  let definitions : {d : C.definitions | C.origins d} = refine_ program.C.definitions in
  let table = C.manifest definitions in
  ghost_ (M.lookup_present table id ());
  match C.lookup definitions id with
  | None -> unreachable_ ()
  | Some d ->
    ghost_ (
      I.valid_def d.C.body.B.origin;
      T.definition_valid_def d.C.body.B.origin.I.earlier d.C.body.B.origin.I.definition;
      Hmc_admission.callable_def d.C.body.B.origin.I.definition.T.source;
      callable_def d.C.code; C.erase_def d.C.code;
      let entry = {M.body = d.C.body; dependencies = C.links d.C.code} in
      M.lookup_closed table id entry ();
      B.valid_def d.C.body;
      Hmc_reference_tree.context_def D.Empty_context (T.context d.C.body.B.origin.I.earlier);
      H.preservation table d.C.body.B.origin.I.earlier D.Empty_context d.C.code
        (G.mono d.C.body.B.origin.I.ty) d.C.body.B.derivation ());
    d
