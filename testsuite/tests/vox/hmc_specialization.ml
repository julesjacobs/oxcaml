module D = Hm_declarative
module F = Hmc_frontend
module T = Hmc_templates
module C = Hmc_monomorphic

type result = Unbound_variable | Type_error | Entry_type_mismatch
  | Unsupported_fragment of Hmc_admission.error | Compiled of C.program [@@inductive]

let compile : (source : D.term) @ immutable ->
    {r : result | match r with Compiled p -> T.rebuild p.C.source.T.globals p.C.source.T.entry === source
      | _ -> true} @ immutable = fun source ->
  match F.prepare source with
  | F.Unbound_variable -> Unbound_variable
  | F.Type_error -> Type_error
  | F.Entry_type_mismatch -> Entry_type_mismatch
  | F.Unsupported_fragment error -> Unsupported_fragment error
  | F.Prepared ready ->
    let expansion = Hmc_expansion.build (refine_ ready) in
    let manifest = Hmc_manifest.build expansion in
    let program = C.build manifest in
    ghost_ (Hmc_monomorphic_typing.program_typed program);
    Compiled program
