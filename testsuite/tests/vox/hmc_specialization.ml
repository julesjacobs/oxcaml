(* Specialization: the frontend, then a monomorphic program with one copy
   of a top-level function for each use of it.

   In the grounded derivation, each use of a top-level definition in the
   entry is at a ground instance of its scheme, and so is each use in the
   body of a definition instantiated at ground types
   ([Hmc_specialized_body.instantiate]). Hmc_reference_tree records the
   instance at each use; Hmc_expansion expands each instance's body in turn,
   and Hmc_manifest flattens the resulting tree into a table in which each
   specialized body refers only to entries before it. The expansion
   terminates because a body refers only to earlier definitions: its fuel
   is the number of definitions. [C.build] then rewrites the entry and each
   body to refer to table entries.

   The result type says only that the program rebuilds to the source; the
   invariant [C.ready] of [C.program] is what the typing lemmas
   (Hmc_monomorphic_typing) and Hmc_monomorphic_simulation use. *)
module D = Hm_declarative
module F = Hmc_frontend
module T = Hmc_templates
module C = Hmc_monomorphic

type result = Unbound_variable
  | Type_error of F.inference [@immediate_all_void_constructor]
  | Entry_type_mismatch of F.inference [@immediate_all_void_constructor]
  | Unsupported_fragment of Hmc_admission.error | Compiled of C.program [@@inductive]

let compile : (source : D.term) @ immutable ->
    {r : result | match r with
      | Compiled p -> T.rebuild p.C.source.T.globals p.C.source.T.entry === source
      | Unbound_variable -> not (D.scoped_term D.Z source)
      | Type_error run -> F.untyped source run
      | Entry_type_mismatch run -> F.mistyped source run
      | Unsupported_fragment error -> Hmc_admission.meaning source error} @ immutable = fun source ->
  match F.prepare source with
  | F.Unbound_variable -> Unbound_variable
  | F.Type_error run -> Type_error run
  | F.Entry_type_mismatch run -> Entry_type_mismatch run
  | F.Unsupported_fragment error -> Unsupported_fragment error
  | F.Prepared ready ->
    let expansion = Hmc_expansion.build (refine_ ready) in
    let manifest = Hmc_manifest.build expansion in
    let program = C.build manifest in
    ghost_ (Hmc_monomorphic_typing.program_typed program);
    Compiled program
