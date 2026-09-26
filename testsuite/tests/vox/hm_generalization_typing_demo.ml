(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_generalization.ml hm_generalization_proofs.ml hm_generalization_typing_demo.ml";
 { bytecode; }
 { native; }
*)
module D = Hm_declarative
module G = Hm_generalization

let (identity @ total) : (p : Copy_spec.node Pref.t) @ immutable ->
    {d : D.typing | D.typed D.Z D.Empty_context (D.Lambda (D.Bound D.Z))
      (D.Function (D.Free p, D.Free p)) d} @ immutable ghost =
  fun p -> ghost_ (
    let z = D.Z in let a = D.Free p in let scheme = D.Forall (z, a) in
    let context = D.Binding (scheme, D.Empty_context) in let args = D.No_arguments in
    let body = D.Variable args in let out = D.Abstraction (a, body) in
    D.add_def z z; D.mono_wf_def z a; D.scheme_wf_def z scheme;
    D.context_wf_def z D.Empty_context; D.context_wf_def z context;
    D.arguments_wf_def z args; D.length_def args; D.arity_def scheme;
    D.lookup_def context z; D.open_scheme_def scheme args; D.open_type_def args a;
    D.typed_def z context (D.Bound z) a body;
    D.mono_wf_def z (D.Function (a, a));
    D.typed_def z D.Empty_context (D.Lambda (D.Bound z)) (D.Function (a, a)) out;
    out)

let () =
  let allocated = Pref.alloc (Copy_spec.cell Copy_spec.Var 0) (Pref.empty ()) in
  let p = allocated.value in
  let source = D.Lambda (D.Bound D.Z) in
  let ty = D.Function (D.Free p, D.Free p) in
  let original = ghost_ (identity p) in
  let generalized = G.generalize D.Empty_context ty in
    let _typing = ghost_ (
      Hm_generalization_proofs.typing generalized.G.variables D.Z D.Empty_context source ty original ()) in
    match generalized.G.scheme with
    | D.Forall (D.S D.Z, D.Function (D.Parameter D.Z, D.Parameter D.Z)) -> ()
    | _ -> failwith "incorrect generalized identity"
