(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_declarative_demo.ml hm_substitution.ml hm_substitution_proofs.ml hm_substitution_demo.ml";
 { bytecode; }
*)
open Hm_declarative
open Hm_type_proofs
open Hm_substitution
open Hm_substitution_proofs

let (substituted_identity @ total) :
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    (p : Copy_spec.node Pref.t) @ immutable ->
    {d : typing | typed Z Empty_context
      (Let (Lambda (Bound Z), Apply (Bound Z, Bound Z)))
      (Function (embed (rho p), embed (rho p))) d} @ immutable ghost = fun rho p -> ghost_ (
    let z = Z in let a = Free p in mono_wf_def z a; let u = () in
    let refine_ original = Hm_declarative_demo.id_id a (refine_ u) in
    let bound = Bound z in let source = Let (Lambda bound, Apply (bound, bound)) in
    let t = Function (a, a) in let g = Empty_context in
    substitution_typed rho z g source t original (refine_ u);
    substitute_context_def rho g; substitute_type_def rho t; substitute_type_def rho a;
    let d = substitute_typing rho original in refine_ d)

let () =
  let refine_ state = Pref.empty () in
  let v = Copy_spec.cell Copy_spec.Var 0 in
  let refine_ step = Pref.alloc v state in let p = step.value in
  let rho : Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total =
    fun _ -> Copy_spec.Function (Copy_spec.Boolean, Copy_spec.Boolean) in
  ghost_ (let _ = substituted_identity rho p in ());
  let z = Z in let one = S z in
  assert (substitute_type rho (Parameter z) = Parameter z);
  assert (substitute_scheme rho (Forall (one, Function (Parameter z, Free p)))
    = Forall (one, Function (Parameter z, Function (Boolean, Boolean))))
