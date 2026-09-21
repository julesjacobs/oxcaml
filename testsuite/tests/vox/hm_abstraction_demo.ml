(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hm_declarative.ml hm_type_proofs.ml hm_declarative_demo.ml hm_substitution.ml hm_substitution_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_abstraction_demo.ml";
 { bytecode; }
 { native; }
*)
open Hm_declarative
open Hm_type_proofs
open Hm_abstraction
open Hm_abstraction_proofs

let (abstracted_id_id @ total) : (p : Copy_spec.node Pref.t) @ immutable ->
    {d : typing | typed (S Z) Empty_context
      (Let (Lambda (Bound Z), Apply (Bound Z, Bound Z)))
      (Function (Parameter Z, Parameter Z)) d} @ immutable ghost = fun p -> ghost_ (
    let z = Z in let a = Free p in mono_wf_def z a;
    let u = () in let original = Hm_declarative_demo.id_id a (u) in
    let no_names = No_names in let names = Name (p, no_names) in
    let empty = Empty_context in context_avoids_def names empty;
    let bound = Bound z in let source = Let (Lambda bound, Apply (bound, bound)) in
    let t = Function (a, a) in generalize_typing names z empty source t original (u);
    count_def no_names; count_def names; let one = S z in add_def one z; add_def z z;
    weaken_context_def one empty; abstract_type_def names z t; abstract_type_def names z a;
    abstract_free_def names z p; position_def names p;
    let d = abstract_typing names z original in d)

let (abstract_then_substitute @ total) : (p : Copy_spec.node Pref.t) @ immutable ->
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    {d : typing | typed (S Z) Empty_context
      (Let (Lambda (Bound Z), Apply (Bound Z, Bound Z)))
      (Function (Parameter Z, Parameter Z)) d} @ immutable ghost = fun p rho -> ghost_ (
    let original = abstracted_id_id p in
    let z = Z in let one = S z in let a = Parameter z in let t = Function (a, a) in
    let g = Empty_context in let bound = Bound z in let source = Let (Lambda bound, Apply (bound, bound)) in
    let u = () in Hm_substitution_proofs.substitution_typed rho one g source t original (u);
    Hm_substitution.substitute_context_def rho g;
    Hm_substitution.substitute_type_def rho t; Hm_substitution.substitute_type_def rho a;
    let d = Hm_substitution.substitute_typing rho original in d)

let () =
  let state = Pref.empty () in let v = Copy_spec.cell Copy_spec.Var 0 in
  let allocated = Pref.alloc v state in let p = allocated.value in
  ghost_ (
    let rho : Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total =
      fun _ -> Copy_spec.Variable p in
    let _ = abstract_then_substitute p rho in ());
  ()
