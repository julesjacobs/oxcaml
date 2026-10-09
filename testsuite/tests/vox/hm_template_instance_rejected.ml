(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml level_finite_spec.ml hm_declarative.ml hm_type_proofs.ml hm_environment_spec.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_freshness_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_template_instance_proofs.ml";
 readonly_files = "hm_template_instance_rejected.ml";
 compile_only = "true";
 { setup-ocamlc.byte-build-env; ocamlc.byte; run-expect; check-program-output; }
 { setup-ocamlopt.byte-build-env; ocamlopt.byte; run-expectnat; check-program-output; }
*)
open Copy_spec;;
module P = Hm_template_instance_proofs;;
module A = Hm_abstraction;;
module T = Hm_type_proofs;;
[%%expect{|
module P = Hm_template_instance_proofs
module A = Hm_abstraction
module T = Hm_type_proofs
|}]

module Capture_boundary = struct
  let bad : (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | true} @ ghost = fun p q -> ghost_ (
    let empty = A.No_names in let names = A.Name (p, empty) in let schema = Boundary q in
    let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun _x -> Variable p in
    rho_def q; let var = Variable p in Hm_declarative.embed_def var;
    P.boundaries_avoid_def names rho schema;
    let free = Hm_declarative.Free p in A.avoids_def names free; A.position_def names p;
    let _wrong : {u : unit | P.boundaries_avoid names rho schema} = refine_ () in
    let u = () in refine_ u)
end;;
[%%expect{|
Line 9, characters 68-78:
9 |     let _wrong : {u : unit | P.boundaries_avoid names rho schema} = refine_ () in
                                                                        ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
