(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml level_finite_spec.ml level_mgu_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_environment_spec.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_freshness_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_template_instance_proofs.ml hm_scheme_transport_proofs.ml hm_template_instance_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
module P = Hm_template_instance_proofs
module D = Hm_declarative
module F = Hm_freshness_proofs
module A = Hm_abstraction
module T = Hm_type_proofs

let (mixed_instance @ total) : (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (root : node Pref.t) @ immutable -> {u : unit | true} @ ghost = fun p q root -> ghost_ (
    let left = Boundary q in let right = Parameter p in let schema = Product (root, left, right) in
    let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun _x -> Variable p in
    let[@def] choices : node Pref.t @ immutable total -> ty @ immutable total = fun _x -> Boolean in
    let refine_ args = P.direct_instance rho choices schema in
    interpret_def rho choices schema; interpret_def rho choices left; interpret_def rho choices right;
    rho_def q; choices_def p; let wanted = Function (Variable p, Boolean) in D.embed_def wanted;
    let sigma = P.scheme rho schema in
    let _checked : {u : unit | D.open_scheme sigma args === D.embed wanted} = refine_ () in
    P.scheme_wf rho schema; let u = () in refine_ u)

let (shared_parameter @ total) : (p : node Pref.t) @ immutable ->
    (root : node Pref.t) @ immutable -> (t : ty) @ immutable -> {u : unit | true} @ ghost = fun p root t -> ghost_ (
    let parameter = Parameter p in let schema = Product (root, parameter, parameter) in
    let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun _x -> Boolean in
    let[@def] choices : node Pref.t @ immutable total -> ty @ immutable total = fun _x -> t in
    let refine_ args = P.direct_instance rho choices schema in
    interpret_def rho choices schema; interpret_def rho choices parameter; choices_def p;
    let wanted = Function (t, t) in let sigma = P.scheme rho schema in
    let _checked : {u : unit | D.open_scheme sigma args === D.embed wanted} = refine_ () in
    P.scheme_wf rho schema; let u = () in refine_ u)
