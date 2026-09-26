(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hmc_ground_type.ml hmc_no_free.ml hmc_grounding.ml hmc_admission.ml hmc_templates.ml hmc_ground_arguments.ml hmc_instance.ml hmc_admission_demo.ml";
 { bytecode; }
 { native; }
*)
module D = Hm_declarative
module C = Hm_checked_elaboration
module G = Hmc_grounding
module A = Hmc_admission
module T = Hmc_templates

let checked term ty d = match C.check term ty d with
  | Some checked -> checked | None -> failwith "invalid test derivation"
let grounded : D.term @ immutable -> D.typing @ immutable -> G.grounded @ immutable =
  fun term d -> match G.ground (checked term (D.Function (D.Word64, D.Word64)) d) with
  | G.Grounded p -> p | _ -> failwith "grounding rejected Word64 entry"

let () =
  let var = D.Variable D.No_arguments in
  let id = D.Lambda (D.Bound D.Z) in
  let word_id = D.Abstraction (D.Word64, var) in
  let w = D.Function (D.Word64, D.Word64) in
  let input = grounded id word_id in
  (match A.admit input with
  | A.Rejected _ -> failwith "monomorphic entry rejected"
  | A.Admitted a ->
    let p = T.extract a in
    ghost_ (T.ready_def p);
    (match T.select p.T.globals D.Z () with None -> () | _ -> failwith "empty catalog lookup");
    if not (Hm_elaboration_check.check D.Z D.Empty_context
      (T.rebuild p.T.globals p.T.entry) w (T.rebuild_derivation p.T.globals p.T.derivation))
    then failwith "reconstructed monomorphic derivation");
  (match G.ground (checked id (D.Function (D.Boolean, D.Boolean)) (D.Abstraction (D.Boolean, var))) with
  | G.Entry_type_mismatch -> () | _ -> failwith "Boolean identity entry accepted");
  let param = D.Parameter D.Z in
  let scheme = D.Forall (D.S D.Z, D.Function (param, param)) in
  let rhs = D.Abstraction (param, var) in
  let term = D.Let (id, id) in
  let proof = D.Let_binding (scheme, rhs, word_id) in
  let input = grounded term proof in
  (match A.admit input with
  | A.Rejected _ -> failwith "outer polymorphism rejected"
  | A.Admitted a ->
    let p = T.extract a in ghost_ (T.ready_def p);
    (match T.select p.T.globals D.Z () with Some s ->
      if not (A.callable s.T.definition.T.source) then failwith "non-callable template"
    | _ -> failwith "missing outer declaration");
    if not (Hm_elaboration_check.check D.Z D.Empty_context
      (T.rebuild p.T.globals p.T.entry) w (T.rebuild_derivation p.T.globals p.T.derivation))
    then failwith "reconstructed polymorphic derivation");
  let local = D.Lambda (D.Let (id, D.Bound (D.S D.Z))) in
  let proof = D.Abstraction (D.Word64, D.Let_binding (scheme, rhs, var)) in
  (match A.admit (grounded local proof) with
  | A.Rejected A.Unsupported_polymorphic_local_let -> ()
  | _ -> failwith "strict local generalization policy");
  let word = D.Word {Hmc_word64.lo = 0; hi = 0} in
  let term = D.Let (word, id) in
  let proof = D.Let_binding (D.Forall (D.Z, D.Word64), D.Word_constant, word_id) in
  (match A.admit (grounded term proof) with
  | A.Rejected A.Non_callable_outer_binding -> ()
  | _ -> failwith "computational outer binding policy");
  (match A.check_local D.Truth (D.Abstraction (D.Boolean, D.Constant)) with
  | Some A.Invalid_annotation -> () | _ -> failwith "mismatched annotation shape");
  if A.local (D.Lambda D.Truth) D.Constant then failwith "forged local shape accepted"

module Args = Hmc_ground_arguments
module Ground = Hmc_ground_type
module Instance = Hmc_instance

let () =
  let param = D.Parameter D.Z in
  let scheme = D.Forall (D.S D.Z, D.Function (param, param)) in
  let id = D.Lambda (D.Bound D.Z) in
  let var = D.Variable D.No_arguments in
  let rhs = D.Abstraction (param, var) in
  let term = D.Let (id, D.Let (id, id)) in
  let proof = D.Let_binding (scheme, rhs,
    D.Let_binding (scheme, rhs, D.Abstraction (D.Word64, var))) in
  let admitted : A.admitted = match A.admit (grounded term proof) with
  | A.Admitted p -> p | _ -> raise (Failure "template fixture rejected") in
  let p = T.extract admitted in
  ghost_ (T.ready_def p);
  let catalog : {c : T.catalog | T.valid c} = refine_ p.T.globals in
  let words = Args.Argument (Ground.Word64, Args.Empty) in
  let lists = Args.Argument (Ground.List Ground.Word64, Args.Empty) in
  let first = match Instance.request catalog D.Z words with
  | Instance.Instance d -> d | _ -> failwith "word identity instance" in
  let second = match Instance.request catalog (D.S D.Z) words with
  | Instance.Instance d -> d | _ -> failwith "other word identity instance" in
  if Args.key_equal first.Instance.key second.Instance.key then failwith "distinct source bindings merged";
  (match Instance.request catalog D.Z words with
  | Instance.Instance d when Args.key_equal first.Instance.key d.Instance.key -> ()
  | _ -> failwith "unstable specialization key");
  (match Instance.request catalog D.Z lists with
  | Instance.Instance d ->
    if Args.key_equal first.Instance.key d.Instance.key then failwith "distinct type arguments merged";
    if not (Ground.equal d.Instance.ty (Ground.Arrow (Ground.List Ground.Word64, Ground.List Ground.Word64)))
    then failwith "instantiated list identity type"
  | _ -> failwith "list identity instance");
  (match Instance.request catalog D.Z Args.Empty with
  | Instance.Wrong_arity -> () | _ -> failwith "missing type argument accepted");
  (match Instance.request catalog D.Z (Args.Argument (Ground.Bool, words)) with
  | Instance.Wrong_arity -> () | _ -> failwith "extra type argument accepted");
  (match Instance.request catalog (D.S (D.S D.Z)) words with
  | Instance.Unknown_template -> () | _ -> failwith "unknown template accepted");
  (match Args.read (D.Argument (D.Function (D.Word64, D.List_type D.Boolean), D.No_arguments)) with
  | Some a -> if not (Args.equal a (Args.Argument (Ground.Arrow (Ground.Word64, Ground.List Ground.Bool), Args.Empty)))
    then failwith "ground argument readback"
  | None -> failwith "ground type arguments rejected");
  (match Args.read (D.Argument (D.Parameter D.Z, D.No_arguments)) with
  | None -> () | Some _ -> failwith "uninstantiated parameter accepted")
