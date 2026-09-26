(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_annotation_trace.ml hm_elaboration_check.ml hm_instantiation.ml hm_generalization.ml hm_elaboration.ml hm_elaboration_test_support.ml hm_elaboration_demo.ml";
 { bytecode; }
 { native; }
*)
module D = Hm_declarative
module A = Hm_annotation_trace

let () =
  let first = Pref.alloc (Copy_spec.cell Copy_spec.Var 0) (Pref.empty ()) in
  let second = Pref.alloc (Copy_spec.cell Copy_spec.Var 0) first.state in
  let third = Pref.alloc (Copy_spec.cell Copy_spec.Var 0) second.state in
  let fourth = Pref.alloc (Copy_spec.cell Copy_spec.Var 0) third.state in
  let fifth = Pref.alloc (Copy_spec.cell Copy_spec.Var 0) fourth.state in
  let p = first.value in
  let q = second.value in
  let b = third.value in
  let c = fourth.value in
  let s = fifth.value in
  let bb = Copy_spec.Function (Copy_spec.Boolean, Copy_spec.Boolean) in
  let read (r : Copy_spec.node Pref.t @ immutable) =
    if Pref.equal r p then Copy_spec.Variable p else
    if Pref.equal r q then
      Copy_spec.Function (Copy_spec.Variable p, Copy_spec.Variable p) else
    if Pref.equal r b then Copy_spec.Boolean else
    if Pref.equal r c then bb else
    if Pref.equal r s then Copy_spec.Function (bb, bb) else
    failwith "unknown recorded root" in
  let scope = Hm_elaboration.Quantifiers (Hm_generalization.Variable (q, Hm_generalization.Empty),
    Hm_elaboration.Quantifiers (Hm_generalization.Variable (p, Hm_generalization.Empty), Hm_elaboration.Empty)) in
  let input_type = Copy_spec.Function (Copy_spec.Variable p, Copy_spec.List_type (Copy_spec.Variable q)) in
  ghost_ (Hm_elaboration.interpret_wf scope input_type; Hm_elaboration.interpret_empty input_type);
  let interpreted : {ty : D.mono | D.mono_wf (Hm_elaboration.depth scope) ty} @ immutable =
    Hm_elaboration.interpret scope input_type in
  if not (Hm_elaboration_check.mono_equal interpreted
    (D.Function (D.Parameter (D.S D.Z), D.List_type (D.Parameter D.Z)))) then
    failwith "incorrect nested quantifier interpretation";
  let mixed = Copy_spec.Function (Copy_spec.Variable p, Copy_spec.List_type (Copy_spec.Variable s)) in
  (let generalized = Hm_generalization.generalize D.Empty_context (Hm_elaboration.interpret scope mixed) in
    ghost_ (Hm_elaboration.selected_scope scope D.Empty_context mixed;
      Hm_elaboration.interpret_generalized scope generalized.Hm_generalization.variables mixed ());
    let extended = Hm_elaboration.Quantifiers (generalized.Hm_generalization.variables, scope) in
    let result : {ty : D.mono | ty === Hm_generalization.abstract
        (Hm_generalization.count generalized.Hm_generalization.variables)
        generalized.Hm_generalization.variables (Hm_elaboration.interpret scope mixed)} @ immutable =
      Hm_elaboration.interpret extended mixed in
    if not (Hm_elaboration_check.mono_equal result
      (D.Function (D.Parameter (D.S (D.S D.Z)), D.List_type (D.Parameter D.Z)))) then
      failwith "incorrect generalized annotation scope");
  let id = D.Lambda (D.Bound D.Z) in
  let id_trace = A.Abstraction (q, p, A.Variable_use p) in
  let source = D.Let (id, D.Apply (D.Bound D.Z, D.Truth)) in
  let trace = A.Let_binding (id_trace,
    A.Application (Some b, A.Variable_use c, A.Boolean_literal b)) in
  (match Hm_elaboration_test_support.elaborate source trace read D.Boolean with
   | None -> failwith "polymorphic identity elaboration failed"
   | Some _ -> ());
  let source = D.Let (id, D.Apply (D.Bound D.Z, id)) in
  let trace = A.Let_binding (id_trace,
    A.Application (Some c, A.Variable_use s,
      A.Abstraction (c, b, A.Variable_use b))) in
  (match Hm_elaboration_test_support.elaborate source trace read
     (D.Function (D.Boolean, D.Boolean)) with
   | None -> failwith "higher-order instance elaboration failed"
   | Some _ -> ());
  (match Hm_elaboration_test_support.elaborate source trace read D.Boolean with
   | None -> () | Some _ -> failwith "accepted incorrect root type");
  (match Hm_elaboration_test_support.elaborate D.Truth trace read D.Boolean with
   | None -> () | Some _ -> failwith "accepted mismatched trace shape")
