(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_elaboration_check.ml hm_generalization.ml hm_generalization_demo.ml";
 { bytecode; }
 { native; }
*)
module D = Hm_declarative
module G = Hm_generalization

let expect context ty arity body =
  let g = G.generalize context ty in
    match g.G.scheme with
    | D.Forall (n, t) ->
      if not (Hm_elaboration_check.index_equal n arity
        && Hm_elaboration_check.mono_equal t body) then
        failwith "wrong generalized scheme"

let selection : (p : Copy_spec.node Pref.t) @ immutable -> (context : D.context) @ immutable -> (ty : D.mono) @ immutable -> unit =
  fun p context ty ->
    let g = G.generalize context ty in
      ghost_ (G.collect_member p context ty G.Empty; G.find_def p G.Empty);
      let selected : {i : D.index option | not (i === None) = (G.occurs p ty && not (G.in_context p context))} @ immutable =
        G.find p g.G.variables in
      ignore selected

let () =
  let first = Pref.alloc (Copy_spec.cell Copy_spec.Var 0) (Pref.empty ()) in
  let second = Pref.alloc (Copy_spec.cell Copy_spec.Var 0) first.state in
  let p = D.Free first.value in
  let q = D.Free second.value in
  let z = D.Z in
  let one = D.S z in
  let a = D.Parameter z in
  expect D.Empty_context (D.Function (p, p)) one (D.Function (a, a));
  let context = D.Binding (D.Forall (z, p), D.Empty_context) in
  expect context (D.Function (p, q)) one (D.Function (p, a));
  selection first.value context (D.Function (p, q));
  selection second.value context (D.Function (p, q));
  selection first.value D.Empty_context (D.Function (p, D.List_type p));
  expect D.Empty_context (D.Function (p, a)) one
    (D.Function (a, D.Parameter one));
  expect context p z p;
  expect D.Empty_context D.Boolean z D.Boolean;
  expect D.Empty_context D.Word64 z D.Word64;
  expect D.Empty_context (D.List_type (D.Function (p, D.Word64))) one
    (D.List_type (D.Function (a, D.Word64)));
  expect context (D.List_type p) z (D.List_type p)
