(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_elaboration_check.ml hm_instantiation.ml hm_instantiation_demo.ml";
 { bytecode; }
 { native; }
*)
module D = Hm_declarative

let expect scheme target expected =
  match Hm_instantiation.infer scheme target with
  | None -> failwith "missing instantiation"
  | Some args ->
    if not (Hm_elaboration_check.mono_equal
      (D.open_scheme scheme args) expected) then
      failwith "wrong instantiation"

let expect_instance :
    (scheme : D.scheme) @ immutable ->
    (args : {args : D.arguments | D.length args === D.arity scheme}) @ immutable -> unit =
  fun scheme args ->
  let target = D.open_scheme scheme args in
  ghost_ (Hm_instantiation.complete scheme target args ());
  let result : {out : D.arguments option | match out with None -> false | Some _ -> true} @ immutable =
    Hm_instantiation.infer scheme target in
  match result with None -> failwith "proved instance rejected" | Some _ -> ()

let () =
  let z = D.Z in
  let one = D.S z in
  let two = D.S one in
  let b = D.Boolean in
  let bb = D.Function (b, b) in
  let a = D.Parameter z in
  let c = D.Parameter one in
  let id = D.Forall (one, D.Function (a, a)) in
  let args = D.Argument (D.Word64, D.No_arguments) in
  ghost_ (D.length_def args; D.length_def D.No_arguments; D.arity_def id);
  expect_instance id args;
  let unused = D.Forall (one, b) in
  ghost_ (D.arity_def unused);
  expect_instance unused args;
  expect id bb bb;
  let higher = D.Function (bb, bb) in
  expect id higher higher;
  let words = D.Function (D.Word64, D.Word64) in
  expect id words words;
  let lists = D.Function (D.List_type D.Word64, D.List_type D.Word64) in
  expect id lists lists;
  let select = D.Forall (two, D.Function (a, D.Function (c, a))) in
  let target = D.Function (b, D.Function (bb, b)) in
  expect select target target;
  expect (D.Forall (one, D.Function (a, c)))
    (D.Function (b, D.Parameter z)) (D.Function (b, D.Parameter z));
  expect (D.Forall (one, b)) b b;
  (match Hm_instantiation.infer id (D.Function (b, bb)) with
   | None -> () | Some _ -> failwith "inconsistent repeated parameter");
  (match Hm_instantiation.infer (D.Forall (z, b)) bb with
   | None -> () | Some _ -> failwith "changed fixed type");
  (match Hm_instantiation.infer id (D.Function (D.List_type D.Word64, D.List_type b)) with
   | None -> () | Some _ -> failwith "inconsistent repeated list parameter")
