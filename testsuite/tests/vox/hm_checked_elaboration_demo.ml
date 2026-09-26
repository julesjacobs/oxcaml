(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_checked_elaboration_demo.ml";
 { bytecode; }
*)
module D = Hm_declarative
module C = Hm_checked_elaboration

let () =
  match C.check D.Truth D.Boolean D.Constant with
  | None -> failwith "valid elaboration rejected"
  | Some checked ->
    let proof = C.derivation checked in
    if not (Hm_elaboration_check.check D.Z D.Empty_context
      (C.source checked) (C.root checked) proof) then
      failwith "invalid exported derivation"

let () =
  match C.check (D.Bound D.Z) D.Boolean (D.Variable D.No_arguments) with
  | None -> ()
  | Some _ -> failwith "unbound variable accepted"
