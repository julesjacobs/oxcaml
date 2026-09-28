(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 source_directories += " ${test_source_directory}/../../../verification/demos";
 readonly_files = "vox_diff_spec.mli vox_diff_spec.ml vox_diff.mli vox_diff.ml";
 readonly_files += " diff_public_client.ml emitted_code.ml";
 readonly_files += " diff_boundary_check.ml";
 readonly_files += " diff_demo.ml";
 set lib = "";
 {
   setup-ocamlc.opt-build-env;
   lib = "${test_build_directory_prefix}/ocamlc.opt";
   module = "vox_diff_spec.mli";
   ocamlc.opt;
   module = "vox_diff_spec.ml";
   ocamlc.opt;
   module = "vox_diff.mli";
   ocamlc.opt;
   flags += " -drawlambda";
   compiler_output2 = "${lib}/vox_diff.lambda";
   module = "vox_diff.ml";
   ocamlc.opt;
   src = "${lib}/vox_diff_spec.cmi ${lib}/vox_diff.cmi";
   dst = "${test_build_directory_prefix}/ocamlc.opt.public/";
   compiler_directory_suffix = ".public";
   readonly_files = "diff_public_client.ml emitted_code.ml";
   readonly_files += " diff_boundary_check.ml";
   compiler_output2 = "${lib}.public/client.lambda";
   setup-ocamlc.opt-build-env;
   copy;
   flags += " -principal";
   module = "diff_public_client.ml";
   ocamlc.opt;
   unset module;
   compiler_output2 = "${lib}.public/checker.output";
   flags = "";
   compile_only = "false";
   all_modules = "emitted_code.ml diff_boundary_check.ml";
   program = "${lib}.public/check.exe";
   ocamlc.opt;
   arguments = "${lib}/vox_diff.lambda ${lib}.public/client.lambda";
   output = "${lib}.public/check.output";
   run;
   check-program-output;
 }
 {
   setup-ocamlopt.opt-build-env;
   lib = "${test_build_directory_prefix}/ocamlopt.opt";
   module = "vox_diff_spec.mli";
   ocamlopt.opt;
   module = "vox_diff_spec.ml";
   ocamlopt.opt;
   module = "vox_diff.mli";
   ocamlopt.opt;
   flags += " -drawlambda";
   compiler_output2 = "${lib}/vox_diff.lambda";
   module = "vox_diff.ml";
   ocamlopt.opt;
   src = "${lib}/vox_diff_spec.cmi ${lib}/vox_diff.cmi";
   src += " ${lib}/vox_diff_spec.cmx ${lib}/vox_diff.cmx";
   dst = "${test_build_directory_prefix}/ocamlopt.opt.public/";
   compiler_directory_suffix = ".public";
   readonly_files = "diff_public_client.ml emitted_code.ml";
   readonly_files += " diff_boundary_check.ml";
   compiler_output2 = "${lib}.public/client.lambda";
   setup-ocamlopt.opt-build-env;
   copy;
   flags += " -principal";
   module = "diff_public_client.ml";
   ocamlopt.opt;
   unset module;
   compiler_output2 = "${lib}.public/checker.output";
   flags = "";
   compile_only = "false";
   all_modules = "emitted_code.ml diff_boundary_check.ml";
   program = "${lib}.public/check.exe";
   ocamlopt.opt;
   arguments = "${lib}/vox_diff.lambda ${lib}.public/client.lambda";
   output = "${lib}.public/check.output";
   run;
   check-program-output;
 }
 {
   compiler_directory_suffix = ".demo";
   setup-ocamlc.opt-build-env;
   all_modules = "vox_diff_spec.mli vox_diff_spec.ml vox_diff.mli vox_diff.ml";
   all_modules += " diff_demo.ml";
   program = "${test_build_directory}/diff_demo.byte";
   ocamlc.opt;
   arguments = "ABCABBA CBABAC";
   output = "${test_build_directory}/demo.output";
   reference = "${test_source_directory}/diff_boundary.demo.reference";
   run;
   check-program-output;
 }
*)

(* The Myers diff demo's boundary: the executable functions of vox_diff.ml
   call no proof, metric or big-integer code, [search] has no runtime fuel,
   and the public client, compiled with only the two public interfaces,
   makes exactly one call, to [Vox_diff.diff]. The last block runs the
   demo program (scripts/run-diff-demo). *)
