(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 set here = "${test_source_directory}";
 set lib = "";
 set default_flags = "-extension refinement_types";
 set principal_flags = "-extension refinement_types -principal";
 readonly_files = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli";
 readonly_files += " vox_int_sequence.ml vox_iarray.mli vox_iarray.ml";
 readonly_files += " borrow.mli borrow.ml functional_queue.mli";
 readonly_files += " functional_queue.ml int_set_intf.mli avl_sets.mli";
 readonly_files += " avl_sets.ml sorted_array_proofs.ml sorted_array.mli";
 readonly_files += " sorted_array.ml quicksort_model.ml quicksort.mli";
 readonly_files += " quicksort.ml sparse_overlay.mli sparse_overlay.ml";
 {
   compiler_directory_suffix = "";
   setup-ocamlc.opt-build-env;
   lib = "${test_build_directory_prefix}/ocamlc.opt";
   flags = "${default_flags}";
   compiler_output2 = "${lib}/vox_sequence.mli.output";
   module = "vox_sequence.mli";
   ocamlc.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/vox_sequence.lambda";
   module = "vox_sequence.ml";
   ocamlc.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/vox_int_sequence.mli.output";
   module = "vox_int_sequence.mli";
   ocamlc.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/vox_int_sequence.lambda";
   module = "vox_int_sequence.ml";
   ocamlc.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/vox_iarray.mli.output";
   module = "vox_iarray.mli";
   ocamlc.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/vox_iarray.lambda";
   module = "vox_iarray.ml";
   ocamlc.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/borrow.mli.output";
   module = "borrow.mli";
   ocamlc.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/borrow.lambda";
   module = "borrow.ml";
   ocamlc.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/functional_queue.mli.output";
   module = "functional_queue.mli";
   ocamlc.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/functional_queue.lambda";
   module = "functional_queue.ml";
   ocamlc.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/int_set_intf.mli.output";
   module = "int_set_intf.mli";
   ocamlc.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/avl_sets.mli.output";
   module = "avl_sets.mli";
   ocamlc.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/avl_sets.lambda";
   module = "avl_sets.ml";
   ocamlc.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/sorted_array_proofs.lambda";
   module = "sorted_array_proofs.ml";
   ocamlc.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/sorted_array.mli.output";
   module = "sorted_array.mli";
   ocamlc.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/sorted_array.lambda";
   module = "sorted_array.ml";
   ocamlc.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/quicksort_model.lambda";
   module = "quicksort_model.ml";
   ocamlc.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/quicksort.mli.output";
   module = "quicksort.mli";
   ocamlc.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/quicksort.lambda";
   module = "quicksort.ml";
   ocamlc.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/sparse_overlay.mli.output";
   module = "sparse_overlay.mli";
   ocamlc.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/sparse_overlay.lambda";
   module = "sparse_overlay.ml";
   ocamlc.opt;
   unset module;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/clients.output";
   src = "${lib}/vox_sequence.cmi ${lib}/vox_int_sequence.cmi";
   src += " ${lib}/vox_iarray.cmi ${lib}/borrow.cmi";
   src += " ${lib}/functional_queue.cmi ${lib}/int_set_intf.cmi";
   src += " ${lib}/avl_sets.cmi ${lib}/sorted_array.cmi";
   src += " ${lib}/quicksort.cmi ${lib}/sparse_overlay.cmi";
   dst = "${lib}.public/";
   compiler_directory_suffix = ".public";
   all_modules = "queue_client.ml avl_set_client.ml";
   all_modules += " sorted_array_client.ml quicksort_client.ml";
   all_modules += " sparse_overlay_client.ml";
   all_modules += " collections_boundary_client.ml";
   all_modules += " quicksort_frame_client.ml borrow_parallel.ml";
   all_modules += " avl_stdlib_set.ml";
   readonly_files = "collections_boundary.ml emitted_code.ml";
   readonly_files += " collections_boundary_check.ml";
   setup-ocamlc.opt-build-env;
   copy;
   compile_only = "false";
   binary_modules = "${lib}/vox_sequence ${lib}/vox_int_sequence";
   binary_modules += " ${lib}/vox_iarray ${lib}/borrow";
   binary_modules += " ${lib}/functional_queue ${lib}/avl_sets";
   binary_modules += " ${lib}/sorted_array_proofs ${lib}/sorted_array";
   binary_modules += " ${lib}/quicksort_model ${lib}/quicksort";
   binary_modules += " ${lib}/sparse_overlay";
   all_modules = "queue_client.ml";
   program = "${lib}.public/queue_client.exe";
   ocamlc.opt;
   output = "${lib}.public/queue_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/queue_client.reference";
   run;
   check-program-output;
   all_modules = "avl_set_client.ml";
   program = "${lib}.public/avl_set_client.exe";
   ocamlc.opt;
   output = "${lib}.public/avl_set_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/collections_boundary.avl_set_client.reference";
   run;
   check-program-output;
   all_modules = "sorted_array_client.ml";
   program = "${lib}.public/sorted_array_client.exe";
   ocamlc.opt;
   output = "${lib}.public/sorted_array_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/sorted_array_client.reference";
   run;
   check-program-output;
   all_modules = "quicksort_client.ml";
   program = "${lib}.public/quicksort_client.exe";
   ocamlc.opt;
   output = "${lib}.public/quicksort_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/quicksort_client.reference";
   run;
   check-program-output;
   all_modules = "sparse_overlay_client.ml";
   program = "${lib}.public/sparse_overlay_client.exe";
   ocamlc.opt;
   output = "${lib}.public/sparse_overlay_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/sparse_overlay_client.reference";
   run;
   check-program-output;
   all_modules = "collections_boundary_client.ml";
   program = "${lib}.public/collections_boundary_client.exe";
   ocamlc.opt;
   output = "${lib}.public/collections_boundary_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/collections_boundary_client.reference";
   run;
   check-program-output;
   check-ocamlc.opt-output;
   (* Unchecked output: these clients have unused proof bindings. *)
   compiler_output2 = "${lib}.public/parallel.output";
   all_modules = "quicksort_frame_client.ml";
   program = "${lib}.public/quicksort_frame_client.exe";
   ocamlc.opt;
   all_modules = "borrow_parallel.ml";
   program = "${lib}.public/borrow_parallel.exe";
   ocamlc.opt;
   binary_modules = "${lib}/avl_sets avl_set_client";
   all_modules = "avl_stdlib_set.ml";
   program = "${lib}.public/avl_legacy.exe";
   ocamlc.opt;
   output = "${lib}.public/avl_legacy.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/avl_sets.reference";
   run;
   check-program-output;
   unset binary_modules;
   flags = "";
   compiler_output2 = "${lib}.public/checker.output";
   all_modules = "emitted_code.ml collections_boundary_check.ml";
   program = "${lib}.public/check.exe";
   ocamlc.opt;
   arguments = "${lib}";
   output = "${lib}.public/check.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/collections_boundary.checks.reference";
   run;
   check-program-output;
   flags = "${default_flags}";
   binary_modules = "${lib}/vox_sequence ${lib}/vox_int_sequence";
   binary_modules += " ${lib}/vox_iarray ${lib}/borrow";
   binary_modules += " ${lib}/functional_queue ${lib}/avl_sets";
   binary_modules += " ${lib}/sorted_array_proofs ${lib}/sorted_array";
   binary_modules += " ${lib}/quicksort_model ${lib}/quicksort";
   binary_modules += " ${lib}/sparse_overlay";
   run-expect;
   check-program-output;
 }
 {
   compiler_directory_suffix = "";
   setup-ocamlopt.opt-build-env;
   lib = "${test_build_directory_prefix}/ocamlopt.opt";
   flags = "${default_flags}";
   compiler_output2 = "${lib}/vox_sequence.mli.output";
   module = "vox_sequence.mli";
   ocamlopt.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/vox_sequence.lambda";
   module = "vox_sequence.ml";
   ocamlopt.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/vox_int_sequence.mli.output";
   module = "vox_int_sequence.mli";
   ocamlopt.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/vox_int_sequence.lambda";
   module = "vox_int_sequence.ml";
   ocamlopt.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/vox_iarray.mli.output";
   module = "vox_iarray.mli";
   ocamlopt.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/vox_iarray.lambda";
   module = "vox_iarray.ml";
   ocamlopt.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/borrow.mli.output";
   module = "borrow.mli";
   ocamlopt.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/borrow.lambda";
   module = "borrow.ml";
   ocamlopt.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/functional_queue.mli.output";
   module = "functional_queue.mli";
   ocamlopt.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/functional_queue.lambda";
   module = "functional_queue.ml";
   ocamlopt.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/int_set_intf.mli.output";
   module = "int_set_intf.mli";
   ocamlopt.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/avl_sets.mli.output";
   module = "avl_sets.mli";
   ocamlopt.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/avl_sets.lambda";
   module = "avl_sets.ml";
   ocamlopt.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/sorted_array_proofs.lambda";
   module = "sorted_array_proofs.ml";
   ocamlopt.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/sorted_array.mli.output";
   module = "sorted_array.mli";
   ocamlopt.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/sorted_array.lambda";
   module = "sorted_array.ml";
   ocamlopt.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/quicksort_model.lambda";
   module = "quicksort_model.ml";
   ocamlopt.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/quicksort.mli.output";
   module = "quicksort.mli";
   ocamlopt.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/quicksort.lambda";
   module = "quicksort.ml";
   ocamlopt.opt;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/sparse_overlay.mli.output";
   module = "sparse_overlay.mli";
   ocamlopt.opt;
   flags = "${default_flags} -dlambda";
   compiler_output2 = "${lib}/sparse_overlay.lambda";
   module = "sparse_overlay.ml";
   ocamlopt.opt;
   unset module;
   flags = "${default_flags}";
   compiler_output2 = "${lib}/clients.output";
   src = "${lib}/vox_sequence.cmi ${lib}/vox_int_sequence.cmi";
   src += " ${lib}/vox_iarray.cmi ${lib}/borrow.cmi";
   src += " ${lib}/functional_queue.cmi ${lib}/int_set_intf.cmi";
   src += " ${lib}/avl_sets.cmi ${lib}/sorted_array.cmi";
   src += " ${lib}/quicksort.cmi ${lib}/sparse_overlay.cmi";
   src += " ${lib}/vox_sequence.cmx ${lib}/vox_int_sequence.cmx";
   src += " ${lib}/vox_iarray.cmx ${lib}/borrow.cmx";
   src += " ${lib}/functional_queue.cmx ${lib}/avl_sets.cmx";
   src += " ${lib}/sorted_array_proofs.cmx";
   src += " ${lib}/sorted_array.cmx ${lib}/quicksort_model.cmx";
   src += " ${lib}/quicksort.cmx ${lib}/sparse_overlay.cmx";
   dst = "${lib}.public/";
   compiler_directory_suffix = ".public";
   all_modules = "queue_client.ml avl_set_client.ml";
   all_modules += " sorted_array_client.ml quicksort_client.ml";
   all_modules += " sparse_overlay_client.ml";
   all_modules += " collections_boundary_client.ml";
   all_modules += " quicksort_frame_client.ml borrow_parallel.ml";
   all_modules += " avl_stdlib_set.ml";
   readonly_files = "collections_boundary.ml emitted_code.ml";
   readonly_files += " collections_boundary_check.ml";
   setup-ocamlopt.opt-build-env;
   copy;
   compile_only = "false";
   binary_modules = "${lib}/vox_sequence ${lib}/vox_int_sequence";
   binary_modules += " ${lib}/vox_iarray ${lib}/borrow";
   binary_modules += " ${lib}/functional_queue ${lib}/avl_sets";
   binary_modules += " ${lib}/sorted_array_proofs ${lib}/sorted_array";
   binary_modules += " ${lib}/quicksort_model ${lib}/quicksort";
   binary_modules += " ${lib}/sparse_overlay";
   all_modules = "queue_client.ml";
   program = "${lib}.public/queue_client.exe";
   ocamlopt.opt;
   output = "${lib}.public/queue_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/queue_client.reference";
   run;
   check-program-output;
   all_modules = "avl_set_client.ml";
   program = "${lib}.public/avl_set_client.exe";
   ocamlopt.opt;
   output = "${lib}.public/avl_set_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/collections_boundary.avl_set_client.reference";
   run;
   check-program-output;
   all_modules = "sorted_array_client.ml";
   program = "${lib}.public/sorted_array_client.exe";
   ocamlopt.opt;
   output = "${lib}.public/sorted_array_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/sorted_array_client.reference";
   run;
   check-program-output;
   all_modules = "quicksort_client.ml";
   program = "${lib}.public/quicksort_client.exe";
   ocamlopt.opt;
   output = "${lib}.public/quicksort_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/quicksort_client.reference";
   run;
   check-program-output;
   all_modules = "sparse_overlay_client.ml";
   program = "${lib}.public/sparse_overlay_client.exe";
   ocamlopt.opt;
   output = "${lib}.public/sparse_overlay_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/sparse_overlay_client.reference";
   run;
   check-program-output;
   all_modules = "collections_boundary_client.ml";
   program = "${lib}.public/collections_boundary_client.exe";
   ocamlopt.opt;
   output = "${lib}.public/collections_boundary_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/collections_boundary_client.reference";
   run;
   check-program-output;
   check-ocamlopt.opt-output;
   (* Unchecked output: these clients have unused proof bindings. *)
   compiler_output2 = "${lib}.public/parallel.output";
   all_modules = "quicksort_frame_client.ml";
   program = "${lib}.public/quicksort_frame_client.exe";
   ocamlopt.opt;
   all_modules = "borrow_parallel.ml";
   program = "${lib}.public/borrow_parallel.exe";
   ocamlopt.opt;
   binary_modules = "${lib}/avl_sets avl_set_client";
   all_modules = "avl_stdlib_set.ml";
   program = "${lib}.public/avl_legacy.exe";
   ocamlopt.opt;
   output = "${lib}.public/avl_legacy.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/avl_sets.reference";
   run;
   check-program-output;
   unset binary_modules;
   flags = "";
   compiler_output2 = "${lib}.public/checker.output";
   all_modules = "emitted_code.ml collections_boundary_check.ml";
   program = "${lib}.public/check.exe";
   ocamlopt.opt;
   arguments = "${lib}";
   output = "${lib}.public/check.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/collections_boundary.checks.reference";
   run;
   check-program-output;
 }
 {
   compiler_directory_suffix = ".principal";
   setup-ocamlc.opt-build-env;
   lib = "${test_build_directory_prefix}/ocamlc.opt.principal";
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/vox_sequence.mli.output";
   module = "vox_sequence.mli";
   ocamlc.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/vox_sequence.lambda";
   module = "vox_sequence.ml";
   ocamlc.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/vox_int_sequence.mli.output";
   module = "vox_int_sequence.mli";
   ocamlc.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/vox_int_sequence.lambda";
   module = "vox_int_sequence.ml";
   ocamlc.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/vox_iarray.mli.output";
   module = "vox_iarray.mli";
   ocamlc.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/vox_iarray.lambda";
   module = "vox_iarray.ml";
   ocamlc.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/borrow.mli.output";
   module = "borrow.mli";
   ocamlc.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/borrow.lambda";
   module = "borrow.ml";
   ocamlc.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/functional_queue.mli.output";
   module = "functional_queue.mli";
   ocamlc.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/functional_queue.lambda";
   module = "functional_queue.ml";
   ocamlc.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/int_set_intf.mli.output";
   module = "int_set_intf.mli";
   ocamlc.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/avl_sets.mli.output";
   module = "avl_sets.mli";
   ocamlc.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/avl_sets.lambda";
   module = "avl_sets.ml";
   ocamlc.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/sorted_array_proofs.lambda";
   module = "sorted_array_proofs.ml";
   ocamlc.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/sorted_array.mli.output";
   module = "sorted_array.mli";
   ocamlc.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/sorted_array.lambda";
   module = "sorted_array.ml";
   ocamlc.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/quicksort_model.lambda";
   module = "quicksort_model.ml";
   ocamlc.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/quicksort.mli.output";
   module = "quicksort.mli";
   ocamlc.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/quicksort.lambda";
   module = "quicksort.ml";
   ocamlc.opt;
   unset module;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/clients.output";
   src = "${lib}/vox_sequence.cmi ${lib}/vox_int_sequence.cmi";
   src += " ${lib}/vox_iarray.cmi ${lib}/borrow.cmi";
   src += " ${lib}/functional_queue.cmi ${lib}/int_set_intf.cmi";
   src += " ${lib}/avl_sets.cmi ${lib}/sorted_array.cmi";
   src += " ${lib}/quicksort.cmi";
   dst = "${lib}.public/";
   compiler_directory_suffix = ".principal.public";
   all_modules = "queue_client.ml avl_set_client.ml";
   all_modules += " sorted_array_client.ml quicksort_client.ml";
   all_modules += " collections_boundary_client.ml";
   all_modules += " quicksort_frame_client.ml borrow_parallel.ml";
   all_modules += " avl_stdlib_set.ml";
   readonly_files = "collections_boundary.ml emitted_code.ml";
   readonly_files += " collections_boundary_check.ml";
   setup-ocamlc.opt-build-env;
   copy;
   compile_only = "false";
   binary_modules = "${lib}/vox_sequence ${lib}/vox_int_sequence";
   binary_modules += " ${lib}/vox_iarray ${lib}/borrow";
   binary_modules += " ${lib}/functional_queue ${lib}/avl_sets";
   binary_modules += " ${lib}/sorted_array_proofs ${lib}/sorted_array";
   binary_modules += " ${lib}/quicksort_model ${lib}/quicksort";
   all_modules = "queue_client.ml";
   program = "${lib}.public/queue_client.exe";
   ocamlc.opt;
   output = "${lib}.public/queue_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/queue_client.reference";
   run;
   check-program-output;
   all_modules = "avl_set_client.ml";
   program = "${lib}.public/avl_set_client.exe";
   ocamlc.opt;
   output = "${lib}.public/avl_set_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/collections_boundary.avl_set_client.reference";
   run;
   check-program-output;
   all_modules = "sorted_array_client.ml";
   program = "${lib}.public/sorted_array_client.exe";
   ocamlc.opt;
   output = "${lib}.public/sorted_array_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/sorted_array_client.reference";
   run;
   check-program-output;
   all_modules = "quicksort_client.ml";
   program = "${lib}.public/quicksort_client.exe";
   ocamlc.opt;
   output = "${lib}.public/quicksort_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/quicksort_client.reference";
   run;
   check-program-output;
   all_modules = "collections_boundary_client.ml";
   program = "${lib}.public/collections_boundary_client.exe";
   ocamlc.opt;
   output = "${lib}.public/collections_boundary_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/collections_boundary_client.reference";
   run;
   check-program-output;
   check-ocamlc.opt-output;
   (* Unchecked output: these clients have unused proof bindings. *)
   compiler_output2 = "${lib}.public/parallel.output";
   all_modules = "quicksort_frame_client.ml";
   program = "${lib}.public/quicksort_frame_client.exe";
   ocamlc.opt;
   all_modules = "borrow_parallel.ml";
   program = "${lib}.public/borrow_parallel.exe";
   ocamlc.opt;
   binary_modules = "${lib}/avl_sets avl_set_client";
   all_modules = "avl_stdlib_set.ml";
   program = "${lib}.public/avl_legacy.exe";
   ocamlc.opt;
   output = "${lib}.public/avl_legacy.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/avl_sets.reference";
   run;
   check-program-output;
   unset binary_modules;
   flags = "";
   compiler_output2 = "${lib}.public/checker.output";
   all_modules = "emitted_code.ml collections_boundary_check.ml";
   program = "${lib}.public/check.exe";
   ocamlc.opt;
   arguments = "${lib}";
   output = "${lib}.public/check.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/collections_boundary.checks-principal.reference";
   run;
   check-program-output;
 }
 {
   compiler_directory_suffix = ".principal";
   setup-ocamlopt.opt-build-env;
   lib = "${test_build_directory_prefix}/ocamlopt.opt.principal";
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/vox_sequence.mli.output";
   module = "vox_sequence.mli";
   ocamlopt.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/vox_sequence.lambda";
   module = "vox_sequence.ml";
   ocamlopt.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/vox_int_sequence.mli.output";
   module = "vox_int_sequence.mli";
   ocamlopt.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/vox_int_sequence.lambda";
   module = "vox_int_sequence.ml";
   ocamlopt.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/vox_iarray.mli.output";
   module = "vox_iarray.mli";
   ocamlopt.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/vox_iarray.lambda";
   module = "vox_iarray.ml";
   ocamlopt.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/borrow.mli.output";
   module = "borrow.mli";
   ocamlopt.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/borrow.lambda";
   module = "borrow.ml";
   ocamlopt.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/functional_queue.mli.output";
   module = "functional_queue.mli";
   ocamlopt.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/functional_queue.lambda";
   module = "functional_queue.ml";
   ocamlopt.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/int_set_intf.mli.output";
   module = "int_set_intf.mli";
   ocamlopt.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/avl_sets.mli.output";
   module = "avl_sets.mli";
   ocamlopt.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/avl_sets.lambda";
   module = "avl_sets.ml";
   ocamlopt.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/sorted_array_proofs.lambda";
   module = "sorted_array_proofs.ml";
   ocamlopt.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/sorted_array.mli.output";
   module = "sorted_array.mli";
   ocamlopt.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/sorted_array.lambda";
   module = "sorted_array.ml";
   ocamlopt.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/quicksort_model.lambda";
   module = "quicksort_model.ml";
   ocamlopt.opt;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/quicksort.mli.output";
   module = "quicksort.mli";
   ocamlopt.opt;
   flags = "${principal_flags} -dlambda";
   compiler_output2 = "${lib}/quicksort.lambda";
   module = "quicksort.ml";
   ocamlopt.opt;
   unset module;
   flags = "${principal_flags}";
   compiler_output2 = "${lib}/clients.output";
   src = "${lib}/vox_sequence.cmi ${lib}/vox_int_sequence.cmi";
   src += " ${lib}/vox_iarray.cmi ${lib}/borrow.cmi";
   src += " ${lib}/functional_queue.cmi ${lib}/int_set_intf.cmi";
   src += " ${lib}/avl_sets.cmi ${lib}/sorted_array.cmi";
   src += " ${lib}/quicksort.cmi ${lib}/vox_sequence.cmx";
   src += " ${lib}/vox_int_sequence.cmx ${lib}/vox_iarray.cmx";
   src += " ${lib}/borrow.cmx ${lib}/functional_queue.cmx";
   src += " ${lib}/avl_sets.cmx ${lib}/sorted_array_proofs.cmx";
   src += " ${lib}/sorted_array.cmx ${lib}/quicksort_model.cmx";
   src += " ${lib}/quicksort.cmx";
   dst = "${lib}.public/";
   compiler_directory_suffix = ".principal.public";
   all_modules = "queue_client.ml avl_set_client.ml";
   all_modules += " sorted_array_client.ml quicksort_client.ml";
   all_modules += " collections_boundary_client.ml";
   all_modules += " quicksort_frame_client.ml borrow_parallel.ml";
   all_modules += " avl_stdlib_set.ml";
   readonly_files = "collections_boundary.ml emitted_code.ml";
   readonly_files += " collections_boundary_check.ml";
   setup-ocamlopt.opt-build-env;
   copy;
   compile_only = "false";
   binary_modules = "${lib}/vox_sequence ${lib}/vox_int_sequence";
   binary_modules += " ${lib}/vox_iarray ${lib}/borrow";
   binary_modules += " ${lib}/functional_queue ${lib}/avl_sets";
   binary_modules += " ${lib}/sorted_array_proofs ${lib}/sorted_array";
   binary_modules += " ${lib}/quicksort_model ${lib}/quicksort";
   all_modules = "queue_client.ml";
   program = "${lib}.public/queue_client.exe";
   ocamlopt.opt;
   output = "${lib}.public/queue_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/queue_client.reference";
   run;
   check-program-output;
   all_modules = "avl_set_client.ml";
   program = "${lib}.public/avl_set_client.exe";
   ocamlopt.opt;
   output = "${lib}.public/avl_set_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/collections_boundary.avl_set_client.reference";
   run;
   check-program-output;
   all_modules = "sorted_array_client.ml";
   program = "${lib}.public/sorted_array_client.exe";
   ocamlopt.opt;
   output = "${lib}.public/sorted_array_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/sorted_array_client.reference";
   run;
   check-program-output;
   all_modules = "quicksort_client.ml";
   program = "${lib}.public/quicksort_client.exe";
   ocamlopt.opt;
   output = "${lib}.public/quicksort_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/quicksort_client.reference";
   run;
   check-program-output;
   all_modules = "collections_boundary_client.ml";
   program = "${lib}.public/collections_boundary_client.exe";
   ocamlopt.opt;
   output = "${lib}.public/collections_boundary_client.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/collections_boundary_client.reference";
   run;
   check-program-output;
   check-ocamlopt.opt-output;
   (* Unchecked output: these clients have unused proof bindings. *)
   compiler_output2 = "${lib}.public/parallel.output";
   all_modules = "quicksort_frame_client.ml";
   program = "${lib}.public/quicksort_frame_client.exe";
   ocamlopt.opt;
   all_modules = "borrow_parallel.ml";
   program = "${lib}.public/borrow_parallel.exe";
   ocamlopt.opt;
   binary_modules = "${lib}/avl_sets avl_set_client";
   all_modules = "avl_stdlib_set.ml";
   program = "${lib}.public/avl_legacy.exe";
   ocamlopt.opt;
   output = "${lib}.public/avl_legacy.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/avl_sets.reference";
   run;
   check-program-output;
   unset binary_modules;
   flags = "";
   compiler_output2 = "${lib}.public/checker.output";
   all_modules = "emitted_code.ml collections_boundary_check.ml";
   program = "${lib}.public/check.exe";
   ocamlopt.opt;
   arguments = "${lib}";
   output = "${lib}.public/check.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${here}/collections_boundary.checks-principal.reference";
   run;
   check-program-output;
 }
*)

(* The collection demos' boundary: the functional queue, AVL sets, sorted
   arrays, quicksort and sparse overlays. With each compiler, with and
   without -principal (sparse overlays only without), the modules are
   compiled with -dlambda, and the public clients are compiled against the
   interfaces other than Sorted_array_proofs and Quicksort_model, linked
   and run; the two that spawn domains are only linked, since their own
   tests run them. avl_stdlib_set.ml, linked with the AVL client, must
   print avl_sets.reference. collections_boundary_check.ml checks that the
   runtime functions call no model or proof code. The programs below are
   rejected against the public interfaces. *)

(* A positive control. *)
let singleton () = Functional_queue.enqueue Functional_queue.empty 1;;
[%%expect{|
val singleton : unit -> int Functional_queue.t = <fun>
|}]

(* queue_empty *)
let () =
  let q = Functional_queue.empty in
  let nonempty : {q : int Functional_queue.t |
    (Functional_queue.contents q === []) === false} = q in
  let _ = Functional_queue.dequeue nonempty in ();;
[%%expect{|
Line 4, characters 54-55:
4 |     (Functional_queue.contents q === []) === false} = q in
                                                          ^
Error: Refinement could not be proved (counterexample)
Line 4, characters 4-50:
4 |     (Functional_queue.contents q === []) === false} = q in
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* queue_representation *)
let invalid (q : int Functional_queue.t) = q.front;;
[%%expect{|
Line 1, characters 45-50:
1 | let invalid (q : int Functional_queue.t) = q.front;;
                                                 ^^^^^
Error: Unbound record field "front"
|}]

(* avl_representation *)
let invalid = Avl_sets.Core.Leaf;;
[%%expect{|
Line 1, characters 14-27:
1 | let invalid = Avl_sets.Core.Leaf;;
                  ^^^^^^^^^^^^^
Error: Unbound module "Avl_sets.Core"
|}]

(* avl_structural_equality *)
let invalid (a : Avl_sets.t) (b : Avl_sets.t)
  (same : {u : unit | Avl_sets.equal a b}) : {u : unit | a === b} =
  let u = same in u;;
[%%expect{|
Line 3, characters 18-19:
3 |   let u = same in u;;
                      ^
Error: Refinement could not be proved (counterexample)
Line 2, characters 57-64:
2 |   (same : {u : unit | Avl_sets.equal a b}) : {u : unit | a === b} =
                                                             ^^^^^^^
  The refinement is stated here.
|}]

(* sorted_representation *)
let invalid (a : Sorted_array.t) = Iarray.length a;;
[%%expect{|
Line 1, characters 49-50:
1 | let invalid (a : Sorted_array.t) = Iarray.length a;;
                                                     ^
Error: The value "a" has type "Sorted_array.t"
       but an expression was expected of type "'a iarray"
|}]

(* sorted_empty_removal *)
let () =
  let a = Sorted_array.empty in
  let zero = 0 in
  let u = () in
  let _ = Sorted_array.remove_at a zero (u) in ();;
[%%expect{|
Line 5, characters 40-43:
5 |   let _ = Sorted_array.remove_at a zero (u) in ();;
                                            ^^^
Error: Refinement could not be proved (counterexample)
File "sorted_array.mli", line 29, characters 14-55:
  The refinement is stated here.
|}]

(* sorted_proofs *)
let invalid = Sorted_array_proofs.Arrays.at;;
[%%expect{|
Line 1, characters 14-33:
1 | let invalid = Sorted_array_proofs.Arrays.at;;
                  ^^^^^^^^^^^^^^^^^^^
Error: Unbound module "Sorted_array_proofs"
|}]

(* quicksort_proofs *)
let invalid = Quicksort_model.swap_partition;;
[%%expect{|
Line 1, characters 14-29:
1 | let invalid = Quicksort_model.swap_partition;;
                  ^^^^^^^^^^^^^^^
Error: Unbound module "Quicksort_model"
|}]

(* quicksort_false_count *)
let invalid (input : int iarray) =
  let a = Borrow.Owned_array.of_iarray input in
  let b = Quicksort.sort_array a in
  let result = Borrow.Owned_array.into_iarray b in
  let before = ghost_ (Vox_sequence.of_iarray input) in
  let after = ghost_ (Vox_sequence.of_iarray result) in
  let target = 7 in
  ghost_ (
    Quicksort.Spec.permutation_count before after target;
    let u = () in (u : {u : unit |
      Quicksort.Spec.count after target ===
        Bigint.add 1Z (Quicksort.Spec.count before target)}));;
[%%expect{|
Line 10, characters 19-20:
10 |     let u = () in (u : {u : unit |
                        ^
Error: Refinement could not be proved (counterexample)
Lines 11-12, characters 6-58:
11 | ......Quicksort.Spec.count after target ===
12 |         Bigint.add 1Z (Quicksort.Spec.count before target).....
  The refinement is stated here.
|}]

(* sparse_representation *)
let invalid (a : int Sparse_overlay.t) = a.updates;;
[%%expect{|
Line 1, characters 43-50:
1 | let invalid (a : int Sparse_overlay.t) = a.updates;;
                                               ^^^^^^^
Error: Unbound record field "updates"
|}]

(* The sparse fixtures use these laws. *)
module L = Sparse_overlay.Laws(struct type t = int end);;
[%%expect{|
module L :
  sig
    val length_equation :
      (overlay : int Sparse_overlay.t) ->
      {u : unit
        | (Sparse_overlay.length overlay) =
            (Iarray.length (Sparse_overlay.base overlay))}
      @@ total
    val get_lookup :
      (overlay : int Sparse_overlay.t) ->
      (index : {i : int | (0 <= i) && (i < (Sparse_overlay.length overlay))}) ->
      {u : unit
        | let i = index in
          (Sparse_overlay.lookup i overlay) ===
            (Some (Sparse_overlay.get overlay index))}
      @@ total
    val lookup_outside :
      (overlay : int Sparse_overlay.t) ->
      (index : int) ->
      {u : unit
        | if (index < 0) || ((Sparse_overlay.length overlay) <= index)
          then (Sparse_overlay.lookup index overlay) === None
          else true}
      @@ total
    val empty_base :
      (values : int iarray) ->
      {u : unit
        | (Sparse_overlay.base (Sparse_overlay.empty values)) === values}
      @@ total
    val empty_lookup :
      (values : int iarray) ->
      (index : int) ->
      {u : unit
        | (Sparse_overlay.lookup index (Sparse_overlay.empty values)) ===
            (Vox_iarray.at values index)}
      @@ total
    val set_base :
      (overlay : int Sparse_overlay.t) ->
      (index : int) ->
      (value : int) ->
      {u : unit
        | (Sparse_overlay.base (Sparse_overlay.set index value overlay)) ===
            (Sparse_overlay.base overlay)}
      @@ total
    val set_lookup :
      (overlay : int Sparse_overlay.t) ->
      (index : int) ->
      (value : int) ->
      (query : int) ->
      {u : unit
        | (Sparse_overlay.lookup query
             (Sparse_overlay.set index value overlay))
            ===
            (if
               (0 <= query) &&
                 ((query < (Sparse_overlay.length overlay)) &&
                    (query = index))
             then Some value
             else Sparse_overlay.lookup query overlay)}
      @@ total
    val clear_base :
      (overlay : int Sparse_overlay.t) ->
      (index : int) ->
      {u : unit
        | (Sparse_overlay.base (Sparse_overlay.clear index overlay)) ===
            (Sparse_overlay.base overlay)}
      @@ total
    val clear_lookup :
      (overlay : int Sparse_overlay.t) ->
      (index : int) ->
      (query : int) ->
      {u : unit
        | (Sparse_overlay.lookup query (Sparse_overlay.clear index overlay))
            ===
            (if query = index
             then Vox_iarray.at (Sparse_overlay.base overlay) query
             else Sparse_overlay.lookup query overlay)}
      @@ total
  end
|}]

(* sparse_bounds *)
let () =
  let values = [: :] in
  let a = Sparse_overlay.empty values in
  let zero = 0 in
  ghost_ (L.empty_base values; L.length_equation a);
  let _ = Sparse_overlay.get a (zero) in ();;
[%%expect{|
Line 6, characters 31-37:
6 |   let _ = Sparse_overlay.get a (zero) in ();;
                                   ^^^^^^
Error: Refinement could not be proved (counterexample)
File "sparse_overlay.mli", line 10, characters 33-61:
  The refinement is stated here.
|}]

(* sparse_false_update *)
let invalid : (a : int Sparse_overlay.t) -> (index : int) -> (value : int) ->
    {u : unit | 0 <= index && index < Sparse_overlay.length a} -> unit =
  fun a index value bound ->
  let _bounds = bound in
  let b = Sparse_overlay.set index value a in
  ghost_ (L.set_lookup a index value index);
  let u = () in
  let false_claim : {u : unit |
    Sparse_overlay.lookup index b === Some (value + 1)} = u in
  let _proof = false_claim in ();;
[%%expect{|
Line 9, characters 58-59:
9 |     Sparse_overlay.lookup index b === Some (value + 1)} = u in
                                                              ^
Error: Refinement could not be proved (counterexample)
Line 9, characters 4-54:
9 |     Sparse_overlay.lookup index b === Some (value + 1)} = u in
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]
