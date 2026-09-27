(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli";
 all_modules += " vox_int_sequence.ml vox_iarray.mli vox_iarray.ml";
 all_modules += " vox_string_view.mli vox_string_view.ml borrow_iarray.mli";
 all_modules += " borrow_iarray.ml pref.mli pref.ml ghost_pref.mli";
 all_modules += " ghost_pref.ml raw_memory.mli raw_memory.ml";
 all_modules += " vox_lz4_spec_storage.ml vox_lz4_spec_parse.ml";
 all_modules += " vox_lz4_spec_decode.ml vox_lz4_spec_decode_bytes.ml";
 all_modules += " vox_lz4_spec_bytes.ml vox_lz4_heap_bytes.ml";
 all_modules += " vox_lz4_spec_match.ml vox_lz4_spec_plan.ml";
 all_modules += " vox_lz4_spec_token.ml vox_lz4_spec_wire.ml";
 all_modules += " vox_lz4_spec_hashes.ml vox_lz4_spec_scan.ml";
 all_modules += " vox_lz4_spec.ml vox_lz4_buffer.ml vox_lz4_packed.ml";
 all_modules += " vox_lz4_encode_buffer.ml vox_lz4_packed_encode.ml";
 all_modules += " vox_lz4_snapshot.ml vox_lz4_string_copy.mli";
 all_modules += " vox_lz4_string_copy.ml vox_lz4_roundtrip.ml";
 all_modules += " vox_lz4_general_match.ml vox_lz4_string_match.ml";
 all_modules += " vox_lz4_general_plan.ml vox_lz4_general_encode.ml";
 all_modules += " vox_lz4_general_wire.ml vox_lz4_decode_bytes_proof.ml";
 all_modules += " vox_lz4_decode_bytes_roundtrip.ml vox_lz4_general_cost.ml";
 all_modules += " vox_lz4_general_bridge.ml vox_lz4_general_sized.ml";
 all_modules += " vox_lz4_string_encode.ml vox_lz4_general_roundtrip.ml";
 all_modules += " vox_lz4_fast_plan_model.ml vox_lz4_mutable_scan.ml";
 all_modules += " vox_lz4_string_scan.ml vox_lz4_string_decode.ml";
 all_modules += " vox_lz4_string_codec.ml vox_lz4_string_roundtrip.ml";
 all_modules += " vox_lz4_forward_model.ml vox_lz4_streaming.ml";
 all_modules += " vox_lz4_streaming_codec.ml vox_lz4_streaming_roundtrip.ml";
 all_modules += " vox_lz4_fast_plan_roundtrip.ml";
 all_modules += " vox_lz4_fast_hints_reference.ml vox_lz4_checked_api.ml";
 all_modules += " vox_lz4.mli vox_lz4.ml";
 set lib = "";
 {
   setup-ocamlc.opt-build-env;
   lib = "${test_build_directory_prefix}/ocamlc.opt";
   compile_only = "true";
   ocamlc.opt;
   compile_only = "false";
   flags = "-extension refinement_types -principal";
   src = "${lib}/vox_lz4.cmi ${lib}/vox_string_view.cmi";
   src += " ${lib}/vox_iarray.cmi ${lib}/vox_sequence.cmi";
   src += " ${lib}/vox_lz4_spec.cmi ${lib}/vox_lz4_spec_parse.cmi";
   src += " ${lib}/vox_lz4_spec_decode_bytes.cmi";
   src += " ${lib}/vox_lz4_spec_bytes.cmi";
   src += " ${lib}/vox_lz4_spec_match.cmi";
   src += " ${lib}/vox_lz4_spec_plan.cmi";
   src += " ${lib}/vox_lz4_spec_token.cmi";
   src += " ${lib}/vox_lz4_spec_wire.cmi";
   src += " ${lib}/vox_lz4_spec_hashes.cmi";
   src += " ${lib}/vox_lz4_spec_scan.cmi";
   dst = "${test_build_directory_prefix}/ocamlc.opt.public/";
   compiler_directory_suffix = ".public";
   all_modules = "vox_lz4_public_client.ml";
   readonly_files = "vox_lz4_public_client.ml emitted_code.ml";
   readonly_files += " lz4_boundary_check.ml lz4_boundary.ml";
   setup-ocamlc.opt-build-env;
   copy;
   binary_modules = "${lib}/vox_sequence ${lib}/vox_int_sequence";
   binary_modules += " ${lib}/vox_iarray ${lib}/vox_string_view";
   binary_modules += " ${lib}/borrow_iarray ${lib}/pref ${lib}/ghost_pref";
   binary_modules += " ${lib}/raw_memory ${lib}/vox_lz4_spec_storage";
   binary_modules += " ${lib}/vox_lz4_spec_parse ${lib}/vox_lz4_spec_decode";
   binary_modules += " ${lib}/vox_lz4_spec_decode_bytes";
   binary_modules += " ${lib}/vox_lz4_spec_bytes ${lib}/vox_lz4_heap_bytes";
   binary_modules += " ${lib}/vox_lz4_spec_match ${lib}/vox_lz4_spec_plan";
   binary_modules += " ${lib}/vox_lz4_spec_token ${lib}/vox_lz4_spec_wire";
   binary_modules += " ${lib}/vox_lz4_spec_hashes ${lib}/vox_lz4_spec_scan";
   binary_modules += " ${lib}/vox_lz4_spec ${lib}/vox_lz4_buffer";
   binary_modules += " ${lib}/vox_lz4_packed ${lib}/vox_lz4_encode_buffer";
   binary_modules += " ${lib}/vox_lz4_packed_encode ${lib}/vox_lz4_snapshot";
   binary_modules += " ${lib}/vox_lz4_string_copy ${lib}/vox_lz4_roundtrip";
   binary_modules += " ${lib}/vox_lz4_general_match";
   binary_modules += " ${lib}/vox_lz4_string_match ${lib}/vox_lz4_general_plan";
   binary_modules += " ${lib}/vox_lz4_general_encode";
   binary_modules += " ${lib}/vox_lz4_general_wire";
   binary_modules += " ${lib}/vox_lz4_decode_bytes_proof";
   binary_modules += " ${lib}/vox_lz4_decode_bytes_roundtrip";
   binary_modules += " ${lib}/vox_lz4_general_cost";
   binary_modules += " ${lib}/vox_lz4_general_bridge";
   binary_modules += " ${lib}/vox_lz4_general_sized";
   binary_modules += " ${lib}/vox_lz4_string_encode";
   binary_modules += " ${lib}/vox_lz4_general_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_plan_model";
   binary_modules += " ${lib}/vox_lz4_mutable_scan ${lib}/vox_lz4_string_scan";
   binary_modules += " ${lib}/vox_lz4_string_decode";
   binary_modules += " ${lib}/vox_lz4_string_codec";
   binary_modules += " ${lib}/vox_lz4_string_roundtrip";
   binary_modules += " ${lib}/vox_lz4_forward_model ${lib}/vox_lz4_streaming";
   binary_modules += " ${lib}/vox_lz4_streaming_codec";
   binary_modules += " ${lib}/vox_lz4_streaming_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_plan_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_hints_reference";
   binary_modules += " ${lib}/vox_lz4_checked_api ${lib}/vox_lz4";
   compile_only = "true";
   flags += " -dlambda";
   compiler_output2 = "${lib}.public/client.lambda";
   all_modules = "vox_lz4_public_client.ml";
   ocamlc.opt;
   compile_only = "false";
   flags = "-extension refinement_types -principal";
   compiler_output2 = "${lib}.public/link.output";
   all_modules = "vox_lz4_public_client.ml";
   program = "${lib}.public/client.exe";
   ocamlc.opt;
   check-ocamlc.opt-output;
   output = "${lib}.public/client.output";
   stdout = "${output}";
   stderr = "${output}";
   run;
   check-program-output;
   unset binary_modules;
   flags = "";
   compiler_output2 = "${lib}.public/checker.output";
   all_modules = "emitted_code.ml lz4_boundary_check.ml";
   program = "${lib}.public/check.exe";
   ocamlc.opt;
   arguments = "${lib}.public/client.lambda";
   output = "${lib}.public/check.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${test_source_directory}/lz4_boundary.checks.reference";
   run;
   check-program-output;
   flags = "-extension refinement_types -principal";
   binary_modules = "${lib}/vox_sequence ${lib}/vox_int_sequence";
   binary_modules += " ${lib}/vox_iarray ${lib}/vox_string_view";
   binary_modules += " ${lib}/borrow_iarray ${lib}/pref ${lib}/ghost_pref";
   binary_modules += " ${lib}/raw_memory ${lib}/vox_lz4_spec_storage";
   binary_modules += " ${lib}/vox_lz4_spec_parse ${lib}/vox_lz4_spec_decode";
   binary_modules += " ${lib}/vox_lz4_spec_decode_bytes";
   binary_modules += " ${lib}/vox_lz4_spec_bytes ${lib}/vox_lz4_heap_bytes";
   binary_modules += " ${lib}/vox_lz4_spec_match ${lib}/vox_lz4_spec_plan";
   binary_modules += " ${lib}/vox_lz4_spec_token ${lib}/vox_lz4_spec_wire";
   binary_modules += " ${lib}/vox_lz4_spec_hashes ${lib}/vox_lz4_spec_scan";
   binary_modules += " ${lib}/vox_lz4_spec ${lib}/vox_lz4_buffer";
   binary_modules += " ${lib}/vox_lz4_packed ${lib}/vox_lz4_encode_buffer";
   binary_modules += " ${lib}/vox_lz4_packed_encode ${lib}/vox_lz4_snapshot";
   binary_modules += " ${lib}/vox_lz4_string_copy ${lib}/vox_lz4_roundtrip";
   binary_modules += " ${lib}/vox_lz4_general_match";
   binary_modules += " ${lib}/vox_lz4_string_match ${lib}/vox_lz4_general_plan";
   binary_modules += " ${lib}/vox_lz4_general_encode";
   binary_modules += " ${lib}/vox_lz4_general_wire";
   binary_modules += " ${lib}/vox_lz4_decode_bytes_proof";
   binary_modules += " ${lib}/vox_lz4_decode_bytes_roundtrip";
   binary_modules += " ${lib}/vox_lz4_general_cost";
   binary_modules += " ${lib}/vox_lz4_general_bridge";
   binary_modules += " ${lib}/vox_lz4_general_sized";
   binary_modules += " ${lib}/vox_lz4_string_encode";
   binary_modules += " ${lib}/vox_lz4_general_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_plan_model";
   binary_modules += " ${lib}/vox_lz4_mutable_scan ${lib}/vox_lz4_string_scan";
   binary_modules += " ${lib}/vox_lz4_string_decode";
   binary_modules += " ${lib}/vox_lz4_string_codec";
   binary_modules += " ${lib}/vox_lz4_string_roundtrip";
   binary_modules += " ${lib}/vox_lz4_forward_model ${lib}/vox_lz4_streaming";
   binary_modules += " ${lib}/vox_lz4_streaming_codec";
   binary_modules += " ${lib}/vox_lz4_streaming_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_plan_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_hints_reference";
   binary_modules += " ${lib}/vox_lz4_checked_api ${lib}/vox_lz4";
   run-expect;
   check-program-output;
   compiler_directory_suffix = ".finalizers";
   readonly_files = "lz4_finalizers.ml lz4_finalizer_stubs.c";
   readonly_files += " raw_memory_demo.ml";
   setup-ocamlc.opt-build-env;
   binary_modules = "${lib}/vox_sequence ${lib}/vox_int_sequence";
   binary_modules += " ${lib}/vox_iarray ${lib}/vox_string_view";
   binary_modules += " ${lib}/borrow_iarray ${lib}/pref ${lib}/ghost_pref";
   binary_modules += " ${lib}/raw_memory ${lib}/vox_lz4_spec_storage";
   binary_modules += " ${lib}/vox_lz4_spec_parse ${lib}/vox_lz4_spec_decode";
   binary_modules += " ${lib}/vox_lz4_spec_decode_bytes";
   binary_modules += " ${lib}/vox_lz4_spec_bytes ${lib}/vox_lz4_heap_bytes";
   binary_modules += " ${lib}/vox_lz4_spec_match ${lib}/vox_lz4_spec_plan";
   binary_modules += " ${lib}/vox_lz4_spec_token ${lib}/vox_lz4_spec_wire";
   binary_modules += " ${lib}/vox_lz4_spec_hashes ${lib}/vox_lz4_spec_scan";
   binary_modules += " ${lib}/vox_lz4_spec ${lib}/vox_lz4_buffer";
   binary_modules += " ${lib}/vox_lz4_packed ${lib}/vox_lz4_encode_buffer";
   binary_modules += " ${lib}/vox_lz4_packed_encode ${lib}/vox_lz4_snapshot";
   binary_modules += " ${lib}/vox_lz4_string_copy ${lib}/vox_lz4_roundtrip";
   binary_modules += " ${lib}/vox_lz4_general_match";
   binary_modules += " ${lib}/vox_lz4_string_match ${lib}/vox_lz4_general_plan";
   binary_modules += " ${lib}/vox_lz4_general_encode";
   binary_modules += " ${lib}/vox_lz4_general_wire";
   binary_modules += " ${lib}/vox_lz4_decode_bytes_proof";
   binary_modules += " ${lib}/vox_lz4_decode_bytes_roundtrip";
   binary_modules += " ${lib}/vox_lz4_general_cost";
   binary_modules += " ${lib}/vox_lz4_general_bridge";
   binary_modules += " ${lib}/vox_lz4_general_sized";
   binary_modules += " ${lib}/vox_lz4_string_encode";
   binary_modules += " ${lib}/vox_lz4_general_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_plan_model";
   binary_modules += " ${lib}/vox_lz4_mutable_scan ${lib}/vox_lz4_string_scan";
   binary_modules += " ${lib}/vox_lz4_string_decode";
   binary_modules += " ${lib}/vox_lz4_string_codec";
   binary_modules += " ${lib}/vox_lz4_string_roundtrip";
   binary_modules += " ${lib}/vox_lz4_forward_model ${lib}/vox_lz4_streaming";
   binary_modules += " ${lib}/vox_lz4_streaming_codec";
   binary_modules += " ${lib}/vox_lz4_streaming_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_plan_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_hints_reference";
   binary_modules += " ${lib}/vox_lz4_checked_api ${lib}/vox_lz4";
   flags = "-extension refinement_types -principal -g -custom -I ${lib}";
   all_modules = "lz4_finalizer_stubs.c lz4_finalizers.ml";
   compiler_output2 = "${lib}.finalizers/compile.output";
   program = "${lib}.finalizers/finalizers.exe";
   ocamlc.opt;
   output = "${lib}.finalizers/finalizers.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${test_source_directory}/lz4_boundary.finalizers.reference";
   run;
   check-program-output;
   flags = "-extension refinement_types -principal -I ${lib}";
   all_modules = "raw_memory_demo.ml";
   program = "${lib}.finalizers/raw_memory.exe";
   ocamlc.opt;
   output = "${lib}.finalizers/raw_memory.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${test_source_directory}/lz4_boundary.raw_memory.reference";
   run;
   check-program-output;
 }
 {
   setup-ocamlopt.opt-build-env;
   lib = "${test_build_directory_prefix}/ocamlopt.opt";
   flags = "-extension refinement_types -principal -O3";
   compile_only = "true";
   ocamlopt.opt;
   compile_only = "false";
   flags = "-extension refinement_types -principal";
   src = "${lib}/vox_lz4.cmi ${lib}/vox_string_view.cmi";
   src += " ${lib}/vox_iarray.cmi ${lib}/vox_sequence.cmi";
   src += " ${lib}/vox_lz4_spec.cmi ${lib}/vox_lz4_spec_parse.cmi";
   src += " ${lib}/vox_lz4_spec_decode_bytes.cmi";
   src += " ${lib}/vox_lz4_spec_bytes.cmi";
   src += " ${lib}/vox_lz4_spec_match.cmi";
   src += " ${lib}/vox_lz4_spec_plan.cmi";
   src += " ${lib}/vox_lz4_spec_token.cmi";
   src += " ${lib}/vox_lz4_spec_wire.cmi";
   src += " ${lib}/vox_lz4_spec_hashes.cmi";
   src += " ${lib}/vox_lz4_spec_scan.cmi ${lib}/vox_sequence.cmx";
   src += " ${lib}/vox_int_sequence.cmx ${lib}/vox_iarray.cmx";
   src += " ${lib}/vox_string_view.cmx ${lib}/borrow_iarray.cmx";
   src += " ${lib}/pref.cmx ${lib}/ghost_pref.cmx";
   src += " ${lib}/raw_memory.cmx ${lib}/vox_lz4_spec_storage.cmx";
   src += " ${lib}/vox_lz4_spec_parse.cmx";
   src += " ${lib}/vox_lz4_spec_decode.cmx";
   src += " ${lib}/vox_lz4_spec_decode_bytes.cmx";
   src += " ${lib}/vox_lz4_spec_bytes.cmx";
   src += " ${lib}/vox_lz4_heap_bytes.cmx";
   src += " ${lib}/vox_lz4_spec_match.cmx";
   src += " ${lib}/vox_lz4_spec_plan.cmx";
   src += " ${lib}/vox_lz4_spec_token.cmx";
   src += " ${lib}/vox_lz4_spec_wire.cmx";
   src += " ${lib}/vox_lz4_spec_hashes.cmx";
   src += " ${lib}/vox_lz4_spec_scan.cmx ${lib}/vox_lz4_spec.cmx";
   src += " ${lib}/vox_lz4_buffer.cmx ${lib}/vox_lz4_packed.cmx";
   src += " ${lib}/vox_lz4_encode_buffer.cmx";
   src += " ${lib}/vox_lz4_packed_encode.cmx";
   src += " ${lib}/vox_lz4_snapshot.cmx";
   src += " ${lib}/vox_lz4_string_copy.cmx";
   src += " ${lib}/vox_lz4_roundtrip.cmx";
   src += " ${lib}/vox_lz4_general_match.cmx";
   src += " ${lib}/vox_lz4_string_match.cmx";
   src += " ${lib}/vox_lz4_general_plan.cmx";
   src += " ${lib}/vox_lz4_general_encode.cmx";
   src += " ${lib}/vox_lz4_general_wire.cmx";
   src += " ${lib}/vox_lz4_decode_bytes_proof.cmx";
   src += " ${lib}/vox_lz4_decode_bytes_roundtrip.cmx";
   src += " ${lib}/vox_lz4_general_cost.cmx";
   src += " ${lib}/vox_lz4_general_bridge.cmx";
   src += " ${lib}/vox_lz4_general_sized.cmx";
   src += " ${lib}/vox_lz4_string_encode.cmx";
   src += " ${lib}/vox_lz4_general_roundtrip.cmx";
   src += " ${lib}/vox_lz4_fast_plan_model.cmx";
   src += " ${lib}/vox_lz4_mutable_scan.cmx";
   src += " ${lib}/vox_lz4_string_scan.cmx";
   src += " ${lib}/vox_lz4_string_decode.cmx";
   src += " ${lib}/vox_lz4_string_codec.cmx";
   src += " ${lib}/vox_lz4_string_roundtrip.cmx";
   src += " ${lib}/vox_lz4_forward_model.cmx";
   src += " ${lib}/vox_lz4_streaming.cmx";
   src += " ${lib}/vox_lz4_streaming_codec.cmx";
   src += " ${lib}/vox_lz4_streaming_roundtrip.cmx";
   src += " ${lib}/vox_lz4_fast_plan_roundtrip.cmx";
   src += " ${lib}/vox_lz4_fast_hints_reference.cmx";
   src += " ${lib}/vox_lz4_checked_api.cmx ${lib}/vox_lz4.cmx";
   dst = "${test_build_directory_prefix}/ocamlopt.opt.public/";
   compiler_directory_suffix = ".public";
   all_modules = "vox_lz4_public_client.ml";
   readonly_files = "vox_lz4_public_client.ml emitted_code.ml";
   readonly_files += " lz4_boundary_check.ml lz4_boundary.ml";
   setup-ocamlopt.opt-build-env;
   copy;
   binary_modules = "${lib}/vox_sequence ${lib}/vox_int_sequence";
   binary_modules += " ${lib}/vox_iarray ${lib}/vox_string_view";
   binary_modules += " ${lib}/borrow_iarray ${lib}/pref ${lib}/ghost_pref";
   binary_modules += " ${lib}/raw_memory ${lib}/vox_lz4_spec_storage";
   binary_modules += " ${lib}/vox_lz4_spec_parse ${lib}/vox_lz4_spec_decode";
   binary_modules += " ${lib}/vox_lz4_spec_decode_bytes";
   binary_modules += " ${lib}/vox_lz4_spec_bytes ${lib}/vox_lz4_heap_bytes";
   binary_modules += " ${lib}/vox_lz4_spec_match ${lib}/vox_lz4_spec_plan";
   binary_modules += " ${lib}/vox_lz4_spec_token ${lib}/vox_lz4_spec_wire";
   binary_modules += " ${lib}/vox_lz4_spec_hashes ${lib}/vox_lz4_spec_scan";
   binary_modules += " ${lib}/vox_lz4_spec ${lib}/vox_lz4_buffer";
   binary_modules += " ${lib}/vox_lz4_packed ${lib}/vox_lz4_encode_buffer";
   binary_modules += " ${lib}/vox_lz4_packed_encode ${lib}/vox_lz4_snapshot";
   binary_modules += " ${lib}/vox_lz4_string_copy ${lib}/vox_lz4_roundtrip";
   binary_modules += " ${lib}/vox_lz4_general_match";
   binary_modules += " ${lib}/vox_lz4_string_match ${lib}/vox_lz4_general_plan";
   binary_modules += " ${lib}/vox_lz4_general_encode";
   binary_modules += " ${lib}/vox_lz4_general_wire";
   binary_modules += " ${lib}/vox_lz4_decode_bytes_proof";
   binary_modules += " ${lib}/vox_lz4_decode_bytes_roundtrip";
   binary_modules += " ${lib}/vox_lz4_general_cost";
   binary_modules += " ${lib}/vox_lz4_general_bridge";
   binary_modules += " ${lib}/vox_lz4_general_sized";
   binary_modules += " ${lib}/vox_lz4_string_encode";
   binary_modules += " ${lib}/vox_lz4_general_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_plan_model";
   binary_modules += " ${lib}/vox_lz4_mutable_scan ${lib}/vox_lz4_string_scan";
   binary_modules += " ${lib}/vox_lz4_string_decode";
   binary_modules += " ${lib}/vox_lz4_string_codec";
   binary_modules += " ${lib}/vox_lz4_string_roundtrip";
   binary_modules += " ${lib}/vox_lz4_forward_model ${lib}/vox_lz4_streaming";
   binary_modules += " ${lib}/vox_lz4_streaming_codec";
   binary_modules += " ${lib}/vox_lz4_streaming_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_plan_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_hints_reference";
   binary_modules += " ${lib}/vox_lz4_checked_api ${lib}/vox_lz4";
   compile_only = "true";
   flags += " -dlambda";
   compiler_output2 = "${lib}.public/client.lambda";
   all_modules = "vox_lz4_public_client.ml";
   ocamlopt.opt;
   compile_only = "false";
   flags = "-extension refinement_types -principal";
   compiler_output2 = "${lib}.public/link.output";
   all_modules = "vox_lz4_public_client.ml";
   program = "${lib}.public/client.exe";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   output = "${lib}.public/client.output";
   stdout = "${output}";
   stderr = "${output}";
   run;
   check-program-output;
   unset binary_modules;
   flags = "";
   compiler_output2 = "${lib}.public/checker.output";
   all_modules = "emitted_code.ml lz4_boundary_check.ml";
   program = "${lib}.public/check.exe";
   ocamlopt.opt;
   arguments = "${lib}.public/client.lambda";
   output = "${lib}.public/check.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${test_source_directory}/lz4_boundary.checks.reference";
   run;
   check-program-output;
   compiler_directory_suffix = ".finalizers";
   readonly_files = "lz4_finalizers.ml lz4_finalizer_stubs.c";
   readonly_files += " raw_memory_demo.ml";
   setup-ocamlopt.opt-build-env;
   binary_modules = "${lib}/vox_sequence ${lib}/vox_int_sequence";
   binary_modules += " ${lib}/vox_iarray ${lib}/vox_string_view";
   binary_modules += " ${lib}/borrow_iarray ${lib}/pref ${lib}/ghost_pref";
   binary_modules += " ${lib}/raw_memory ${lib}/vox_lz4_spec_storage";
   binary_modules += " ${lib}/vox_lz4_spec_parse ${lib}/vox_lz4_spec_decode";
   binary_modules += " ${lib}/vox_lz4_spec_decode_bytes";
   binary_modules += " ${lib}/vox_lz4_spec_bytes ${lib}/vox_lz4_heap_bytes";
   binary_modules += " ${lib}/vox_lz4_spec_match ${lib}/vox_lz4_spec_plan";
   binary_modules += " ${lib}/vox_lz4_spec_token ${lib}/vox_lz4_spec_wire";
   binary_modules += " ${lib}/vox_lz4_spec_hashes ${lib}/vox_lz4_spec_scan";
   binary_modules += " ${lib}/vox_lz4_spec ${lib}/vox_lz4_buffer";
   binary_modules += " ${lib}/vox_lz4_packed ${lib}/vox_lz4_encode_buffer";
   binary_modules += " ${lib}/vox_lz4_packed_encode ${lib}/vox_lz4_snapshot";
   binary_modules += " ${lib}/vox_lz4_string_copy ${lib}/vox_lz4_roundtrip";
   binary_modules += " ${lib}/vox_lz4_general_match";
   binary_modules += " ${lib}/vox_lz4_string_match ${lib}/vox_lz4_general_plan";
   binary_modules += " ${lib}/vox_lz4_general_encode";
   binary_modules += " ${lib}/vox_lz4_general_wire";
   binary_modules += " ${lib}/vox_lz4_decode_bytes_proof";
   binary_modules += " ${lib}/vox_lz4_decode_bytes_roundtrip";
   binary_modules += " ${lib}/vox_lz4_general_cost";
   binary_modules += " ${lib}/vox_lz4_general_bridge";
   binary_modules += " ${lib}/vox_lz4_general_sized";
   binary_modules += " ${lib}/vox_lz4_string_encode";
   binary_modules += " ${lib}/vox_lz4_general_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_plan_model";
   binary_modules += " ${lib}/vox_lz4_mutable_scan ${lib}/vox_lz4_string_scan";
   binary_modules += " ${lib}/vox_lz4_string_decode";
   binary_modules += " ${lib}/vox_lz4_string_codec";
   binary_modules += " ${lib}/vox_lz4_string_roundtrip";
   binary_modules += " ${lib}/vox_lz4_forward_model ${lib}/vox_lz4_streaming";
   binary_modules += " ${lib}/vox_lz4_streaming_codec";
   binary_modules += " ${lib}/vox_lz4_streaming_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_plan_roundtrip";
   binary_modules += " ${lib}/vox_lz4_fast_hints_reference";
   binary_modules += " ${lib}/vox_lz4_checked_api ${lib}/vox_lz4";
   flags = "-extension refinement_types -principal -g -O3 -I ${lib}";
   all_modules = "lz4_finalizer_stubs.c lz4_finalizers.ml";
   compiler_output2 = "${lib}.finalizers/compile.output";
   program = "${lib}.finalizers/finalizers.exe";
   ocamlopt.opt;
   output = "${lib}.finalizers/finalizers.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${test_source_directory}/lz4_boundary.finalizers.reference";
   run;
   check-program-output;
   flags = "-extension refinement_types -principal -I ${lib}";
   all_modules = "raw_memory_demo.ml";
   program = "${lib}.finalizers/raw_memory.exe";
   ocamlopt.opt;
   output = "${lib}.finalizers/raw_memory.output";
   stdout = "${output}";
   stderr = "${output}";
   reference = "${test_source_directory}/lz4_boundary.raw_memory.reference";
   run;
   check-program-output;
 }
*)

(* The LZ4 codec's boundary. With each compiler, the library is compiled
   (natively at -O3, as verification/library/build.sh does), and
   vox_lz4_public_client.ml is compiled with only the public semantic
   interfaces, linked and run; lz4_boundary_check.ml checks that its
   Lambda calls Vox_lz4 twice and nothing of the specification. The
   phrases below are rejected against the same interfaces. Last,
   lz4_finalizers.ml checks explicit release and reclamation by the
   finalizer after simulated Out_of_memory exits, and raw_memory_demo.ml
   the raw-memory primitives, linked with the library. *)

(* A positive control. *)
let compress (s : string) = Vox_lz4.compress s;;
[%%expect{|
val compress : string -> string = <fun>
|}]

(* The decoding model and the codec's internals are hidden. *)
let f = Vox_lz4_spec_decode.decode_model;;
[%%expect{|
Line 1, characters 8-27:
1 | let f = Vox_lz4_spec_decode.decode_model;;
            ^^^^^^^^^^^^^^^^^^^
Error: Unbound module "Vox_lz4_spec_decode"
|}]

let f = Vox_lz4.C.compress_string;;
[%%expect{|
Line 1, characters 8-17:
1 | let f = Vox_lz4.C.compress_string;;
            ^^^^^^^^^
Error: Unbound module "Vox_lz4.C"
|}]

(* A string's contents are ghost. *)
let f (s : string) : int = Iarray.length (Vox_string_view.contents s);;
[%%expect{|
Line 1, characters 41-69:
1 | let f (s : string) : int = Iarray.length (Vox_string_view.contents s);;
                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "ghost" but is expected to be "real".
Hint: if this is proof code, wrap the enclosing expression in "ghost_ (...)".
|}]

(* Compression is not the identity, and decoding at capacity 0 may fail
   with any error. *)
let f (s : string) :
    {w : string | Vox_string_view.contents w === Vox_string_view.contents s} =
  Vox_lz4.compress s;;
[%%expect{|
Line 3, characters 2-20:
3 |   Vox_lz4.compress s;;
      ^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 18-75:
2 |     {w : string | Vox_string_view.contents w === Vox_string_view.contents s} =
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let f (wire : string) :
    {d : Vox_lz4_spec.decoded | match d with
      | Ok _ -> true
      | Error Vox_lz4_spec.Output_limit -> true
      | Error _ -> false} =
  Vox_lz4.decompress_verified wire 0;;
[%%expect{|
Line 6, characters 2-36:
6 |   Vox_lz4.decompress_verified wire 0;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Lines 2-5, characters 32-24:
2 | ................................match d with
3 |       | Ok _ -> true
4 |       | Error Vox_lz4_spec.Output_limit -> true
5 |       | Error _ -> false...
  The refinement is stated here.
|}]
