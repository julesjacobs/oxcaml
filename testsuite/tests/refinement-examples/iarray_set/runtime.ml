(* TEST
 readonly_files = "\
   iarray_model.mli iarray_model.ml iarray_set.mli iarray_set.ml \
   sorted_iarray_stubs.c runtime_test.ml runtime_test.reference \
   runtime_exhaustive_test.ml runtime_exhaustive_test.reference \
 ";
 setup-ocamlc.byte-build-env;
 module = "${test_source_directory}/../set_group/set_intf.ml";
 flags = "-o set_intf.cmo";
 ocamlc.byte;
 module = "iarray_model.mli";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 module = "iarray_model.ml";
 ocamlc.byte;
 module = "iarray_set.mli";
 ocamlc.byte;
 module = "iarray_set.ml";
 ocamlc.byte;
 module = "";
 program = "${test_build_directory}/runtime_test.exe";
 all_modules = "iarray_model.cmo iarray_set.cmo sorted_iarray_stubs.c runtime_test.ml";
 flags = "-custom -I . -ccopt -Wall -ccopt -Wextra -ccopt -Werror";
 ocamlc.byte;
 output = "${test_build_directory}/runtime_test.output";
 stdout = "${output}";
 stderr = "${output}";
 run;
 reference = "${test_source_directory}/runtime_test.reference";
 check-program-output;
 program = "${test_build_directory}/runtime_exhaustive_test.exe";
 all_modules = "iarray_model.cmo iarray_set.cmo sorted_iarray_stubs.c runtime_exhaustive_test.ml";
 ocamlc.byte;
 output = "${test_build_directory}/runtime_exhaustive_test.output";
 stdout = "${output}";
 stderr = "${output}";
 run;
 reference = "${test_source_directory}/runtime_exhaustive_test.reference";
 check-program-output;
 program = "${test_build_directory}/runtime_wrong_member.exe";
 all_modules = "iarray_model.cmo iarray_set.cmo sorted_iarray_stubs.c runtime_test.ml";
 flags = "-custom -I . -ccopt -Wall -ccopt -Wextra -ccopt -Werror -ccopt -DVOX_WRONG_MEMBER";
 ocamlc.byte;
 stdout = "${test_build_directory}/runtime_wrong_member.output";
 stderr = "${stdout}";
 exit_status = "2";
 run;
 program = "${test_build_directory}/runtime_wrong_insert.exe";
 flags = "-custom -I . -ccopt -Wall -ccopt -Wextra -ccopt -Werror -ccopt -DVOX_WRONG_INSERT";
 ocamlc.byte;
 stdout = "${test_build_directory}/runtime_wrong_insert.output";
 stderr = "${stdout}";
 run;
 program = "${test_build_directory}/exhaustive_wrong_member.exe";
 all_modules = "iarray_model.cmo iarray_set.cmo sorted_iarray_stubs.c runtime_exhaustive_test.ml";
 flags = "-custom -I . -ccopt -Wall -ccopt -Wextra -ccopt -Werror -ccopt -DVOX_WRONG_MEMBER";
 ocamlc.byte;
 stdout = "${test_build_directory}/exhaustive_wrong_member.output";
 stderr = "${stdout}";
 run;
 program = "${test_build_directory}/exhaustive_wrong_insert.exe";
 flags = "-custom -I . -ccopt -Wall -ccopt -Wextra -ccopt -Werror -ccopt -DVOX_WRONG_INSERT";
 ocamlc.byte;
 stdout = "${test_build_directory}/exhaustive_wrong_insert.output";
 stderr = "${stdout}";
 run;
*)
