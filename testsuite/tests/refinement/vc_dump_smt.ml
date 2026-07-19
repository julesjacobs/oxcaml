(* TEST
 readonly_files = "\
   vc_dump_smt_check.py \
   vc_dump_smt_regular.ml vc_dump_smt_nonregular.ml \
 ";
 setup-ocamlc.byte-build-env;
 flags = "-vox-dump-vc -vox-dump-vc-json vcs.json \
          -vox-dump-vc-json-smt -c";
 compiler_output = "vc_dump_smt.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/vc_dump_smt_check.py vcs.json";
 script;

 (* The introspection flag must not change acceptance or any verdict. *)
 module = "vc_dump_smt_regular.ml";
 flags = "-vox-dump-vc-json regular-default-off.json -c";
 compiler_output = "regular-default-off.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 flags = "-vox-dump-vc-json regular-default-on.json \
          -vox-dump-vc-json-smt -c";
 compiler_output = "regular-default-on.output";
 ocamlc.byte;

 module = "vc_dump_smt_nonregular.ml";
 flags = "-vox-dump-vc-json nonregular-default-off.json -c";
 compiler_output = "nonregular-default-off.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 flags = "-vox-dump-vc-json nonregular-default-on.json \
          -vox-dump-vc-json-smt -c";
 compiler_output = "nonregular-default-on.output";
 ocamlc.byte;

 module = "vc_dump_smt_regular.ml";
 flags = "-vox-backend z3 \
          -vox-smt-solver \
            '/j/office/app/z3/prod/4.8.5/install/bin/z3 -in' \
          -vox-dump-vc-json regular-z3-off.json -c";
 compiler_output = "regular-z3-off.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 flags = "-vox-backend z3 \
          -vox-smt-solver \
            '/j/office/app/z3/prod/4.8.5/install/bin/z3 -in' \
          -vox-dump-vc-json regular-z3-on.json \
          -vox-dump-vc-json-smt -c";
 compiler_output = "regular-z3-on.output";
 ocamlc.byte;

 module = "vc_dump_smt_nonregular.ml";
 flags = "-vox-backend z3 \
          -vox-smt-solver \
            '/j/office/app/z3/prod/4.8.5/install/bin/z3 -in' \
          -vox-dump-vc-json nonregular-z3-off.json -c";
 compiler_output = "nonregular-z3-off.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 flags = "-vox-backend z3 \
          -vox-smt-solver \
            '/j/office/app/z3/prod/4.8.5/install/bin/z3 -in' \
          -vox-dump-vc-json nonregular-z3-on.json \
          -vox-dump-vc-json-smt -c";
 compiler_output = "nonregular-z3-on.output";
 ocamlc.byte;

 module = "vc_dump_smt_regular.ml";
 flags = "-vox-backend oxsmt \
          -vox-dump-vc-json regular-oxsmt-off.json -c";
 compiler_output = "regular-oxsmt-off.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 flags = "-vox-backend oxsmt \
          -vox-dump-vc-json regular-oxsmt-on.json \
          -vox-dump-vc-json-smt -c";
 compiler_output = "regular-oxsmt-on.output";
 ocamlc.byte;

 module = "vc_dump_smt_nonregular.ml";
 flags = "-vox-backend oxsmt \
          -vox-dump-vc-json nonregular-oxsmt-off.json -c";
 compiler_output = "nonregular-oxsmt-off.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 flags = "-vox-backend oxsmt \
          -vox-dump-vc-json nonregular-oxsmt-on.json \
          -vox-dump-vc-json-smt -c";
 compiler_output = "nonregular-oxsmt-on.output";
 ocamlc.byte;

 script = "python3 ${test_source_directory}/vc_dump_smt_check.py acceptance";
 script;
*)

let positive (x : int{ _ > 0 }) = x
let annotation = (3 : int{ _ >= 3 })
let contract = positive 1
let branch y = if y > 0 then positive y else 0

