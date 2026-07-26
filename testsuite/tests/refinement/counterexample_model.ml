(* TEST
 readonly_files = "\
   counterexample_model_refuted.ml \
   counterexample_model_proved.ml \
   counterexample_model_check.py \
 ";
 setup-ocamlc.byte-build-env;

 module = "counterexample_model_refuted.ml";
 flags = "-vox-backend z3 -vox-smt-solver 'z3 -in' \
          -vox-dump-vc-json z3-refuted.json -c";
 compiler_output = "z3-refuted.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "counterexample_model_proved.ml";
 flags = "-vox-backend z3 -vox-smt-solver 'z3 -in' \
          -vox-dump-vc-json z3-proved.json -c";
 compiler_output = "z3-proved.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;

 module = "counterexample_model_refuted.ml";
 flags = "-vox-backend oxsmt -vox-dump-vc-json oxsmt-refuted.json -c";
 compiler_output = "oxsmt-refuted.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 script = "python3 ${test_source_directory}/counterexample_model_check.py \
           z3-refuted.json z3-proved.json oxsmt-refuted.json";
 script;
*)

(* An obligation the solver refutes has a witness to its failure, and the
   editor has a place to show it.  The witness was never asked for: the prove
   query returned satisfiable and its assignment was discarded, so every
   refuted obligation arrived with a null counterexample.  It is now
   requested once, after the verdict is settled, from the external solver
   path only. *)
