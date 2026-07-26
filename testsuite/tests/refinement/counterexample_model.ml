(* TEST
 readonly_files = "\
   counterexample_model_refuted.ml \
   counterexample_model_proved.ml \
   counterexample_model_solver.sh \
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

 (* A reply is only an assignment if the exchange that produced it was
    clean.  These five use a controlled stand-in for a solver: the verdict is
    the same every time, and only the shape of the model reply differs. *)

 module = "counterexample_model_refuted.ml";
 flags = "-vox-backend z3 \
          -vox-smt-solver 'sh counterexample_model_solver.sh clean' \
          -vox-dump-vc-json controlled-clean.json -c";
 compiler_output = "controlled-clean.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "counterexample_model_refuted.ml";
 flags = "-vox-backend z3 \
          -vox-smt-solver 'sh counterexample_model_solver.sh banner' \
          -vox-dump-vc-json controlled-banner.json -c";
 compiler_output = "controlled-banner.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "counterexample_model_refuted.ml";
 flags = "-vox-backend z3 \
          -vox-smt-solver 'sh counterexample_model_solver.sh error_after' \
          -vox-dump-vc-json controlled-error.json -c";
 compiler_output = "controlled-error.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "counterexample_model_refuted.ml";
 flags = "-vox-backend z3 \
          -vox-smt-solver 'sh counterexample_model_solver.sh nonzero' \
          -vox-dump-vc-json controlled-nonzero.json -c";
 compiler_output = "controlled-nonzero.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "counterexample_model_refuted.ml";
 flags = "-vox-backend z3 \
          -vox-smt-solver 'sh counterexample_model_solver.sh contradictory' \
          -vox-dump-vc-json controlled-contradictory.json -c";
 compiler_output = "controlled-contradictory.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "counterexample_model_refuted.ml";
 flags = "-vox-backend z3 \
          -vox-smt-solver 'sh counterexample_model_solver.sh bracketed_banner' \
          -vox-dump-vc-json controlled-brackets.json -c";
 compiler_output = "controlled-brackets.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "counterexample_model_refuted.ml";
 flags = "-vox-backend z3 \
          -vox-smt-solver 'sh counterexample_model_solver.sh empty_model' \
          -vox-dump-vc-json controlled-empty.json -c";
 compiler_output = "controlled-empty.output";
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
