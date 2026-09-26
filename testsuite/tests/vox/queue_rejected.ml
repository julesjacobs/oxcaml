(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "vox_sequence.mli functional_queue.mli";
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types -principal";
 module = "vox_sequence.mli";
 ocamlc.opt;
 module = "functional_queue.mli";
 ocamlc.opt;
 module = "queue_rejected.ml";
 ocamlc_opt_exit_status = "2";
 ocamlc.opt;
 check-ocamlc.opt-output;
*)

let () =
  let refine_ empty = Functional_queue.empty in
  let nonempty :
      {q : int Functional_queue.t |
        (Functional_queue.contents q === []) === false} =
    refine_ empty
  in
  let refine_ _result = Functional_queue.dequeue nonempty in
  ()
