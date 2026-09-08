(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "vox_sequence.mli functional_queue.mli";
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types -principal";
 module = "vox_sequence.mli";
 ocamlc.byte;
 module = "functional_queue.mli";
 ocamlc.byte;
 module = "queue_rejected.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
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
