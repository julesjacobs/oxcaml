(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml vox_lz4_model.ml";
 { native; }
*)

let () =
  let source : {i : int | i = 10} = Vox_lz4_model.source_position 10 1 1 in
  assert (source = 10)
