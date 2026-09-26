(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_lz4_model.ml lz4_model.ml";
 { native; }
*)

module M = Vox_lz4_model

let () =
  ghost_ (M.consume_match_sound [97] [97; 97; 97; 97; 97] 1 5);
  let block = M.Match ([97], 1, 14, M.Last [97; 97; 97; 97; 97]) in
  match M.decode [] block with
  | Some decoded -> assert (List.length decoded = 20)
  | None -> failwith "valid overlapping match rejected"
