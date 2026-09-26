(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml verified_atomic.mli verified_atomic.ml unique_cell.mli unique_cell.ml one_shot.mli one_shot.ml one_shot_public_client.ml";
 { bytecode; }
*)
let roundtrip : (n : int) -> {r : int | r = n} = fun n ->
  let (tx, rx : {v : int | v = n} One_shot.send *
      {v : int | v = n} One_shot.recv) = One_shot.create () in
  One_shot.send tx n;
  One_shot.recv rx

let () = assert (roundtrip 42 = 42)
