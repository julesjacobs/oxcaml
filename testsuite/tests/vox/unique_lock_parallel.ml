(* TEST
 has-z3;
 multicore;
 flags = "-extension refinement_types -alert -do_not_spawn_domains";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml verified_atomic.mli verified_atomic.ml unique_cell.mli unique_cell.ml unique_lock.mli unique_lock.ml unique_lock_demo.ml unique_lock_parallel.ml";
 { bytecode; }
*)

open Unique_lock_demo

let () =
  let initial = 0 in
  let a = L.make initial in
  let workers = Array.init 4 (fun _ -> Domain.Safe.spawn (fun () ->
    for i = 1 to 1000 do
      while not (try_increment a) do Domain.cpu_relax () done;
      if i mod 100 = 0 then Gc.full_major ()
    done)) in
  Array.iter Domain.join workers;
  assert (take_final a = 4000)
