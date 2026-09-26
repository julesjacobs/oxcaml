(* TEST
 has-z3;
 multicore;
 flags = "-extension refinement_types -alert -do_not_spawn_domains";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml verified_atomic.mli verified_atomic.ml reference_lock.mli reference_lock.ml reference_lock_parallel.ml";
 { bytecode; }
*)
module L = Reference_lock
let () =
  let a = L.make 0 in
  let workers = Array.init 4 (fun _ -> Domain.Safe.spawn (fun () ->
    for i = 1 to 1000 do
      while not (L.try_increment a) do Domain.cpu_relax () done;
      if i mod 100 = 0 then Gc.full_major ()
    done)) in
  Array.iter Domain.join workers;
  let r = L.try_acquire a in
  assert r.value;
  if r.value then begin
    let actual : {n : int | n >= 0} = L.read_owned a (borrow_ r.state) in
    assert (actual = 4000);
    let failed = L.try_acquire a in
    assert (not failed.value);
    if not failed.value then begin
      let empty : {t : int Ghost_pref.token |
        Ghost_pref.own t === Ghost_pref.Heap.empty ()} = failed.state in
      let _ = empty in ()
    end;
    let _ = L.release a r.state in ()
  end
