(* TEST
 has-z3;
 flags = "-extension refinement_types -alert -do_not_spawn_domains";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml verified_atomic.mli verified_atomic.ml reference_lock.mli reference_lock.ml atomic_lock.ml";
 { bytecode; }
 { native; }
*)

open Reference_lock
module P = Ghost_pref
let () =
  let zero = 0 in
  let initial : {n : int | 0 <= n} = zero in
  let a = make initial in
  for _i = 1 to 10 do assert (try_increment a) done;
  let r = try_acquire a in
  assert r.P.value;
  if r.P.value then begin
    let t = r.P.state in
    let t : {t : int P.token | owned a (P.own t)} = t in
    let actual = read_owned a (borrow_ t) in
    assert (actual = 10);
    let failed = try_acquire a in
    assert (not failed.P.value);
    let _ = release a t in ()
  end;
  assert (try_increment a)
