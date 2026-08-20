(* TEST
 include ocamlcommon;
 include unix;
 hasunix;
 readonly_files = "has_z3.sh";
 bytecode;
*)

(* [Vox_backend.resolve_z3] and the test gate (has_z3.sh) must make the
   same decision in any environment: a gate that skips where the driver
   would run (or the reverse) would let a fixture's verdict come from
   nothing.  Each scenario rewrites PATH to a scratch directory holding a
   decoy — a directory named z3, a non-executable file named z3 — plus the
   empty and set shapes of VOX_TEST_Z3, and compares the two decisions.
   Only agreement is printed: whether the pinned install exists is a
   property of the machine, so the decisions themselves are not stable
   output. *)

let gate () = Sys.command "/bin/sh has_z3.sh" = 0

let resolver () = Option.is_some (Vox_backend.resolve_z3 ())

let scenario label =
  let g = gate () in
  let r = resolver () in
  if Bool.equal g r
  then Printf.printf "%s: agree\n" label
  else Printf.printf "%s: DISAGREE (gate %b, resolver %b)\n" label g r

(* [Unix.putenv] cannot unset; both readers treat an empty VOX_TEST_Z3 as
   unset, so restoring an absent variable as "" is faithful. *)
let with_env pairs f =
  let saved = List.map (fun (k, _) -> k, Sys.getenv_opt k) pairs in
  List.iter (fun (k, v) -> Unix.putenv k v) pairs;
  f ();
  List.iter
    (fun (k, v) -> Unix.putenv k (Option.value v ~default:""))
    saved

let () =
  let decoys = Filename.concat (Sys.getcwd ()) "decoys" in
  let dir_decoy = Filename.concat decoys "dir" in
  let file_decoy = Filename.concat decoys "file" in
  Unix.mkdir decoys 0o755;
  Unix.mkdir dir_decoy 0o755;
  Unix.mkdir (Filename.concat dir_decoy "z3") 0o755;
  Unix.mkdir file_decoy 0o755;
  let oc = open_out (Filename.concat file_decoy "z3") in
  output_string oc "#!/bin/sh\n";
  close_out oc;
  with_env ["VOX_TEST_Z3", ""; "PATH", dir_decoy] (fun () ->
    scenario "directory named z3 on PATH");
  with_env ["VOX_TEST_Z3", ""; "PATH", file_decoy] (fun () ->
    scenario "non-executable z3 on PATH");
  with_env ["VOX_TEST_Z3", ""; "PATH", decoys] (fun () ->
    scenario "no z3 on PATH");
  with_env ["VOX_TEST_Z3", "/bin/true"] (fun () ->
    scenario "VOX_TEST_Z3 set")
