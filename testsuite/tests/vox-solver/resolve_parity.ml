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
   decoy — a directory named z3, a non-executable file named z3, an
   executable script named z3 — plus the empty and set shapes of
   VOX_TEST_Z3, and checks both decisions against the scenario's ground
   truth: WHICH command must be selected (computed from first principles —
   the env override, the PATH command when it is genuinely executable, or
   the pinned install when it exists, and never a decoy that cannot run),
   not merely whether something was found.  A
   selected command must also actually execute: a resolver that picks up a
   directory or a non-executable file agrees with the gate on availability
   (both fall through to the pinned install's existence) yet hands the
   driver a command that cannot run — availability parity alone cannot
   catch it.  Only agreement is printed: whether the pinned install exists
   is a property of the machine, so the decisions themselves are not
   stable output. *)

let pinned = "/j/office/app/z3/prod/4.8.5/install/bin/z3"

let gate () = Sys.command "/bin/sh has_z3.sh" = 0

(* The selected command runs with a trivial argument; a directory or a
   non-executable file fails here (126/127) where an executable answers. *)
let executable command =
  Sys.command (Filename.quote command ^ " -version > /dev/null 2>&1") = 0

(* For the mismatch diagnostic only. *)
let describe = function
  | None -> "none"
  | Some command ->
    if String.equal command pinned then "pinned"
    else if String.equal command "z3" then "path"
    else "command " ^ command

let scenario label ~expected =
  let g = gate () in
  let r = Vox_backend.resolve_z3 () in
  let ok =
    match r, expected with
    | None, None -> not g
    | Some command, Some expected_command ->
      g && String.equal command expected_command && executable command
    | None, Some _ | Some _, None -> false
  in
  if ok
  then Printf.printf "%s: agree\n" label
  else
    Printf.printf "%s: DISAGREE (gate %b, resolver %s, expected %s)\n" label
      g (describe r) (describe expected)

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
  let exec_decoy = Filename.concat decoys "exec" in
  Unix.mkdir decoys 0o755;
  Unix.mkdir dir_decoy 0o755;
  Unix.mkdir (Filename.concat dir_decoy "z3") 0o755;
  Unix.mkdir file_decoy 0o755;
  let oc = open_out (Filename.concat file_decoy "z3") in
  output_string oc "#!/bin/sh\n";
  close_out oc;
  (* An executable z3 on PATH is the one scenario where the correct
     selection IS the PATH command: a resolver that skips the [command -v]
     step entirely still agrees with the gate on every decoy scenario
     (both fall through to the pinned install), so only this scenario
     discriminates it — it would select the pinned install (or nothing)
     where "z3" is required. *)
  Unix.mkdir exec_decoy 0o755;
  let exec_z3 = Filename.concat exec_decoy "z3" in
  let oc = open_out exec_z3 in
  output_string oc "#!/bin/sh\n[ \"$1\" = -version ] && echo decoy-z3\nexit 0\n";
  close_out oc;
  Unix.chmod exec_z3 0o755;
  (* With no usable z3 on PATH, the one correct selection is the pinned
     install when it is executable, and nothing otherwise. *)
  let fallback =
    if Sys.command ("test -x " ^ Filename.quote pinned) = 0
    then Some pinned
    else None
  in
  with_env ["VOX_TEST_Z3", ""; "PATH", dir_decoy] (fun () ->
    scenario "directory named z3 on PATH" ~expected:fallback);
  with_env ["VOX_TEST_Z3", ""; "PATH", file_decoy] (fun () ->
    scenario "non-executable z3 on PATH" ~expected:fallback);
  with_env ["VOX_TEST_Z3", ""; "PATH", exec_decoy] (fun () ->
    scenario "executable z3 on PATH" ~expected:(Some "z3"));
  with_env ["VOX_TEST_Z3", ""; "PATH", decoys] (fun () ->
    scenario "no z3 on PATH" ~expected:fallback);
  with_env ["VOX_TEST_Z3", "/bin/true"] (fun () ->
    scenario "VOX_TEST_Z3 set" ~expected:(Some "/bin/true"))
