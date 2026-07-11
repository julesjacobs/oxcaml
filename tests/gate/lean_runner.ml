(* Invoke the Lean oracle on a generated source file, with a wall-clock cap enforced
   in-process (Unix create_process + polled waitpid + kill). We do not depend on the
   /usr/bin/timeout binary.

   Classification (see NOTES.md):
   - exit 0 -> Proved
   - killed by timeout -> Timed_out
   - a known "tactic gave up" msg -> Tactic_failed (expected negative)
   - anything else on failure -> Elab_error (encoder bug: loud) *)

type result =
  | Proved
  | Tactic_failed of string
  | Elab_error of string
  | Timed_out

let lean_bin () =
  match Sys.getenv_opt "OXSMT_LEAN" with
  | Some p -> p
  | None ->
    let home =
      try Sys.getenv "HOME" with
      | Not_found -> ""
    in
    Filename.concat home ".dispatch/bin/lean"
;;

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))
;;

let write_file path s =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc s)
;;

let substring_mem ~needle haystack =
  let nl = String.length needle
  and hl = String.length haystack in
  if nl = 0
  then true
  else (
    let rec loop i =
      if i + nl > hl
      then false
      else if String.sub haystack i nl = needle
      then true
      else loop (i + 1)
    in
    loop 0)
;;

let tactic_gaveup_markers =
  [ "`grind` failed"
  ; "tactic 'grind' failed"
  ; "tactic 'decide' failed"
  ; "tactic 'native_decide' failed"
  ; "that the proposition"
    (* Lean 4.31: `decide` proved / `native_decide` evaluated the goal false *)
  ; "failed to reduce to true"
  ; "of the given hypotheses" (* grind variant *)
  ; "failed to synthesize" (* Decidable synth on sat goal: model inadequate *)
  ; "maximum recursion depth"
  ]
;;

(* Spawn lean on [src_file], capture combined output to [out_file], enforce [timeout]
   seconds. Returns (exited_ok, output_string, timed_out). *)
let run_process ~lean ~timeout ~src_file ~out_file : bool * string * bool =
  let out_fd =
    Unix.openfile out_file [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let pid = Unix.create_process lean [| lean; src_file |] devnull out_fd out_fd in
  Unix.close out_fd;
  Unix.close devnull;
  let start = Unix.gettimeofday () in
  let rec wait () =
    let p, status = Unix.waitpid [ Unix.WNOHANG ] pid in
    if p = 0
    then
      if Unix.gettimeofday () -. start > timeout
      then (
        (try Unix.kill pid Sys.sigkill with
         | _ -> ());
        (try ignore (Unix.waitpid [] pid) with
         | _ -> ());
        ( false
        , (try read_file out_file with
           | _ -> "")
        , true ))
      else (
        Unix.sleepf 0.02;
        wait ())
    else (
      let ok =
        match status with
        | Unix.WEXITED 0 -> true
        | _ -> false
      in
      ( ok
      , (try read_file out_file with
         | _ -> "")
      , false ))
  in
  wait ()
;;

(* The Lean toolchain version string, folded into the cache key so a toolchain change
   invalidates prior certifications. *)
let version () =
  let lean = lean_bin () in
  let tmp = Filename.temp_file "oxsmt-leanver" ".txt" in
  (try
     let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644 in
     let dn = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
     let pid = Unix.create_process lean [| lean; "--version" |] dn fd fd in
     Unix.close fd;
     Unix.close dn;
     ignore (Unix.waitpid [] pid)
   with
   | _ -> ());
  let v =
    try String.trim (read_file tmp) with
    | _ -> "unknown"
  in
  (try Sys.remove tmp with
   | _ -> ());
  v
;;

(* [tag] names the run for the log file (e.g. "unsat", "sat", "refute"). *)
let run ~logdir ~timeout ~tag ~src : result =
  let lean = lean_bin () in
  let base = Filename.concat logdir tag in
  let src_file = base ^ ".lean" in
  let out_file = base ^ ".out" in
  write_file src_file src;
  let ok, output, timed_out = run_process ~lean ~timeout ~src_file ~out_file in
  if timed_out
  then Timed_out
  else if ok
  then Proved
  else (
    let snippet =
      (* first error line, for the digest *)
      match
        String.split_on_char '\n' output |> List.find_opt (substring_mem ~needle:"error")
      with
      | Some l -> String.trim l
      | None -> "lean failed (see log)"
    in
    if List.exists (fun m -> substring_mem ~needle:m output) tactic_gaveup_markers
    then Tactic_failed snippet
    else Elab_error snippet)
;;
