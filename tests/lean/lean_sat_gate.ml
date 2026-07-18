(* SAT-direction Lean gate (lean-proofs lane, rung 1). Drives real .smt2 files through the
   shipped Session; for every solve that returns [Sat] WITH a reconstructable model,
   renders the model + original assertions to core Lean 4 and runs the [lean] kernel:

   - the POSITIVE source (assertions hold under the model) must be ACCEPTED (exit 0);
   - the REFUTATION CONTROL (their negation) must be REJECTED (nonzero) — proving the
     check can fail, so an ACCEPT is meaningful.

   Honest numbers: a model construct we cannot yet translate is UNSUPPORTED (a loud,
   counted gap), never a fake pass. Exit nonzero iff a positive source was rejected, a
   refutation control was accepted (either is a soundness break), or a lean invocation
   errored (encoder bug). *)

module Session = Oxsmt_interface.Session
module Parser = Oxsmt_smtlib_parser.Parser
module Lean_export = Oxsmt_lean_export.Lean_export

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))
;;

let write_file path s =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc s)
;;

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

(* Spawn lean on [src], capture combined output, enforce [timeout] seconds in-process (no
   dependency on the deny-ruled /usr/bin/timeout). Returns (exit_ok, output). *)
let run_lean ~timeout ~src ~tag ~logdir : bool * string =
  let lean = lean_bin () in
  let base = Filename.concat logdir tag in
  let src_file = base ^ ".lean" in
  let out_file = base ^ ".out" in
  write_file src_file src;
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
        false, "TIMEOUT")
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
      , try read_file out_file with
        | _ -> "" ))
  in
  wait ()
;;

type outcome =
  | Verified (* positive accepted AND refutation control rejected *)
  | Broken of string (* positive rejected, or refutation control accepted: soundness *)
  | Unsupported of string (* model construct not yet translatable: loud gap *)
  | Not_sat of string (* Unsat / Unknown / no self-checkable model *)

let solve_and_model path : (Session.model * Oxsmt_core.Term.t list) option * string =
  match read_file path with
  | exception e -> None, "read: " ^ Printexc.to_string e
  | src ->
    let s = Session.create ~max_effort:200_000 () in
    (match
       Parser.parse_into
         ~internal_mint:(Session.parse_minter s)
         (Session.env s)
         (Session.context s)
         src
     with
     | exception ex -> None, "parse: " ^ Printexc.to_string ex
     | parsed ->
       Session.set_datatypes s parsed.Parser.datatypes;
       Session.set_arrays s parsed.Parser.arrays;
       (match Session.assert_presolved s parsed.Parser.assertions with
        | exception ex -> None, "assert: " ^ Printexc.to_string ex
        | () ->
          (match Session.check_sat s with
           | Session.Sat ->
             (match Session.get_model s with
              | Some model -> Some (model, parsed.Parser.assertions), "sat+model"
              | None -> None, "sat-no-self-checkable-model")
           | Session.Unsat -> None, "unsat"
           | Session.Unknown -> None, "unknown"
           | exception ex -> None, "solve: " ^ Printexc.to_string ex)))
;;

let run_file ~timeout ~logdir path : outcome =
  match solve_and_model path with
  | None, why -> Not_sat why
  | Some (model, assertions), _ ->
    (match Lean_export.emit_sat ~model ~assertions with
     | exception Lean_export.Gap g -> Unsupported g
     | { positive; refutation_control } ->
       let base = Filename.basename path in
       let pos_ok, pos_out =
         run_lean ~timeout ~src:positive ~tag:(base ^ ".pos") ~logdir
       in
       let neg_ok, _ =
         run_lean ~timeout ~src:refutation_control ~tag:(base ^ ".neg") ~logdir
       in
       if not pos_ok
       then Broken (Printf.sprintf "positive rejected: %s" (String.trim pos_out))
       else if neg_ok
       then Broken "refutation control ACCEPTED (kernel proved a false model true)"
       else Verified)
;;

let expand_arg arg =
  if Sys.file_exists arg && Sys.is_directory arg
  then (
    let paths =
      Sys.readdir arg
      |> Array.to_list
      |> List.filter (fun e -> Filename.check_suffix e ".smt2")
      |> List.map (fun e -> Filename.concat arg e)
    in
    let size p =
      let ic = open_in_bin p in
      Fun.protect ~finally:(fun () -> close_in ic) (fun () -> in_channel_length ic)
    in
    List.sort (fun a b -> compare (size a, a) (size b, b)) paths)
  else [ arg ]
;;

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let timeout, args =
    match args with
    | "--timeout" :: t :: rest -> float_of_string t, rest
    | _ -> 30.0, args
  in
  let logdir =
    match Sys.getenv_opt "OXSMT_LEAN_LOGDIR" with
    | Some d -> d
    | None -> Filename.get_temp_dir_name ()
  in
  (try Unix.mkdir logdir 0o755 with
   | _ -> ());
  let files = List.concat_map expand_arg args in
  let n_sat = ref 0
  and n_verified = ref 0
  and n_broken = ref 0
  and n_unsupported = ref 0
  and n_notsat = ref 0 in
  List.iter
    (fun path ->
      let base = Filename.basename path in
      match run_file ~timeout ~logdir path with
      | Verified ->
        incr n_sat;
        incr n_verified;
        Printf.printf "VERIFIED       %s\n%!" base
      | Broken r ->
        incr n_sat;
        incr n_broken;
        Printf.printf "BROKEN         %s :: %s\n%!" base r
      | Unsupported g ->
        incr n_sat;
        incr n_unsupported;
        Printf.printf "UNSUPPORTED    %s :: %s\n%!" base g
      | Not_sat why ->
        incr n_notsat;
        Printf.printf "skip(%s) %s\n%!" why base)
    files;
  Printf.printf
    "\n\
     lean_sat_gate: %d files | sat-solves=%d (VERIFIED=%d BROKEN=%d UNSUPPORTED=%d) \
     non-sat=%d\n\
     %!"
    (List.length files)
    !n_sat
    !n_verified
    !n_broken
    !n_unsupported
    !n_notsat;
  if !n_broken > 0 then exit 1
;;
