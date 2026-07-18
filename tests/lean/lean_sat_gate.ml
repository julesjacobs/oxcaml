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

(* Outcome of a [lean] invocation (rider R1): a SEMANTIC rejection (kernel/elaboration
   error) is distinguished from an OPERATIONAL failure (timeout / signal death), so a
   negative-control or wrong-model run that times out is never miscounted as the kernel
   having rejected it. *)
type lean_run =
  | Accepted of string (* WEXITED 0 *)
  | Rejected of string (* WEXITED n≠0 with diagnostic output: a real kernel rejection *)
  | Operational of string (* timeout, signal death, or a nonzero exit with no output *)

(* Spawn lean on [src], capture combined output, enforce [timeout] seconds in-process (no
   dependency on the deny-ruled /usr/bin/timeout). *)
let run_lean ~timeout ~src ~tag ~logdir : lean_run =
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
        Operational "TIMEOUT")
      else (
        Unix.sleepf 0.02;
        wait ())
    else (
      let out =
        try read_file out_file with
        | _ -> ""
      in
      match status with
      | Unix.WEXITED 0 -> Accepted out
      | Unix.WEXITED _ when String.trim out <> "" -> Rejected out
      | Unix.WEXITED n -> Operational (Printf.sprintf "exit %d, no diagnostic output" n)
      | Unix.WSIGNALED n -> Operational (Printf.sprintf "killed by signal %d" n)
      | Unix.WSTOPPED n -> Operational (Printf.sprintf "stopped by signal %d" n))
  in
  wait ()
;;

type outcome =
  | Verified (* positive accepted AND refutation control semantically rejected *)
  | Broken of string (* positive rejected, or refutation control accepted: soundness *)
  | Unsupported of string (* model construct not yet translatable: loud gap *)
  | Not_sat of string (* Unsat / Unknown / no self-checkable model *)
  | Errored of
      string (* a pipeline phase raised (rider R2: loud, counted, fails the run) *)

(* rider R2: a raised pipeline phase is reported as [`Errored] (loud + fails the run),
   kept distinct from a legitimate non-sat verdict ([`Nonsat]). *)
let solve_and_model path
  : [ `Solved of Session.model * Oxsmt_core.Term.t list
    | `Nonsat of string
    | `Errored of string
    ]
  =
  match read_file path with
  | exception e -> `Errored ("read: " ^ Printexc.to_string e)
  | src ->
    let s = Session.create ~max_effort:200_000 () in
    (match
       Parser.parse_into
         ~internal_mint:(Session.parse_minter s)
         (Session.env s)
         (Session.context s)
         src
     with
     | exception ex -> `Errored ("parse: " ^ Printexc.to_string ex)
     | parsed ->
       Session.set_datatypes s parsed.Parser.datatypes;
       Session.set_arrays s parsed.Parser.arrays;
       (match Session.assert_presolved s parsed.Parser.assertions with
        | exception ex -> `Errored ("assert: " ^ Printexc.to_string ex)
        | () ->
          (match Session.check_sat s with
           | Session.Sat ->
             (match Session.get_model s with
              | Some model -> `Solved (model, parsed.Parser.assertions)
              | None -> `Nonsat "sat-no-self-checkable-model")
           | Session.Unsat -> `Nonsat "unsat"
           | Session.Unknown -> `Nonsat "unknown"
           | exception ex -> `Errored ("solve: " ^ Printexc.to_string ex))))
;;

(* Corrupt a model by tweaking the FIRST constant binding by one minimal step (flip a
   Bool, +1 an Int, +1 an element index). Used for the wrong-model discrimination arm. *)
let corrupt_model ((cards, bindings) : Session.model) : Session.model =
  let done_ = ref false in
  let bindings =
    List.map
      (fun (b : Session.model_binding) ->
        match b with
        | Session.Const (n, v) when not !done_ ->
          let v' =
            match v with
            | Session.VBool x -> Some (Session.VBool (not x))
            | Session.VInt x ->
              Some (Session.VInt (Oxsmt_core.Bigint.add x Oxsmt_core.Bigint.one))
            | Session.VUninterp i -> Some (Session.VUninterp (i + 1))
            | Session.VReal _ -> None
          in
          (match v' with
           | Some v' ->
             done_ := true;
             Session.Const (n, v')
           | None -> b)
        | _ -> b)
      bindings
  in
  cards, bindings
;;

let corrupt_caught = ref 0
let corrupt_total = ref 0

let run_file ~timeout ~logdir path : outcome =
  match solve_and_model path with
  | `Errored why -> Errored why
  | `Nonsat why -> Not_sat why
  | `Solved (model, assertions) ->
    (match Lean_export.emit_sat ~model ~assertions with
     | exception Lean_export.Gap g -> Unsupported g
     | { positive; refutation_control } ->
       let base = Filename.basename path in
       let pos = run_lean ~timeout ~src:positive ~tag:(base ^ ".pos") ~logdir in
       let neg = run_lean ~timeout ~src:refutation_control ~tag:(base ^ ".neg") ~logdir in
       (match pos with
        | Rejected out ->
          Broken (Printf.sprintf "positive rejected: %s" (String.trim out))
        | Operational why -> Broken (Printf.sprintf "positive did not check (%s)" why)
        | Accepted pos_out ->
          (* rider R1: the refutation control must be rejected SEMANTICALLY. An accept is
             a soundness break; an operational failure (timeout/crash) is not valid
             evidence the control was rejected, so we fail closed rather than credit it. *)
          (match neg with
           | Accepted _ ->
             Broken "refutation control ACCEPTED (kernel proved a false model true)"
           | Operational why ->
             Broken
               (Printf.sprintf
                  "refutation control did not semantically reject (%s); no \
                   discrimination evidence"
                  why)
           | Rejected _ ->
             (match Lean_export.check_axioms ~theorem_name:"oxsmt_sat" pos_out with
              | Error m -> Broken ("axiom-whitelist violation: " ^ m)
              | Ok () ->
                (* wrong-model discrimination: tweak one value; the corrupted positive
                   should be SEMANTICALLY rejected. A tweak that still satisfies (another
                   valid model) is not a soundness break, so this is a REPORTED metric
                   (caught/total), not a hard gate; the guaranteed per-file discrimination
                   is the refutation control above. An operational failure is not counted
                   as caught. *)
                (match Lean_export.emit_sat ~model:(corrupt_model model) ~assertions with
                 | exception Lean_export.Gap _ -> ()
                 | { positive = cpos; _ } ->
                   incr corrupt_total;
                   (match
                      run_lean ~timeout ~src:cpos ~tag:(base ^ ".corrupt") ~logdir
                    with
                    | Rejected _ -> incr corrupt_caught
                    | Accepted _ | Operational _ -> ()));
                Verified))))
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

(* rider R2: floor on VERIFIED so an all-skip / all-error / no-args run keyed on exit code
   cannot pass green. [--min-verified N] or [OXSMT_LEAN_MIN_VERIFIED]; default 1. *)
let min_verified_default () =
  match Sys.getenv_opt "OXSMT_LEAN_MIN_VERIFIED" with
  | Some s ->
    (try int_of_string (String.trim s) with
     | _ -> 1)
  | None -> 1
;;

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let timeout, args =
    match args with
    | "--timeout" :: t :: rest -> float_of_string t, rest
    | _ -> 30.0, args
  in
  let min_verified, args =
    match args with
    | "--min-verified" :: n :: rest -> int_of_string n, rest
    | _ -> min_verified_default (), args
  in
  let logdir =
    match Sys.getenv_opt "OXSMT_LEAN_LOGDIR" with
    | Some d -> d
    | None -> Filename.get_temp_dir_name ()
  in
  (try Unix.mkdir logdir 0o755 with
   | _ -> ());
  if args = []
  then (
    Printf.printf "lean_sat_gate: no input files/dirs given (nothing to verify)\n%!";
    exit 1);
  let files = List.concat_map expand_arg args in
  let n_sat = ref 0
  and n_verified = ref 0
  and n_broken = ref 0
  and n_unsupported = ref 0
  and n_notsat = ref 0
  and n_error = ref 0 in
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
        Printf.printf "skip(%s) %s\n%!" why base
      | Errored why ->
        incr n_error;
        Printf.printf "ERROR          %s :: %s\n%!" base why)
    files;
  Printf.printf
    "\n\
     lean_sat_gate: %d files | sat-solves=%d (VERIFIED=%d BROKEN=%d UNSUPPORTED=%d) \
     non-sat=%d ERROR=%d | wrong-model-rejected=%d/%d\n\
     %!"
    (List.length files)
    !n_sat
    !n_verified
    !n_broken
    !n_unsupported
    !n_notsat
    !n_error
    !corrupt_caught
    !corrupt_total;
  if !n_error > 0
  then (
    Printf.printf "lean_sat_gate: FAILED (%d pipeline error(s))\n%!" !n_error;
    exit 1);
  if !n_verified < min_verified
  then (
    Printf.printf
      "lean_sat_gate: FAILED (VERIFIED=%d < required minimum %d)\n%!"
      !n_verified
      min_verified;
    exit 1);
  if !n_broken > 0 then exit 1
;;
