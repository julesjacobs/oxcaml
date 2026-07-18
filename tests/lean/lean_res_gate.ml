(* UNSAT resolution-skeleton Lean gate (lean-proofs lane, Rung 3a). Drives real .smt2
   files through the shipped Session with the certificate recorder installed; for every
   solve that returns Unsat, emits ONE self-contained core-Lean-4 proof of [False] over
   {!res_prelude} (via {!Lean_res}) and runs the [lean] kernel:

   - the refutation must be ACCEPTED (exit 0) and depend only on [{propext, Quot.sound}];
   - a TAMPERED copy (one resolution pivot rewritten to an unused variable) must be
     REJECTED — the discrimination arm proving a broken resolution skeleton cannot pass.

   Honest numbers: a solve whose skeleton this rung cannot replay is UNSUPPORTED (a loud,
   counted gap), never a fake pass. Exit nonzero iff a valid refutation was rejected or a
   tampered one was accepted (either is a soundness break). *)

module Session = Oxsmt_interface.Session
module Parser = Oxsmt_smtlib_parser.Parser
module Recorder = Oxsmt_certificate.Recorder
module Checker = Oxsmt_certificate.Checker
module Lean_res = Oxsmt_lean_export.Lean_res

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
    Filename.concat
      (try Sys.getenv "HOME" with
       | Not_found -> "")
      ".dispatch/bin/lean"
;;

(* The emitted refutation is checked over the res + Farkas + bridge preludes (Rung 3b
   discharges LIA leaves through {!OxsmtBridge.leaf_sat}; the Farkas prelude backs it). *)
let prelude_text () =
  let read env dflt =
    let path =
      match Sys.getenv_opt env with
      | Some p -> p
      | None -> dflt
    in
    try read_file path with
    | _ -> failwith ("cannot read prelude at " ^ path ^ " (set " ^ env ^ ")")
  in
  String.concat
    "\n"
    [ read "OXSMT_RES_PRELUDE" "tests/lean/res_prelude.lean"
    ; read "OXSMT_FARKAS_PRELUDE" "tests/lean/farkas_prelude.lean"
    ; read "OXSMT_BRIDGE_PRELUDE" "tests/lean/bridge_prelude.lean"
    ]
;;

(* Outcome of a [lean] invocation (rider R1). A SEMANTIC rejection (kernel/elaboration
   error, [WEXITED n≠0] with diagnostic output) is distinguished from an OPERATIONAL
   failure (timeout or signal death), because only the former is valid evidence that a
   tampered/negative proof was actually REJECTED by the kernel — a timeout/crash on an
   expected-reject run must not be mistaken for successful discrimination. *)
type lean_run =
  | Accepted of string (* WEXITED 0 *)
  | Rejected of string (* WEXITED n≠0 with diagnostic output: a real kernel rejection *)
  | Operational of string (* timeout, signal death, or a nonzero exit with no output *)

(* Spawn lean with an in-process wall-clock cap. *)
let run_lean ~timeout ~src ~file : lean_run =
  let lean = lean_bin () in
  let out_file = file ^ ".out" in
  write_file file src;
  let out_fd =
    Unix.openfile out_file [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let pid = Unix.create_process lean [| lean; file |] devnull out_fd out_fd in
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

let find_sub ?(from = 0) (hay : string) (needle : string) : int option =
  let nl = String.length needle
  and hl = String.length hay in
  let rec loop i =
    if i + nl > hl
    then None
    else if String.sub hay i nl = needle
    then Some i
    else loop (i + 1)
  in
  loop from
;;

(* Tamper A (swapped pivot): rewrite the FIRST resolution pivot to an unused-looking var
   (999999). That resolution no longer cancels its pivot, so the derived clause stops
   reducing to [] (or a [sat_mono] subset [by decide] fails) and the kernel must REJECT.
   The emitted form is [(resolve <rho> N _ _ ...)]; skip the balanced [<rho>] to reach N. *)
let tamper_pivot (src : string) : string option =
  let hl = String.length src in
  match find_sub src "resolve " with
  | None -> None
  | Some i ->
    let p = ref (i + String.length "resolve ") in
    (* skip the rho argument: a balanced (...) group, else a bare token *)
    if !p < hl && src.[!p] = '('
    then (
      let depth = ref 0 in
      let continue = ref true in
      while !continue && !p < hl do
        if src.[!p] = '(' then incr depth else if src.[!p] = ')' then decr depth;
        incr p;
        if !depth = 0 then continue := false
      done)
    else
      while !p < hl && src.[!p] <> ' ' do
        incr p
      done;
    while !p < hl && src.[!p] = ' ' do
      incr p
    done;
    let j = !p in
    let k = ref j in
    while !k < hl && src.[!k] >= '0' && src.[!k] <= '9' do
      incr k
    done;
    if !k = j
    then None
    else Some (String.sub src 0 j ^ "999999" ^ String.sub src !k (hl - !k))
;;

(* Tamper B (dropped premise): replace the FIRST resolution [(resolve <rho> N _ _ A B)] in
   the proof body with just its first antecedent [A], dropping premise [B] and the pivot
   step. The derived clause is then wrong, so the refutation no longer reduces to [] (or a
   [sat_mono] subset [by decide] fails) and the kernel must REJECT. [None] if there is no
   resolution to drop. *)
let drop_premise (src : string) : string option =
  let hl = String.length src in
  (* index just past the ')' matching the '(' at [g0]. *)
  let scan_group g0 =
    let depth = ref 0
    and p = ref g0
    and stop = ref (-1) in
    while !stop < 0 && !p < hl do
      if src.[!p] = '(' then incr depth else if src.[!p] = ')' then decr depth;
      incr p;
      if !depth = 0 then stop := !p
    done;
    !stop
  in
  match find_sub src "(resolve " with
  | None -> None
  | Some g0 ->
    let g1 = scan_group g0 in
    if g1 < 0
    then None
    else (
      match find_sub ~from:g0 src "_ _ " with
      | Some m when m < g1 ->
        let a0 = ref (m + 4) in
        while !a0 < g1 && src.[!a0] = ' ' do
          incr a0
        done;
        let a1 =
          if !a0 < g1 && src.[!a0] = '('
          then scan_group !a0
          else (
            let p = ref !a0 in
            while !p < g1 && src.[!p] <> ' ' do
              incr p
            done;
            !p)
        in
        if a1 <= !a0
        then None
        else
          Some
            (String.sub src 0 g0
             ^ String.sub src !a0 (a1 - !a0)
             ^ String.sub src g1 (hl - g1))
      | _ -> None)
;;

(* Tamper C (broken EUF/DT theory chain): the emitter discharges an EUF/DT leaf with an
   explicit contradiction term over [he_i] equality facts — [absurd <congruence> hne_k]
   for EUF, or [Ind.noConfusion he_i] for a datatype constructor clash. Replace the FIRST
   equality fact [he_i] USED in that contradiction (the first [he_] after the [absurd] /
   [noConfusion] head) with [rfl]: the reconstructed equality now proves the wrong thing,
   so the contradiction no longer typechecks and the kernel must REJECT. [None] when the
   body has no EUF/DT discharge to break. *)
let tamper_euf (src : string) : string option =
  let anchor =
    match find_sub src "absurd ", find_sub src "noConfusion " with
    | Some a, Some b -> Some (min a b)
    | Some a, None -> Some a
    | None, Some b -> Some b
    | None, None -> None
  in
  match anchor with
  | None -> None
  | Some a ->
    (match find_sub ~from:a src "he_" with
     | None -> None
     | Some j ->
       let hl = String.length src in
       let k = ref (j + 3) in
       while !k < hl && src.[!k] >= '0' && src.[!k] <= '9' do
         incr k
       done;
       if !k = j + 3
       then None
       else Some (String.sub src 0 j ^ "rfl" ^ String.sub src !k (hl - !k)))
;;

(* Is [c] an identifier char (Lean idents also allow ?, ! and '; we treat those as part of
   a token so `exact?`/`decide!`/`simp_all` are matched whole and never split). *)
let ident_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '_'
  || c = '\''
  || c = '?'
  || c = '!'
;;

(* Read the token starting at [i] (a maximal run of ident chars). *)
let token_at (s : string) (i : int) : string =
  let hl = String.length s in
  let j = ref i in
  while !j < hl && ident_char s.[!j] do
    incr j
  done;
  String.sub s i (!j - i)
;;

(* MECHANICAL NO-AUTOMATION GUARD (team-lead ruling 2026-07-17), reworked per rider R3.
   The EMITTED per-certificate body must never contain a proof-search / decision-procedure
   tactic; the emitter only ever produces the top-level structural `by` block (have /
   exact / let, no automation) and nested ground `by decide` checks, `rfl`, and term-mode
   applications. Two complementary checks, both word-boundary aware so a constructor name
   containing a banned substring (e.g. `fn_simple` ⊃ `simp`) no longer false-positives and
   a distinct tactic (e.g. `exact?`, `trivial`) is no longer missed:

   1. ALLOWLIST on nested `by` blocks. Every `by` token whose block is written inline as
      `by <tactic>` (a space, not the top-level `:= by\n<newline>` opener) may only invoke
      `decide`. This closes any tactic — automation or otherwise — sneaking into a nested
      block, without the false-positive of a raw substring scan.
   2. Word-boundary BLOCKLIST (defense in depth) for automation tactics appearing as whole
      tokens ANYWHERE in the body, covering both the classic decision procedures and the
      false-negatives the old substring scan missed (`exact?`, `apply?`, `trivial`, …).

   Returns [Some reason] on the first violation. *)
let banned_automation_token (body : string) : string option =
  let blocked =
    [ "omega"
    ; "simp"
    ; "simp_all"
    ; "grind"
    ; "native_decide"
    ; "linarith"
    ; "nlinarith"
    ; "polyrith"
    ; "aesop"
    ; "norm_num"
    ; "tauto"
    ; "decide!"
    ; "bv_decide"
    ; "exact?"
    ; "apply?"
    ; "rfl?"
    ; "trivial"
    ; "hint"
    ; "omega?"
    ; "decide?"
    ]
  in
  let hl = String.length body in
  (* check 1: nested `by <tactic>` allowlist (only `by decide` permitted inline). *)
  let rec scan_by i =
    if i >= hl
    then None
    else if token_at body i = "by"
            && (i = 0 || not (ident_char body.[i - 1]))
            && i + 2 < hl
            && body.[i + 2] = ' ' (* an inline `by <tactic>`; the opener is `by\n` *)
    then (
      let k = ref (i + 3) in
      while !k < hl && body.[!k] = ' ' do
        incr k
      done;
      let tac = token_at body !k in
      if tac = "decide"
      then scan_by (i + 2)
      else Some (Printf.sprintf "nested `by %s` (only `by decide` is allowed)" tac))
    else scan_by (i + 1)
  in
  match scan_by 0 with
  | Some _ as r -> r
  | None ->
    (* check 2: word-boundary blocklist. *)
    let rec scan_block i =
      if i >= hl
      then None
      else if i = 0 || not (ident_char body.[i - 1])
      then (
        let tok = token_at body i in
        if tok <> "" && List.mem tok blocked
        then Some (Printf.sprintf "automation tactic token `%s`" tok)
        else scan_block (i + max 1 (String.length tok)))
      else scan_block (i + 1)
    in
    scan_block 0
;;

let cnt = ref 0
let cnt_verified = ref 0
let cnt_broken = ref 0
let cnt_unsupported = ref 0
let cnt_error = ref 0

(* Status of one tamper (discrimination) arm (rider R1). *)
type arm =
  | Absent (* the tamper produced nothing to corrupt on this proof *)
  | Sem_rejected
    (* the corrupted proof was REJECTED by the kernel: valid discrimination *)
  | Op_failed of
      string (* the corrupted run timed out / crashed: NOT valid discrimination *)
  | Accepted_bad (* the corrupted proof was ACCEPTED: a soundness break *)

let install_traces s rec_ =
  Session.install_cert_trace s (Some (Recorder.trace rec_));
  Session.install_leaf_certificate_trace
    s
    (Some
       { Oxsmt_interface.Cdclt.on_theory_atom =
           (fun ~var ~atom -> Recorder.record_theory_atom rec_ ~var ~atom)
       ; on_euf_leaf = (fun ~clause -> Recorder.record_euf_leaf rec_ ~clause)
       ; on_dt_distinctness =
           (fun ~registry ~clause ~left ~right ->
             Recorder.record_dt_distinctness rec_ ~registry ~clause ~left ~right)
       ; on_lia_conflict =
           (fun ~premise_lits ~multipliers ->
             Recorder.record_lia_conflict rec_ ~premise_lits ~multipliers)
       })
;;

(* Loud, counted skip for a phase that raised (rider R2: the pipeline exceptions used to
   be silently swallowed as [-> ()]; now every one is reported and makes the run fail). *)
let phase_error base phase msg =
  incr cnt_error;
  Printf.printf "ERROR        %s :: %s: %s\n%!" base phase msg
;;

let process_file ~prelude ~timeout ~logdir path : unit =
  let base = Filename.basename path in
  match read_file path with
  | exception e -> phase_error base "read" (Printexc.to_string e)
  | src ->
    let s = Session.create ~max_effort:200_000 () in
    let rec_ = Recorder.create () in
    install_traces s rec_;
    (match
       Parser.parse_into
         ~internal_mint:(Session.parse_minter s)
         (Session.env s)
         (Session.context s)
         src
     with
     | exception e -> phase_error base "parse" (Printexc.to_string e)
     | parsed ->
       Session.set_datatypes s parsed.Parser.datatypes;
       Session.set_arrays s parsed.Parser.arrays;
       (match Session.assert_presolved s parsed.Parser.assertions with
        | exception e -> phase_error base "assert" (Printexc.to_string e)
        | () ->
          (match Session.check_sat s with
           | exception e -> phase_error base "check_sat" (Printexc.to_string e)
           | Session.Sat | Session.Unknown -> ()
           | Session.Unsat ->
             incr cnt;
             let ev =
               Checker.of_recorder rec_ ~assumptions:(Session.cert_assumptions s)
             in
             let name =
               String.map
                 (fun c ->
                   if (c >= 'a' && c <= 'z')
                      || (c >= 'A' && c <= 'Z')
                      || (c >= '0' && c <= '9')
                   then c
                   else '_')
                 base
             in
             (match Lean_res.emit_refutation ev with
              | exception Lean_res.Gap g ->
                incr cnt_unsupported;
                Printf.printf "UNSUPPORTED  %s :: %s\n%!" base g
              | body when banned_automation_token body <> None ->
                incr cnt_broken;
                Printf.printf
                  "BROKEN       %s :: emitted per-cert automation tactic: %s\n%!"
                  base
                  (Option.get (banned_automation_token body))
              | body ->
                let full = prelude ^ "\n" ^ body in
                let f = Filename.concat logdir (name ^ ".lean") in
                let pos = run_lean ~timeout ~src:full ~file:f in
                (* classify each discrimination arm as absent / semantic-reject /
                   operational failure / accepted (rider R1). *)
                let classify label f_tamper : arm =
                  match f_tamper body with
                  | None -> Absent
                  | Some tbody ->
                    let tf = Filename.concat logdir (name ^ "." ^ label ^ ".lean") in
                    (match run_lean ~timeout ~src:(prelude ^ "\n" ^ tbody) ~file:tf with
                     | Accepted _ -> Accepted_bad
                     | Rejected _ -> Sem_rejected
                     | Operational why -> Op_failed why)
                in
                let pivot = classify "pivot" tamper_pivot in
                let drop = classify "drop" drop_premise in
                let euf = classify "euf" tamper_euf in
                let arm_str = function
                  | Absent -> "absent"
                  | Sem_rejected -> "semantic-reject"
                  | Op_failed why -> "OPERATIONAL(" ^ why ^ ")"
                  | Accepted_bad -> "ACCEPTED"
                in
                (* HARD discrimination arms: every resolution proof (all of them) carries
                   a swapped-pivot and a dropped-premise arm, and each MUST produce a
                   semantic rejection — an absent arm (regex miss) or an operational
                   failure is not evidence of discrimination (rider R1). The EUF/DT [euf]
                   arm is a bonus arm whose tamper is not always constructible (e.g. an
                   [rfl] congruence closing has no [he_] to break), so it is checked for
                   soundness (accept => break) and otherwise only reported, never
                   hard-required. *)
                let has_resolve = find_sub body "resolve " <> None in
                let hard_arms =
                  [ "swapped-pivot", pivot; "dropped-premise", drop ]
                  |> List.filter (fun _ -> has_resolve)
                in
                let all_arms =
                  [ "swapped-pivot", pivot
                  ; "dropped-premise", drop
                  ; "broken-euf-chain", euf
                  ]
                in
                (match List.find_opt (fun (_, a) -> a = Accepted_bad) all_arms with
                 | Some (label, _) ->
                   incr cnt_broken;
                   Printf.printf
                     "BROKEN       %s :: TAMPERED refutation ACCEPTED (%s)\n%!"
                     base
                     label
                 | None ->
                   (match pos with
                    | Rejected out ->
                      incr cnt_broken;
                      Printf.printf
                        "BROKEN       %s :: valid refutation REJECTED: %s\n%!"
                        base
                        (match
                           String.split_on_char '\n' out
                           |> List.find_opt (fun l -> String.length l > 0)
                         with
                         | Some l -> l
                         | None -> "?")
                    | Operational why ->
                      incr cnt_broken;
                      Printf.printf
                        "BROKEN       %s :: valid refutation did not check (%s)\n%!"
                        base
                        why
                    | Accepted out ->
                      (match
                         List.find_opt (fun (_, a) -> a <> Sem_rejected) hard_arms
                       with
                       | Some (label, a) ->
                         incr cnt_broken;
                         Printf.printf
                           "BROKEN       %s :: discrimination arm %s not a semantic \
                            rejection: %s\n\
                            %!"
                           base
                           label
                           (arm_str a)
                       | None ->
                         (match
                            Oxsmt_lean_export.Lean_export.check_axioms
                              ~theorem_name:"refute"
                              out
                          with
                          | Error m ->
                            incr cnt_broken;
                            Printf.printf
                              "BROKEN       %s :: axiom-whitelist violation: %s\n%!"
                              base
                              m
                          | Ok () ->
                            incr cnt_verified;
                            (* surface a bonus euf arm that could not demonstrate a
                               semantic rejection (non-fatal, loud). *)
                            let euf_note =
                              match euf with
                              | Sem_rejected | Absent -> ""
                              | Op_failed why -> " [euf arm OPERATIONAL: " ^ why ^ "]"
                              | Accepted_bad -> ""
                            in
                            if hard_arms = []
                            then
                              Printf.printf
                                "VERIFIED     %s (trivial: no resolution arm)%s\n%!"
                                base
                                euf_note
                            else Printf.printf "VERIFIED     %s%s\n%!" base euf_note))))))))
;;

let expand_arg arg =
  if Sys.file_exists arg && Sys.is_directory arg
  then
    Sys.readdir arg
    |> Array.to_list
    |> List.filter (fun e -> Filename.check_suffix e ".smt2")
    |> List.map (fun e -> Filename.concat arg e)
    |> List.sort compare
  else [ arg ]
;;

(* rider R2: a floor on VERIFIED so an all-skip / all-error / no-args run keyed on exit
   code cannot pass green. Set on the command line ([--min-verified N]) or via
   [OXSMT_LEAN_MIN_VERIFIED]; defaults to 1 (at least one proof must actually verify). *)
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
    | _ -> 60.0, args
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
    Printf.printf "lean_res_gate: no input files/dirs given (nothing to verify)\n%!";
    exit 1);
  let prelude = prelude_text () in
  let files = List.concat_map expand_arg args in
  List.iter (fun p -> process_file ~prelude ~timeout ~logdir p) files;
  Printf.printf
    "\nlean_res_gate: unsat-solves=%d | VERIFIED=%d BROKEN=%d UNSUPPORTED=%d ERROR=%d\n%!"
    !cnt
    !cnt_verified
    !cnt_broken
    !cnt_unsupported
    !cnt_error;
  if !cnt_error > 0
  then (
    Printf.printf "lean_res_gate: FAILED (%d pipeline error(s))\n%!" !cnt_error;
    exit 1);
  if !cnt_verified < min_verified
  then (
    Printf.printf
      "lean_res_gate: FAILED (VERIFIED=%d < required minimum %d)\n%!"
      !cnt_verified
      min_verified;
    exit 1);
  if !cnt_broken > 0 then exit 1
;;
