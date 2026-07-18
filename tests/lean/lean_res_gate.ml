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

(* Spawn lean with an in-process wall-clock cap. Returns (exit_ok, output). *)
let run_lean ~timeout ~src ~file : bool * string =
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

(* MECHANICAL NO-AUTOMATION GUARD (team-lead ruling 2026-07-17): the EMITTED
   per-certificate body must never contain a proof-search / decision-procedure tactic —
   omega, simp, grind, native_decide, linarith, etc. (omega is permitted ONLY in the
   once-proved prelude substrate, never per-cert). The emitter only ever produces `by
   decide`, `rfl`, and term-mode applications; this scan turns that invariant into
   enforced evidence. Returns [Some tok] if a banned token appears in [body]. *)
let banned_automation_token (body : string) : string option =
  let banned =
    [ "omega"
    ; "simp"
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
    ]
  in
  List.find_opt (fun tok -> find_sub body tok <> None) banned
;;

let cnt = ref 0
let cnt_verified = ref 0
let cnt_broken = ref 0
let cnt_unsupported = ref 0

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

let process_file ~prelude ~timeout ~logdir path : unit =
  let base = Filename.basename path in
  match read_file path with
  | exception _ -> ()
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
     | exception _ -> ()
     | parsed ->
       Session.set_datatypes s parsed.Parser.datatypes;
       Session.set_arrays s parsed.Parser.arrays;
       (match Session.assert_presolved s parsed.Parser.assertions with
        | exception _ -> ()
        | () ->
          (match Session.check_sat s with
           | exception _ -> ()
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
                let ok, out = run_lean ~timeout ~src:full ~file:f in
                let tamper_accepts label f_tamper =
                  match f_tamper body with
                  | None -> false (* nothing to tamper => treat as rejected (safe) *)
                  | Some tbody ->
                    let tf = Filename.concat logdir (name ^ "." ^ label ^ ".lean") in
                    fst (run_lean ~timeout ~src:(prelude ^ "\n" ^ tbody) ~file:tf)
                in
                let pivot_ok = tamper_accepts "pivot" tamper_pivot in
                let drop_ok = tamper_accepts "drop" drop_premise in
                let euf_ok = tamper_accepts "euf" tamper_euf in
                let tampered_ok = pivot_ok || drop_ok || euf_ok in
                if not ok
                then (
                  incr cnt_broken;
                  Printf.printf
                    "BROKEN       %s :: valid refutation REJECTED: %s\n%!"
                    base
                    (match
                       String.split_on_char '\n' out
                       |> List.find_opt (fun l -> String.length l > 0)
                     with
                     | Some l -> l
                     | None -> "?"))
                else if tampered_ok
                then (
                  incr cnt_broken;
                  Printf.printf
                    "BROKEN       %s :: TAMPERED refutation ACCEPTED (%s)\n%!"
                    base
                    (if pivot_ok
                     then "swapped-pivot"
                     else if drop_ok
                     then "dropped-premise"
                     else "broken-euf-chain"))
                else (
                  match Oxsmt_lean_export.Lean_export.check_axioms out with
                  | Error m ->
                    incr cnt_broken;
                    Printf.printf
                      "BROKEN       %s :: axiom-whitelist violation: %s\n%!"
                      base
                      m
                  | Ok () ->
                    incr cnt_verified;
                    Printf.printf "VERIFIED     %s\n%!" base)))))
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

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let timeout, args =
    match args with
    | "--timeout" :: t :: rest -> float_of_string t, rest
    | _ -> 60.0, args
  in
  let logdir =
    match Sys.getenv_opt "OXSMT_LEAN_LOGDIR" with
    | Some d -> d
    | None -> Filename.get_temp_dir_name ()
  in
  (try Unix.mkdir logdir 0o755 with
   | _ -> ());
  let prelude = prelude_text () in
  let files = List.concat_map expand_arg args in
  List.iter (fun p -> process_file ~prelude ~timeout ~logdir p) files;
  Printf.printf
    "\nlean_res_gate: unsat-solves=%d | VERIFIED=%d BROKEN=%d UNSUPPORTED=%d\n%!"
    !cnt
    !cnt_verified
    !cnt_broken
    !cnt_unsupported;
  if !cnt_broken > 0 then exit 1
;;
