(* chc_cli <file.smt2>: parse a HORN problem, solve it, print one verdict line.

   Output is the SMT-LIB HORN convention: [sat] (SAFE — an interpretation / inductive
   invariant exists), [unsat] (UNSAFE — a derivation of false exists), or [unknown]. With
   [-v] the detail/provenance line is printed to stderr. *)

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s
;;

(* Enable the multi-row integer-elimination gcd cut (task #128, [OXSMT_LIA_GCD_CUT]) in
   every oracle Session this process creates. The shipped LIA theory reads that flag ONCE
   at module initialization — before [main] runs and before any Session exists — so there
   is no per-Session knob and a runtime [putenv] cannot take effect in this process. The
   only freeze-respecting way to turn it on from inside the binary is to set it and
   re-exec self once (guarded so it happens at most once). The cut closes the raw-Session
   parity/GCD wall (e.g. the [2q = 2q'+1]-class obligation) whose branch-and-bound
   otherwise diverges — the oracle wall the modular template was blocked on. *)
let ensure_gcd_cut () =
  match Sys.getenv_opt "OXSMT_LIA_GCD_CUT" with
  | Some _ -> ()
  | None ->
    (try
       Unix.putenv "OXSMT_LIA_GCD_CUT" "1";
       Unix.execv Sys.executable_name Sys.argv
     with
     | Unix.Unix_error _ ->
       () (* re-exec unavailable: continue with the cut OFF (sound) *))
;;

let () =
  ensure_gcd_cut ();
  let verbose = ref false in
  let file = ref None in
  Array.iteri
    (fun i a ->
      if i > 0 then if String.equal a "-v" then verbose := true else file := Some a)
    Sys.argv;
  match !file with
  | None ->
    prerr_endline "usage: chc_cli [-v] <file.smt2>";
    exit 2
  | Some path ->
    let src = read_file path in
    (* Engine selection (env [OXSMT_CHC_ENGINE]):
       - [pdr] (or unset legacy): the proven backward search — single-predicate system to
         the transition-system engine (Chc_engine), multi-predicate to the linear PDR
         (Chc_pdr);
       - [cegis]: the syntax-guided (Houdini/CEGIS) synthesis engine (Chc_cegis) only;
       - [portfolio] (DEFAULT): run Chc_cegis first (it catches the
         sum/conjunctive-invariant SAFE class that backward PDR generalization diverges
         on) and, if it does not return a definite verdict, fall through to the PDR path.
         Every path is gated by the same independent re-verification firewall, so any
         combination is sound. *)
    (* [OXSMT_CHC_ALLPDR] (landed with the MBP-PDR stack): when set, route even a
       single-predicate system to the multi-predicate PDR instead of the transition-system
       engine. Folded into [run_pdr] so the PDR path honors it and the portfolio/cegis
       modes inherit it. *)
    let all_pdr =
      match Sys.getenv_opt "OXSMT_CHC_ALLPDR" with
      | Some ("0" | "false" | "") | None -> false
      | Some _ -> true
    in
    let pdr_verdict_str = function
      | Oxsmt_chc.Chc_pdr.Safe -> "sat"
      | Oxsmt_chc.Chc_pdr.Unsafe -> "unsat"
      | Oxsmt_chc.Chc_pdr.Unknown _ -> "unknown"
    in
    let run_pdr sys =
      if List.length sys.Oxsmt_chc.Chc_ast.preds <= 1 && not all_pdr
      then (
        let r = Oxsmt_chc.Chc_engine.solve sys in
        Oxsmt_chc.Chc_engine.verdict_to_smtlib r.Oxsmt_chc.Chc_engine.verdict, r.detail)
      else (
        let r = Oxsmt_chc.Chc_pdr.solve sys in
        pdr_verdict_str r.Oxsmt_chc.Chc_pdr.verdict, r.detail)
    in
    let run_cegis ?budget ?max_effort sys =
      let r = Oxsmt_chc.Chc_cegis.solve ?budget ?max_effort sys in
      pdr_verdict_str r.Oxsmt_chc.Chc_cegis.verdict, r.detail
    in
    let env_int name default =
      match Sys.getenv_opt name with
      | Some s ->
        (try int_of_string s with
         | _ -> default)
      | None -> default
    in
    (* In the portfolio, cegis is TIME-BOXED: its wins are cheap (all observed <0.1s), so
       a tight query/effort cap captures them while bailing fast enough to leave the PDR
       leg its full wall. Without this cap, cegis's slow-but-eventual successes (5-20s)
       would starve PDR of the 2s wall and net-REGRESS the portfolio (measured).
       Env-tunable. *)
    let pf_cegis_budget = env_int "OXSMT_CHC_CEGIS_BUDGET" 600 in
    let pf_cegis_effort = env_int "OXSMT_CHC_CEGIS_EFFORT" 6000 in
    let mode =
      match Sys.getenv_opt "OXSMT_CHC_ENGINE" with
      | Some "pdr" -> `Pdr
      | Some "cegis" -> `Cegis
      | Some "portfolio" | None -> `Portfolio
      | Some other -> failwith ("unknown OXSMT_CHC_ENGINE: " ^ other)
    in
    let smt, detail =
      match Oxsmt_chc.Chc_parse.parse src with
      | sys ->
        (match mode with
         | `Pdr -> run_pdr sys
         | `Cegis -> run_cegis ~budget:pf_cegis_budget ~max_effort:pf_cegis_effort sys
         | `Portfolio ->
           let smt, detail =
             run_cegis ~budget:pf_cegis_budget ~max_effort:pf_cegis_effort sys
           in
           if String.equal smt "unknown" then run_pdr sys else smt, detail)
      | exception Oxsmt_chc.Chc_parse.Unsupported m -> "unknown", "unsupported: " ^ m
      | exception Oxsmt_chc.Chc_parse.Malformed m -> "unknown", "malformed: " ^ m
    in
    print_endline smt;
    if !verbose
    then (
      prerr_endline ("; " ^ detail);
      let a, f, v, u = Oxsmt_chc.Chc_engine.interp_stats () in
      Printf.eprintf
        "; interp-stats attempts=%d farkas=%d verified=%d used=%d\n%!"
        a
        f
        v
        u)
;;
