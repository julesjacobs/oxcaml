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
    (* Dispatch: a single-predicate system goes to the proven transition-system engine
       (Chc_engine); multiple predicates go to the multi-predicate linear PDR (Chc_pdr).
       Both are guarded by independent re-verification, so either path is sound. *)
    let smt, detail =
      match Oxsmt_chc.Chc_parse.parse src with
      | sys ->
        if List.length sys.Oxsmt_chc.Chc_ast.preds <= 1
        then (
          let r = Oxsmt_chc.Chc_engine.solve sys in
          Oxsmt_chc.Chc_engine.verdict_to_smtlib r.Oxsmt_chc.Chc_engine.verdict, r.detail)
        else (
          let r = Oxsmt_chc.Chc_pdr.solve sys in
          ( (match r.Oxsmt_chc.Chc_pdr.verdict with
             | Oxsmt_chc.Chc_pdr.Safe -> "sat"
             | Oxsmt_chc.Chc_pdr.Unsafe -> "unsat"
             | Oxsmt_chc.Chc_pdr.Unknown _ -> "unknown")
          , r.detail ))
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
