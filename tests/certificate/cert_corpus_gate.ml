(* Cert step-2 CORPUS GATE (ADR-0013 §4.1 step-1 acceptance, checker side). Drives real
   .smt2 files through the shipped Session with a recorder installed on the inner SAT
   core, and for every solve that returns Unsat runs {!Oxsmt_certificate.Checker.check}
   over the recorded stream. Reports the fraction of certs that check VALID and the
   per-failure reason — honest numbers (a checker that finds an emission/pipeline gap is a
   SUCCESS, not a failure to hide).

   The Session Unsat exit depends on the base-frame encoding (session.ml). Under the
   default base-l0 encoding (OXSMT_BASE_L0 default-ON) the base frame is forced TRUE by a
   permanent level-0 unit, so a level-0 refutation concludes E2 (a Boolean level-0
   conflict) or the base-l0 empty-core E3 (a theory level-0 conflict, base_l0_cert_mode),
   while deeper refutations and any pushed-frame selectors give the E3 failed-assumption
   exit. Under the opt-out (OXSMT_BASE_L0=0) the base frame is selector-assumed, so
   Session Unsat is always the E3 failed-assumption / selector exit. E1/E4 are exercised
   by the raw-Sat fixture suite (checker_test) instead. The checker realizes the §1.0
   selector strip by seeding the active selector assumptions true.

   Usage: cert_corpus_gate FILE.smt2 ... (each a single-check-sat, push/pop-free batch)
   Exit nonzero iff a file that solved Unsat produced a NON-VALID cert (so the gate can be
   wired as a regression once the VALID fraction is 100% on a curated sample); the summary
   is always printed. *)

module Session = Oxsmt_interface.Session
module Parser = Oxsmt_smtlib_parser.Parser
module Recorder = Oxsmt_certificate.Recorder
module Checker = Oxsmt_certificate.Checker

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))
;;

type outcome =
  | Cert_valid
  | Cert_invalid of string
  | Cert_unsupported of string
  | Not_unsat of string (* Sat / Unknown / parse error — no cert to check *)
  | Error of string

(* Solve one file traced; if Unsat, check its cert. [recheck] re-solves the SAME session a
   second time (repeat-solve terminal re-emission) and checks that cert too. *)
let run_file path : outcome * outcome option =
  match read_file path with
  | exception e -> Error (Printexc.to_string e), None
  | src ->
    (* Bounded effort: the minimization-off traced solve is a weaker solver (ADR-0013
       §1.4(b)), so a budget-marginal instance can hit a search cliff. A finite cap turns
       that into a clean [Unknown] (booked non-unsat) instead of a hang — never a
       soundness signal, re-runnable at a larger cap. *)
    let s = Session.create ~max_effort:200_000 () in
    let rec_ = Recorder.create () in
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
         });
    Session.install_presolve_certificate_trace
      s
      (Some
         { Session.on_equality_elimination =
             (fun ~context ~original ~reduced ~definitions ->
               Recorder.record_equality_elimination
                 rec_
                 ~context
                 ~original
                 ~reduced
                 ~definitions:
                   (List.map
                      (fun (definition : Session.certificate_presolve_definition) ->
                         { Recorder.name = definition.name
                         ; sort = definition.sort
                         ; value = definition.value
                         })
                      definitions))
         ; on_clausify_begin =
             (fun ~rewrite_id ~source ~preprocessed ->
               Recorder.begin_clausify rec_ ~statement_id:rewrite_id ~source ~preprocessed)
         ; on_clausify_bindings =
             (fun ~selector ~bindings ->
               Recorder.record_clausify_bindings rec_ ~selector ~bindings)
         ; on_clausify_end = (fun () -> Recorder.end_clausify rec_)
         });
    (match
       Parser.parse_into
         ~internal_mint:(Session.parse_minter s)
         (Session.env s)
         (Session.context s)
         src
     with
     | exception ex -> Error ("parse: " ^ Printexc.to_string ex), None
     | parsed ->
       (* install datatype shapes before asserting, so a QF_DT/QF_UFDT file runs the DT
          theory on the cert path too (else it degrades to unknown here while the product
          CLI decides it — a latent driver divergence). Empty on a non-datatype file. *)
       Session.set_datatypes s parsed.Parser.datatypes;
       (* likewise the arrays registry, so an array file's select/store atoms classify and
          its unsat conflicts emit theory clauses that replay as theory leaves. *)
       Session.set_arrays s parsed.Parser.arrays;
       (match Session.assert_presolved s parsed.Parser.assertions with
        | exception ex -> Error ("assert: " ^ Printexc.to_string ex), None
        | () ->
          let check_after () =
            let ev = Checker.of_recorder rec_ ~assumptions:(Session.cert_assumptions s) in
            match Checker.check_assertions ~original:parsed.Parser.assertions ev with
            (* skeleton-valid, theory leaves trusted this tranche (MED-1); plain [Valid]
               is reserved for the leaf-checker tranche and treated identically here. *)
            | Checker.Valid_modulo_unchecked_steps | Checker.Valid -> Cert_valid
            | Checker.Invalid r -> Cert_invalid r
            | Checker.Unsupported f -> Cert_unsupported f
          in
          (match Session.check_sat s with
           | Session.Unsat ->
             let first = check_after () in
             (* repeat-solve on the now-unsat core must re-emit a checkable conclusion *)
             let second =
               match Session.check_sat s with
               | Session.Unsat -> Some (check_after ())
               | _ -> Some (Not_unsat "re-solve not unsat")
             in
             first, second
           | Session.Sat -> Not_unsat "sat", None
           | Session.Unknown -> Not_unsat "unknown", None
           | exception ex -> Error ("solve: " ^ Printexc.to_string ex), None)))
;;

(* Expand each argv entry: a directory contributes its sorted *.smt2 children; a file is
   taken as-is. Avoids shell globbing (which the sandbox tends to deny on long lines) and
   makes the sample deterministic. *)
let expand_arg arg =
  if Sys.file_exists arg && Sys.is_directory arg
  then (
    let paths =
      Sys.readdir arg
      |> Array.to_list
      |> List.filter (fun e -> Filename.check_suffix e ".smt2")
      |> List.map (fun e -> Filename.concat arg e)
    in
    (* smallest-first: keeps the sample fast and deterministic (a lexicographic sort would
       pull in eq_diamond100 before eq_diamond2, and the minimization-off traced solve
       makes the big instances slow). *)
    let size p =
      let ic = open_in_bin p in
      Fun.protect ~finally:(fun () -> close_in ic) (fun () -> in_channel_length ic)
    in
    List.sort (fun a b -> compare (size a, a) (size b, b)) paths)
  else [ arg ]
;;

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  (* optional leading [--limit N] caps how many files are taken (after expansion). *)
  let limit, args =
    match args with
    | "--limit" :: n :: rest -> Some (int_of_string n), rest
    | _ -> None, args
  in
  let files = List.concat_map expand_arg args in
  let files =
    match limit with
    | Some n -> List.filteri (fun i _ -> i < n) files
    | None -> files
  in
  let n_unsat = ref 0
  and n_valid = ref 0
  and n_invalid = ref 0
  and n_unsupported = ref 0
  and n_nonunsat = ref 0
  and n_error = ref 0
  and reemit_ok = ref 0
  and reemit_bad = ref 0 in
  Checker.reset_fallback_firings ();
  List.iter
    (fun path ->
       let base = Filename.basename path in
       let first, second = run_file path in
       (match first with
        | Cert_valid ->
          incr n_unsat;
          incr n_valid;
          Printf.printf "VALID          %s\n" base
        | Cert_invalid r ->
          incr n_unsat;
          incr n_invalid;
          Printf.printf "INVALID        %s :: %s\n" base r
        | Cert_unsupported f ->
          incr n_unsat;
          incr n_unsupported;
          Printf.printf "UNSUPPORTED    %s :: %s\n" base f
        | Not_unsat why ->
          incr n_nonunsat;
          Printf.printf "skip(%s) %s\n" why base
        | Error e ->
          incr n_error;
          Printf.printf "ERROR          %s :: %s\n" base e);
       match second with
       | Some Cert_valid -> incr reemit_ok
       | Some other when first = Cert_valid ->
         incr reemit_bad;
         Printf.printf
           "  re-emit MISMATCH %s :: %s\n"
           base
           (match other with
            | Cert_invalid r -> "INVALID " ^ r
            | Cert_unsupported f -> "UNSUPPORTED " ^ f
            | Not_unsat w -> "not-unsat " ^ w
            | _ -> "?")
       | _ -> ())
    files;
  Printf.printf
    "\n\
     cert_corpus_gate: %d files | unsat-solves=%d (VALID=%d INVALID=%d UNSUPPORTED=%d) \
     non-unsat=%d error=%d | repeat-solve re-emit VALID=%d bad=%d | learned-clause \
     fallback-firings=%d\n"
    (List.length files)
    !n_unsat
    !n_valid
    !n_invalid
    !n_unsupported
    !n_nonunsat
    !n_error
    !reemit_ok
    !reemit_bad
    (Checker.fallback_firing_count ());
  (* nonzero iff an unsat-solve produced a non-VALID cert, or a re-emit diverged. *)
  if !n_invalid > 0 || !n_unsupported > 0 || !reemit_bad > 0 then exit 1
;;
