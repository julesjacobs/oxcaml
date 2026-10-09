(* Driver-equivalence guard (task #133). The headline corpus sweep is driven by
   [corpus_classify] (tests/corpus); the SHIPPED batch path is [oxsmt_cli]'s [solve_batch]
   (tests/solver). Both must drive the {!Oxsmt_interface.Session} the SAME way, or the
   official measurement would score a solver path that differs from the one that ships — a
   measurement-integrity hazard, because a driver split can hide presolve wins AND
   regressions (exactly the trap this closes: before task #133 corpus_classify STREAMED
   [assert_term] while the CLI BATCHED [assert_presolved], so the whole SMPT
   equality-elimination win was invisible to the headline). Both now submit the whole
   assertion set through [Session.assert_presolved]; this standing test runs a
   deterministic sample of files through BOTH executables and asserts their verdicts are
   equivalent, so any future divergence is loud.

   {b The equivalence relation.} Both drivers report the SAME [Session.check_sat] verdict,
   but interpret it with ONE documented, presolve-independent asymmetry:

   - [Unsat] and [Unknown] are reported verbatim by both, so they must match EXACTLY.
   - [corpus_classify] reports a raw [Sat] as [solved-sat]. [oxsmt_cli]'s [solve_batch]
     additionally requires a reconstructable, renderable model to emit [sat]; a [Sat]
     whose model needs a function table it cannot reconstruct, or names a symbol the
     printer cannot render, is DOWNGRADED to a sound [unknown] (oxsmt_cli.ml [solve_batch]
     — e.g. the committed honeypots
     [bool_empty_symbol_unknown]/[bool_operator_collision_unknown], whose model names
     [||]/[|+|]). So a [corpus_classify] [Sat] corresponds to an [oxsmt_cli] [Sat] OR an
     [oxsmt_cli] [Unknown]; that is the only permitted disagreement.

   This asymmetry is ONE-DIRECTIONAL and permanent: the CLI can only ever downgrade a
   [Sat] to [Unknown], never upgrade — it drives the same [Session] and reads the same
   [check_sat] verdict — so tolerating it can never mask a driver split that makes the CLI
   MORE or DIFFERENTLY definite than the measurement driver. The only residual blind spot
   is a hypothetical split that leaves the classifier at [Sat] while the CLI reaches a
   GENUINE [Unknown]; from stdout alone that is indistinguishable from the benign model
   downgrade. Every OTHER disagreement is a divergence and fails: a soundness
   contradiction ([Sat] vs [Unsat]), the CLI definite where the classifier is [Unknown]
   (or vice versa), or the classifier emitting [mismatch] (its label tripwire — never
   expected on a label-consistent sample, and a hard failure if seen).

   Test-only, stdlib + Unix (via harness_lib). Exit 0 iff every sampled file is equivalent
   and the classifier is deterministic across two runs. *)

module Harness = Harness_lib.Harness

(* The classifier is exit-0-always and prints "<token> <effort>"; the CLI prints one
   [(result (verdict ...) ...)] block per check-sat. We normalize both to this. *)
type nverdict =
  | Sat
  | Unsat
  | Unknown
  | Mismatch (* corpus_classify's soundness tripwire; never expected here *)

let nverdict_to_string = function
  | Sat -> "sat"
  | Unsat -> "unsat"
  | Unknown -> "unknown"
  | Mismatch -> "mismatch"
;;

(* corpus_classify stdout -> normalized verdict. The token is the first
   whitespace-delimited field; unknown / unknown-incremental / parse-fail all fold to
   [Unknown] (the classifier distinguishes them for the baseline, but they are the same
   solver-verdict-free bucket for equivalence). Any unexpected token is a hard error. *)
let classify_norm (out : string) : (nverdict, string) result =
  let token =
    match String.split_on_char ' ' (String.trim out) with
    | tok :: _ -> tok
    | [] -> ""
  in
  match token with
  | "solved-sat" -> Ok Sat
  | "solved-unsat" -> Ok Unsat
  | "unknown" | "unknown-incremental" | "parse-fail" -> Ok Unknown
  | "mismatch" -> Ok Mismatch
  | other -> Error (Printf.sprintf "unrecognized corpus_classify token %S" other)
;;

let of_harness_verdict = function
  | Harness.Sat -> Sat
  | Harness.Unsat -> Unsat
  | Harness.Unknown -> Unknown
;;

(* oxsmt_cli stdout -> normalized verdict, via the shipped-contract parser (harness_lib).
   A batch file yields exactly one (result ...) block; an incremental file (push/pop or
   multiple check-sats) yields several unknown blocks, and a malformed file yields none —
   both fold to [Unknown], mirroring corpus_classify's unknown-incremental / parse-fail. *)
let cli_norm ~cli_bin file : (nverdict, string) result =
  match Harness.run_solver cli_bin file with
  | Error e, _ -> Error ("oxsmt_cli: " ^ e)
  | Ok [ g ], _ -> Ok (of_harness_verdict g.Harness.verdict)
  | Ok _, _ -> Ok Unknown
;;

let run_classify ~classify_bin file : (string, string) result =
  match Harness.spawn_capture [| classify_bin; file |] with
  | exception e ->
    Error (Printf.sprintf "failed to run corpus_classify: %s" (Printexc.to_string e))
  | out, _errs, Unix.WEXITED 0 -> Ok out
  | _out, errs, _ -> Error ("corpus_classify exited nonzero: " ^ String.trim errs)
;;

(* The relation: identical, modulo the CLI's documented one-directional sat-model
   downgrade (a classifier [Sat] may show as a CLI [Unknown]). [Mismatch] on either side,
   or any other pairing, is a divergence. *)
let equivalent (c : nverdict) (cli : nverdict) : bool =
  match c, cli with
  | Sat, Sat | Unsat, Unsat | Unknown, Unknown -> true
  | Sat, Unknown -> true
  | _ -> false
;;

(* Recursively collect *.smt2 under each dir arg; a file arg is taken verbatim. Sorted for
   a deterministic sample (I6). *)
let rec collect acc path =
  match Sys.is_directory path with
  | true ->
    let names = Sys.readdir path in
    Array.sort String.compare names;
    Array.fold_left (fun acc name -> collect acc (Filename.concat path name)) acc names
  | false -> if Filename.check_suffix path ".smt2" then path :: acc else acc
  | exception Sys_error _ -> acc
;;

let () =
  let classify_bin = ref None in
  let cli_bin = ref None in
  let determinism = ref true in
  let inputs = ref [] in
  let rec parse = function
    | [] -> ()
    | "--classify" :: p :: rest ->
      classify_bin := Some p;
      parse rest
    | "--cli" :: p :: rest ->
      cli_bin := Some p;
      parse rest
    | "--no-determinism" :: rest ->
      determinism := false;
      parse rest
    | arg :: rest ->
      inputs := arg :: !inputs;
      parse rest
  in
  parse (List.tl (Array.to_list Sys.argv));
  let classify_bin =
    match !classify_bin with
    | Some p -> p
    | None ->
      prerr_endline "driver_equiv_test: --classify <corpus_classify.exe> is required";
      exit 2
  in
  let cli_bin =
    match !cli_bin with
    | Some p -> p
    | None ->
      prerr_endline "driver_equiv_test: --cli <oxsmt_cli.exe> is required";
      exit 2
  in
  let files =
    List.fold_left collect [] (List.rev !inputs) |> List.sort_uniq String.compare
  in
  if files = []
  then (
    prerr_endline "driver_equiv_test: no .smt2 files in the given inputs";
    exit 2);
  let divergences = ref 0 in
  let errors = ref 0 in
  let nondeterministic = ref 0 in
  let report kind file msg =
    incr
      (match kind with
       | `Divergence -> divergences
       | `Error -> errors
       | `Nondet -> nondeterministic);
    Printf.printf
      "%s %s: %s\n"
      (match kind with
       | `Divergence -> "DIVERGE"
       | `Error -> "ERROR"
       | `Nondet -> "NONDET")
      file
      msg
  in
  List.iter
    (fun file ->
       match run_classify ~classify_bin file with
       | Error e -> report `Error file e
       | Ok c_out ->
         (* Determinism (I6): the classifier is the driver this task changed, so re-run it
           and require byte-identical output (token AND effort) before trusting the
           verdict. *)
         if !determinism
         then (
           match run_classify ~classify_bin file with
           | Ok c_out2 when String.equal (String.trim c_out) (String.trim c_out2) -> ()
           | Ok c_out2 ->
             report
               `Nondet
               file
               (Printf.sprintf "run1=%S run2=%S" (String.trim c_out) (String.trim c_out2))
           | Error e -> report `Error file ("second classify run: " ^ e));
         (match classify_norm c_out, cli_norm ~cli_bin file with
          | Error e, _ -> report `Error file e
          | _, Error e -> report `Error file e
          | Ok c, Ok cli ->
            if not (equivalent c cli)
            then
              report
                `Divergence
                file
                (Printf.sprintf
                   "corpus_classify=%s oxsmt_cli=%s"
                   (nverdict_to_string c)
                   (nverdict_to_string cli))))
    files;
  let n = List.length files in
  Printf.printf
    "driver-equiv: %d file(s) | %d divergence(s) | %d error(s) | %d nondeterministic\n"
    n
    !divergences
    !errors
    !nondeterministic;
  if !divergences = 0 && !errors = 0 && !nondeterministic = 0
  then
    print_endline "driver-equiv: OK — corpus_classify and oxsmt_cli agree on every file"
  else exit 1
;;
