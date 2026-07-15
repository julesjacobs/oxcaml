(* Unit self-test for the harness classification logic.

   Proves the harness goes RED correctly — a green gate that cannot demonstrate it can
   fail is unaudited (DESIGN.md §10). Everything here is pure: we feed evaluate hand-built
   solver output and goldens, and assert the outcome. No subprocess, no filesystem. Run by
   `make test` before the golden regression. *)

open Harness_lib
open Harness

let failures = ref 0

let check name cond =
  if cond
  then Printf.printf "  ok   %s\n" name
  else (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

(* Substring search (the stdlib has none). *)
let contains haystack needle =
  let hn = String.length haystack
  and nn = String.length needle in
  let rec at i =
    if i + nn > hn
    then false
    else if String.equal (String.sub haystack i nn) needle
    then true
    else at (i + 1)
  in
  at 0
;;

let unknown_goal =
  { verdict = Unknown
  ; core_size = None
  ; model = None
  ; counters = { conflicts = 0; decisions = 0; propagations = 0 }
  }
;;

let sat_goal =
  { verdict = Sat
  ; core_size = None
  ; model = Some (Flat [ "y", "1"; "x", "0" ])
  ; counters = { conflicts = 5; decisions = 250; propagations = 3000 }
  }
;;

let unsat_goal =
  { verdict = Unsat
  ; core_size = Some 3
  ; model = None
  ; counters = { conflicts = 42; decisions = 9; propagations = 900 }
  }
;;

let eval ?(expected = []) ?golden ?(evals = []) out =
  evaluate
    ~path:"<test>"
    ~expected_statuses:expected
    ~golden
    ~solver_result:(Ok out)
    ~eval_outcomes:evals
    ()
;;

let is_pass = function
  | Pass -> true
  | _ -> false
;;

let is_missing = function
  | Fail_missing_golden -> true
  | _ -> false
;;

let is_mismatch = function
  | Fail_golden_mismatch -> true
  | _ -> false
;;

let is_label = function
  | Fail_label_mismatch _ -> true
  | _ -> false
;;

let is_error = function
  | Fail_error _ -> true
  | _ -> false
;;

let is_unsound = function
  | Fail_model_unsound _ -> true
  | _ -> false
;;

let is_eval_unusable = function
  | Fail_eval_unusable _ -> true
  | _ -> false
;;

let () =
  print_endline "harness self-test:";
  (* Bucketing boundaries. *)
  check "bucket 0 -> <10" (String.equal (Bucket.label 0) "<10");
  check "bucket 9 -> <10" (String.equal (Bucket.label 9) "<10");
  check "bucket 10 -> <100" (String.equal (Bucket.label 10) "<100");
  check "bucket 999 -> <1k" (String.equal (Bucket.label 999) "<1k");
  check "bucket 1000 -> <10k" (String.equal (Bucket.label 1000) "<10k");
  check "bucket 10000 -> >=10k" (String.equal (Bucket.label 10000) ">=10k");
  (* Golden text is canonical: model sorted, counters bucketed. *)
  let sat_text = produced_text [ sat_goal ] in
  check "sat model sorted by name" (contains sat_text "(model ((x 0) (y 1)))");
  check
    "sat counters bucketed"
    (contains sat_text "(conflicts <10) (decisions <1k) (propagations <10k)");
  check
    "unsat core-size present"
    (contains (produced_text [ unsat_goal ]) "(core-size 3)");
  (* Quoted-symbol round-trip (the eval-model bridge): a model name that is not a simple
     SMT-LIB symbol must be re-quoted on output, in both the golden text and the eval
     sidecar, or a name like [p q] re-lexes as two tokens and eval rejects the model. *)
  check
    "quote_symbol: simple symbol unchanged"
    (String.equal (Sexp.quote_symbol "x0") "x0");
  check "quote_symbol: space wrapped" (String.equal (Sexp.quote_symbol "p q") "|p q|");
  check "quote_symbol: empty wrapped" (String.equal (Sexp.quote_symbol "") "||");
  check
    "quote_symbol: leading digit wrapped"
    (String.equal (Sexp.quote_symbol "1a") "|1a|");
  let quoted_goal =
    { verdict = Sat
    ; core_size = None
    ; model = Some (Flat [ "p q", "true" ])
    ; counters = { conflicts = 0; decisions = 0; propagations = 0 }
    }
  in
  check
    "golden re-quotes a non-simple model name"
    (contains (produced_text [ quoted_goal ]) "(model ((|p q| true)))");
  check
    "sidecar bridge re-quotes a non-simple model name"
    (contains (model_to_sidecar [ "p q", "true" ]) "(const |p q| true)");
  (* R9 transport: a function-table (sidecar-grammar) model body round-trips through
     parse_solver_output -> Table, renders back verbatim in the golden, and re-quotes a
     non-simple symbol NAME on the way out (the harness Sexp reader drops the |bars|). *)
  let table_output =
    "(result (verdict sat) (model (sort S 2) (const a 0) (fun f (default 0) (case (0) 0) \
     (case (1) 0))) (counters (conflicts 0) (decisions 0) (propagations 0)))"
  in
  let table_goal =
    match parse_solver_output table_output with
    | Ok [ g ] -> g
    | _ -> failwith "table_output did not parse to one goal"
  in
  check
    "table body parses to a Table model (not Flat, not bad-model error)"
    (match table_goal.model with
     | Some (Table _) -> true
     | _ -> false);
  check
    "table golden renders the sidecar entries verbatim"
    (contains
       (produced_text [ table_goal ])
       "(model (sort S 2) (const a 0) (fun f (default 0) (case (0) 0) (case (1) 0)))");
  check
    "table stats: max_card = 2, table_rows = 2"
    (model_table_stats table_goal.model = (2, 2));
  let quoted_table =
    match
      parse_solver_output
        "(result (verdict sat) (model (sort S 1) (const |a b| 0)) (counters (conflicts \
         0) (decisions 0) (propagations 0)))"
    with
    | Ok [ g ] -> g
    | _ -> failwith "quoted table did not parse"
  in
  check
    "table golden re-quotes a non-simple const name"
    (contains (produced_text [ quoted_table ]) "(const |a b| 0)");
  (* Faithful carrier (codex HIGH): a QUOTED payload token in a table body — [|true|] as a
     case result — is MALFORMED (a value is only ever a bare numeral / true|false / (-
     n)). The harness must PRESERVE the bars, never launder [|true|] into a valid bare
     [true], so the eval reader can fail the model closed rather than the harness silently
     repairing a solver regression. Both the golden and the eval sidecar must still show
     [|true|]. *)
  let launder_goal =
    match
      parse_solver_output
        "(result (verdict sat) (model (sort S 1) (fun p (default false) (case (0) \
         |true|))) (counters (conflicts 0) (decisions 0) (propagations 0)))"
    with
    | Ok [ g ] -> g
    | _ -> failwith "laundering input did not parse"
  in
  check
    "quoted payload token preserved in golden (not laundered to bare true)"
    (contains (produced_text [ launder_goal ]) "(case (0) |true|)");
  check
    "quoted payload token preserved in eval sidecar (not laundered)"
    (match launder_goal.model with
     | Some m -> contains (sidecar_of_model m) "|true|"
     | None -> false);
  (* Pass: golden matches produced. *)
  let g = produced_text [ unknown_goal ] in
  check
    "golden match -> Pass"
    (is_pass (eval ~expected:[ None ] ~golden:g [ unknown_goal ]).outcome);
  (* Golden mismatch -> RED. *)
  check
    "golden mismatch -> Fail_golden_mismatch"
    (is_mismatch
       (eval ~expected:[ None ] ~golden:"(goal 1 (verdict sat))" [ unknown_goal ]).outcome);
  (* Missing golden -> RED. *)
  check
    "missing golden -> Fail_missing_golden"
    (is_missing (eval ~expected:[ None ] [ unknown_goal ]).outcome);
  (* Label mismatch (soundness) -> RED, even when golden matches. *)
  let sat_g = produced_text [ sat_goal ] in
  check
    "label mismatch dominates matching golden"
    (is_label (eval ~expected:[ Some Unsat ] ~golden:sat_g [ sat_goal ]).outcome);
  check
    "unknown vs sat label is NOT a failure (completeness gap)"
    (is_pass
       (eval
          ~expected:[ Some Sat ]
          ~golden:(produced_text [ unknown_goal ])
          [ unknown_goal ])
         .outcome);
  check
    "sat matching sat label -> Pass"
    (is_pass (eval ~expected:[ Some Sat ] ~golden:sat_g [ sat_goal ]).outcome);
  (* Block-count mismatch -> RED. *)
  check
    "too few result blocks -> Fail_error"
    (is_error (eval ~expected:[ None; None ] ~golden:g [ unknown_goal ]).outcome);
  (* Solver error -> RED. *)
  check
    "solver error -> Fail_error"
    (is_error
       (evaluate
          ~path:"<test>"
          ~expected_statuses:[ None ]
          ~golden:None
          ~solver_result:(Error "boom")
          ())
         .outcome);
  (* Eval self-check (layer 1). sat + MODEL-SATISFIES + matching golden -> Pass. *)
  check
    "sat + eval satisfies -> Pass"
    (is_pass
       (eval ~expected:[ Some Sat ] ~golden:sat_g ~evals:[ Eval_satisfies ] [ sat_goal ])
         .outcome);
  (* sat + MODEL-FAILS -> soundness RED, dominating a matching golden. *)
  check
    "eval MODEL-FAILS -> Fail_model_unsound (dominates matching golden)"
    (is_unsound
       (eval
          ~expected:[ Some Sat ]
          ~golden:sat_g
          ~evals:[ Eval_fails "trace" ]
          [ sat_goal ])
         .outcome);
  (* sat + eval unusable (exit 2 / not configured) -> harness failure, distinct. *)
  check
    "eval unusable -> Fail_eval_unusable"
    (is_eval_unusable
       (eval
          ~expected:[ Some Sat ]
          ~golden:sat_g
          ~evals:[ Eval_unusable "malformed" ]
          [ sat_goal ])
         .outcome);
  (* --- Model-optional goldens (b'): a golden that OMITS the (model ...) line pins
     verdict + counters + eval-validity but NOT the model text (a sat instance may have
     many equivalent models; base-l0 et al. reshuffle which one search lands on). --- *)
  let sat_g_model_less = produced_text ~include_model:false [ sat_goal ] in
  check
    "model-optional golden carries no (model ...) line"
    (not (contains sat_g_model_less "(model"));
  let sat_goal_altmodel = { sat_goal with model = Some (Flat [ "x", "5"; "y", "9" ]) } in
  (* A DIFFERENT valid model (same verdict/counters) still matches a model-less golden. *)
  check
    "model-less golden + different valid model -> Pass (model text not pinned)"
    (is_pass
       (eval
          ~expected:[ Some Sat ]
          ~golden:sat_g_model_less
          ~evals:[ Eval_satisfies ]
          [ sat_goal_altmodel ])
         .outcome);
  (* DISCRIMINATION (the (b') obligation): relaxing the golden must NOT let an unsound
     model through — the layer-1 eval self-check still dominates. This RED-fails if the
     model-less branch ever short-circuits to Pass ahead of the soundness check. *)
  check
    "model-less golden + MODEL-FAILS -> Fail_model_unsound (soundness still dominates)"
    (is_unsound
       (eval
          ~expected:[ Some Sat ]
          ~golden:sat_g_model_less
          ~evals:[ Eval_fails "trace" ]
          [ sat_goal ])
         .outcome);
  (* Backward-compat: a model-BEARING golden still pins the model text. *)
  check
    "model-bearing golden + different model -> Fail_golden_mismatch (still pinned)"
    (is_mismatch
       (eval
          ~expected:[ Some Sat ]
          ~golden:sat_g
          ~evals:[ Eval_satisfies ]
          [ sat_goal_altmodel ])
         .outcome);
  (* Neither soundness failure nor an unreadable model is promotable. *)
  check "model-unsound not promotable" (not (promotable (Fail_model_unsound "x")));
  check "eval-unusable not promotable" (not (promotable (Fail_eval_unusable "x")));
  (* Counter overflow clamps to >=10k rather than erroring (M0-harness-hygiene). *)
  check
    "counter > max_int clamps to >=10k"
    (match
       parse_solver_output
         "(result (verdict unsat) (counters (conflicts 99999999999999999999) (decisions \
          0) (propagations 0)))"
     with
     | Ok [ g ] ->
       g.counters.conflicts = max_int && String.equal (Bucket.label max_int) ">=10k"
     | _ -> false);
  (* Output parsing round-trips the contract. *)
  check
    "parse_solver_output ok"
    (match
       parse_solver_output
         "(result (verdict unsat) (core-size 2) (counters (conflicts 1) (decisions 2) \
          (propagations 3)))"
     with
     | Ok [ g ] -> g.verdict = Unsat && g.core_size = Some 2
     | _ -> false);
  check
    "parse_solver_output rejects missing counters"
    (match parse_solver_output "(result (verdict sat))" with
     | Error _ -> true
     | Ok _ -> false);
  (* expected_statuses tracks the in-effect :status per check-sat. *)
  check
    "expected_statuses reads status + counts check-sats"
    (let sexps =
       Sexp.parse_all
         "(set-info :status unsat) (check-sat) (push 1) (set-info :status sat) \
          (check-sat)"
     in
     expected_statuses sexps = [ Some Unsat; Some Sat ]);
  (* End-to-end lying-model demonstration (integration): when the Makefile passes the
     built eval CLI + a real sat case, spawn eval on a CORRECT model (expect satisfies)
     and on a WRONG model (expect MODEL-FAILS), and confirm a lying solver — one that
     emits sat with the wrong model — is driven RED through evaluate. Skipped (with a
     note) for a bare `dune exec` without args, so the pure checks still stand alone. *)
  (match Array.to_list Sys.argv with
   | _ :: eval_bin :: case_smt2 :: _ ->
     (* bool_or_sat: (or p q) and (not q); q=true violates (not q). *)
     let good = Flat [ "p", "true"; "q", "false" ] in
     let bad = Flat [ "p", "false"; "q", "true" ] in
     let sat_of model =
       { verdict = Sat
       ; core_size = None
       ; model = Some model
       ; counters = { conflicts = 0; decisions = 0; propagations = 0 }
       }
     in
     let eo_good = Harness.run_eval ~eval_bin ~smt2:case_smt2 ~model:good in
     let eo_bad = Harness.run_eval ~eval_bin ~smt2:case_smt2 ~model:bad in
     check
       "e2e: eval accepts a correct model"
       (match eo_good with
        | Eval_satisfies -> true
        | _ -> false);
     check
       "e2e: eval rejects a lying (wrong) model"
       (match eo_bad with
        | Eval_fails _ -> true
        | _ -> false);
     check
       "e2e: lying-model solver is driven RED via eval"
       (is_unsound
          (evaluate
             ~path:case_smt2
             ~expected_statuses:[ Some Sat ]
             ~golden:(Some (produced_text [ sat_of bad ]))
             ~solver_result:(Ok [ sat_of bad ])
             ~eval_outcomes:[ eo_bad ]
             ())
            .outcome)
   | _ -> Printf.printf "  note: e2e eval checks skipped (no eval-bin/case args)\n");
  if !failures = 0
  then Printf.printf "harness self-test: all checks passed\n"
  else (
    Printf.printf "harness self-test: %d check(s) FAILED\n" !failures;
    exit 1)
;;
