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
  ; model = Some [ "y", "1"; "x", "0" ]
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

let eval ?(expected = []) ?golden out =
  evaluate ~path:"<test>" ~expected_statuses:expected ~golden ~solver_result:(Ok out)
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
          ~solver_result:(Error "boom"))
         .outcome);
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
  if !failures = 0
  then Printf.printf "harness self-test: all checks passed\n"
  else (
    Printf.printf "harness self-test: %d check(s) FAILED\n" !failures;
    exit 1)
;;
