(* Nonlinear-integer (QF_NIA) abstraction self-test (dark OXSMT_NIA), driven end-to-end
   through the real parse -> Session.check_sat dispatch (the same path as the CLI). No z3
   dependency (z3 is dev-only, never a gate).

   Two modes, selected by the lever so a single test source gates BOTH states:
   - OXSMT_NIA on -> the ABSTRACTION suite: sign/zero/unit lemmas refute the common
     nonlinear unsat cores; a model-consistent product is accepted [sat] (re-checked under
     REAL multiplication); an abstraction-sat whose product is violated fails CLOSED to
     unknown (the SAT-soundness gate), never a wrong sat.
   - OXSMT_NIA off -> the DARK suite: a QF_NIA document is rejected at [set-logic]
     (unknown), byte-identical to trunk; a plain QF_LIA document is unaffected. *)

module Session = Oxsmt_interface.Session
module Parser = Oxsmt_smtlib_parser.Parser
module Nia_config = Oxsmt_core.Nia_config

let failures = ref 0
let checks = ref 0

let solve src =
  let s = Session.create () in
  match
    Parser.parse_into
      ~internal_mint:(Session.parse_minter s)
      (Session.env s)
      (Session.context s)
      src
  with
  | exception (Parser.Malformed _ | Parser.Unsupported _) -> "unknown"
  | parsed ->
    if not (Oxsmt_query_loader.assert_all ~presolve:true s parsed)
    then "unknown"
    else (
      match Session.check_sat s with
      | Session.Sat -> "sat"
      | Session.Unsat -> "unsat"
      | Session.Unknown -> "unknown")
;;

let expect label want src =
  incr checks;
  match solve src with
  | got when String.equal got want -> ()
  | got ->
    incr failures;
    Printf.printf "  FAIL %s: expected %s, got %s\n" label want got
;;

(* Verdict must be sound but need not be definite: sat and wrong are BOTH forbidden here;
   unsat or unknown are both acceptable (used for the fail-closed cases). *)
let expect_not_sat label src =
  incr checks;
  match solve src with
  | "sat" ->
    incr failures;
    Printf.printf "  FAIL %s: got sat on an unsatisfiable query (unsound!)\n" label
  | _ -> ()
;;

let nia_hdr = "(set-logic QF_NIA)\n(declare-fun x () Int)\n(declare-fun y () Int)\n"

let abstraction_suite () =
  (* zero lemma: x=0 => x*y=0, contradicting x*y=6 *)
  expect "zero" "unsat" (nia_hdr ^ "(assert (= (* x y) 6))(assert (= x 0))(check-sat)");
  (* sign/square: x*x cannot be negative *)
  expect
    "square-neg"
    "unsat"
    "(set-logic QF_NIA)(declare-fun x () Int)(assert (= (* x x) (- 1)))(check-sat)";
  (* sign: both nonneg => product nonneg, contradicting product = -1 *)
  expect
    "sign"
    "unsat"
    (nia_hdr ^ "(assert (>= x 0))(assert (>= y 0))(assert (= (* x y) (- 1)))(check-sat)");
  (* unit: x=1 => x*y=y; with y=8 and product=7 that is unsat *)
  expect
    "unit"
    "unsat"
    (nia_hdr ^ "(assert (= x 1))(assert (= (* x y) 7))(assert (= y 8))(check-sat)");
  (* a genuinely satisfiable product, re-checked under real multiplication *)
  expect "sat" "sat" (nia_hdr ^ "(assert (= (* x y) 6))(assert (= (+ x y) 5))(check-sat)");
  (* FAIL-CLOSED: x=2,y=2 forces the product to 4, so asserting the product = 5 is unsat.
     The abstraction admits an uninterpreted product = 5, but Model_check re-evaluates
     under real multiplication and rejects -> must be unsat or unknown, NEVER sat. *)
  expect_not_sat
    "fail-closed"
    (nia_hdr ^ "(assert (= (* x y) 5))(assert (= x 2))(assert (= y 2))(check-sat)");
  (* n-ary product abstraction (left-assoc chain): x*x*x with a positive product but the
     sign lemma refutes a negative one *)
  expect
    "cube-neg"
    "unsat"
    "(set-logic QF_NIA)(declare-fun x () Int)(assert (>= x 0))(assert (= (* x x x) (- \
     8)))(check-sat)"
;;

let dark_suite () =
  (* QF_NIA is rejected wholesale when the lever is off (byte-identical to trunk). *)
  expect
    "off-rejects-nia"
    "unknown"
    (nia_hdr ^ "(assert (= (* x y) 6))(assert (= (+ x y) 5))(check-sat)");
  (* a plain linear QF_LIA document is unaffected by the (off) lever. *)
  expect
    "off-linear-lia"
    "sat"
    "(set-logic QF_LIA)(declare-fun x () Int)(assert (= (* 2 x) 10))(check-sat)"
;;

let () =
  if Nia_config.enabled () then abstraction_suite () else dark_suite ();
  if !failures = 0
  then Printf.printf "nia_test: %d checks OK (nia=%b)\n" !checks (Nia_config.enabled ())
  else (
    Printf.printf
      "nia_test: %d/%d FAILED (nia=%b)\n"
      !failures
      !checks
      (Nia_config.enabled ());
    exit 1)
;;
