(* Unit tests for the M1 wiring: the session layer (push/pop retraction,
   assert-after-check, THE SOUNDNESS RULE downgrade, get_model) and the two namespace
   guards (board #48).

   Lives under tests/ (not smt/interface/test) because it links the TEST-ONLY SMT-LIB
   parser to exercise the parser-side guard, and the dependency firewall forbids anything
   under smt/ except the smtlib tests from depending on the parser (AGENTS.md, I3).

   Stdlib-only, deterministic. *)

open Oxsmt_core
module Session = Oxsmt_interface.Session
module Parser = Oxsmt_smtlib_parser.Parser

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let check_raises name f =
  incr checks;
  match f () with
  | exception _ -> ()
  | _ ->
    incr failures;
    Printf.printf "  FAIL %s (expected an exception)\n" name
;;

let verdict_str = function
  | Session.Sat -> "sat"
  | Session.Unsat -> "unsat"
  | Session.Unknown -> "unknown"
;;

let check_verdict name expected got =
  check (name ^ " (" ^ verdict_str got ^ ")") (expected = got)
;;

(* ------------------------------------------------------------------ *)
(* push/pop retraction: assert p (sat), push, assert ¬p (unsat), pop, sat again. *)

let test_push_pop () =
  let s = Session.create () in
  let ctx = Session.context s in
  let p = Context.const ctx (Session.declare_const s "p" Sort.bool) in
  Session.assert_term s p;
  check_verdict "base: p alone" Session.Sat (Session.check_sat s);
  Session.push s;
  Session.assert_term s (Context.not_ ctx p);
  check_verdict "pushed: p /\\ ¬p" Session.Unsat (Session.check_sat s);
  Session.pop s;
  check_verdict "popped: p alone again" Session.Sat (Session.check_sat s);
  (* nested push/pop *)
  Session.push s;
  Session.push s;
  Session.assert_term s (Context.not_ ctx p);
  check_verdict "nested: unsat" Session.Unsat (Session.check_sat s);
  Session.pop s;
  Session.pop s;
  check_verdict "nested popped: sat" Session.Sat (Session.check_sat s);
  check_raises "pop with no matching push" (fun () -> Session.pop s)
;;

(* assert-after-check: keep asserting between checks; the verdict tracks the growing set. *)

let test_assert_after_check () =
  let s = Session.create () in
  let ctx = Session.context s in
  let p = Context.const ctx (Session.declare_const s "p" Sort.bool) in
  let q = Context.const ctx (Session.declare_const s "q" Sort.bool) in
  Session.assert_term s (Context.or_ ctx [ p; q ]);
  check_verdict "after (or p q)" Session.Sat (Session.check_sat s);
  Session.assert_term s (Context.not_ ctx p);
  check_verdict "after also ¬p" Session.Sat (Session.check_sat s);
  Session.assert_term s (Context.not_ ctx q);
  check_verdict "after also ¬q" Session.Unsat (Session.check_sat s)
;;

(* THE SOUNDNESS RULE: with a theory atom present, a satisfiable Boolean skeleton must NOT
   be reported sat (the SAT core cannot see x>0 /\ x<0 is contradictory) — it degrades to
   unknown. Propositional unsat with a theory atom stays unsat (sound). *)

let test_soundness_rule () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let zero = Context.int_const ctx 0 in
  Session.assert_term s (Context.gt ctx x zero);
  check_verdict "theory atom present -> not sat" Session.Unknown (Session.check_sat s);
  Session.assert_term s (Context.lt ctx x zero);
  (* x>0 and x<0 are independent atoms to the SAT core: skeleton is satisfiable, so the
     rule downgrades to unknown rather than (unsoundly) sat or (undeducibly) unsat. *)
  check_verdict "x>0 /\\ x<0 -> unknown (not sat!)" Session.Unknown (Session.check_sat s);
  check "get_model None under theory" (Session.get_model s = None);
  (* But the SAME atom asserted and negated is a propositional contradiction -> sound
     unsat even though it is a theory atom. *)
  let s2 = Session.create () in
  let ctx2 = Session.context s2 in
  let y = Context.const ctx2 (Session.declare_const s2 "y" Sort.int) in
  let a = Context.gt ctx2 y (Context.int_const ctx2 0) in
  Session.assert_term s2 a;
  Session.assert_term s2 (Context.not_ ctx2 a);
  check_verdict "atom /\\ ¬atom -> unsat" Session.Unsat (Session.check_sat s2)
;;

(* get_model on a pure-Boolean sat returns a value for every propositional variable. *)

let test_get_model () =
  let s = Session.create () in
  let ctx = Session.context s in
  let p = Context.const ctx (Session.declare_const s "p" Sort.bool) in
  let q = Context.const ctx (Session.declare_const s "q" Sort.bool) in
  Session.assert_term s (Context.or_ ctx [ p; q ]);
  Session.assert_term s (Context.not_ ctx q);
  (match Session.check_sat s, Session.get_model s with
   | Session.Sat, Some m ->
     check "model has p and q" (List.length m = 2);
     check "model sorted by name" (m = List.sort (fun (a, _) (b, _) -> compare a b) m);
     check "q is false" (List.assoc "q" m = false);
     check "p is true (forced by (or p q) /\\ ¬q)" (List.assoc "p" m = true)
   | v, _ -> check ("expected sat with model, got " ^ verdict_str v) false);
  (* after unsat, no model *)
  Session.assert_term s (Context.not_ ctx p);
  check_verdict "now unsat" Session.Unsat (Session.check_sat s);
  check "no model after unsat" (Session.get_model s = None)
;;

(* Namespace guard (#48): neither the session nor the parser may declare a user symbol in
   the reserved ".oxsmt.*" fresh-symbol namespace. *)

let test_namespace_guard () =
  let s = Session.create () in
  check_raises "session rejects .oxsmt. const" (fun () ->
    Session.declare_const s ".oxsmt.sneaky" Sort.int);
  check_raises "session rejects .oxsmt. fun" (fun () ->
    Session.declare_fun s ".oxsmt.f" (Rank.create [ Sort.int ] Sort.int));
  check_raises "session rejects .oxsmt. sort" (fun () ->
    Session.declare_sort s ".oxsmt.S");
  (* a normal name still works *)
  check
    "normal declaration still allowed"
    (match Session.declare_const s "ok" Sort.int with
     | _ -> true
     | exception _ -> false);
  (* parser side: a declaration in the reserved namespace is Malformed *)
  check_raises "parser rejects .oxsmt. declare-const" (fun () ->
    Parser.parse "(declare-const .oxsmt.x Int)(assert (= .oxsmt.x .oxsmt.x))(check-sat)");
  check_raises "parser rejects .oxsmt. declare-fun" (fun () ->
    Parser.parse "(declare-fun .oxsmt.g (Int) Int)(check-sat)");
  check_raises "parser rejects .oxsmt. declare-sort" (fun () ->
    Parser.parse "(declare-sort .oxsmt.S 0)(check-sat)");
  (* a normal parse still works *)
  check
    "parser accepts normal declaration"
    (match Parser.parse "(declare-const ok Bool)(assert ok)(check-sat)" with
     | _ -> true
     | exception _ -> false)
;;

(* End-to-end through the parser into a session (the CLI's batch path in miniature). *)

let test_parser_into_session () =
  let s = Session.create () in
  let parsed =
    Parser.parse_into
      (Session.env s)
      (Session.context s)
      "(declare-const p Bool)(declare-const q Bool)(assert (= p q))(assert (not \
       p))(assert q)(check-sat)"
  in
  List.iter (Session.assert_term s) parsed.Parser.assertions;
  (* (p<=>q) /\ ¬p /\ q is unsat *)
  check_verdict "parsed unsat formula" Session.Unsat (Session.check_sat s)
;;

(* Degradation honeypots (mirrors the tests/cases/degrade_*.smt2 files): formulas whose
   boolean skeleton is satisfiable but whose theory is unsat MUST verdict `unknown`, never
   `sat`. This is the exact asymmetry a later refactor could silently break. *)

let test_degradation_honeypots () =
  (* LIA: x<0 /\ x>0 *)
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let zero = Context.int_const ctx 0 in
  Session.assert_term s (Context.lt ctx x zero);
  Session.assert_term s (Context.gt ctx x zero);
  check_verdict "honeypot LIA x<0 /\\ x>0" Session.Unknown (Session.check_sat s);
  (* EUF: x=y /\ f(x)≠f(y) *)
  let s = Session.create () in
  let ctx = Session.context s in
  let su = Session.declare_sort s "S" in
  let ssort = Sort.uninterpreted su in
  let f = Session.declare_fun s "f" (Rank.create [ ssort ] ssort) in
  let x = Context.const ctx (Session.declare_const s "x" ssort) in
  let y = Context.const ctx (Session.declare_const s "y" ssort) in
  Session.assert_term s (Context.eq ctx x y);
  Session.assert_term
    s
    (Context.not_
       ctx
       (Context.eq ctx (Context.app ctx f [ x ]) (Context.app ctx f [ y ])));
  check_verdict "honeypot EUF x=y /\\ f(x)≠f(y)" Session.Unknown (Session.check_sat s);
  (* mixed: x=y /\ f(x)<f(y) *)
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" (Rank.create [ Sort.int ] Sort.int) in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let y = Context.const ctx (Session.declare_const s "y" Sort.int) in
  Session.assert_term s (Context.eq ctx x y);
  Session.assert_term
    s
    (Context.lt ctx (Context.app ctx f [ x ]) (Context.app ctx f [ y ]));
  check_verdict "honeypot mixed x=y /\\ f(x)<f(y)" Session.Unknown (Session.check_sat s)
;;

let () =
  test_push_pop ();
  test_assert_after_check ();
  test_soundness_rule ();
  test_get_model ();
  test_namespace_guard ();
  test_parser_into_session ();
  test_degradation_honeypots ();
  Printf.printf "wiring_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
