(* Session-level unit tests for the M4 wiring: the full CDCL(T) loop (EUF+LIA via the
   Nelson-Oppen combinator behind the theory seam), the rewritten SOUNDNESS RULE
   (theory-validated Sat; poison/overflow/budget firewall → unknown), push/pop retraction,
   assert-after-check, the new structured [get_model], and the two namespace guards (#48).

   Lives under tests/ (not smt/interface/test) because it links the TEST-ONLY SMT-LIB
   parser to exercise the parser-side guard; the dependency firewall forbids anything
   under smt/ (except the smtlib tests) from depending on the parser (AGENTS.md, I3).

   Stdlib-only, deterministic. Nonzero exit on any failed check. *)

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
  check (name ^ " (got " ^ verdict_str got ^ ")") (expected = got)
;;

(* ------------------------------------------------------------------ *)
(* Pure-Boolean plumbing (unchanged by M4): push/pop retraction, assert-after-check. *)

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
  Session.push s;
  Session.push s;
  Session.assert_term s (Context.not_ ctx p);
  check_verdict "nested: unsat" Session.Unsat (Session.check_sat s);
  Session.pop s;
  Session.pop s;
  check_verdict "nested popped: sat" Session.Sat (Session.check_sat s);
  check_raises "pop with no matching push" (fun () -> Session.pop s)
;;

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

(* ------------------------------------------------------------------ *)
(* EUF: congruence + transitivity chains are now REALLY decided (M2 engine behind the
   seam), not degraded. *)

let test_euf_unsat () =
  (* a=b, b=c, a≠c is unsat (transitivity). *)
  let s = Session.create () in
  let ctx = Session.context s in
  let su = Session.declare_sort s "S" in
  let ss = Sort.uninterpreted su in
  let c name = Context.const ctx (Session.declare_const s name ss) in
  let a = c "a"
  and b = c "b"
  and cc = c "c" in
  Session.assert_term s (Context.eq ctx a b);
  Session.assert_term s (Context.eq ctx b cc);
  Session.assert_term s (Context.not_ ctx (Context.eq ctx a cc));
  check_verdict "EUF transitivity a=b=c, a≠c" Session.Unsat (Session.check_sat s);
  (* a=b ⇒ f(a)=f(b) (congruence): asserting f(a)≠f(b) is unsat. *)
  let s = Session.create () in
  let ctx = Session.context s in
  let su = Session.declare_sort s "S" in
  let ss = Sort.uninterpreted su in
  let f = Session.declare_fun s "f" (Rank.create [ ss ] ss) in
  let a = Context.const ctx (Session.declare_const s "a" ss) in
  let b = Context.const ctx (Session.declare_const s "b" ss) in
  Session.assert_term s (Context.eq ctx a b);
  Session.assert_term
    s
    (Context.not_
       ctx
       (Context.eq ctx (Context.app ctx f [ a ]) (Context.app ctx f [ b ])));
  check_verdict "EUF congruence a=b, f(a)≠f(b)" Session.Unsat (Session.check_sat s)
;;

(* ------------------------------------------------------------------ *)
(* LIA: bounds/Farkas conflicts decided; a feasible integer problem is Sat with a model;
   an integrality-infeasible problem is unsat via branch-and-bound. *)

let test_lia_unsat () =
  (* 0<=x, x<=0, x≠0 is unsat. *)
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let zero = Context.int_const ctx 0 in
  Session.assert_term s (Context.ge ctx x zero);
  Session.assert_term s (Context.le ctx x zero);
  Session.assert_term s (Context.not_ ctx (Context.eq ctx x zero));
  check_verdict "LIA 0<=x<=0, x≠0" Session.Unsat (Session.check_sat s);
  (* Farkas: x+y<=2, x>=2, y>=1 is unsat. *)
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let y = Context.const ctx (Session.declare_const s "y" Sort.int) in
  Session.assert_term s (Context.le ctx (Context.add ctx x y) (Context.int_const ctx 2));
  Session.assert_term s (Context.ge ctx x (Context.int_const ctx 2));
  Session.assert_term s (Context.ge ctx y (Context.int_const ctx 1));
  check_verdict "LIA Farkas x+y<=2, x>=2, y>=1" Session.Unsat (Session.check_sat s)
;;

let test_lia_sat () =
  (* 0<=x<=5, x=3 → sat, model x=3. *)
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  Session.assert_term s (Context.ge ctx x (Context.int_const ctx 0));
  Session.assert_term s (Context.le ctx x (Context.int_const ctx 5));
  Session.assert_term s (Context.eq ctx x (Context.int_const ctx 3));
  check_verdict "LIA 0<=x<=5, x=3" Session.Sat (Session.check_sat s);
  match Session.get_model s with
  | Some (_, [ Session.Const ("x", Session.VInt 3) ]) -> check "LIA sat model x=3" true
  | Some _ | None -> check "LIA sat model x=3" false
;;

let test_lia_branch_and_bound () =
  (* 2*x = 1 has no integer solution: rationally feasible (x=1/2), integrality-unsat.
     Exercises the branch-and-bound Split path (a rational model that is non-integral). *)
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  Session.assert_term
    s
    (Context.eq ctx (Context.mul_const ctx 2 x) (Context.int_const ctx 1));
  check_verdict "LIA 2x=1 (no integer)" Session.Unsat (Session.check_sat s)
;;

(* ------------------------------------------------------------------ *)
(* Mixed QF_UFLIA needing model-based Nelson-Oppen combination. x=y+1, y=0 ⇒ x=1, so g(x)
   must equal g(1); asserting g(x)≠g(1) is unsat — resolvable only by splitting on the
   shared equality x=1 (the arithmetic value forces the congruence). *)

let test_mixed_split () =
  let s = Session.create () in
  let ctx = Session.context s in
  let g = Session.declare_fun s "g" (Rank.create [ Sort.int ] Sort.int) in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let y = Context.const ctx (Session.declare_const s "y" Sort.int) in
  Session.assert_term s (Context.eq ctx x (Context.add ctx y (Context.int_const ctx 1)));
  Session.assert_term s (Context.eq ctx y (Context.int_const ctx 0));
  Session.assert_term
    s
    (Context.not_
       ctx
       (Context.eq
          ctx
          (Context.app ctx g [ x ])
          (Context.app ctx g [ Context.int_const ctx 1 ])));
  check_verdict "mixed x=y+1, y=0, g(x)≠g(1)" Session.Unsat (Session.check_sat s)
;;

(* ------------------------------------------------------------------ *)
(* THE M4 SOUNDNESS RULE. A single theory atom no longer forces unknown: the theory
   decides. x>0 is Sat (theory finds x=1); x>0 ∧ x<0 is now real Unsat (theory conflict),
   the regime flip from M1's unknown. *)

let test_soundness_rule () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let zero = Context.int_const ctx 0 in
  Session.assert_term s (Context.gt ctx x zero);
  check_verdict "x>0 alone -> sat (theory)" Session.Sat (Session.check_sat s);
  Session.assert_term s (Context.lt ctx x zero);
  check_verdict
    "x>0 /\\ x<0 -> unsat (theory conflict, was unknown in M1)"
    Session.Unsat
    (Session.check_sat s)
;;

(* Degradation honeypots: propositionally-satisfiable, theory-unsat. With the theories
   wired these are now REAL unsat (the M4 regime flip; the M1 wall degraded them to
   unknown). degrade_mixed (a function application appearing only inside an arithmetic
   atom) is a documented combination completeness gap and stays a SOUND unknown — never a
   [Sat] (that would be a soundness break; the honeypot's label check still fires red on
   any regression to sat). *)

let test_honeypot_flips () =
  (* LIA: x<0 ∧ x>0 → unsat. *)
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let zero = Context.int_const ctx 0 in
  Session.assert_term s (Context.lt ctx x zero);
  Session.assert_term s (Context.gt ctx x zero);
  check_verdict "honeypot LIA x<0 ∧ x>0 -> unsat" Session.Unsat (Session.check_sat s);
  (* EUF: x=y ∧ f(x)≠f(y) → unsat. *)
  let s = Session.create () in
  let ctx = Session.context s in
  let su = Session.declare_sort s "S" in
  let ss = Sort.uninterpreted su in
  let f = Session.declare_fun s "f" (Rank.create [ ss ] ss) in
  let x = Context.const ctx (Session.declare_const s "x" ss) in
  let y = Context.const ctx (Session.declare_const s "y" ss) in
  Session.assert_term s (Context.eq ctx x y);
  Session.assert_term
    s
    (Context.not_
       ctx
       (Context.eq ctx (Context.app ctx f [ x ]) (Context.app ctx f [ y ])));
  check_verdict
    "honeypot EUF x=y ∧ f(x)≠f(y) -> unsat"
    Session.Unsat
    (Session.check_sat s);
  (* mixed W1: x=y ∧ f(x)<f(y) → UNSAT. Under the OLD combine this degraded to a sound
     UNKNOWN (the wrong-SAT bug family); the internalization combinator now DECIDES it
     (x=y ⇒ f(x)=f(y) by congruence, contradicting f(x)<f(y)) — the solved-rate mover. *)
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" (Rank.create [ Sort.int ] Sort.int) in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let y = Context.const ctx (Session.declare_const s "y" Sort.int) in
  Session.assert_term s (Context.eq ctx x y);
  Session.assert_term
    s
    (Context.lt ctx (Context.app ctx f [ x ]) (Context.app ctx f [ y ]));
  check_verdict "honeypot mixed W1 x=y ∧ f(x)<f(y)" Session.Unsat (Session.check_sat s)
;;

(* ------------------------------------------------------------------ *)
(* CONTRACT-POISON / I8 firewall: an overflow degrades to unknown, never a verdict, never
   a crash. A coefficient-blowup LIA problem drives the exact-rational engine past native
   int range; the escaping [Rational.Overflow] is caught at the session boundary. *)

let test_overflow_firewall () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let big = Context.int_const ctx max_int in
  (* x >= max_int and 2*x <= max_int: the bound reasoning multiplies coefficients toward
     native-int overflow; whatever the engine does, the session must not crash and must
     return a sound verdict (unsat here, or unknown if it overflows first). *)
  Session.assert_term s (Context.ge ctx x big);
  Session.assert_term s (Context.le ctx (Context.mul_const ctx 2 x) big);
  let v = Session.check_sat s in
  check
    "overflow firewall: sound verdict, never a crash"
    (v = Session.Unsat || v = Session.Unknown)
;;

(* core-bignum W2, dual-review F2: the REAL production R1 degrade paths run through
   [Session.check_sat] -> [Sat.solve] -> the LIA adapter's EAGER model/branch projection
   (the [cdclt]/[combine_models] snapshot inside the theory-driving solve), degrading via
   the CONTRACT-POISON firewall — NOT via [Lia.solve_integer], which the session never
   calls (the lia_test (a)/(b) fixtures drive that declared-unused path). These fixtures
   pin the two R1 int-projection sinks on the path the solver actually takes; both must
   degrade to [Unknown], never a truncated model or a wrong sat/unsat verdict. *)
let test_bignum_r1_session_degrade () =
  (* (i) Big-MODEL SAT: [max_int*x + y <= 0] with [x >= 2] is ℚ-feasible (y unbounded
     below), but the integral model binds y = -2*max_int (Big); projecting it to native
     int at the eager model sink overflows -> firewall -> Unknown (never a truncated
     model). *)
  (let s = Session.create () in
   let ctx = Session.context s in
   let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
   let y = Context.const ctx (Session.declare_const s "y" Sort.int) in
   Session.assert_term s (Context.ge ctx x (Context.int_const ctx 2));
   Session.assert_term
     s
     (Context.le
        ctx
        (Context.add ctx (Context.mul_const ctx max_int x) y)
        (Context.int_const ctx 0));
   check_verdict
     "F2(i): Big-model SAT degrades to Unknown via the eager-projection firewall"
     Session.Unknown
     (Session.check_sat s));
  (* (ii) Big B&B BRANCH-BOUND: pin x0=0; promote x1 = x0+min_int = -2^62 and x2 =
     x1+min_int = -2^63; then 2*x3 + 1 = x2, so the ℚ relaxation binds x3 = -(2^63+1)/2, a
     Big non-integer. B&B branches on x3 and floors it (< min_int, exceeds int63) -> the
     adapter guard catches the projection Overflow -> firewall -> Unknown (never a
     truncated bound). Session mirror of lia_test's fixture (b), exercising
     [suggest_branch]. *)
  let s = Session.create () in
  let ctx = Session.context s in
  let mkv name = Context.const ctx (Session.declare_const s name Sort.int) in
  let x0 = mkv "x0" in
  let x1 = mkv "x1" in
  let x2 = mkv "x2" in
  let x3 = mkv "x3" in
  let ic k = Context.int_const ctx k in
  Session.assert_term s (Context.eq ctx x0 (ic 0));
  Session.assert_term s (Context.eq ctx (Context.add ctx x0 (ic min_int)) x1);
  Session.assert_term s (Context.eq ctx (Context.add ctx x1 (ic min_int)) x2);
  Session.assert_term
    s
    (Context.eq ctx (Context.add ctx (Context.mul_const ctx 2 x3) (ic 1)) x2);
  check_verdict
    "F2(ii): Big B&B branch-bound degrades to Unknown via the adapter guard"
    Session.Unknown
    (Session.check_sat s)
;;

(* get_model on a pure-Boolean sat returns a Bool value per propositional variable. *)

let test_get_model_bool () =
  let s = Session.create () in
  let ctx = Session.context s in
  let p = Context.const ctx (Session.declare_const s "p" Sort.bool) in
  let q = Context.const ctx (Session.declare_const s "q" Sort.bool) in
  Session.assert_term s (Context.or_ ctx [ p; q ]);
  Session.assert_term s (Context.not_ ctx q);
  (match Session.check_sat s, Session.get_model s with
   | Session.Sat, Some (_, m) ->
     let find n =
       List.find_map
         (function
           | Session.Const (k, v) -> if k = n then Some v else None
           | Session.Fun _ -> None)
         m
     in
     check "model has p and q" (List.length m = 2);
     check "q is false" (find "q" = Some (Session.VBool false));
     check "p is true (forced by (or p q) ∧ ¬q)" (find "p" = Some (Session.VBool true))
   | v, _ -> check ("expected sat with model, got " ^ verdict_str v) false);
  Session.assert_term s (Context.not_ ctx p);
  check_verdict "now unsat" Session.Unsat (Session.check_sat s);
  check "no model after unsat" (Session.get_model s = None)
;;

(* A mixed Boolean/theory Sat must expose BOTH the Boolean propositional variables (owned
   by the SAT core, never in the theory snapshot) AND the theory constants, or the §8
   evaluator rejects the model as omitting a declared symbol (codex HIGH: bool consts
   missing from the mixed model). *)

let test_mixed_bool_theory_model () =
  let s = Session.create () in
  let ctx = Session.context s in
  let p = Context.const ctx (Session.declare_const s "p" Sort.bool) in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  Session.assert_term s p;
  Session.assert_term s (Context.eq ctx x (Context.int_const ctx 0));
  match Session.check_sat s, Session.get_model s with
  | Session.Sat, Some (_, m) ->
    let find n =
      List.find_map
        (function
          | Session.Const (k, v) -> if k = n then Some v else None
          | Session.Fun _ -> None)
        m
    in
    check
      "mixed model includes Bool const p (=true)"
      (find "p" = Some (Session.VBool true));
    check "mixed model includes Int const x (=0)" (find "x" = Some (Session.VInt 0));
    check "mixed model has exactly p and x" (List.length m = 2)
  | v, _ -> check ("mixed bool/theory: expected sat+model, got " ^ verdict_str v) false
;;

(* UF-models (ADR-UF-models): a QF_UF query that needs a FUNCTION TABLE now flips unknown
   -> sat with a self-checked model (R1 in-process checker gates the promotion). euf_sat:
   (distinct a b) ∧ (f a = f b) — SAT (a,b distinct, f collapses them). The model must
   carry a sort cardinality for S and a table for f; a,b get distinct elements and f maps
   both to one element. The R1 checker having passed is implied by the [Sat]. *)
let test_uf_function_model () =
  let s = Session.create () in
  let ctx = Session.context s in
  let ssort = Sort.uninterpreted (Session.declare_sort s "S") in
  let f = Session.declare_fun s "f" (Rank.create [ ssort ] ssort) in
  let a = Context.const ctx (Session.declare_fun s "a" (Rank.create [] ssort)) in
  let b = Context.const ctx (Session.declare_fun s "b" (Rank.create [] ssort)) in
  Session.assert_term s (Context.not_ ctx (Context.eq ctx a b));
  Session.assert_term
    s
    (Context.eq ctx (Context.app ctx f [ a ]) (Context.app ctx f [ b ]));
  match Session.check_sat s, Session.get_model s with
  | Session.Sat, Some (sorts, bindings) ->
    check
      "UF model: S has cardinality >= 2 (a,b distinct)"
      (List.exists
         (fun { Session.sort_name; card } -> sort_name = "S" && card >= 2)
         sorts);
    check
      "UF model: f has a function table"
      (List.exists
         (function
           | Session.Fun ("f", _) -> true
           | _ -> false)
         bindings);
    (* a,b are distinct elements; f collapses them to one *)
    let fa_fb_equal =
      List.exists
        (function
          | Session.Fun ("f", { cases; _ }) ->
            (match List.map snd cases with
             | [ r1; r2 ] -> r1 = r2
             | _ -> false)
          | _ -> false)
        bindings
    in
    check "UF model: f(a) = f(b) in the table (both cases → one result)" fa_fb_equal
  | v, _ -> check ("UF function model: expected sat+model, got " ^ verdict_str v) false
;;

(* Reserved preprocessing witnesses ([.oxsmt.*], here an ITE lift) must not leak into the
   external model — they name no user symbol, so the §8 evaluator would reject them (codex
   HIGH: witnesses leak). *)

let test_model_excludes_witnesses () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let ite =
    Context.ite
      ctx
      (Context.gt ctx x (Context.int_const ctx 0))
      x
      (Context.int_const ctx 0)
  in
  Session.assert_term s (Context.eq ctx ite (Context.int_const ctx 1));
  let reserved n = String.length n >= 7 && String.equal (String.sub n 0 7) ".oxsmt." in
  match Session.check_sat s, Session.get_model s with
  | Session.Sat, Some (_, m) ->
    let name_of = function
      | Session.Const (n, _) -> n
      | Session.Fun (n, _) -> n
    in
    check
      "model excludes .oxsmt.* witnesses"
      (List.for_all (fun b -> not (reserved (name_of b))) m);
    check "model still names x" (List.exists (fun b -> name_of b = "x") m)
  | v, _ -> check ("ite witness: expected sat+model, got " ^ verdict_str v) false
;;

(* Split-budget exhaustion (W-2): 2x=1 forces a branch-and-bound split; a budget of 0
   refuses the first split → sound [Unknown] with [budget_exhausted], and the session
   stays degraded (sticky). Drives the exact budget firewall path deterministically. *)

let test_split_budget_exhaustion () =
  let s = Session.create ~split_budget:0 () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  Session.assert_term
    s
    (Context.eq ctx (Context.mul_const ctx 2 x) (Context.int_const ctx 1));
  check_verdict "budget 0: 2x=1 -> unknown" Session.Unknown (Session.check_sat s);
  check "budget_exhausted flag set" (Session.budget_exhausted s);
  check "no model after budget unknown" (Session.get_model s = None);
  (* sticky: a later check stays unknown even with a further (feasible) assertion. *)
  Session.assert_term s (Context.ge ctx x (Context.int_const ctx 0));
  check_verdict "budget degrade is sticky" Session.Unknown (Session.check_sat s)
;;

(* ------------------------------------------------------------------ *)
(* Board #60: the deterministic COUNTED effort budget (effort = SAT conflicts + decisions
   + seam Final-rounds). A per-check, poison-free cap that turns an unfinished search into
     [Unknown] with the BUDGET tag — never a sat/unsat from an unfinished search. Distinct
     from the split budget above: NOT sticky, does not degrade the session. *)

(* A mixed QF_UFLIA UNSAT probe that drives real search (a decision + a Nelson-Oppen Final
   split): x = y+1 ∧ y = 0 ∧ g(x) ≠ g(1). Reused across the budget tests so the terminal
   effort is a fixed, strictly-positive target to calibrate caps against. *)
let build_budget_probe s =
  let ctx = Session.context s in
  let g = Session.declare_fun s "g" (Rank.create [ Sort.int ] Sort.int) in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let y = Context.const ctx (Session.declare_const s "y" Sort.int) in
  Session.assert_term s (Context.eq ctx x (Context.add ctx y (Context.int_const ctx 1)));
  Session.assert_term s (Context.eq ctx y (Context.int_const ctx 0));
  Session.assert_term
    s
    (Context.not_
       ctx
       (Context.eq
          ctx
          (Context.app ctx g [ x ])
          (Context.app ctx g [ Context.int_const ctx 1 ])))
;;

let test_effort_unbounded_matches () =
  (* Default (no [max_effort]) is unbounded: the counter runs but never cuts off, so the
     verdict equals a build with no budget. Confirmed here on a small sample; the whole-
     suite byte-identity ([make test]) is the stronger check. *)
  let unbounded build =
    let s = Session.create () in
    build s;
    Session.check_sat s
  in
  let x_pos s =
    let ctx = Session.context s in
    let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
    Session.assert_term s (Context.gt ctx x (Context.int_const ctx 0))
  in
  check_verdict "unbounded: mixed probe" Session.Unsat (unbounded build_budget_probe);
  check_verdict "unbounded: x>0 sat" Session.Sat (unbounded x_pos);
  let s = Session.create () in
  build_budget_probe s;
  let _ = Session.check_sat s in
  check "unbounded run records effort > 0" (Session.effort s > 0);
  check "unbounded run never flags BUDGET" (not (Session.effort_exhausted s))
;;

let test_effort_budget_exhaustion () =
  (* Self-calibrate: measure the probe's real terminal effort E (unbounded); a cap of E-1
     must cut the search off (BUDGET), a cap of E must still decide it. Ticking never
     changes the search, so the event sequence up to the E-th tick is identical either way
     — hence exhaustion at E-1 and success at E are both deterministic. *)
  let effort_of () =
    let s = Session.create () in
    build_budget_probe s;
    let _ = Session.check_sat s in
    Session.effort s
  in
  let e = effort_of () in
  check "probe consumes some effort" (e > 0);
  let s = Session.create ~max_effort:(e - 1) () in
  build_budget_probe s;
  check_verdict "cap E-1: BUDGET unknown" Session.Unknown (Session.check_sat s);
  check "effort_exhausted set" (Session.effort_exhausted s);
  check "not the (sticky) split-budget path" (not (Session.budget_exhausted s));
  check "no model after BUDGET unknown" (Session.get_model s = None);
  (* Poison-free / NOT sticky: a BUDGET cutoff never latches the session to [Unknown]
     (contrast test_split_budget_exhaustion, whose degrade is sticky and
     forever-[Unknown]). A second check on the SAME session re-counts effort from zero
     (per-check reset) and re-runs the search — and because the SAT core keeps its learned
     clauses across checks (incrementality), that second pass prunes enough to finish
     UNDER the same cap here, solving to [Unsat]. The invariants that matter and hold
     regardless: the re-run yields a SOUND verdict (never a [Sat] from an unfinished
     search) and NEVER takes the sticky split-budget path. (Measurement is immune to this
     cross-check effect: corpus_classify uses a fresh session per file.) *)
  let v_rerun = Session.check_sat s in
  check
    "re-run is sound (unsat or budget-unknown, never sat)"
    (v_rerun = Session.Unsat || v_rerun = Session.Unknown);
  check
    "re-run never took the sticky split-budget path"
    (not (Session.budget_exhausted s));
  check "re-run re-counted effort (per-check reset, not frozen)" (Session.effort s > 0);
  (* Raise N and retry: the SAME query at cap = E is decided — BUDGET is never a verdict. *)
  let s_ok = Session.create ~max_effort:e () in
  build_budget_probe s_ok;
  check_verdict
    "cap E: real verdict (re-runnable at larger N)"
    Session.Unsat
    (Session.check_sat s_ok);
  check "cap E: not exhausted" (not (Session.effort_exhausted s_ok))
;;

let test_effort_determinism () =
  (* The load-bearing calibration claim: effort is a deterministic function of the input.
     Two runs of the same query report byte-identical effort AND verdict — unbounded, and
     capped just below E (identical (Unknown, effort) both times). *)
  let run max_effort =
    let s =
      match max_effort with
      | None -> Session.create ()
      | Some m -> Session.create ~max_effort:m ()
    in
    build_budget_probe s;
    let v = Session.check_sat s in
    v, Session.effort s
  in
  let v1, e1 = run None in
  let v2, e2 = run None in
  check "determinism: same verdict (unbounded)" (v1 = v2 && v1 = Session.Unsat);
  check "determinism: byte-identical effort (unbounded)" (e1 = e2);
  let cap = e1 - 1 in
  let w1, f1 = run (Some cap) in
  let w2, f2 = run (Some cap) in
  check "determinism: same capped verdict" (w1 = w2 && w1 = Session.Unknown);
  check "determinism: byte-identical capped effort" (f1 = f2)
;;

(* ------------------------------------------------------------------ *)
(* Namespace guards (#48), unchanged. *)

let test_namespace_guard () =
  let s = Session.create () in
  check_raises "session rejects .oxsmt. const" (fun () ->
    Session.declare_const s ".oxsmt.sneaky" Sort.int);
  check_raises "session rejects .oxsmt. fun" (fun () ->
    Session.declare_fun s ".oxsmt.f" (Rank.create [ Sort.int ] Sort.int));
  check_raises "session rejects .oxsmt. sort" (fun () ->
    Session.declare_sort s ".oxsmt.S");
  check
    "normal declaration still allowed"
    (match Session.declare_const s "ok" Sort.int with
     | _ -> true
     | exception _ -> false);
  check_raises "parser rejects .oxsmt. declare-const" (fun () ->
    Parser.parse "(declare-const .oxsmt.x Int)(assert (= .oxsmt.x .oxsmt.x))(check-sat)");
  check_raises "parser rejects .oxsmt. declare-fun" (fun () ->
    Parser.parse "(declare-fun .oxsmt.g (Int) Int)(check-sat)");
  check_raises "parser rejects .oxsmt. declare-sort" (fun () ->
    Parser.parse "(declare-sort .oxsmt.S 0)(check-sat)");
  check
    "parser accepts normal declaration"
    (match Parser.parse "(declare-const ok Bool)(assert ok)(check-sat)" with
     | _ -> true
     | exception _ -> false)
;;

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
  check_verdict "parsed unsat formula" Session.Unsat (Session.check_sat s)
;;

(* Determinism (I6): the same query yields the same verdict every run. *)

let test_determinism () =
  let solve () =
    let s = Session.create () in
    let ctx = Session.context s in
    let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
    let y = Context.const ctx (Session.declare_const s "y" Sort.int) in
    Session.assert_term s (Context.le ctx (Context.add ctx x y) (Context.int_const ctx 2));
    Session.assert_term s (Context.ge ctx x (Context.int_const ctx 2));
    Session.assert_term s (Context.ge ctx y (Context.int_const ctx 1));
    Session.check_sat s
  in
  check
    "determinism: identical verdict across runs"
    (solve () = solve () && solve () = Session.Unsat)
;;

(* CLI-level regression (wiring fix round 2). THE HIGH-3 fix (quoting model symbol names
   via the SMT-LIB printer) regressed the printer-REFUSED subset: the empty symbol [||]
   and a predefined-operator collision like [|+|] make [Printer.quote_symbol] raise
   [Printer.Unsupported], which — before the fix — escaped [render_model] and aborted the
   CLI (exit 2, no verdict). The refusal is semantically correct (quoting is lexical and
   cannot disambiguate these), so the CLI must CATCH it and degrade that goal to a sound
   [unknown] with no model — never crash, never emit a malformed model. A
   merely-non-simple but representable name ([|a b|]) must still come out sat with a
   quoted model. Drives the built oxsmt_cli.exe (a sibling of this test binary) end-to-end
   via [Sys.command] (stdlib only — no unix dep, matching the dune's I3 note). *)
let contains_substr s sub =
  let ls = String.length s
  and lsub = String.length sub in
  let rec go i = i + lsub <= ls && (String.sub s i lsub = sub || go (i + 1)) in
  lsub = 0 || go 0
;;

let run_cli src =
  let cli = Filename.concat (Filename.dirname Sys.executable_name) "oxsmt_cli.exe" in
  let inp = Filename.temp_file "wiring-cli" ".smt2" in
  let outp = Filename.temp_file "wiring-cli" ".out" in
  let oc = open_out inp in
  output_string oc src;
  close_out oc;
  let code =
    Sys.command
      (Printf.sprintf
         "%s %s > %s 2>/dev/null"
         (Filename.quote cli)
         (Filename.quote inp)
         (Filename.quote outp))
  in
  let out =
    let ic = open_in outp in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () -> In_channel.input_all ic)
  in
  Sys.remove inp;
  Sys.remove outp;
  out, code
;;

let test_cli_refused_symbol_degrades () =
  let check_cli label src ~verdict =
    let out, code = run_cli src in
    check (label ^ ": clean exit (no crash)") (code = 0);
    check (Printf.sprintf "%s: %s" label verdict) (contains_substr out verdict)
  in
  check_cli
    "empty symbol ||"
    "(declare-const || Int)(assert (= || 0))(check-sat)"
    ~verdict:"(verdict unknown)";
  check_cli
    "predefined-op collision |+|"
    "(declare-const |+| Int)(assert (= |+| 0))(check-sat)"
    ~verdict:"(verdict unknown)";
  check_cli
    "representable |a b|"
    "(declare-const |a b| Int)(assert (= |a b| 0))(check-sat)"
    ~verdict:"(verdict sat)";
  let out, _ = run_cli "(declare-const |a b| Int)(assert (= |a b| 0))(check-sat)" in
  check
    "representable |a b|: model carries the quoted name"
    (contains_substr out "(|a b| 0)")
;;

(* A negative Int model value renders as the well-formed SMT-LIB [(- N)] (token_of_value
   strips the leading '-' from [string_of_int] rather than negating — so [min_int] cannot
   overflow into the malformed [(- -N)]; mirrors the shipped printer's [add_int_lit]). The
   reachable path is pinned here end-to-end via the real CLI; [min_int] itself is a
   boundary the solver never models and the eval reader cannot reingest (both overflow),
   so it is correctness-by-mirroring-the-printer, not a round-trippable case (see report). *)
let test_cli_negative_int_token () =
  let out, code = run_cli "(declare-const y Int)(assert (= y (- 5)))(check-sat)" in
  check "negative-int CLI: clean exit" (code = 0);
  check "negative-int CLI: renders (- 5)" (contains_substr out "(- 5)");
  check "negative-int CLI: no malformed double-minus" (not (contains_substr out "(- -"))
;;

(* ------------------------------------------------------------------ *)
(* ADR-0010 §6 acceptance corpus, at the SESSION level (end-to-end through the
   internalization Combine + EUF + LIA stack). Two verdict regimes to keep straight:
   - UNSAT repros pass through unchanged (Unsat needs no model, sound regardless of
     functions) — these are the wrong-SAT bug family the internalization combinator now
     DECIDES (the solved-rate mover).
   - A theory-SAT whose model needs a function table is degraded to Unknown by THE
     SOUNDNESS RULE (self-checkable-model firewall), and the Bool buried/compound shapes
     degrade to Unknown via [Combine.Incomplete] (§3.6). So a combine-level "SAT with
     functions" or "Incomplete" reads as Session-level Unknown here — sound by design. *)

let uf1 s name = Session.declare_fun s name (Rank.create [ Sort.int ] Sort.int)

let test_adr0010_unsat_repros () =
  (* Nested tower: x=y ∧ g(f(x))<g(f(y)) → UNSAT (congruence up the tower). *)
  let s = Session.create () in
  let ctx = Session.context s in
  let f = uf1 s "f"
  and g = uf1 s "g" in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let y = Context.const ctx (Session.declare_const s "y" Sort.int) in
  let app1 fn a = Context.app ctx fn [ a ] in
  Session.assert_term s (Context.eq ctx x y);
  Session.assert_term s (Context.lt ctx (app1 g (app1 f x)) (app1 g (app1 f y)));
  check_verdict "tower x=y ∧ g(f(x))<g(f(y))" Session.Unsat (Session.check_sat s);
  (* R1 (codex round-7): x=0 ∧ f(x+1)<f(1) → UNSAT (x+1 and 1 are shared boundary nodes). *)
  let s = Session.create () in
  let ctx = Session.context s in
  let f = uf1 s "f" in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let one = Context.int_const ctx 1 in
  Session.assert_term s (Context.eq ctx x (Context.int_const ctx 0));
  Session.assert_term
    s
    (Context.lt
       ctx
       (Context.app ctx f [ Context.add ctx x one ])
       (Context.app ctx f [ one ]));
  check_verdict "R1 x=0 ∧ f(x+1)<f(1)" Session.Unsat (Session.check_sat s);
  (* Second dual-leg repro: x=y ∧ f(x+1)<f(y+1) → UNSAT. *)
  let s = Session.create () in
  let ctx = Session.context s in
  let f = uf1 s "f" in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let y = Context.const ctx (Session.declare_const s "y" Sort.int) in
  let one = Context.int_const ctx 1 in
  Session.assert_term s (Context.eq ctx x y);
  Session.assert_term
    s
    (Context.lt
       ctx
       (Context.app ctx f [ Context.add ctx x one ])
       (Context.app ctx f [ Context.add ctx y one ]));
  check_verdict "f(x+1)<f(y+1) with x=y" Session.Unsat (Session.check_sat s);
  (* Numeral corner: x=1 ∧ f(x)≠f(1) → UNSAT (numeral 1 under f is a boundary node). *)
  let s = Session.create () in
  let ctx = Session.context s in
  let f = uf1 s "f" in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let one = Context.int_const ctx 1 in
  Session.assert_term s (Context.eq ctx x one);
  Session.assert_term
    s
    (Context.not_
       ctx
       (Context.eq ctx (Context.app ctx f [ x ]) (Context.app ctx f [ one ])));
  check_verdict "numeral-corner x=1 ∧ f(x)≠f(1)" Session.Unsat (Session.check_sat s)
;;

(* Use-history transition (invariant (ii)): x≤0 ∧ x≥0 is pure-LIA SAT (x=0, function-free
   → self-checkable → real Sat); adding f(x)≠f(0) makes x a both-used interface member and
   the query flips to UNSAT. Catches a memoized "x not shared" classification. *)
let test_adr0010_use_history () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let zero = Context.int_const ctx 0 in
  Session.assert_term s (Context.le ctx x zero);
  Session.assert_term s (Context.ge ctx x zero);
  check_verdict "use-history: x≤0 ∧ x≥0 (pure LIA)" Session.Sat (Session.check_sat s);
  let f = uf1 s "f" in
  Session.assert_term
    s
    (Context.not_
       ctx
       (Context.eq ctx (Context.app ctx f [ x ]) (Context.app ctx f [ zero ])));
  check_verdict
    "use-history flips to unsat with f(x)≠f(0)"
    Session.Unsat
    (Session.check_sat s)
;;

(* Bool boundary (§3.6, C6 + H2 errata) at the SESSION level. IMPORTANT wiring-level
   distinction from the ADR §6 combine-level fixtures: a bare Bool variable [b] is a
   PROPOSITIONAL variable (a nullary Bool [App]), NOT a theory atom — so the seam
   ({!Cdclt.on_assign}) never forwards its truth value to the combinator (only theory
   atoms are forwarded). From the combinator's view every such [b] under [h(b)] is
   therefore BURIED/UNBOUND — the ADR's "surfaced/bound leaf" precondition (b asserted as
   an atom EUF sees) is not met through the wiring — so the leaf bridge cannot fire and
   the combinator degrades via [Combine.Incomplete]. Consequently ALL
   Bool-leaf/compound-under- UF shapes come out Unknown at the Session level (sound; the
   ADR's UNSAT/SAT leaf verdicts are combine-test-level, where b is asserted directly to
   the combinator). This is a documented wiring completeness gap
   [[wiring-bool-leaf-forwarding]], never a wrong verdict, and it exercises the
   [Incomplete] named-catch. *)
let test_adr0010_bool_boundary () =
  let hb s = Session.declare_fun s "h" (Rank.create [ Sort.bool ] Sort.bool) in
  let neq ctx a b = Context.not_ ctx (Context.eq ctx a b) in
  (* leaf ¬b ∧ h(b)≠h(false): combine-level UNSAT, but at the wiring level b is a
     propositional var not forwarded to the theory → buried → Incomplete → sound Unknown. *)
  let s = Session.create () in
  let ctx = Session.context s in
  let h = hb s in
  let b = Context.const ctx (Session.declare_const s "b" Sort.bool) in
  let hfalse = Context.app ctx h [ Context.bool_const ctx false ] in
  Session.assert_term s (Context.not_ ctx b);
  Session.assert_term s (neq ctx (Context.app ctx h [ b ]) hfalse);
  check_verdict
    "bool leaf ¬b ∧ h(b)≠h(false) (buried at wiring → unknown)"
    Session.Unknown
    (Session.check_sat s);
  (* leaf b ∧ h(b)≠h(false): likewise b is not forwarded → Unknown (buried; never Sat). *)
  let s = Session.create () in
  let ctx = Session.context s in
  let h = hb s in
  let b = Context.const ctx (Session.declare_const s "b" Sort.bool) in
  let hfalse = Context.app ctx h [ Context.bool_const ctx false ] in
  Session.assert_term s b;
  Session.assert_term s (neq ctx (Context.app ctx h [ b ]) hfalse);
  check_verdict
    "bool leaf b ∧ h(b)≠h(false) (buried at wiring → unknown)"
    Session.Unknown
    (Session.check_sat s);
  (* buried H2: h(b)≠h(true) ∧ h(b)≠h(false) → UNKNOWN (b never surfaces; Incomplete). *)
  let s = Session.create () in
  let ctx = Session.context s in
  let h = hb s in
  let b = Context.const ctx (Session.declare_const s "b" Sort.bool) in
  Session.assert_term
    s
    (neq
       ctx
       (Context.app ctx h [ b ])
       (Context.app ctx h [ Context.bool_const ctx true ]));
  Session.assert_term
    s
    (neq
       ctx
       (Context.app ctx h [ b ])
       (Context.app ctx h [ Context.bool_const ctx false ]));
  check_verdict
    "bool buried H2 h(b)≠h(true) ∧ h(b)≠h(false)"
    Session.Unknown
    (Session.check_sat s);
  (* structured compound under a UF arg: ¬b ∧ h(b∧c)≠h(false) → UNKNOWN (degrade at walk). *)
  let s = Session.create () in
  let ctx = Session.context s in
  let h = hb s in
  let b = Context.const ctx (Session.declare_const s "b" Sort.bool) in
  let c = Context.const ctx (Session.declare_const s "c" Sort.bool) in
  let hfalse = Context.app ctx h [ Context.bool_const ctx false ] in
  Session.assert_term s (Context.not_ ctx b);
  Session.assert_term s (neq ctx (Context.app ctx h [ Context.and_ ctx [ b; c ] ]) hfalse);
  check_verdict
    "bool compound ¬b ∧ h(b∧c)≠h(false) (Incomplete degrade)"
    Session.Unknown
    (Session.check_sat s)
;;

(* Regression (codex HIGH, task #110 fix round): the R1 checker's [mul_ovf] must fail
   CLOSED on the [min_int * -1] wrap. [-min_int] wraps back to [min_int] and
   [min_int / -1] wraps too, so a wrapped product slips past the quotient check — a false
   [true] that silently defeats the fail-closed TCB guard. No solver path models
   [min_int], so this is unreachable end-to-end (hence a DIRECT Model_check call, not a
   CLI drive): a model binding x = min_int and the assertion [-x = min_int]. Evaluating
   [-x] must raise inside the checker so the assertion fails closed and check = false (NOT
   the wrap-true the old guard returned). Discriminating: the pre-fix guard returns true
   here. *)
let test_model_check_min_int_guard () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let assertion = Context.eq ctx (Context.neg ctx x) (Context.int_const ctx min_int) in
  let model = [], [ Session.Const ("x", Session.VInt min_int) ] in
  check
    "Model_check min_int*-1 guard: -x = min_int fails closed (not a wrap-true)"
    (not (Oxsmt_interface.Model_check.check model [ assertion ]))
;;

(* Task #117 guard parity: {!Cdclt}'s §10-v2 gap-B structural Arith fold uses its own copy
   of the overflow-guarded add/mul; R1 ({!Model_check}) re-folds every table key with its
   own copy. If the two ever diverge on an overflow edge, R1 rejects a key the extractor
   computed differently — a valid model gratuitously degrades to [unknown]. Pin them EQUAL
   over an edge matrix that includes the [min_int * -1] / [-1 * min_int] wrap (the clause
   a bare quotient check misses) and the additive-overflow corners. [Cdclt] returns
   [int option] (None = overflow -> Degrade); [Model_check] raises on overflow (-> Bad ->
   the assertion fails closed); normalize both to [int option] and require agreement. *)
let test_ovf_guard_parity () =
  let mc_opt f a b =
    try Some (f a b) with
    | _ -> None
  in
  let edges = [ 0; 1; -1; 2; -2; 7; max_int; min_int; max_int - 1; min_int + 1 ] in
  List.iter
    (fun a ->
       List.iter
         (fun b ->
            check
              (Printf.sprintf "add_ovf parity a=%d b=%d" a b)
              (Oxsmt_interface.Cdclt.add_ovf a b
               = mc_opt Oxsmt_interface.Model_check.add_ovf a b);
            check
              (Printf.sprintf "mul_ovf parity a=%d b=%d" a b)
              (Oxsmt_interface.Cdclt.mul_ovf a b
               = mc_opt Oxsmt_interface.Model_check.mul_ovf a b))
         edges)
    edges;
  (* Discriminating spot-checks: the min_int wrap the quotient check alone would MISS must
     be rejected (None) by both, and a normal product must survive. *)
  check "mul_ovf min_int*-1 -> None" (Oxsmt_interface.Cdclt.mul_ovf min_int (-1) = None);
  check "mul_ovf -1*min_int -> None" (Oxsmt_interface.Cdclt.mul_ovf (-1) min_int = None);
  check
    "add_ovf min_int+min_int -> None"
    (Oxsmt_interface.Cdclt.add_ovf min_int min_int = None);
  check "mul_ovf 6*7 = 42" (Oxsmt_interface.Cdclt.mul_ovf 6 7 = Some 42);
  check "add_ovf 6+7 = 13" (Oxsmt_interface.Cdclt.add_ovf 6 7 = Some 13)
;;

(* ------------------------------------------------------------------ *)
(* W1b equality-elimination presolve (logs/w1b-design.md). The batch [assert_presolved]
   path: verdict soundness in both directions, first-wins / cycle / conditional guards,
   the interface-variable no-op, model reconstruction of eliminated variables (R1),
   neutrality, and determinism. These are also the ORACLES the registry presolve mutants
   (tools/mutants/registry, module=presolve) go red against. *)

let find_int_in_model m n =
  List.find_map
    (function
      | Session.Const (k, Session.VInt v) when k = n -> Some v
      | _ -> None)
    m
;;

(* Alias CHAIN: x = y, y = 5, x >= 3. Both x and y are eliminated (x resolves through y to
   5); the retained bound becomes 5 >= 3 = true. Sat, and the reconstructed model must
   bind BOTH eliminated variables to 5 (R1 evaluates the ORIGINAL assertions). *)
let test_presolve_alias_chain () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let y = Context.const ctx (Session.declare_const s "y" Sort.int) in
  Session.assert_presolved
    s
    [ Context.eq ctx x y
    ; Context.eq ctx y (Context.int_const ctx 5)
    ; Context.ge ctx x (Context.int_const ctx 3)
    ];
  (match Session.check_sat s, Session.get_model s with
   | Session.Sat, Some (_, m) ->
     check "chain: x re-derived to 5" (find_int_in_model m "x" = Some 5);
     check "chain: y re-derived to 5" (find_int_in_model m "y" = Some 5)
   | v, _ -> check ("chain: expected sat+model, got " ^ verdict_str v) false);
  check
    "chain: both x and y eliminated"
    (List.sort compare (Session.eliminated_vars s) = [ "x"; "y" ])
;;

(* CYCLE guard: x = y, y = x, x <= 0. The first alias eliminates x -> y; the second closes
   a cycle, so it is NOT eliminated (kept as an ordinary — trivially true — constraint).
   Sound Sat; only x eliminated. A missing cycle guard would add y -> x and loop. *)
let test_presolve_cycle_guard () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let y = Context.const ctx (Session.declare_const s "y" Sort.int) in
  Session.assert_presolved
    s
    [ Context.eq ctx x y; Context.eq ctx y x; Context.le ctx x (Context.int_const ctx 0) ];
  check_verdict "cycle: sat" Session.Sat (Session.check_sat s);
  check "cycle: only x eliminated (no loop)" (Session.eliminated_vars s = [ "x" ]);
  match Session.get_model s with
  | Some (_, m) ->
    check "cycle: model binds x" (find_int_in_model m "x" <> None);
    check "cycle: model binds y" (find_int_in_model m "y" <> None)
  | None -> check "cycle: expected a model" false
;;

(* SHADOWED alias (first-wins): x = 5 then x = 6. The first defines x; the second is
   retained and rewritten to 5 = 6 = false -> Unsat. A last-wins bug would keep x = 6. *)
let test_presolve_shadowed_alias () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  Session.assert_presolved
    s
    [ Context.eq ctx x (Context.int_const ctx 5)
    ; Context.eq ctx x (Context.int_const ctx 6)
    ];
  check_verdict "shadowed: first-wins -> unsat" Session.Unsat (Session.check_sat s);
  check "shadowed: x eliminated once" (Session.eliminated_vars s = [ "x" ])
;;

(* MODEL RECONSTRUCTION / R1: x = 5, x <= 10. x is eliminated; the reduced set is 5 <= 10
   = true. The reported Sat REQUIRES the model to bind x = 5 or R1 (which evaluates the
   original (= x 5)) rejects it -> the wrong-re-derivation mutant surfaces here as a sat
   -> unknown flip. *)
let test_presolve_model_r1 () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  Session.assert_presolved
    s
    [ Context.eq ctx x (Context.int_const ctx 5)
    ; Context.le ctx x (Context.int_const ctx 10)
    ];
  match Session.check_sat s, Session.get_model s with
  | Session.Sat, Some (_, m) ->
    check "R1: eliminated x present in model as 5" (find_int_in_model m "x" = Some 5)
  | v, _ -> check ("R1: expected sat+model (re-derivation), got " ^ verdict_str v) false
;;

(* CONDITIONAL equality is NOT a definition: (or (= x 5) (= x 6)) /\ x >= 6. The
   equalities live under Or, so nothing is eliminated; the answer is Sat (x = 6). A mutant
   that descended into Or would eliminate x -> 5, drop the disjunction, and flip to Unsat
   (5 >= 6). Both the verdict and the empty elimination set are oracles. *)
let test_presolve_conditional_no_elim () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  Session.assert_presolved
    s
    [ Context.or_
        ctx
        [ Context.eq ctx x (Context.int_const ctx 5)
        ; Context.eq ctx x (Context.int_const ctx 6)
        ]
    ; Context.ge ctx x (Context.int_const ctx 6)
    ];
  check "conditional: nothing eliminated" (Session.eliminated_vars s = []);
  check_verdict "conditional: sat (x=6)" Session.Sat (Session.check_sat s)
;;

(* A non-definition assert is NEVER dropped: x = 5 /\ x >= 10. x is eliminated but the
   retained bound rewrites to 5 >= 10 = false -> Unsat. A mutant that dropped the retained
   conjunct would flip to Sat. *)
let test_presolve_dropped_nondef () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  Session.assert_presolved
    s
    [ Context.eq ctx x (Context.int_const ctx 5)
    ; Context.ge ctx x (Context.int_const ctx 10)
    ];
  check_verdict
    "dropped-nondef: retained bound kept -> unsat"
    Session.Unsat
    (Session.check_sat s)
;;

(* INTERFACE-VARIABLE no-op (ADR-0010 §3.1). QF_UFLIA: x = 5 /\ (g x) <> (g 5). x occurs
   UNDER the uninterpreted function g, so it must NOT be eliminated — leaving x = 5 for
   the theory, congruence gives (g x) = (g 5), contradicting the disequality -> Unsat. The
   guard is the oracle: a mutant that eliminated the interface variable would report x in
   [eliminated_vars]. (The verdict stays Unsat either way — constant propagation is sound
   — so the STRUCTURAL check is what discriminates.) *)
let test_presolve_interface_noop () =
  let s = Session.create () in
  let ctx = Session.context s in
  let g = Session.declare_fun s "g" (Rank.create [ Sort.int ] Sort.int) in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  Session.assert_presolved
    s
    [ Context.eq ctx x (Context.int_const ctx 5)
    ; Context.distinct
        ctx
        [ Context.app ctx g [ x ]; Context.app ctx g [ Context.int_const ctx 5 ] ]
    ];
  check "interface: under-UF variable NOT eliminated" (Session.eliminated_vars s = []);
  check_verdict "interface: unsat (congruence)" Session.Unsat (Session.check_sat s)
;;

(* NEUTRALITY: a zero-alias input eliminates nothing and returns Sat, identical to the
   per-term [assert_term] path. *)
let test_presolve_neutral () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  Session.assert_presolved
    s
    [ Context.ge ctx x (Context.int_const ctx 0)
    ; Context.le ctx x (Context.int_const ctx 10)
    ];
  check "neutral: nothing eliminated" (Session.eliminated_vars s = []);
  check_verdict "neutral: sat" Session.Sat (Session.check_sat s)
;;

(* DETERMINISM (I6): the same batch presolved twice yields the same verdict, the same
   elimination order, and the same effort count. *)
let test_presolve_determinism () =
  let run () =
    let s = Session.create () in
    let ctx = Session.context s in
    let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
    let y = Context.const ctx (Session.declare_const s "y" Sort.int) in
    Session.assert_presolved
      s
      [ Context.eq ctx x y
      ; Context.eq ctx y (Context.int_const ctx 5)
      ; Context.ge ctx x (Context.int_const ctx 3)
      ];
    let v = Session.check_sat s in
    v, Session.eliminated_vars s, Session.effort s
  in
  let a = run () in
  let b = run () in
  check "presolve determinism: identical (verdict, order, effort)" (a = b)
;;

(* Direct {!Presolve.run} structural oracles (the transform in isolation): a conditional
   equality yields no defs; an under-UF variable is skipped; a plain top-level alias is
   taken. *)
let test_presolve_run_direct () =
  let s = Session.create () in
  let ctx = Session.context s in
  let g = Session.declare_fun s "g" (Rank.create [ Sort.int ] Sort.int) in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let names r =
    List.map (fun (d : Oxsmt_interface.Presolve.def) -> d.Oxsmt_interface.Presolve.name) r
  in
  let r_plain =
    Oxsmt_interface.Presolve.run ctx [ Context.eq ctx x (Context.int_const ctx 5) ]
  in
  check "run: plain alias taken" (names r_plain.Oxsmt_interface.Presolve.defs = [ "x" ]);
  let r_cond =
    Oxsmt_interface.Presolve.run
      ctx
      [ Context.or_
          ctx
          [ Context.eq ctx x (Context.int_const ctx 5)
          ; Context.eq ctx x (Context.int_const ctx 6)
          ]
      ]
  in
  check "run: conditional equality not a def" (r_cond.Oxsmt_interface.Presolve.defs = []);
  let r_uf =
    Oxsmt_interface.Presolve.run
      ctx
      [ Context.eq ctx x (Context.int_const ctx 5)
      ; Context.le ctx (Context.app ctx g [ x ]) (Context.int_const ctx 0)
      ]
  in
  check "run: under-UF variable skipped" (r_uf.Oxsmt_interface.Presolve.defs = [])
;;

(* codex H1 == same-model F1 (both legs, independently). Substitution composes
   coefficients through the arithmetic smart constructors, so an alias inlined into a
   huge-coefficient term overflows int63. That MUST degrade to a clean [Unknown]
   (assert_term's I8 discipline), NOT escape [assert_presolved] as a crash. Two triggers,
   one per leg — both on the NON-NORMALIZING Eq/Arith path (an inequality would be
   gcd-tightened at build and not overflow, so it would not discriminate):
   - codex, COEFFICIENT composition (mul): x = 1e9, then 1e12·x = 3·w -> 1e21 on
     substitution. *)
let test_presolve_overflow_coeff_degrades () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let w = Context.const ctx (Session.declare_const s "w" Sort.int) in
  (match
     Session.assert_presolved
       s
       [ Context.eq ctx x (Context.int_const ctx 1_000_000_000)
       ; Context.eq
           ctx
           (Context.mul_const ctx 1_000_000_000_000 x)
           (Context.mul_const ctx 3 w)
       ]
   with
   | () -> ()
   | exception e ->
     check ("coeff overflow raised " ^ Printexc.to_string e ^ " (want unknown)") false);
  check_verdict
    "overflow(coeff): degrades to unknown"
    Session.Unknown
    (Session.check_sat s)
;;

(* - reviewer, CONSTANT composition (add; not gcd-divided): y = max_int, x = y + 1, x >= 0
     -> max_int + 1 on substitution. *)
let test_presolve_overflow_const_degrades () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let y = Context.const ctx (Session.declare_const s "y" Sort.int) in
  (match
     Session.assert_presolved
       s
       [ Context.eq ctx y (Context.int_const ctx max_int)
       ; Context.eq ctx x (Context.add ctx y (Context.int_const ctx 1))
       ; Context.ge ctx x (Context.int_const ctx 0)
       ]
   with
   | () -> ()
   | exception e ->
     check ("const overflow raised " ^ Printexc.to_string e ^ " (want unknown)") false);
  check_verdict
    "overflow(const): degrades to unknown"
    Session.Unknown
    (Session.check_sat s)
;;

(* codex H2 (DOC-ONLY ruling, same-model adjudication): substituting a variable by an
   equal term is equisatisfiable IN ANY THEORY, so eliminating an e-graph-member variable
   is sound even for QF_UFLIA (the interface guard is defense-in-depth, not a soundness
   gate), and R1 gates every reported Sat. This pins that (= x (f a)) /\ (= x 5) — where x
   IS eliminated (x is not syntactically under a UF, so the under-UF guard does not flag
   it) — is verdict-sound with a valid R1-checked model (f a = 5, x = 5). *)
let test_presolve_eq_uf_side_sound () =
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" (Rank.create [ Sort.int ] Sort.int) in
  let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  Session.assert_presolved
    s
    [ Context.eq ctx x (Context.app ctx f [ a ])
    ; Context.eq ctx x (Context.int_const ctx 5)
    ];
  (* A reported Sat has already passed the R1 in-process check over the ORIGINAL
     assertions ((= x (f a)) and (= x 5)); the model must bind x = 5. Whether x is
     eliminated is an optimization detail, not asserted — the point is verdict-soundness. *)
  match Session.check_sat s, Session.get_model s with
  | Session.Sat, Some (_, m) ->
    check "eq-uf-side: R1-checked model binds x = 5" (find_int_in_model m "x" = Some 5)
  | v, _ -> check ("eq-uf-side: expected sat+valid model, got " ^ verdict_str v) false
;;

(* codex M1: an eliminated Int def can carry a NON-Int leaf — (= x (ite b 1 2)) has a Bool
   guard b. When x is eliminated and appears nowhere else, b never reaches the theory, so
   re-derivation must DEFAULT b (not just Int leaves) or eval_value returns None and a
   satisfiable formula spuriously degrades. Expect sat with x re-derived to 2 (b:=false). *)
let test_presolve_bool_dep_default () =
  let s = Session.create () in
  let ctx = Session.context s in
  let b = Context.const ctx (Session.declare_const s "b" Sort.bool) in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  Session.assert_presolved
    s
    [ Context.eq
        ctx
        x
        (Context.ite ctx b (Context.int_const ctx 1) (Context.int_const ctx 2))
    ];
  check "M1: x eliminated" (Session.eliminated_vars s = [ "x" ]);
  match Session.check_sat s, Session.get_model s with
  | Session.Sat, Some (_, m) ->
    check "M1: x re-derived to 2 (b defaulted false)" (find_int_in_model m "x" = Some 2)
  | v, _ -> check ("M1: expected sat+model, got " ^ verdict_str v) false
;;

(* codex M2 (the wrong-unsat surface): an equality UNDER a Not is not a top-level
   conjunct, so it must NOT be eliminated — (not (= x 5)) /\ x >= 6 is sat (x = 6). A
   flatten that descended into Not would eliminate x -> 5 and flip to unsat (5 >= 6).
   Guards the flatten-Not-descent mutant. *)
let test_presolve_negated_eq_not_eliminated () =
  let s = Session.create () in
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  Session.assert_presolved
    s
    [ Context.not_ ctx (Context.eq ctx x (Context.int_const ctx 5))
    ; Context.ge ctx x (Context.int_const ctx 6)
    ];
  check "negated-eq: nothing eliminated" (Session.eliminated_vars s = []);
  check_verdict "negated-eq: sat (x=6)" Session.Sat (Session.check_sat s)
;;

(* Shared-DAG blowup guard (dag-memo). The assert-side walks — [Session.term_has_reserved]
   (the R1 reserved-symbol gate) and [Presolve.under_uf_vars] — must be memoized over the
   hash-cons DAG. A maximally-shared term (built here as [a_{i+1} = f(a_i, b_i)],
   [b_{i+1} = f(b_i, a_i)]) has [2*depth] distinct nodes but 2^depth root-to-leaf paths,
   so an UN-memoized per-path recursion is exponential (this is exactly the nec-smt
   bounded-model-checking VC shape). At depth 27 the unmemoized walk is ~10^8 node-visits
   per walk and takes multiple CPU-seconds; the memoized walk is O(depth) and returns in
   microseconds. Drive the defect's own path: [assert_presolved] runs BOTH walks, so this
   FAILS (blows past the budget) if EITHER regresses to the unmemoized form. *)
let test_dag_sharing_no_blowup () =
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" (Rank.create [ Sort.int; Sort.int ] Sort.int) in
  let x = Context.const ctx (Session.declare_const s "x" Sort.int) in
  let y = Context.const ctx (Session.declare_const s "y" Sort.int) in
  let depth = 27 in
  let rec build i a b =
    if i = 0
    then a, b
    else build (i - 1) (Context.app ctx f [ a; b ]) (Context.app ctx f [ b; a ])
  in
  let a, b = build depth x y in
  let top = Context.eq ctx a b in
  let t0 = Sys.time () in
  Session.assert_presolved s [ top ];
  let elapsed = Sys.time () -. t0 in
  check
    (Printf.sprintf "shared-DAG assert does not blow up (%.3fs CPU, want < 2.0s)" elapsed)
    (elapsed < 2.0)
;;

let () =
  test_push_pop ();
  test_assert_after_check ();
  test_euf_unsat ();
  test_lia_unsat ();
  test_lia_sat ();
  test_lia_branch_and_bound ();
  test_mixed_split ();
  test_soundness_rule ();
  test_honeypot_flips ();
  test_adr0010_unsat_repros ();
  test_adr0010_use_history ();
  test_adr0010_bool_boundary ();
  test_overflow_firewall ();
  test_bignum_r1_session_degrade ();
  test_get_model_bool ();
  test_mixed_bool_theory_model ();
  test_uf_function_model ();
  test_model_excludes_witnesses ();
  test_split_budget_exhaustion ();
  test_effort_unbounded_matches ();
  test_effort_budget_exhaustion ();
  test_effort_determinism ();
  test_namespace_guard ();
  test_parser_into_session ();
  test_determinism ();
  test_cli_refused_symbol_degrades ();
  test_cli_negative_int_token ();
  test_model_check_min_int_guard ();
  test_ovf_guard_parity ();
  test_presolve_alias_chain ();
  test_presolve_cycle_guard ();
  test_presolve_shadowed_alias ();
  test_presolve_model_r1 ();
  test_presolve_conditional_no_elim ();
  test_presolve_dropped_nondef ();
  test_presolve_interface_noop ();
  test_presolve_neutral ();
  test_presolve_determinism ();
  test_presolve_run_direct ();
  test_presolve_overflow_coeff_degrades ();
  test_presolve_overflow_const_degrades ();
  test_presolve_eq_uf_side_sound ();
  test_presolve_bool_dep_default ();
  test_presolve_negated_eq_not_eliminated ();
  test_dag_sharing_no_blowup ();
  Printf.printf "wiring_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
