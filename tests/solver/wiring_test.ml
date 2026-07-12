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
  | Some [ Session.Const ("x", Session.VInt 3) ] -> check "LIA sat model x=3" true
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

(* get_model on a pure-Boolean sat returns a Bool value per propositional variable. *)

let test_get_model_bool () =
  let s = Session.create () in
  let ctx = Session.context s in
  let p = Context.const ctx (Session.declare_const s "p" Sort.bool) in
  let q = Context.const ctx (Session.declare_const s "q" Sort.bool) in
  Session.assert_term s (Context.or_ ctx [ p; q ]);
  Session.assert_term s (Context.not_ ctx q);
  (match Session.check_sat s, Session.get_model s with
   | Session.Sat, Some m ->
     let find n =
       List.find_map (fun (Session.Const (k, v)) -> if k = n then Some v else None) m
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
  | Session.Sat, Some m ->
    let find n =
      List.find_map (fun (Session.Const (k, v)) -> if k = n then Some v else None) m
    in
    check
      "mixed model includes Bool const p (=true)"
      (find "p" = Some (Session.VBool true));
    check "mixed model includes Int const x (=0)" (find "x" = Some (Session.VInt 0));
    check "mixed model has exactly p and x" (List.length m = 2)
  | v, _ -> check ("mixed bool/theory: expected sat+model, got " ^ verdict_str v) false
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
  | Session.Sat, Some m ->
    check
      "model excludes .oxsmt.* witnesses"
      (List.for_all (fun (Session.Const (n, _)) -> not (reserved n)) m);
    check "model still names x" (List.exists (fun (Session.Const (n, _)) -> n = "x") m)
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
  test_get_model_bool ();
  test_mixed_bool_theory_model ();
  test_model_excludes_witnesses ();
  test_split_budget_exhaustion ();
  test_namespace_guard ();
  test_parser_into_session ();
  test_determinism ();
  test_cli_refused_symbol_degrades ();
  test_cli_negative_int_token ();
  Printf.printf "wiring_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
