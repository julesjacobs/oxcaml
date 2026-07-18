(* CHC solver self-tests: a graded suite of hand-written linear-LIA HORN problems, solved
   through the full parse -> PDR/BMC pipeline.

   Grading policy (soundness first):
   - [Safe]/[Unsafe] expected: reporting the OPPOSITE definite verdict is a SOUNDNESS
     failure (hard, exit 1). Reporting [unknown] is an incompleteness miss (soft).
   - [Must]: problems the v1 engine is expected to actually decide; an [unknown] here is a
     hard failure (guards against a regression that silently stops solving).
   - [Unknown] expected (out-of-fragment): must be [unknown]. *)

module Engine = Oxsmt_chc.Chc_engine
module Pdr = Oxsmt_chc.Chc_pdr
module Cegis = Oxsmt_chc.Chc_cegis
module Parse = Oxsmt_chc.Chc_parse

(* Dispatch exactly as the CLI does: single-predicate -> transition-system engine,
   multi-predicate -> multi-predicate linear PDR. *)
let dispatch sys =
  if List.length sys.Oxsmt_chc.Chc_ast.preds <= 1
  then Engine.solve ~budget:800 ~max_frames:20 sys
  else (
    let r = Pdr.solve ~budget:800 ~max_frames:20 sys in
    { Engine.verdict =
        (match r.Pdr.verdict with
         | Pdr.Safe -> Engine.Safe
         | Pdr.Unsafe -> Engine.Unsafe
         | Pdr.Unknown m -> Engine.Unknown m)
    ; detail = r.Pdr.detail
    })
;;

type expect =
  | Safe_must
  | Unsafe_must
  | Safe_ok (* safe, but unknown tolerated *)
  | Unsafe_ok
  | Unknown_expected

let solve src =
  match Parse.parse src with
  | sys -> dispatch sys
  | exception Parse.Unsupported m -> { Engine.verdict = Engine.Unknown m; detail = m }
  | exception Parse.Malformed m ->
    { Engine.verdict = Engine.Unknown ("malformed: " ^ m); detail = m }
;;

(* MBP-on solver: force the model-based-projection predecessor lever ([OXSMT_CHC_MBP]) and
   give it a little more room, for the multi-predicate relational cases that octagon
   generalization diverges on. *)
let solve_mbp src =
  match Parse.parse src with
  | sys ->
    let r = Pdr.solve ~mbp:true ~budget:30_000 ~max_frames:40 sys in
    { Engine.verdict =
        (match r.Pdr.verdict with
         | Pdr.Safe -> Engine.Safe
         | Pdr.Unsafe -> Engine.Unsafe
         | Pdr.Unknown m -> Engine.Unknown m)
    ; detail = r.Pdr.detail
    }
  | exception Parse.Unsupported m -> { Engine.verdict = Engine.Unknown m; detail = m }
  | exception Parse.Malformed m ->
    { Engine.verdict = Engine.Unknown ("malformed: " ^ m); detail = m }
;;

let failures = ref 0
let soft = ref 0

let check_with solver name expect src =
  let r = solver src in
  let v = r.Engine.verdict in
  let smt = Engine.verdict_to_smtlib v in
  let ok, tag =
    match expect, v with
    | (Safe_must | Safe_ok), Engine.Safe -> true, "OK"
    | (Unsafe_must | Unsafe_ok), Engine.Unsafe -> true, "OK"
    | Unknown_expected, Engine.Unknown _ -> true, "OK"
    (* soundness violations: opposite definite verdict *)
    | (Safe_must | Safe_ok), Engine.Unsafe -> false, "UNSOUND(safe reported unsafe)"
    | (Unsafe_must | Unsafe_ok), Engine.Safe -> false, "UNSOUND(unsafe reported safe)"
    | Unknown_expected, (Engine.Safe | Engine.Unsafe) ->
      false, "UNEXPECTED-DEFINITE(should be unknown)"
    (* incompleteness misses *)
    | (Safe_must | Unsafe_must), Engine.Unknown _ -> false, "MISS(must-solve got unknown)"
    | (Safe_ok | Unsafe_ok), Engine.Unknown _ -> true, "soft-miss(unknown)"
  in
  if not ok then incr failures;
  (match expect, v with
   | (Safe_ok | Unsafe_ok), Engine.Unknown _ -> incr soft
   | _ -> ());
  Printf.printf "%-28s %-8s %s\n" name smt tag
;;

let check = check_with solve
let check_mbp = check_with solve_mbp

(* Force the multi-predicate PDR engine DIRECTLY, bypassing the CLI's
   [<=1 predicate -> Engine] dispatch — the path a future ALLPDR / portfolio default-on
   flip takes for every system, and how the [Chc_pdr] npreds=0 guard (#155) is exercised.
   [max_effort] is exposed so a test can force the oracle to [R_unknown] (the wrong-Safe
   reproducer). *)
let solve_pdr_direct ?(max_effort = 1_000_000) src =
  match Parse.parse src with
  | sys ->
    let r = Pdr.solve ~budget:800 ~max_frames:20 ~max_effort sys in
    { Engine.verdict =
        (match r.Pdr.verdict with
         | Pdr.Safe -> Engine.Safe
         | Pdr.Unsafe -> Engine.Unsafe
         | Pdr.Unknown m -> Engine.Unknown m)
    ; detail = r.Pdr.detail
    }
  | exception Parse.Unsupported m -> { Engine.verdict = Engine.Unknown m; detail = m }
  | exception Parse.Malformed m ->
    { Engine.verdict = Engine.Unknown ("malformed: " ^ m); detail = m }
;;

let check_pdr_direct = check_with (solve_pdr_direct ~max_effort:1_000_000)
let check_pdr_effort0 = check_with (solve_pdr_direct ~max_effort:0)

(* npreds=0 guard (#155), routed directly through Chc_pdr.solve. *)
let () =
  (* decidably-UNSAT trivially-unsafe body -> SAFE. *)
  check_pdr_direct
    "no-pred-safe"
    Safe_must
    {|(set-logic HORN)
      (assert (forall ((x Int)) (=> (and (= x 0)(< x 0)) false)))|};
  (* satisfiable trivially-unsafe body -> UNSAFE (the accumulator fires first). *)
  check_pdr_direct
    "no-pred-unsafe"
    Unsafe_must
    {|(set-logic HORN)
      (assert (forall ((x Int)) (=> (= x 3) false)))|};
  (* #155 wrong-Safe REPRODUCER (review finding C4), and the guard's discriminating RED
     test: an oracle-undecidable trivially-unsafe body must be UNKNOWN, never Safe. Forced
     to R_unknown with max_effort:0; the body [x = 0] is actually SAT (so the system is
     Unsafe), hence a Safe verdict would be a wrong-`unsat`-class soundness bug. The
     unsound "not R_sat => Safe" guard returns Safe here (test FAILS); no guard crashes
     (Invalid_argument in mk_pdr/ensure on the empty predicate array); only the sound
     guard (positively confirm every body R_unsat, else Unknown) returns Unknown (test
     PASSES). *)
  check_pdr_effort0
    "no-pred-unknown-body"
    Unknown_expected
    {|(set-logic HORN)
      (assert (forall ((x Int)) (=> (= x 0) false)))|}
;;

(* ---- graded problems ---- *)

let () =
  (* 1. simple increasing counter, safety x>=0 holds *)
  check
    "inc-nonneg"
    Safe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 1))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(< x 0)) false)))|};
  (* 2. counter reaches 5, bad x>=5 reachable -> unsafe *)
  check
    "inc-reaches-5"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 1))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(>= x 5)) false)))|};
  (* 3. two-var RELATIONAL invariant x = y always, bad x<>y -> safe. Needs a relational
     lemma (x - y = 0); v1's per-variable interval generalization cannot express it, so
     [unknown] is the expected (soft) outcome — the headline lever for a next stage. *)
  check
    "two-var-eq"
    Safe_ok
    {|(set-logic HORN)
      (declare-fun P (Int Int) Bool)
      (assert (forall ((x Int)(y Int)) (=> (and (= x 0)(= y 0)) (P x y))))
      (assert (forall ((x Int)(y Int)(a Int)(b Int))
        (=> (and (P x y)(= a (+ x 1))(= b (+ y 1))) (P a b))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x y)(not (= x y))) false)))|};
  (* 4. bounded counter 0..10 wrap, invariant 0<=x<=10, bad x>10 -> safe *)
  check
    "bounded-0-10"
    Safe_ok
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(< x 10)(= y (+ x 1))) (P y))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(>= x 10)(= y x)) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(> x 10)) false)))|};
  (* 5. decreasing from 10, invariant x<=10, bad x>10 -> safe *)
  check
    "dec-le-10"
    Safe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 10) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (- x 1))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(> x 10)) false)))|};
  (* 6. immediate violation: init already bad -> unsafe at depth 0 *)
  check
    "init-bad"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 3) (P x))))
      (assert (forall ((x Int)) (=> (and (P x)(= x 3)) false)))|};
  (* 7. sum of two increments stays even difference... invariant x-y=0, safe (with let) *)
  check
    "let-binding-safe"
    Safe_ok
    {|(set-logic HORN)
      (declare-fun P (Int Int) Bool)
      (assert (forall ((x Int)(y Int)) (=> (and (= x 0)(= y 0)) (P x y))))
      (assert (forall ((x Int)(y Int)(a Int)(b Int))
        (=> (let ((d (- x y))) (and (P x y)(= d 0)(= a (+ x 2))(= b (+ y 2)))) (P a b))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x y)(> (- x y) 0)) false)))|};
  (* 8. increasing by 2 from 0, x always even so x<>1 -> safe; bad x=1 *)
  check
    "step2-not-1"
    Safe_ok
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 2))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(= x 1)) false)))|};
  (* 9. unsafe: from 0, +1 each step, bad x=7 reachable *)
  check
    "reach-7"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 1))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(= x 7)) false)))|};
  (* 10. two-predicate chain P->Q, safe (x stays 0) -> multi-pred PDR solves it *)
  check
    "two-preds-safe"
    Safe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (declare-fun Q (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)) (=> (P x) (Q x))))
      (assert (forall ((x Int)) (=> (and (Q x)(< x 0)) false)))|};
  (* 11. safe: x stays >=0 with a guard that can decrement but not below 0 *)
  check
    "guarded-nonneg"
    Safe_ok
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(> x 0)(= y (- x 1))) (P y))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 1))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(< x 0)) false)))|};
  (* 12. unsafe two-var: x can exceed y bound *)
  check
    "two-var-unsafe"
    Unsafe_ok
    {|(set-logic HORN)
      (declare-fun P (Int Int) Bool)
      (assert (forall ((x Int)(y Int)) (=> (and (= x 0)(= y 5)) (P x y))))
      (assert (forall ((x Int)(y Int)(a Int)) (=> (and (P x y)(= a (+ x 1))) (P a y))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x y)(> x y)) false)))|};
  (* 13. multi-predicate ping-pong P<->Q counting up, safe (never negative) *)
  check
    "mp-pingpong-safe"
    Safe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (declare-fun Q (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 1))) (Q y))))
      (assert (forall ((x Int)(y Int)) (=> (and (Q x)(= y (+ x 1))) (P y))))
      (assert (forall ((x Int)) (=> (and (Q x)(< x 0)) false)))|};
  (* 13b. multi-predicate RELATIONAL invariant x=y across a ping-pong (arity-2 preds). The
     decisive interpolation-proxy test: needs the difference-bound (octagon) template in
     the multi-predicate engine. *)
  check
    "mp-relational-eq"
    Safe_must
    {|(set-logic HORN)
      (declare-fun P (Int Int) Bool)
      (declare-fun Q (Int Int) Bool)
      (assert (forall ((x Int)(y Int)) (=> (and (= x 0)(= y 0)) (P x y))))
      (assert (forall ((x Int)(y Int)(a Int)(b Int))
        (=> (and (P x y)(= a (+ x 1))(= b (+ y 1))) (Q a b))))
      (assert (forall ((x Int)(y Int)(a Int)(b Int))
        (=> (and (Q x y)(= a (+ x 1))(= b (+ y 1))) (P a b))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x y)(not (= x y))) false)))
      (assert (forall ((x Int)(y Int)) (=> (and (Q x y)(not (= x y))) false)))|};
  (* 14. multi-predicate ping-pong reaching a bad bound -> unsafe (replay-confirmed) *)
  check
    "mp-pingpong-unsafe"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (declare-fun Q (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 1))) (Q y))))
      (assert (forall ((x Int)(y Int)) (=> (and (Q x)(= y (+ x 1))) (P y))))
      (assert (forall ((x Int)) (=> (and (Q x)(>= x 4)) false)))|};
  (* 15a. mod in a transition guard, bad reachable -> unsafe; exercises front-end mod/div
     elimination (BMC-findable, no strong generalization needed). *)
  check
    "mod-unsafe"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int))
        (=> (and (P x)(= (mod x 2) 0)(= y (+ x 2))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(= x 4)) false)))|};
  (* 15a2. mod in the safety property; invariant is MODULAR (x even), which interval
     generalization cannot express -> soft miss. Before mod/div elimination this bailed to
     unknown at the LIA oracle; now it reaches PDR and (soundly) times out. *)
  check
    "mod-safe-modular"
    Safe_ok
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 2))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(= (mod x 2) 1)) false)))|};
  (* 15b. NONLINEAR clause (two body predicates) -> out of the linear fragment -> unknown *)
  check
    "nonlinear-unknown"
    Unknown_expected
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)(z Int))
        (=> (and (P x)(P y)(= z (+ x y))) (P z))))
      (assert (forall ((x Int)) (=> (and (P x)(< x 0)) false)))|};
  (* 15. three-predicate chain, safe. Invariant C =
     {0}
     is TWO-SIDED (x<=0 /\ x>=0), which one-sided half-space PDR generalization diverges
     on. The forward two-sided interval propagation template propagates [0<=x<=0] down the
     chain and [verify] certifies it, so this is now a MUST-solve (regression guard for
     the template). *)
  check
    "mp-chain3-safe"
    Safe_must
    {|(set-logic HORN)
      (declare-fun A (Int) Bool)
      (declare-fun B (Int) Bool)
      (declare-fun C (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (A x))))
      (assert (forall ((x Int)) (=> (A x) (B x))))
      (assert (forall ((x Int)) (=> (B x) (C x))))
      (assert (forall ((x Int)) (=> (and (C x)(not (= x 0))) false)))|};
  (* 15c. genuine two-sided interval (not a point) propagated across a chain: reachable is
     [0..5], bad is out of range. Exercises the interval template on a wide bound. *)
  check
    "mp-interval-bound"
    Safe_must
    {|(set-logic HORN)
      (declare-fun A (Int) Bool)
      (declare-fun B (Int) Bool)
      (assert (forall ((x Int)) (=> (and (>= x 0)(<= x 5)) (A x))))
      (assert (forall ((x Int)) (=> (A x) (B x))))
      (assert (forall ((x Int)) (=> (and (B x)(or (< x 0)(> x 5))) false)))|};
  (* 15d. SOUNDNESS discrimination for the interval template: the forward interval
     computes a FINITE reachable bound for C ([2..2]) that CONTAINS the bad state x=2, so
     the candidate must FAIL independent [verify] and fall through to PDR/BMC, which
     confirms the genuine counterexample. A template that trusted its own bound guess
     would wrongly report safe here — this test would then flip to an UNSOUND failure. *)
  check
    "mp-interval-unsafe"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun A (Int) Bool)
      (declare-fun B (Int) Bool)
      (declare-fun C (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (A x))))
      (assert (forall ((x Int)(y Int)) (=> (and (A x)(= y (+ x 1))) (B y))))
      (assert (forall ((x Int)(y Int)) (=> (and (B x)(= y (+ x 1))) (C y))))
      (assert (forall ((x Int)) (=> (and (C x)(= x 2)) false)))|};
  (* ---- v2 review front-end findings (discrimination tests) ---- *)
  (* D-B (WRONG-SAFE): a CNF (or-form) Horn clause with a NEGATIVE predicate literal. The
     clause [(or (not (P 0)) false)] is [P(0) => false]; with the fact [P(0)] the system
     is UNSAFE. The pre-fix parser misrouted the negative literal and treated the [false]
     disjunct as the head, dropping the query -> spurious SAFE. RED before fix: reports
     Safe (UNSOUND). *)
  check
    "v2-D-B-cnf-neg-literal"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (P 0))
      (assert (or (not (P 0)) false))|};
  (* D-B control: the =>-form of the same clause must agree (UNSAFE). *)
  check
    "v2-D-B-imp-control"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (P 0))
      (assert (=> (P 0) false))|};
  (* D-C (WRONG-SAFE via capture): a user variable using the reserved [chcmd_] prefix that
     the mod/div elimination mints into. Must be REJECTED at parse time (-> unknown),
     never captured. RED before fix: the minted quotient/remainder collides with the user
     var and corrupts the transition system -> a definite (wrong) verdict. *)
  check
    "v2-D-C-reserved-prefix"
    Unknown_expected
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((chcmd_q0 Int)) (=> (= chcmd_q0 0) (P chcmd_q0))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 2))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(= (mod x 2) 1)) false)))|};
  (* D-A (wrong verdict): a clause applying an UNDECLARED predicate [Q]. Extraction is
     name-agnostic and would conflate [Q] with the declared [P]; z3 errors. Must fail loud
     (-> unknown). RED before fix: [Q] silently treated as [P] -> a definite (wrong)
     verdict. *)
  check
    "v2-D-A-undeclared-pred"
    Unknown_expected
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)) (=> (Q x) false)))|};
  (* D-D (crash): a nonlinear single-predicate transition ([y = x*x]) reaches BMC, whose
     term construction lacked the firewall [solve_exprs] has. Must degrade to a sound
     [unknown]. RED before fix: [Build_error] escapes BMC and crashes the process (exit
     2), taking the whole self-test down. *)
  check
    "v2-D-D-nonlinear-bmc"
    Unknown_expected
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 1) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (* x x))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(= x 0)) false)))|};
  (* ---- v2 review-fixes-2: trivially-unsafe slot family (D-E, D-F) ---- *)
  (* D-E (WRONG-SAFE, introduced by the D-B fix): a tautological or-clause ([true]
     disjunct) was encoded as a query-shaped [[],H_false] clause and OVERWROTE the single
     trivially-unsafe slot, dropping a co-occurring genuine fact-free query. Fix:
     tautology clauses produce NO clause, and the slot is an accumulator. The genuine
     query [x>0 => false] is satisfiable -> UNSAFE. RED before fix: reports Safe
     (UNSOUND). *)
  check
    "v2-D-E-taut-after-query"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (> x 0) false)))
      (assert (or true (P 0)))|};
  (* D-F (WRONG-SAFE, pre-existing): MULTIPLE fact-free queries overwrote the single slot,
     masking an earlier unsafe one. Here the first query [x>0 => false] is satisfiable
     (UNSAFE) but the second [y>0 /\ y<0 => false] is not; the old overwrite kept only the
     second -> Safe. Fix: accumulator checks every query. RED before fix: reports Safe. *)
  check
    "v2-D-F-two-queries"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (> x 0) false)))
      (assert (forall ((y Int)) (=> (and (> y 0) (< y 0)) false)))|};
  (* ---- model-based projection (MBP) predecessor lever ([OXSMT_CHC_MBP]) ---- *)
  (* MBP-SAFE (must-solve with MBP, diverges without): a two-predicate chain that
     maintains the RELATIONAL invariant [x + y = 10] — a SUM, outside the octagon
     (difference-bound) template the default generalization uses. The safety query is a
     DISEQUALITY ([x + y <> 10]); MBP's model-based disequality split turns it into the
     strict half-space the model takes, and the model-based predecessor projection keeps
     the [x + y] relation, so PDR converges to [x + y = 10]. RED: with MBP off (octagon
     only) this runs to the frame/budget limit -> unknown (verified: [off=timeout] on the
     CLI). *)
  check_mbp
    "mbp-rel-sum-safe"
    Safe_must
    {|(set-logic HORN)
      (declare-fun P (Int Int) Bool)
      (declare-fun Q (Int Int) Bool)
      (assert (forall ((x Int)(y Int)) (=> (and (= x 0)(= y 10)) (P x y))))
      (assert (forall ((x Int)(y Int)(a Int)(b Int))
        (=> (and (P x y)(= a (+ x 1))(= b (- y 1))) (Q a b))))
      (assert (forall ((x Int)(y Int)) (=> (Q x y) (P x y))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x y)(not (= (+ x y) 10))) false)))|};
  (* MBP-UNSAFE (soundness discrimination): the SAME shape but the transition breaks the
     relation ([y] is carried unchanged while [x] grows), so [x + y] increases and the
     [x + y <> 10] query IS reachable. A model-based projection that over-generalized the
     predecessor (claimed states reach the POB that do not) could mask this into a wrong
     Safe; the independent replay firewall + a correct under-approximating MBP keep it
     Unsafe. Agrees with z3 (unsat). *)
  check_mbp
    "mbp-rel-sum-unsafe"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int Int) Bool)
      (declare-fun Q (Int Int) Bool)
      (assert (forall ((x Int)(y Int)) (=> (and (= x 0)(= y 10)) (P x y))))
      (assert (forall ((x Int)(y Int)(a Int)) (=> (and (P x y)(= a (+ x 1))) (Q a y))))
      (assert (forall ((x Int)(y Int)) (=> (Q x y) (P x y))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x y)(not (= (+ x y) 10))) false)))|};
  (* ---- trivially-unsafe (fact-free constr => false) SAFE-firewall soundness ---- *)
  (* A fact-free [x*x = 0 => false] body is a genuine query (x=0 makes it satisfiable ->
     UNSAFE), but our LIA oracle cannot decide the nonlinear constraint -> R_unknown. The
     correct verdict is Unknown (we cannot prove it either way); reporting SAFE is
     UNSOUND. RED before fix (both engines): neither the up-front check (tested [= R_sat],
     swallowing R_unknown) nor the SAFE re-verification (never discharged
     trivially_unsafe) caught it, so the invariant path reported [sat]. Fix: SAFE requires
     every trivially_unsafe body provably R_unsat. Two variants exercise BOTH engines via
     the pred-count dispatch. *)
  check
    "triv-nonlinear-unknown-1pred"
    Unknown_expected
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)) (=> (= (* x x) 0) false)))|};
  check
    "triv-nonlinear-unknown-2pred"
    Unknown_expected
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (declare-fun Q (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)) (=> (P x) (Q x))))
      (assert (forall ((x Int)) (=> (= (* x x) 0) false)))|};
  (* Control: a LINEAR fact-free query stays decidable and must remain UNSAFE (the fix
     must not over-suppress genuine constraint-only counterexamples to Unknown). *)
  check
    "triv-linear-unsafe-control"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)) (=> (> x 0) false)))|};
  ()
;;

(* ---- Chc_cegis (syntax-guided synthesis engine) discrimination tests ---- *)

let solve_cegis src =
  match Parse.parse src with
  | sys -> Cegis.solve ~budget:20_000 sys
  | exception Parse.Unsupported m -> { Pdr.verdict = Pdr.Unknown m; detail = m }
  | exception Parse.Malformed m ->
    { Pdr.verdict = Pdr.Unknown ("malformed: " ^ m); detail = m }
;;

(* [expect] semantics for cegis: [Safe_must] the synthesis engine is required to prove
   (regression guard for the sum/conjunctive class); the [_ok] forms tolerate [unknown];
   [Unsafe_must] must be caught by the shared instant checks. A cegis run reporting the
   OPPOSITE definite verdict is a soundness failure. *)
let check_cegis name expect src =
  let r = solve_cegis src in
  let v = r.Pdr.verdict in
  let smt =
    match v with
    | Pdr.Safe -> "sat"
    | Pdr.Unsafe -> "unsat"
    | Pdr.Unknown _ -> "unknown"
  in
  let ok, tag =
    match expect, v with
    | (Safe_must | Safe_ok), Pdr.Safe -> true, "OK"
    | (Unsafe_must | Unsafe_ok), Pdr.Unsafe -> true, "OK"
    | Unknown_expected, Pdr.Unknown _ -> true, "OK"
    | (Safe_must | Safe_ok), Pdr.Unsafe -> false, "UNSOUND(safe reported unsafe)"
    | (Unsafe_must | Unsafe_ok), Pdr.Safe -> false, "UNSOUND(unsafe reported safe)"
    | Unknown_expected, (Pdr.Safe | Pdr.Unsafe) ->
      false, "UNEXPECTED-DEFINITE(should be unknown)"
    | (Safe_must | Unsafe_must), Pdr.Unknown _ -> false, "MISS(must-solve got unknown)"
    | (Safe_ok | Unsafe_ok), Pdr.Unknown _ -> true, "soft-miss(unknown)"
  in
  if not ok then incr failures;
  (match expect, v with
   | (Safe_ok | Unsafe_ok), Pdr.Unknown _ -> incr soft
   | _ -> ());
  Printf.printf "%-28s %-8s %s\n" ("cegis:" ^ name) smt tag
;;

(* The portfolio composition exactly as the CLI runs it: Chc_cegis first (time-boxed), and
   on [Unknown] fall through to the pdr dispatch. Used to prove the inherited hardened
   [Chc_pdr.verify] closes the trivially-unsafe wrong-SAFE on BOTH the cegis-explicit and
   the portfolio paths. *)
let solve_portfolio src =
  match Parse.parse src with
  | sys ->
    let r = Cegis.solve ~budget:600 ~max_effort:6000 sys in
    (match r.Pdr.verdict with
     | Pdr.Safe | Pdr.Unsafe -> r
     | Pdr.Unknown _ ->
       let e = dispatch sys in
       { Pdr.verdict =
           (match e.Engine.verdict with
            | Engine.Safe -> Pdr.Safe
            | Engine.Unsafe -> Pdr.Unsafe
            | Engine.Unknown m -> Pdr.Unknown m)
       ; detail = e.Engine.detail
       })
  | exception Parse.Unsupported m -> { Pdr.verdict = Pdr.Unknown m; detail = m }
  | exception Parse.Malformed m ->
    { Pdr.verdict = Pdr.Unknown ("malformed: " ^ m); detail = m }
;;

let check_portfolio name expect src =
  let r = solve_portfolio src in
  let v = r.Pdr.verdict in
  let smt =
    match v with
    | Pdr.Safe -> "sat"
    | Pdr.Unsafe -> "unsat"
    | Pdr.Unknown _ -> "unknown"
  in
  let ok, tag =
    match expect, v with
    | (Safe_must | Safe_ok), Pdr.Safe -> true, "OK"
    | (Unsafe_must | Unsafe_ok), Pdr.Unsafe -> true, "OK"
    | Unknown_expected, Pdr.Unknown _ -> true, "OK"
    | (Safe_must | Safe_ok), Pdr.Unsafe -> false, "UNSOUND(safe reported unsafe)"
    | (Unsafe_must | Unsafe_ok), Pdr.Safe -> false, "UNSOUND(unsafe reported safe)"
    | Unknown_expected, (Pdr.Safe | Pdr.Unsafe) ->
      false, "UNEXPECTED-DEFINITE(should be unknown)"
    | (Safe_must | Unsafe_must), Pdr.Unknown _ -> false, "MISS(must-solve got unknown)"
    | (Safe_ok | Unsafe_ok), Pdr.Unknown _ -> true, "soft-miss(unknown)"
  in
  if not ok then incr failures;
  (match expect, v with
   | (Safe_ok | Unsafe_ok), Pdr.Unknown _ -> incr soft
   | _ -> ());
  Printf.printf "%-28s %-8s %s\n" ("portfolio:" ^ name) smt tag
;;

let () =
  (* SUM invariant [x + y = 10] with point init [x=0,y=10] and bad [x+y<>10]. This is the
     class backward PDR cube-generalization provably diverges on (the [safe_rel_sum]
     analysis in the chc-v2 report): every minimal LIA conflict under point init is
     single-variable, so the interpolant excludes init and the engine diverges. The
     data-driven full-sum candidate [x+y=10], learned from the observed constant sum and
     certified by [verify], closes it. MUST-solve regression guard for the headline lever. *)
  check_cegis
    "sum-safe"
    Safe_must
    {|(set-logic HORN)
      (declare-fun P (Int Int) Bool)
      (assert (forall ((x Int)(y Int)) (=> (and (= x 0)(= y 10)) (P x y))))
      (assert (forall ((x Int)(y Int)(a Int)(b Int))
        (=> (and (P x y)(= a (+ x 1))(= b (- y 1))) (P a b))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x y)(not (= (+ x y) 10))) false)))|};
  (* Relational equality [x = y] (octagon difference candidate). *)
  check_cegis
    "rel-eq-safe"
    Safe_must
    {|(set-logic HORN)
      (declare-fun P (Int Int) Bool)
      (assert (forall ((x Int)(y Int)) (=> (and (= x 0)(= y 0)) (P x y))))
      (assert (forall ((x Int)(y Int)(a Int)(b Int))
        (=> (and (P x y)(= a (+ x 1))(= b (+ y 1))) (P a b))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x y)(not (= x y))) false)))|};
  (* Single-variable lower bound [x >= 0]. *)
  check_cegis
    "single-bound-safe"
    Safe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x)(= y (+ x 1))) (P y))))
      (assert (forall ((x Int)) (=> (and (P x)(< x 0)) false)))|};
  (* SOUNDNESS discrimination: a genuinely UNSAFE system whose reachable states are
     [x = y] but the bad edge [x + y = 6] (reachable at x=y=3) is INSIDE the reachable
     set. Data-driven sampling will observe the bad state, so no learned atom excludes it
     and the candidate cannot certify Safe. The engine must therefore NOT report Safe — a
     synthesis that trusted its own too-weak guess would flip this to a wrong Safe. (cegis
     leaves the definite UNSAFE verdict to the PDR path, so [unknown] here is the correct
     cegis outcome; the portfolio's PDR leg proves the counterexample.) *)
  check_cegis
    "reachable-bad-not-safe"
    Unknown_expected
    {|(set-logic HORN)
      (declare-fun P (Int Int) Bool)
      (assert (forall ((x Int)(y Int)) (=> (and (= x 0)(= y 0)) (P x y))))
      (assert (forall ((x Int)(y Int)(a Int)(b Int))
        (=> (and (P x y)(= a (+ x 1))(= b (+ y 1))) (P a b))))
      (assert (forall ((x Int)(y Int)) (=> (and (P x y)(= (+ x y) 6)) false)))|};
  (* Instant UNSAFE (init already bad) must be caught by the shared depth-0 check. *)
  check_cegis
    "init-bad-unsafe"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 3) (P x))))
      (assert (forall ((x Int)) (=> (and (P x)(= x 3)) false)))|};
  (* B-2 inherited-firewall reproducer through the CEGIS path. A fact-free NONLINEAR query
     [x*x = 0 => false] is a genuine trivially-unsafe obligation the LIA oracle cannot
     decide (R_unknown). cegis's up-front unsafe check tests [= R_sat] (so R_unknown is
     NOT flagged unsafe), then it synthesizes a candidate invariant for [P] and hands it
     to the SHARED Chc_pdr.verify. Before the landed verify hardening (trivially_unsafe
     obligations were never discharged + R_unknown swallowed), a verified P-invariant
     returned a wrong SAFE; with the hardened verify — which cegis INHERITS unchanged via
     [P.verify] — SAFE now requires every trivially_unsafe body provably R_unsat, so this
     is [unknown]. Must be unknown via BOTH the cegis-explicit engine and the portfolio
     composition. *)
  check_cegis
    "triv-nonlinear-unknown"
    Unknown_expected
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)) (=> (= (* x x) 0) false)))|};
  check_portfolio
    "triv-nonlinear-unknown"
    Unknown_expected
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)) (=> (= (* x x) 0) false)))|};
  (* Control: a LINEAR fact-free query stays a decidable genuine counterexample; the
     shared depth-0/trivially-unsafe check must still catch it as UNSAFE through the
     portfolio. *)
  check_portfolio
    "triv-linear-unsafe-control"
    Unsafe_must
    {|(set-logic HORN)
      (declare-fun P (Int) Bool)
      (assert (forall ((x Int)) (=> (= x 0) (P x))))
      (assert (forall ((x Int)) (=> (> x 0) false)))|};
  Printf.printf "\n%d hard failure(s), %d soft miss(es)\n" !failures !soft;
  if !failures > 0 then exit 1
;;
