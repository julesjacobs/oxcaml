(* Unit + property tests for the LIA THEORY adapter (Lia_adapter), ADR-0005 M4.

   The underlying {!Lia} engine is exhaustively tested by lia_test.ml (brute-force
   enumeration, independent Farkas verifier, overflow/poison). This suite targets the
   ADAPTER's own responsibilities — the thin layer lia_test does not exercise:

   - currency translation: [Atom.t]/[Lit.t] <-> [Term.t], polarity, idempotent register;
   - [check] verdict mapping: rational [Conflict] (rule [Lia_farkas]) vs bound
     [Propagations] (rule [Lia_bound]) at [Propagate]; [Sat]/[Split]/[Conflict] at
     [Final];
   - the B&B [Split] is >=2 DISTINCT, genuinely-constraining atoms (CONTRACT-SPLIT), and
     driving one branch reaches [Sat] with an integral [Model];
   - lazy [explain]: the premise set cached at propagation time is precedence-valid
     (CONTRACT-EX) and frame-scoped (cleared on the [pop] of its decision level);
   - CONTRACT-POISON: an engine overflow is surfaced as unknown (exception -> engine
     degrade), never a sat/unsat verdict; [overflows_to_unknown] counts it; reuse is
     bricked; a fresh adapter is unaffected;
   - recheck-after-backtrack: a conflict established below a push survives the pop
     (calibration E5 / the L3 sole-witness family);
   - determinism (I6): identical op sequences -> identical verdicts + pivot counts.

   Farkas self-verification is done FROM PUBLIC OUTPUTS ALONE (calibration E4): the
   adapter's [Explanation] deliberately carries only premise [Lit.t]s + a rule tag (the
   multipliers stay engine-internal per ADR-0005 D7), so the verifier reconstructs each
   premise's <=0 half-plane from the literal's registered term and checks that a
   test-supplied nonnegative multiplier vector cancels every variable and leaves a
   strictly positive constant.

   Stdlib-only (I3). Deterministic: no wall-clock, no RNG needed (hand cases). *)

open Oxsmt_core
open Oxsmt_lia

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

(* Assert [f ()] raises exactly the exception matched by [pred] (by name, for the digest). *)
let check_raises name pred f =
  incr checks;
  match f () with
  | _ ->
    incr failures;
    Printf.printf "  FAIL %s (expected an exception, got a value)\n" name
  | exception e ->
    if not (pred e)
    then (
      incr failures;
      Printf.printf "  FAIL %s (wrong exception: %s)\n" name (Printexc.to_string e))
;;

let is_overflow = function
  | Rational.Overflow -> true
  | _ -> false
;;

let q = Rational.of_int

(* ================================================================== *)
(* Fixture: a Context over n Int vars, an adapter, an Atom allocator, and a record of each
   asserted literal's NORMALIZED <=0 half-plane (read back from the built term) for the
   public-output Farkas verifier. *)

type fixture =
  { ctx : Context.t
  ; vars : Term.t array
  ; adapter : Lia_adapter.t
  ; alloc : Atom.allocator
  ; hp : (Lit.t, (int * int) list * int) Hashtbl.t
  }

let make_fixture n =
  let env = Env.create () in
  let vsyms =
    Array.init n (fun i ->
      Env.declare_fun env (Printf.sprintf "x%d" i) (Rank.create [] Sort.int))
  in
  let ctx = Context.create env in
  { ctx
  ; vars = Array.map (Context.const ctx) vsyms
  ; adapter = Lia_adapter.create ctx env
  ; alloc = Atom.create_allocator ()
  ; hp = Hashtbl.create 64
  }
;;

(* The atom [Σ cᵢ·x_i + const <= 0] (coeffs by variable index). *)
let mk_le fx coeffs const =
  let pairs = List.map (fun (i, c) -> c, fx.vars.(i)) coeffs in
  Context.le
    fx.ctx
    (Context.linear_combination fx.ctx pairs const)
    (Context.int_const fx.ctx 0)
;;

let idx_of fx (tm : Term.t) =
  let r = ref (-1) in
  Array.iteri (fun i v -> if Term.equal v tm then r := i) fx.vars;
  if !r < 0 then failwith "idx_of: unknown var term";
  !r
;;

(* The <=0 half-plane of an [Le] atom's inner term (the gcd-normalized form LIA reasons
   over), keyed by variable index. *)
let bi_to_int b = Option.get (Bigint.to_int_opt b)

let inner_halfplane fx (inner : Term.t) =
  match inner.Term.node with
  | Term.Arith l ->
    ( Iarr.fold (fun acc (tm, c) -> (idx_of fx tm, bi_to_int c) :: acc) [] l.Term.coeffs
    , bi_to_int l.Term.const )
  | Term.Int_const k -> [], bi_to_int k
  | _ -> [ idx_of fx inner, 1 ], 0
;;

let register_le fx coeffs const =
  let term = mk_le fx coeffs const in
  let atom = Atom.fresh fx.alloc in
  Lia_adapter.register_atom fx.adapter atom term;
  atom, term
;;

(* Register + assert a Le atom at [polarity], recording the literal's <=0 half-plane. *)
let assert_le fx coeffs const ~polarity =
  let atom, term = register_le fx coeffs const in
  let lit = Lit.make atom polarity in
  let ic, ik =
    match term.Term.node with
    | Term.Le inner -> inner_halfplane fx inner
    | _ -> failwith "assert_le: constructed atom is not an Le (constant-folded?)"
  in
  let hp = if polarity then ic, ik else List.map (fun (i, c) -> i, -c) ic, 1 - ik in
  Hashtbl.replace fx.hp lit hp;
  Lia_adapter.assert_lit fx.adapter lit;
  lit
;;

(* Register + assert a positive Int equality [a = b]. *)
let assert_eq fx a b =
  let term = Context.eq fx.ctx a b in
  let atom = Atom.fresh fx.alloc in
  Lia_adapter.register_atom fx.adapter atom term;
  Lia_adapter.assert_lit fx.adapter (Lit.make atom true)
;;

(* Register + assert a Le atom already built as a term (for split disjuncts). *)
let assert_term_true fx term =
  let atom = Atom.fresh fx.alloc in
  Lia_adapter.register_atom fx.adapter atom term;
  Lia_adapter.assert_lit fx.adapter (Lit.make atom true)
;;

(* Independent Farkas check over PUBLIC premises + a test-supplied multiplier per literal
   (DESIGN §7): Σ mult(lit)·half-plane(lit) must cancel every variable and leave a
   strictly positive constant, all multipliers nonnegative. *)
let farkas_cancels fx premises mult =
  let acc = Hashtbl.create 16 in
  let const = ref Rational.zero in
  List.iter
    (fun lit ->
       let coeffs, k = Hashtbl.find fx.hp lit in
       let m = mult lit in
       List.iter
         (fun (i, c) ->
            let cur =
              try Hashtbl.find acc i with
              | Not_found -> Rational.zero
            in
            Hashtbl.replace acc i (Rational.add cur (Rational.mul m (q c))))
         coeffs;
       const := Rational.add !const (Rational.mul m (q k)))
    premises;
  List.for_all (fun l -> Rational.sign (mult l) >= 0) premises
  && Hashtbl.fold (fun _ c ok -> ok && Rational.is_zero c) acc true
  && Rational.sign !const > 0
;;

let propagate fx = Lia_adapter.check fx.adapter Theory.Propagate

(* ================================================================== *)
(* 1. Currency round-trip: sat, conflict-with-Farkas-verify, rule tags. *)

let test_currency () =
  print_endline "currency + conflict:";
  (* Feasible: x <= 5, x >= 0 -> Propagate is consistent (no conflict). *)
  (let fx = make_fixture 1 in
   ignore (assert_le fx [ 0, 1 ] (-5) ~polarity:true);
   (* x - 5 <= 0 *)
   ignore (assert_le fx [ 0, -1 ] 0 ~polarity:true);
   (* -x <= 0 -> x >= 0 *)
   match propagate fx with
   | Theory.Conflict _ -> check "feasible system is not a conflict" false
   | Theory.Propagations _ -> check "feasible system -> Propagations (consistent)" true
   | _ -> check "Propagate returned Sat/Split (illegal)" false);
  (* Classic infeasible chain x <= 0 and x >= 1; premises verified with multipliers [1;1]. *)
  (let fx = make_fixture 1 in
   let l1 = assert_le fx [ 0, 1 ] 0 ~polarity:true in
   (* x <= 0 *)
   let l2 = assert_le fx [ 0, 1 ] (-1) ~polarity:false in
   (* ¬(x-1<=0) = x >= 1 *)
   match propagate fx with
   | Theory.Conflict expl ->
     check
       "conflict rule tag is Lia_farkas"
       (expl.Explanation.rule = Explanation.Rule_tag.Lia_farkas);
     check
       "conflict premises are exactly {l1,l2}"
       (List.sort compare expl.Explanation.premises = List.sort compare [ l1; l2 ]);
     check
       "conflict premises Farkas-cancel (public verify, mult [1;1])"
       (farkas_cancels fx expl.Explanation.premises (fun _ -> q 1))
   | _ -> check "x<=0 ∧ x>=1 must conflict" false);
  (* Two-variable infeasible: x-y<=-1 and y-x<=-1 (sum: 2<=0). Multipliers [1;1]. *)
  let fx = make_fixture 2 in
  let l1 = assert_le fx [ 0, 1; 1, -1 ] 1 ~polarity:true in
  (* x - y + 1 <= 0 *)
  let l2 = assert_le fx [ 0, -1; 1, 1 ] 1 ~polarity:true in
  (* y - x + 1 <= 0 *)
  match propagate fx with
  | Theory.Conflict expl ->
    check
      "2-var conflict premises = {l1,l2}"
      (List.sort compare expl.Explanation.premises = List.sort compare [ l1; l2 ]);
    check
      "2-var conflict Farkas-cancels (public verify)"
      (farkas_cancels fx expl.Explanation.premises (fun _ -> q 1))
  | _ -> check "x-y<=-1 ∧ y-x<=-1 must conflict" false
;;

(* ================================================================== *)
(* 2. Bound propagation + lazy explain (precedence-valid, Lia_bound). *)

let test_propagate_explain () =
  print_endline "propagate + explain:";
  let fx = make_fixture 1 in
  (* Register (but do not assert) A: x <= 5. Assert B: x <= 3. B entails A. *)
  let atom_a, _term_a = register_le fx [ 0, 1 ] (-5) in
  let lit_b = assert_le fx [ 0, 1 ] (-3) ~polarity:true in
  let lit_a_true = Lit.make atom_a true in
  (match propagate fx with
   | Theory.Propagations lits ->
     check "A (x<=5) is theory-propagated true by B (x<=3)" (List.mem lit_a_true lits)
   | _ -> check "expected Propagations" false);
  (* explain serves the cached reason: premise = the asserting literal B, tag Lia_bound. *)
  let expl = Lia_adapter.explain fx.adapter lit_a_true in
  check
    "explain rule tag is Lia_bound"
    (expl.Explanation.rule = Explanation.Rule_tag.Lia_bound);
  check "explain premise is exactly {B}" (expl.Explanation.premises = [ lit_b ]);
  (* Precedence (CONTRACT-EX): every premise was asserted strictly before the propagated
     literal. B was asserted; A was only registered (never asserted), so B precedes A's
     propagation. *)
  check
    "explain premise B <> the propagated literal A (precedence, no self-justification)"
    (not (List.mem lit_a_true expl.Explanation.premises))
;;

(* Re-propagation with a tighter bound must NOT rewrite a propagated literal's cached
   reason to a later-asserted premise (CONTRACT-EX precedence, review A1) nor re-scope it
   to a newer frame so a pop drops it (review A2). Shape: A entailed by B at root; then a
   tighter C at a deeper level re-entails A. explain(A) must stay [{B}] — the first,
   precedence-valid reason — both before AND after popping the deeper frame. *)
let test_reprop_precedence () =
  print_endline "re-propagation precedence (A1/A2):";
  let fx = make_fixture 1 in
  let atom_a, _ = register_le fx [ 0, 1 ] (-5) in
  (* A: x <= 5 (registered, never asserted -> a standing propagation target) *)
  let lit_a_true = Lit.make atom_a true in
  let lit_b = assert_le fx [ 0, 1 ] (-3) ~polarity:true in
  (* B: x <= 3 at root *)
  (match propagate fx with
   | Theory.Propagations lits ->
     check "A propagated at root by B" (List.mem lit_a_true lits)
   | _ -> check "expected root propagation of A" false);
  check
    "root: explain(A) = {B}"
    ((Lia_adapter.explain fx.adapter lit_a_true).Explanation.premises = [ lit_b ]);
  (* Deeper level: a tighter bound C re-entails A. B was asserted BEFORE A was first
     propagated; C is asserted AFTER — so C must never become A's reason. *)
  Lia_adapter.push fx.adapter;
  let _lit_c = assert_le fx [ 0, 1 ] (-2) ~polarity:true in
  (* C: x <= 2 *)
  ignore (propagate fx : Theory.check_result);
  check
    "after tighter C: explain(A) STILL {B} (no precedence-breaking rewrite, A1)"
    ((Lia_adapter.explain fx.adapter lit_a_true).Explanation.premises = [ lit_b ]);
  (* Pop the deeper frame: A's reason was cached at ROOT, so it must survive (A2). *)
  Lia_adapter.pop fx.adapter 1;
  check
    "after pop of deeper frame: explain(A) STILL {B} (reason kept its root frame, A2)"
    ((Lia_adapter.explain fx.adapter lit_a_true).Explanation.premises = [ lit_b ])
;;

(* Re-entailment after the atom's OWN report frame is popped (incremental-delta
   highest-risk path, review F1). Distinct from A1/A2, where A is reported at ROOT and
   survives the pop: here A is first-reported INSIDE the pushed frame, so the pop must (a)
   un-report A and re-arm its var for re-scan, (b) drop its cached reason, and (c) a
   LATER, DIFFERENT entailing bound must re-emit A with a fresh, precedence-valid reason —
   not leave it orphaned/never-re-propagated (the delta's correctness hinge). *)
let test_reentail_after_frame_pop () =
  print_endline "re-entail after own-frame pop (F1):";
  let fx = make_fixture 1 in
  let atom_a, _ = register_le fx [ 0, 1 ] (-5) in
  (* A: x <= 5, registered at root, never asserted (a standing propagation target) *)
  let lit_a_true = Lit.make atom_a true in
  (* Frame 1: B (x<=3) entails A, so A is FIRST-reported in this pushed frame. *)
  Lia_adapter.push fx.adapter;
  let lit_b = assert_le fx [ 0, 1 ] (-3) ~polarity:true in
  (match propagate fx with
   | Theory.Propagations lits ->
     check "A emitted in frame 1 (entailed by B)" (List.mem lit_a_true lits)
   | _ -> check "expected A propagated in frame 1" false);
  check
    "frame 1: explain(A) = {B}"
    ((Lia_adapter.explain fx.adapter lit_a_true).Explanation.premises = [ lit_b ]);
  (* Pop frame 1: A's report frame is unwound; its cached reason must go, and with no
     bound left on x, A is no longer entailed and must NOT be re-emitted. *)
  Lia_adapter.pop fx.adapter 1;
  check_raises
    "after own-frame pop: explain(A) raises (reason dropped, not stale)"
    (function
      | Failure _ -> true
      | _ -> false)
    (fun () -> Lia_adapter.explain fx.adapter lit_a_true);
  (match propagate fx with
   | Theory.Propagations lits ->
     check "A NOT re-emitted while un-entailed after pop" (not (List.mem lit_a_true lits))
   | Theory.Conflict _ -> check "unexpected conflict after pop" false
   | _ -> ());
  (* Frame 1': a DIFFERENT bound C (x<=4) re-entails A. It MUST be re-emitted (the
     un-report
     + re-dirty on pop is what makes this reachable) with a fresh reason [{C}], not [{B}]. *)
  Lia_adapter.push fx.adapter;
  let lit_c = assert_le fx [ 0, 1 ] (-4) ~polarity:true in
  (match propagate fx with
   | Theory.Propagations lits ->
     check
       "A RE-emitted by C after its own report frame was popped"
       (List.mem lit_a_true lits)
   | _ -> check "expected A re-propagated by C" false);
  check
    "after re-entailment: explain(A) = {C} (fresh precedence-valid reason, not stale {B})"
    ((Lia_adapter.explain fx.adapter lit_a_true).Explanation.premises = [ lit_c ])
;;

(* ================================================================== *)
(* 3. Final: Sat when integral; Split (2 distinct constraining atoms) when not; drive a
   branch to Sat and read an integral Model. *)

let test_final_split () =
  print_endline "final + split:";
  (* Integral rational model -> Sat directly. x >= 2, x <= 10. *)
  (let fx = make_fixture 1 in
   ignore (assert_le fx [ 0, -1 ] 2 ~polarity:true);
   (* -x + 2 <= 0 -> x >= 2 *)
   ignore (assert_le fx [ 0, 1 ] (-10) ~polarity:true);
   (* x - 10 <= 0 *)
   match Lia_adapter.check fx.adapter Theory.Final with
   | Theory.Sat ->
     check "integral feasible -> Sat" true;
     let m = Lia_adapter.model fx.adapter in
     (match Model.value m fx.vars.(0) with
      | Some (Model.Int v) ->
        check "model x is integral and within [2,10]" (v >= 2 && v <= 10)
      | _ -> check "model assigns x an Int" false)
   | _ -> check "integral feasible must be Sat" false);
  (* Non-integral rational model -> Split. A SINGLE Le atom always gcd-tightens to an
     integer bound, so a fraction needs two interacting combos: x = y and x + y = 1 pin
     the rational vertex x = y = 1/2 (ℤ-unsat, but that is irrelevant here — we only need
     the rational model non-integral so [check Final] must Split rather than answer Sat). *)
  let fx = make_fixture 2 in
  assert_eq fx fx.vars.(0) fx.vars.(1);
  (* x = y *)
  assert_eq
    fx
    (Context.linear_combination fx.ctx [ 1, fx.vars.(0); 1, fx.vars.(1) ] 0)
    (Context.int_const fx.ctx 1);
  (* x + y = 1 *)
  let split =
    match Lia_adapter.check fx.adapter Theory.Final with
    | Theory.Split terms -> Some terms
    | _ ->
      check "non-integral feasible must Split" false;
      None
  in
  (match split with
   | Some [ a; b ] ->
     (* CONTRACT-SPLIT: >=2 DISTINCT atoms (not the discarded [A v ¬A] tautology). *)
     check "Split has 2 distinct atoms" (not (Term.equal a b));
     check
       "both split disjuncts are theory atoms"
       (Theory_view.is_atom a && Theory_view.is_atom b)
   | Some _ -> check "Split must be exactly the 2 B&B atoms" false
   | None -> ());
  (* Each split disjunct is genuinely CONSTRAINING: asserting it changes the verdict —
     here the lower branch [x<=0] conflicts against x=y ∧ x+y=1. (A tautological disjunct
     could not conflict.) *)
  (match split with
   | Some (branch :: _) ->
     Lia_adapter.push fx.adapter;
     assert_term_true fx branch;
     (match Lia_adapter.check fx.adapter Theory.Final with
      | Theory.Conflict _ -> check "first split disjunct is constraining (conflicts)" true
      | _ -> check "split disjunct should be constraining" false);
     Lia_adapter.pop fx.adapter 1
   | _ -> ());
  (* Split -> Sat end-to-end: x = y ∧ x + y >= 1 lands the rational vertex at x=y=1/2 but
     is ℤ-SAT (x=y=1); driving the upper disjunct [x>=1] reaches an integral model. *)
  let fx = make_fixture 2 in
  assert_eq fx fx.vars.(0) fx.vars.(1);
  (* x = y *)
  ignore (assert_le fx [ 0, -1; 1, -1 ] 1 ~polarity:true);
  (* -x - y + 1 <= 0 -> x + y >= 1 *)
  match Lia_adapter.check fx.adapter Theory.Final with
  | Theory.Split [ _; ge_t ] ->
    Lia_adapter.push fx.adapter;
    assert_term_true fx ge_t;
    (match Lia_adapter.check fx.adapter Theory.Final with
     | Theory.Sat ->
       let m = Lia_adapter.model fx.adapter in
       check
         "Split -> upper branch -> Sat with integral, satisfying model"
         (match Model.value m fx.vars.(0), Model.value m fx.vars.(1) with
          | Some (Model.Int vx), Some (Model.Int vy) -> vx = vy && vx + vy >= 1
          | _ -> false)
     | _ -> check "upper branch of x=y ∧ x+y>=1 should be Sat" false);
    Lia_adapter.pop fx.adapter 1
  | Theory.Sat ->
    (* If simplex happens to land on an integral vertex, that is also correct. *)
    let m = Lia_adapter.model fx.adapter in
    check
      "x=y ∧ x+y>=1 direct Sat has integral, satisfying model"
      (match Model.value m fx.vars.(0), Model.value m fx.vars.(1) with
       | Some (Model.Int vx), Some (Model.Int vy) -> vx = vy && vx + vy >= 1
       | _ -> false)
  | _ -> check "x=y ∧ x+y>=1 must be Split or Sat, never Conflict" false
;;

(* ================================================================== *)
(* 4. push/pop: restoration, recheck-after-backtrack (E5), frame-scoped explain cache. *)

let test_push_pop () =
  print_endline "push/pop:";
  (* Restoration: a conflict introduced inside a pushed frame is gone after pop. *)
  (let fx = make_fixture 1 in
   ignore (assert_le fx [ 0, 1 ] 0 ~polarity:true);
   (* x <= 0 (base) *)
   Lia_adapter.push fx.adapter;
   ignore (assert_le fx [ 0, 1 ] (-1) ~polarity:false);
   (* x >= 1 (frame) -> conflict *)
   (match propagate fx with
    | Theory.Conflict _ -> check "conflict visible inside pushed frame" true
    | _ -> check "expected conflict in frame" false);
   Lia_adapter.pop fx.adapter 1;
   match propagate fx with
   | Theory.Conflict _ -> check "conflict gone after pop of its frame" false
   | Theory.Propagations _ -> check "post-pop base (x<=0 only) is consistent" true
   | _ -> check "unexpected verdict post-pop" false);
  (* Recheck-after-backtrack (calibration E5 / L3 family): a conflict established at ROOT
     must SURVIVE an unrelated push/pop and be re-reported. *)
  (let fx = make_fixture 1 in
   ignore (assert_le fx [ 0, 1 ] 0 ~polarity:true);
   (* x <= 0 at root *)
   ignore (assert_le fx [ 0, 1 ] (-1) ~polarity:false);
   (* x >= 1 at root -> root conflict *)
   (match propagate fx with
    | Theory.Conflict _ -> ()
    | _ -> check "root conflict expected" false);
   Lia_adapter.push fx.adapter;
   Lia_adapter.pop fx.adapter 1;
   match propagate fx with
   | Theory.Conflict _ -> check "root conflict survives an unrelated push/pop (E5)" true
   | _ -> check "root conflict LOST after push/pop (L3 regression)" false);
  (* Frame-scoped explain cache: a reason cached while propagating in a frame is dropped
     on pop of that frame (sole-witness discipline — no stale premise served). *)
  let fx = make_fixture 1 in
  let atom_a, _ = register_le fx [ 0, 1 ] (-5) in
  (* A: x <= 5, registered at root *)
  let lit_a_true = Lit.make atom_a true in
  Lia_adapter.push fx.adapter;
  ignore (assert_le fx [ 0, 1 ] (-3) ~polarity:true);
  (* B: x <= 3 in the frame -> propagates A *)
  (match propagate fx with
   | Theory.Propagations lits ->
     check "A propagated inside frame" (List.mem lit_a_true lits)
   | _ -> check "expected in-frame propagation" false);
  check
    "explain works while frame is live"
    ((Lia_adapter.explain fx.adapter lit_a_true).Explanation.rule
     = Explanation.Rule_tag.Lia_bound);
  Lia_adapter.pop fx.adapter 1;
  check_raises
    "explain of A raises after its frame is popped (cache cleared, no stale reason)"
    (function
      | Failure _ -> true
      | _ -> false)
    (fun () -> Lia_adapter.explain fx.adapter lit_a_true)
;;

(* ================================================================== *)
(* 5. core-bignum W2: the system that used to overflow int63 during check's pivot now
   PROMOTES to Big and the ℚ-simplex completes. The residual native-int ceiling is only
   the OUTPUT projection (R1): the ℤ model binds y = -2·max_int (Big), so MODEL EXTRACTION
   raises Rational.Overflow — which the session's build_model catch degrades to unknown
   (never a truncated model). Adapter-level view of the R1 model-value sink. *)

let mk_overflowing () =
  let fx = make_fixture 2 in
  ignore (assert_le fx [ 0, max_int; 1, 1 ] 0 ~polarity:true);
  (* max_int·x + y <= 0 *)
  ignore (assert_le fx [ 0, -1 ] 2 ~polarity:true);
  (* -x + 2 <= 0 -> x >= 2; the pivot computes max_int·2, which now PROMOTES (no overflow) *)
  fx
;;

let test_poison () =
  print_endline "W2 promote + R1 model-value sink (adapter):";
  (* check PROMOTES: no Rational.Overflow, no poison, nothing counted — the ℚ-simplex is
     feasible and the pivot's max_int·2 grows to Big transparently. *)
  (let fx = mk_overflowing () in
   let v = Lia_adapter.check fx.adapter Theory.Final in
   check
     "check Final PROMOTES to a real verdict (Sat/Split, no Overflow/poison)"
     (match v with
      | Theory.Sat | Theory.Split _ -> true
      | _ -> false);
   check
     "no degradation counted (promotion, not an overflow ceiling)"
     (Lia_adapter.overflows_to_unknown fx.adapter = 0);
   check
     "not poisoned (internal growth promotes, I8)"
     (not (Lia_adapter.is_poisoned fx.adapter));
   (* The ℤ model value y = -2·max_int exceeds int63: extracting it hits the R1 projection
      sink and raises Rational.Overflow (the session degrades that to unknown; never a
      truncated model). *)
   check_raises
     "model extraction hits the R1 int-projection sink -> Rational.Overflow"
     is_overflow
     (fun () -> ignore (Lia_adapter.model fx.adapter)));
  (* A fresh adapter is unaffected. *)
  let fx = make_fixture 1 in
  ignore (assert_le fx [ 0, 1 ] (-3) ~polarity:true);
  (match Lia_adapter.check fx.adapter Theory.Final with
   | Theory.Sat -> check "fresh adapter solves x<=3" true
   | _ -> check "fresh adapter should solve x<=3" false);
  check "fresh adapter not poisoned" (not (Lia_adapter.is_poisoned fx.adapter));
  check "fresh adapter overflow count 0" (Lia_adapter.overflows_to_unknown fx.adapter = 0)
;;

(* ================================================================== *)
(* 6. Idempotent register (C7) + wide-but-safe coefficients (no overflow -> real verdict). *)

let test_idempotent_and_wide () =
  print_endline "idempotence + wide coeffs:";
  (* Registering the SAME atom twice and asserting the same literal twice is a no-op on
     the verdict. *)
  (let fx = make_fixture 1 in
   let term = mk_le fx [ 0, 1 ] 0 in
   let atom = Atom.fresh fx.alloc in
   Lia_adapter.register_atom fx.adapter atom term;
   Lia_adapter.register_atom fx.adapter atom term;
   let lit = Lit.make atom true in
   Lia_adapter.assert_lit fx.adapter lit;
   Lia_adapter.assert_lit fx.adapter lit;
   ignore (assert_le fx [ 0, 1 ] (-1) ~polarity:false);
   match propagate fx with
   | Theory.Conflict _ ->
     check "idempotent register/assert still yields the conflict" true
   | _ -> check "x<=0 ∧ x>=1 conflict after duplicate register/assert" false);
  (* Wide but overflow-safe coefficients: correct verdict, no degradation. 10^9 · x <= 0
     and x >= 1 -> infeasible, well within native int. *)
  let big = 1_000_000_000 in
  let fx = make_fixture 1 in
  ignore (assert_le fx [ 0, big ] 0 ~polarity:true);
  (* big·x <= 0 -> x <= 0 *)
  ignore (assert_le fx [ 0, 1 ] (-1) ~polarity:false);
  (* x >= 1 *)
  (match propagate fx with
   | Theory.Conflict _ -> check "wide-coeff infeasible -> Conflict (no overflow)" true
   | _ -> check "wide-coeff system should conflict" false);
  check "wide-coeff run did not overflow" (Lia_adapter.overflows_to_unknown fx.adapter = 0)
;;

(* ================================================================== *)
(* 7. Determinism (I6): identical op sequences -> identical verdict + pivot count. *)

let scenario () =
  let fx = make_fixture 3 in
  ignore (assert_le fx [ 0, 1; 1, -1 ] 0 ~polarity:true);
  ignore (assert_le fx [ 1, 1; 2, -1 ] 0 ~polarity:true);
  ignore (assert_le fx [ 0, -1 ] 2 ~polarity:true);
  let verdict =
    match Lia_adapter.check fx.adapter Theory.Final with
    | Theory.Sat -> "sat"
    | Theory.Split _ -> "split"
    | Theory.Conflict _ -> "conflict"
    | Theory.Propagations _ -> "prop"
  in
  verdict, Lia_adapter.pivot_count fx.adapter
;;

let test_determinism () =
  print_endline "determinism:";
  let v1, p1 = scenario () in
  let v2, p2 = scenario () in
  check "identical verdict across runs" (v1 = v2);
  check "identical pivot count across runs" (p1 = p2)
;;

(* ================================================================== *)
(* 8. Empty-premise tripwire (LIA parity with Euf_adapter's codex AP4). A conflict or a
   propagation whose reason set is EMPTY is a soundness bug — a premise-free conflict
   would learn the empty clause (spurious [unsat]); a premise-free propagation is an
   unconditional entailment. The adapter's reason builders must fail-close (raise,
   degrading to unknown) rather than hand CDCL(T) an unsound reason. This drives the
   tripwire's own path directly: with the guard removed both empty-premise cases return a
   value and these checks go RED. *)

let test_empty_premise_tripwire () =
  print_endline "empty-premise tripwire (AP4 parity):";
  let is_tripwire = function
    | Failure msg ->
      (* substring match: the message names the AP4 tripwire *)
      let needle = "codex AP4 tripwire" in
      let rec has i =
        i + String.length needle <= String.length msg
        && (String.sub msg i (String.length needle) = needle || has (i + 1))
      in
      has 0
    | _ -> false
  in
  let alloc = Atom.create_allocator () in
  let a_lit = Lit.make (Atom.fresh alloc) true in
  (* Happy path: a non-empty reason builds normally with the right rule tag (proves the
     guard is not over-firing on legitimate reasons). *)
  let c_ok =
    Lia_adapter.conflict_explanation { Lia.premises = [ a_lit ]; farkas = [ q 1 ] }
  in
  check
    "non-empty conflict builds (rule Lia_farkas)"
    (c_ok.Explanation.rule = Explanation.Rule_tag.Lia_farkas);
  check "non-empty conflict keeps its premise" (c_ok.Explanation.premises = [ a_lit ]);
  let p_ok = Lia_adapter.propagation_reason [ a_lit ] in
  check
    "non-empty propagation reason builds (rule Lia_bound)"
    (p_ok.Explanation.rule = Explanation.Rule_tag.Lia_bound);
  (* Discriminating cases: an EMPTY reason set trips the guard for both a conflict and a
     propagation. *)
  check_raises "empty conflict premise set trips tripwire" is_tripwire (fun () ->
    Lia_adapter.conflict_explanation { Lia.premises = []; farkas = [] });
  check_raises "empty propagation reason trips tripwire" is_tripwire (fun () ->
    Lia_adapter.propagation_reason [])
;;

(* ================================================================== *)

let () =
  print_endline "== Lia_adapter tests ==";
  test_currency ();
  test_propagate_explain ();
  test_reprop_precedence ();
  test_reentail_after_frame_pop ();
  test_final_split ();
  test_push_pop ();
  test_poison ();
  test_idempotent_and_wide ();
  test_determinism ();
  test_empty_premise_tripwire ();
  Printf.printf "\n%d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
