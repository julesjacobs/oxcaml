module Sat = Oxsmt_solver.Sat

(* Unit self-test for the CDCL(T) theory-callback seam (ADR-0005 §3, TASKS.md
   M4-adapters).

   The seam is exercised through a tiny SCRIPTED MOCK theory (below), not a real EUF/LIA
   engine — those plug in at M4 and are tested separately. The mock is deliberately dumb:
   it recognizes hard-coded premise sets as conflicts and hard-coded (antecedents ⇒
   consequent) rules as propagations, and it maintains its own assertion trail purely from
   the seam signals [on_assign]/[on_backtrack] (tagging each fact with the decision level
   it queries via {!Sat.decision_level}). That is exactly what a real adapter does, so the
   mock is a faithful stand-in for validating the seam.

   Coverage (spec acceptance):
   - theory conflict at various trail depths (level 1, 2, 3);
   - propagation, then a conflict involving the propagated literal;
   - the lazy reason is retrieved via [explain] EXACTLY when conflict analysis resolves a
     theory-propagated literal — and never otherwise;
   - push/pop synchronization: the mock asserts, on every [check], that no fact it holds
     sits above the current decision level — so a missing/incorrect [on_backtrack] (theory
     not popped on backjump, a propagation surviving its level) is caught; run across many
     backjumps AND Luby restarts via pigeonhole;
   - a final-check split ([T_lemma]) that forces further search and changes the model;
   - a no-theory regression: an inert (consistent, silent) theory yields verdicts, models,
     and the full counter trio bit-identical to no theory at all.

   Stdlib-only; deterministic. Nonzero exit on any failed check. *)

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let dimacs_of_lit l =
  let v = Sat.var_of_lit l + 1 in
  if Sat.sign_of_lit l then v else -v
;;

let sorted_dimacs lits = List.sort compare (List.map dimacs_of_lit (Array.to_list lits))
let show_ints xs = "[" ^ String.concat ";" (List.map string_of_int xs) ^ "]"

(* Collect (learned clause as sorted dimacs, sorted antecedent ids, btlevel) per learned
   clause, via the proof-readiness trace hook — used to pin the exact clause the seam
   learns from a theory conflict. *)
let with_collector s =
  let acc = ref [] in
  Sat.set_trace
    s
    (Some
       { Sat.on_learned =
           (fun ~id ~clause ~antecedents ~btlevel ->
             ignore id;
             acc := (sorted_dimacs clause, List.sort compare antecedents, btlevel) :: !acc)
       ; on_input = (fun ~id:_ ~clause:_ ~origin:_ -> ())
       ; on_unit = (fun ~id:_ ~lit:_ -> ())
       ; on_theory_clause = (fun ~id:_ ~clause:_ ~role:_ -> ())
       ; on_unsat = (fun _ -> ())
       });
  fun () -> List.rev !acc
;;

(* ------------------------------------------------------------------ *)
(* The scripted mock theory. *)

type mock_config =
  { conflicts : Sat.lit list list
      (* each premise set: if all its literals are currently asserted true, it is a theory
         conflict (its conjunction is "T-unsat") *)
  ; implications : (Sat.lit list * Sat.lit) list
      (* (antecedents, consequent): all antecedents asserted true ⇒ propagate consequent,
         with the antecedents as its lazy explanation *)
  ; final_conflicts :
      Sat.lit list list (* extra conflicts recognized only at Final effort *)
  ; final_splits : Sat.lit list list
      (* clauses emitted one-at-a-time at Final effort (CONTRACT-SPLIT disjunctions) until
         all are exhausted *)
  ; explain_override : (Sat.lit -> Sat.lit list) option
      (* if set, [explain] returns this instead of the rule antecedents — used to inject a
         CONTRACT-EX-violating reason and confirm the core raises rather than trusts it *)
  ; propose_once : bool
  (* if true, each consequent is proposed at most once across the whole solve (a latch
     that survives backtracking). Lets a negative test isolate a single guarded path: with
     the guard removed, the solve then TERMINATES without re-proposing into a different
     guard, so the revert-check produces a clean RED instead of raising elsewhere or
     looping. *)
  }

let empty_config =
  { conflicts = []
  ; implications = []
  ; final_conflicts = []
  ; final_splits = []
  ; explain_override = None
  ; propose_once = false
  }
;;

type mock =
  { theory : Sat.theory
  ; explain_calls : int ref
  ; backtracks : int ref
  ; splits_emitted : int ref
  ; invariant_ok : bool ref
  ; trail :
      (Sat.lit * int) list ref (* (asserted literal, decision level), newest first *)
  }

let make_mock st config =
  let trail = ref [] in
  let explain_calls = ref 0 in
  let backtracks = ref 0 in
  let splits_emitted = ref 0 in
  let invariant_ok = ref true in
  let is_true l = List.exists (fun (x, _) -> x = l) !trail in
  let is_false l = is_true (Sat.neg_lit l) in
  let on_assign l = trail := (l, Sat.decision_level st) :: !trail in
  let on_backtrack ~level =
    incr backtracks;
    (* the seam fires on_backtrack after unwinding the Boolean trail, so the solver's
       decision level already equals [level] *)
    if Sat.decision_level st <> level then invariant_ok := false;
    trail := List.filter (fun (_, lv) -> lv <= level) !trail
  in
  let all_true ls = List.for_all is_true ls in
  (* remaining (not-yet-true) splits, popped one at a time *)
  let pending_splits = ref config.final_splits in
  (* consequents already proposed at least once (a latch, not cleared on backtrack) *)
  let proposed = ref [] in
  let check ~final =
    (* push/pop synchronization oracle: NOTHING the theory currently holds may sit above
       the solver's current decision level. A backjump/restart that failed to pop, or a
       theory propagation that outlived its level, violates this. *)
    let dl = Sat.decision_level st in
    if not (List.for_all (fun (_, lv) -> lv <= dl) !trail) then invariant_ok := false;
    let conflict_sets =
      if final then config.conflicts @ config.final_conflicts else config.conflicts
    in
    match List.find_opt all_true conflict_sets with
    | Some premises -> Sat.T_conflict premises
    | None ->
      (* propagate every consequent whose antecedents all hold and that is not already
         asserted true *)
      let props =
        List.filter_map
          (fun (ants, cons) ->
            if all_true ants
               && (not (is_true cons))
               && not (config.propose_once && List.mem cons !proposed)
            then Some cons
            else None)
          config.implications
      in
      let props = List.sort_uniq compare props in
      if config.propose_once then proposed := props @ !proposed;
      if props <> []
      then Sat.T_consistent props
      else if final
      then (
        match !pending_splits with
        | s :: rest ->
          pending_splits := rest;
          incr splits_emitted;
          Sat.T_lemma [ s ]
        | [] -> Sat.T_consistent [])
      else Sat.T_consistent []
  in
  let explain l =
    incr explain_calls;
    match config.explain_override with
    | Some f -> f l
    | None ->
      (* the reason for a propagated literal is the antecedents of the rule that fired it *)
      (match List.find_opt (fun (_, cons) -> cons = l) config.implications with
       | Some (ants, _) -> ants
       | None -> [])
  in
  ignore is_false;
  { theory = { Sat.on_assign; on_backtrack; check; explain }
  ; explain_calls
  ; backtracks
  ; splits_emitted
  ; invariant_ok
  ; trail
  }
;;

(* ------------------------------------------------------------------ *)
(* Theory conflict at increasing trail depths. Assumptions decide their literals at
   successive levels, so a conflict over [{first k assumptions}] surfaces at level k. *)

let test_conflict_at_depth () =
  let run depth =
    let s = Sat.create () in
    let vs = Array.init depth (fun _ -> Sat.new_var s) in
    let lits = Array.to_list (Array.map Sat.pos vs) in
    let mock = make_mock s { empty_config with conflicts = [ lits ] } in
    Sat.set_theory s (Some mock.theory);
    let r = Sat.solve ~assumptions:lits s in
    check (Printf.sprintf "conflict-depth-%d: unsat" depth) (r = Sat.Unsat);
    let failed = List.sort compare (List.map dimacs_of_lit (Sat.failed_assumptions s)) in
    let expected = List.init depth (fun i -> i + 1) in
    check
      (Printf.sprintf
         "conflict-depth-%d: failed core = %s (got %s)"
         depth
         (show_ints expected)
         (show_ints failed))
      (failed = expected);
    check
      (Printf.sprintf "conflict-depth-%d: push/pop invariant held" depth)
      !(mock.invariant_ok)
  in
  run 1;
  run 2;
  run 3
;;

(* Pin the EXACT learned clause from a two-literal theory conflict: it must be the
   negation of the (precedence-valid) premise set, learned like any propositional
   conflict. *)
let test_conflict_learns_negated_premises () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s in
  let la = Sat.pos a
  and lb = Sat.pos b in
  let mock = make_mock s { empty_config with conflicts = [ [ la; lb ] ] } in
  Sat.set_theory s (Some mock.theory);
  let learned = with_collector s in
  let r = Sat.solve ~assumptions:[ la; lb ] s in
  let ls = learned () in
  check "learn-neg: unsat" (r = Sat.Unsat);
  check "learn-neg: exactly one learned clause" (List.length ls = 1);
  (match ls with
   | [ (clause, _ants, bt) ] ->
     check
       (Printf.sprintf "learn-neg: learned = [-2;-1] (got %s)" (show_ints clause))
       (clause = [ -2; -1 ]);
     check "learn-neg: backjump to level 1" (bt = 1)
   | _ -> ());
  check "learn-neg: push/pop invariant held" !(mock.invariant_ok)
;;

(* ------------------------------------------------------------------ *)
(* Theory propagation, then a conflict that involves the propagated literal. Under
   assumption a: rule [{a}] ⇒ c propagates c at level 1; then assumption b at level 2 and
   conflict [{c, b}]. The 1UIP over the negated premises is (¬c ∨ ¬b). *)

let test_propagate_then_conflict () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s
  and c = Sat.new_var s in
  let la = Sat.pos a
  and lb = Sat.pos b
  and lc = Sat.pos c in
  let mock =
    make_mock
      s
      { empty_config with implications = [ [ la ], lc ]; conflicts = [ [ lc; lb ] ] }
  in
  Sat.set_theory s (Some mock.theory);
  let learned = with_collector s in
  let r = Sat.solve ~assumptions:[ la; lb ] s in
  let ls = learned () in
  check "prop-conflict: unsat" (r = Sat.Unsat);
  (match ls with
   | (clause, _, _) :: _ ->
     check
       (Printf.sprintf
          "prop-conflict: first learned = [-3;-2] (got %s)"
          (show_ints clause))
       (clause = [ -3; -2 ])
   | [] -> check "prop-conflict: at least one learned clause" false);
  check "prop-conflict: push/pop invariant held" !(mock.invariant_ok)
;;

(* ------------------------------------------------------------------ *)
(* Lazy explanation. A theory-propagated literal that is RESOLVED during 1UIP (i.e. it is
   more recent than the UIP) forces the core to call [explain] — and only then.

   Chain at level 1: decision a ; clause (¬a ∨ b) propagates b (Boolean) ; rule [{a}] ⇒ c
   propagates c (theory) ; clause (¬b ∨ ¬c) conflicts. 1UIP resolves c against its lazy
   reason on the way back to the UIP a, so [explain c] is called; the learned clause is
   the unit (¬a). *)

let test_lazy_explain_called () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s
  and c = Sat.new_var s in
  let la = Sat.pos a
  and lb = Sat.pos b
  and lc = Sat.pos c in
  (* pristine-attach: install the theory before any clause *)
  let mock = make_mock s { empty_config with implications = [ [ la ], lc ] } in
  Sat.set_theory s (Some mock.theory);
  Sat.add_clause s [ Sat.neg a; lb ];
  (* ¬a ∨ b *)
  Sat.add_clause s [ Sat.neg b; Sat.neg c ];
  (* ¬b ∨ ¬c *)
  let learned = with_collector s in
  let r = Sat.solve ~assumptions:[ la ] s in
  let ls = learned () in
  check "lazy: unsat under a" (r = Sat.Unsat);
  check
    (Printf.sprintf "lazy: explain called (%d times)" !(mock.explain_calls))
    (!(mock.explain_calls) >= 1);
  (match ls with
   | (clause, _, _) :: _ ->
     check
       (Printf.sprintf "lazy: learned unit = [-1] (got %s)" (show_ints clause))
       (clause = [ -1 ])
   | [] -> check "lazy: at least one learned clause" false);
  check "lazy: push/pop invariant held" !(mock.invariant_ok)
;;

(* Laziness the other way: a theory literal that is propagated but NEVER enters a conflict
   must not be explained. Rule [{a}] ⇒ c fires under assumption a; the run is satisfiable
   with no conflict, so [explain] is never called. *)
let test_lazy_explain_not_called () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and c = Sat.new_var s in
  ignore c;
  let la = Sat.pos a
  and lc = Sat.pos c in
  let mock = make_mock s { empty_config with implications = [ [ la ], lc ] } in
  Sat.set_theory s (Some mock.theory);
  let r = Sat.solve ~assumptions:[ la ] s in
  check "lazy-not: sat" (r = Sat.Sat);
  check "lazy-not: c propagated true" (Sat.value s c);
  check
    (Printf.sprintf "lazy-not: explain never called (%d)" !(mock.explain_calls))
    (!(mock.explain_calls) = 0);
  check "lazy-not: push/pop invariant held" !(mock.invariant_ok)
;;

(* Propagation INTO an already-false literal. c is fixed false by a level-0 unit (¬c);
   under assumption a the rule [{a}] ⇒ c fires, but c is false ⇒ the seam turns it into an
   immediate theory conflict (clause [c ∨ ¬a], all-false) rather than enqueuing a false
   literal. Exercises the theory_prop_conflict_clause path. *)
let test_propagate_into_false () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and c = Sat.new_var s in
  let la = Sat.pos a
  and lc = Sat.pos c in
  let mock = make_mock s { empty_config with implications = [ [ la ], lc ] } in
  Sat.set_theory s (Some mock.theory);
  Sat.add_clause s [ Sat.neg c ];
  (* ¬c: c false at level 0 *)
  let r = Sat.solve ~assumptions:[ la ] s in
  check "prop-false: unsat under a" (r = Sat.Unsat);
  check
    (Printf.sprintf "prop-false: explain called (%d)" !(mock.explain_calls))
    (!(mock.explain_calls) >= 1);
  check "prop-false: push/pop invariant held" !(mock.invariant_ok)
;;

(* Lazy explain of a LOW-level theory literal AFTER a backjump popped the higher levels
   that surrounded the conflict. Level 1: assume a ⇒ rule [{a}] ⇒ c propagates c (theory).
   Level 2: assume b; clauses (¬c∨¬b∨e) and (¬c∨¬b∨¬e) conflict on e. 1UIP learns (¬b∨¬c)
   and backjumps to level 1 (popping e and b), keeping a and c. ¬c then forces ¬b, the
   assumption b fails, and the failed-core trace resolves c's Theory_prop reason — calling
   [explain c] after the higher levels are gone. c and its premise a survived at level 1,
   so the reconstruction is valid. *)
let test_explain_after_backjump () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s
  and c = Sat.new_var s
  and e = Sat.new_var s in
  let la = Sat.pos a
  and lb = Sat.pos b
  and lc = Sat.pos c
  and le = Sat.pos e in
  let mock = make_mock s { empty_config with implications = [ [ la ], lc ] } in
  Sat.set_theory s (Some mock.theory);
  Sat.add_clause s [ Sat.neg c; Sat.neg b; le ];
  Sat.add_clause s [ Sat.neg c; Sat.neg b; Sat.neg e ];
  let learned = with_collector s in
  let r = Sat.solve ~assumptions:[ la; lb ] s in
  let ls = learned () in
  check "explain-bj: unsat" (r = Sat.Unsat);
  let failed = List.sort compare (List.map dimacs_of_lit (Sat.failed_assumptions s)) in
  check
    (Printf.sprintf "explain-bj: failed core = [1;2] (got %s)" (show_ints failed))
    (failed = [ 1; 2 ]);
  check
    "explain-bj: a partial backjump to level 1 occurred (higher level popped)"
    (List.exists (fun (_, _, bt) -> bt = 1) ls);
  check
    (Printf.sprintf
       "explain-bj: explain called after backjump (%d)"
       !(mock.explain_calls))
    (!(mock.explain_calls) >= 1);
  check "explain-bj: push/pop invariant held" !(mock.invariant_ok)
;;

(* Lifecycle: attaching a theory after a clause has been asserted must raise (pristine-
   attach). The reordered tests above show pristine attach succeeds; this pins the guard. *)
let test_set_theory_after_assert_raises () =
  let s = Sat.create () in
  let a = Sat.new_var s in
  Sat.add_clause s [ Sat.pos a ];
  let mock = make_mock s empty_config in
  let raised =
    match Sat.set_theory s (Some mock.theory) with
    | () -> false
    | exception Invalid_argument _ -> true
  in
  check "attach-after-assert: raises" raised
;;

(* Lifecycle, subtle case: an unconditional theory conflict (T_conflict []) sets the
   solver's ok flag false with NOTHING stored — no clauses, empty trail — so it LOOKS
   pristine. Re-(de)attaching a theory on it must still raise: a poisoned solver is not
   pristine, and reusing it would return wrong-unsat off the leftover flag. *)
let test_poisoned_not_pristine () =
  let s = Sat.create () in
  (* conflict set [] is vacuously "all asserted" ⇒ the theory reports T_conflict [] on the
     first check, an unconditional contradiction *)
  let mock = make_mock s { empty_config with conflicts = [ [] ] } in
  Sat.set_theory s (Some mock.theory);
  let r = Sat.solve s in
  check "poison: unconditional T_conflict [] → unsat" (r = Sat.Unsat);
  let raised =
    match Sat.set_theory s None with
    | () -> false
    | exception Invalid_argument _ -> true
  in
  check "poison: (de)attach on a poisoned (looks-pristine) solver raises" raised
;;

(* Negative test for the strict CONTRACT-EX guard on BOTH paths that reconstruct a theory
   reason. A theory that propagates c (from a) but then EXPLAINS c with an unasserted
   premise d (trail_pos -1, precedence-violating) must make the core RAISE
   Theory_contract_violation, not silently learn a bogus clause / core. *)
let solve_raises_contract_violation s assumptions =
  match Sat.solve ~assumptions s with
  | _ -> false
  | exception Sat.Theory_contract_violation _ -> true
;;

(* The batch shape that genuinely drives {!theory_reason_clause} (the textbook 1UIP
   lazy-reason path). Under assumption a, the theory propagates BOTH c and d TRUE in one
   [T_consistent [c; d]] batch (nothing forces them false first — the only clause, ¬c∨¬d,
   is not unit while both are unknown). Re-propagation then conflicts ¬c∨¬d, and 1UIP
   resolves the true theory literals d and c against their lazy reasons — so
   [theory.explain] flows through [theory_reason_clause], NOT the already-false
   [theory_prop_conflict_clause] path. (A plain rule ⇒ single-consequent shape does NOT
   reach here: Boolean BCP reaches fixpoint before the theory check, so a clause
   mentioning the consequent propagates it false first, routing through the falsification
   guard.) *)
let reason_clause_setup ?(bad_explain = false) () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and c = Sat.new_var s
  and d = Sat.new_var s
  and e = Sat.new_var s in
  let la = Sat.pos a
  and lc = Sat.pos c
  and ld = Sat.pos d in
  (* [e] is allocated but NEVER asserted; a bad explanation citing it violates strict
     precedence (trail_pos -1) — an allocated var, so we hit the guard, not an out-of-
     bounds access *)
  let explain_override = if bad_explain then Some (fun _ -> [ Sat.pos e ]) else None in
  let mock =
    make_mock
      s
      { empty_config with
        implications = [ [ la ], lc; [ la ], ld ]
      ; explain_override
      ; propose_once = true
      }
  in
  Sat.set_theory s (Some mock.theory);
  Sat.add_clause s [ Sat.neg c; Sat.neg d ];
  s, mock, la
;;

(* Happy path through theory_reason_clause, constructed so the reason clause's CONTENT
   (not just which var is the UIP) flows into the learned clause — so breaking the
   construction (e.g. dropping the negation of the premises) changes the learned clause
   and turns this RED. Two levels: assume p (level 1), then a (level 2). With [{p,a}] the
   theory propagates BOTH c and d TRUE at level 2 (explain each = [p; a]); the clause
   ¬c∨¬d then conflicts. 1UIP resolves the two true theory literals d and c against their
   reasons: the UIP is a (level 2), and each reason contributes its lower-level premise ¬p
   (level 1) to the learned clause. So the learned clause is [¬a ∨ ¬p], backjumping to
   level 1 — and the ¬p literal is exactly what a mis-built reason clause (un-negated
   premise) corrupts to +p. *)
let test_reason_clause_1uip () =
  let s = Sat.create () in
  let p = Sat.new_var s
  and a = Sat.new_var s
  and c = Sat.new_var s
  and d = Sat.new_var s in
  let lp = Sat.pos p
  and la = Sat.pos a
  and lc = Sat.pos c
  and ld = Sat.pos d in
  let mock =
    make_mock s { empty_config with implications = [ [ lp; la ], lc; [ lp; la ], ld ] }
  in
  Sat.set_theory s (Some mock.theory);
  Sat.add_clause s [ Sat.neg c; Sat.neg d ];
  (* ¬c ∨ ¬d *)
  let learned = with_collector s in
  let r = Sat.solve ~assumptions:[ lp; la ] s in
  let ls = learned () in
  check "reason-1uip: unsat under p,a" (r = Sat.Unsat);
  (match ls with
   | (clause, _ants, bt) :: _ ->
     check
       (Printf.sprintf "reason-1uip: first learned = [-2;-1] (got %s)" (show_ints clause))
       (clause = [ -2; -1 ]);
     check (Printf.sprintf "reason-1uip: backjump to level 1 (got %d)" bt) (bt = 1)
   | [] -> check "reason-1uip: at least one learned clause" false);
  check
    (Printf.sprintf "reason-1uip: explain consulted in 1UIP (%d)" !(mock.explain_calls))
    (!(mock.explain_calls) >= 1);
  check "reason-1uip: push/pop invariant held" !(mock.invariant_ok)
;;

(* Negative test on the SAME batch shape: a precedence-violating explanation (an
   unasserted premise) resolved in 1UIP must make theory_reason_clause raise
   Theory_contract_violation rather than learn a bogus clause. Deleting/neutering the
   strict guard makes this pass silently — i.e. this test goes RED without the guard. *)
let test_bad_explain_1uip () =
  let s, _mock, la = reason_clause_setup ~bad_explain:true () in
  check
    "bad-explain-1uip: raises Theory_contract_violation (via theory_reason_clause)"
    (solve_raises_contract_violation s [ la ])
;;

let test_bad_explain_final () =
  (* test_explain_after_backjump shape: explain(c) is consulted by analyze_final; the same
     guard must fire there too. *)
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s
  and c = Sat.new_var s
  and e = Sat.new_var s
  and d = Sat.new_var s in
  let la = Sat.pos a
  and lb = Sat.pos b
  and lc = Sat.pos c
  and le = Sat.pos e
  and ld = Sat.pos d in
  let mock =
    make_mock
      s
      { empty_config with
        implications = [ [ la ], lc ]
      ; explain_override = Some (fun _ -> [ ld ])
      }
  in
  Sat.set_theory s (Some mock.theory);
  Sat.add_clause s [ Sat.neg c; Sat.neg b; le ];
  Sat.add_clause s [ Sat.neg c; Sat.neg b; Sat.neg e ];
  check
    "bad-explain-final: raises Theory_contract_violation"
    (solve_raises_contract_violation s [ la; lb ])
;;

(* ------------------------------------------------------------------ *)
(* Final-check split (T_lemma). With no clauses and phase-saving deciding false-first, the
   solver reaches the all-false model; the theory then splits on (b ∨ c), forcing more
   search until the model satisfies it. The split fires exactly once, then the theory
   accepts. *)

let test_final_split () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s
  and c = Sat.new_var s in
  ignore a;
  let lb = Sat.pos b
  and lc = Sat.pos c in
  let mock = make_mock s { empty_config with final_splits = [ [ lb; lc ] ] } in
  Sat.set_theory s (Some mock.theory);
  let r = Sat.solve s in
  check "split: sat" (r = Sat.Sat);
  check "split: emitted exactly once" (!(mock.splits_emitted) = 1);
  check "split: model satisfies b ∨ c" (Sat.value s b || Sat.value s c);
  check "split: push/pop invariant held" !(mock.invariant_ok)
;;

(* A Final split that simplifies to the empty clause at level 0 ⇒ unsat (merge-blocker
   regression). Units ¬p, ¬q; the theory splits on (p ∨ q), which is falsified at level 0
   → add_clause derives the empty clause (t.ok := false). The solver MUST conclude unsat,
   not run on to a full model and report a spurious Sat. *)
let test_final_split_empty_unsat () =
  let s = Sat.create () in
  let p = Sat.new_var s
  and q = Sat.new_var s in
  let lp = Sat.pos p
  and lq = Sat.pos q in
  let mock = make_mock s { empty_config with final_splits = [ [ lp; lq ] ] } in
  Sat.set_theory s (Some mock.theory);
  Sat.add_clause s [ Sat.neg p ];
  Sat.add_clause s [ Sat.neg q ];
  let r = Sat.solve s in
  check "split-empty: unsat (not spurious sat)" (r = Sat.Unsat);
  check "split-empty: split emitted once" (!(mock.splits_emitted) = 1);
  check "split-empty: push/pop invariant held" !(mock.invariant_ok)
;;

(* A Final-effort conflict rejects a full model. With unit (a) forcing a at level 0, the
   Final check refuses any model with a true, so the query is unsat. *)
let test_final_conflict () =
  let s = Sat.create () in
  let a = Sat.new_var s in
  let la = Sat.pos a in
  let mock = make_mock s { empty_config with final_conflicts = [ [ la ] ] } in
  (* set the theory before asserting, so it observes the level-0 unit on the trail *)
  Sat.set_theory s (Some mock.theory);
  Sat.add_clause s [ la ];
  let r = Sat.solve s in
  check "final-conflict: unsat" (r = Sat.Unsat);
  check "final-conflict: push/pop invariant held" !(mock.invariant_ok)
;;

(* ------------------------------------------------------------------ *)
(* Push/pop synchronization stress: an inert (always-consistent, silent) recorder theory
   plugged into pigeonhole PHP(n+1,n). The search runs long enough to trigger Luby
   restarts and many backjumps; the recorder's per-check invariant (no held fact above the
   current decision level) validates that on_backtrack fires with the right level on every
   unwind — restart included. The verdict must stay unsat and the counters must be
   unchanged from the no-theory run (the seam is transparent when the theory makes no
   claims). *)

let build_php s n =
  let pv = Array.make_matrix (n + 1) n 0 in
  for i = 0 to n do
    for h = 0 to n - 1 do
      pv.(i).(h) <- Sat.new_var s
    done
  done;
  for i = 0 to n do
    Sat.add_clause s (List.init n (fun h -> Sat.pos pv.(i).(h)))
  done;
  for h = 0 to n - 1 do
    for i = 0 to n do
      for j = i + 1 to n do
        Sat.add_clause s [ Sat.neg pv.(i).(h); Sat.neg pv.(j).(h) ]
      done
    done
  done
;;

let test_pushpop_stress () =
  let n = 6 in
  (* baseline: no theory *)
  let s0 = Sat.create () in
  build_php s0 n;
  let r0 = Sat.solve s0 in
  let st0 = Sat.stats s0 in
  (* same instance with an inert recorder theory plugged (pristine-attach: before clauses) *)
  let s1 = Sat.create () in
  let mock = make_mock s1 empty_config in
  Sat.set_theory s1 (Some mock.theory);
  build_php s1 n;
  let r1 = Sat.solve s1 in
  let st1 = Sat.stats s1 in
  check "stress: php unsat (baseline)" (r0 = Sat.Unsat);
  check "stress: php unsat (with inert theory)" (r1 = Sat.Unsat);
  check "stress: many conflicts (drove restarts)" (st1.Sat.Stats.conflicts > 100);
  check "stress: on_backtrack fired repeatedly" (!(mock.backtracks) > 10);
  check "stress: push/pop invariant held throughout" !(mock.invariant_ok);
  (* solve leaves the trail unwound to level 0, so only unconditional (level-0) facts may
     remain — never a stale decision-level fact *)
  check
    "stress: only level-0 facts remain after solve"
    (List.for_all (fun (_, lv) -> lv = 0) !(mock.trail));
  check
    "stress: conflicts identical to baseline"
    (st0.Sat.Stats.conflicts = st1.Sat.Stats.conflicts);
  check
    "stress: decisions identical to baseline"
    (st0.Sat.Stats.decisions = st1.Sat.Stats.decisions);
  check
    "stress: propagations identical to baseline"
    (st0.Sat.Stats.propagations = st1.Sat.Stats.propagations)
;;

(* ------------------------------------------------------------------ *)
(* No-theory regression. An inert theory (sees the whole trail, makes no claim) must leave
   verdict, model, and the full counter trio bit-identical to no theory at all, on a batch
   of random formulas — the "seam present but unplugged" acceptance, in its strongest
   (plugged-but-transparent) form. *)

let lcg = ref 0x1E3779B97F4A7C15

let rand () =
  let x = !lcg in
  let x = x lxor (x lsr 12) in
  let x = x lxor (x lsl 25) in
  let x = x lxor (x lsr 27) in
  lcg := x;
  x * 0x2545F4914F6CDD1D land max_int
;;

let rand_n n = rand () mod n

let random_clause num_vars =
  let width = 1 + rand_n 3 in
  List.init width (fun _ ->
    let v = 1 + rand_n num_vars in
    if rand_n 2 = 0 then Sat.pos (v - 1) else Sat.neg (v - 1))
;;

let build_random num_vars clauses =
  let s = Sat.create () in
  for _ = 0 to num_vars - 1 do
    ignore (Sat.new_var s : Sat.var)
  done;
  List.iter (fun cl -> Sat.add_clause s cl) clauses;
  s
;;

let test_no_theory_regression () =
  lcg := 0xC0FFEE123456;
  let n = 4000 in
  let mismatches = ref 0 in
  for _ = 1 to n do
    let num_vars = 3 + rand_n 10 in
    let num_clauses = 1 + rand_n (num_vars * 4) in
    let clauses = List.init num_clauses (fun _ -> random_clause num_vars) in
    let plain = build_random num_vars clauses in
    let r0 = Sat.solve plain in
    let st0 = Sat.stats plain in
    let m0 = Sat.model plain in
    (* pristine-attach: create, allocate vars, install the theory, THEN add clauses *)
    let withth = Sat.create () in
    for _ = 0 to num_vars - 1 do
      ignore (Sat.new_var withth : Sat.var)
    done;
    let mock = make_mock withth empty_config in
    Sat.set_theory withth (Some mock.theory);
    List.iter (fun cl -> Sat.add_clause withth cl) clauses;
    let r1 = Sat.solve withth in
    let st1 = Sat.stats withth in
    let m1 = Sat.model withth in
    if r0 <> r1
       || st0.Sat.Stats.conflicts <> st1.Sat.Stats.conflicts
       || st0.Sat.Stats.decisions <> st1.Sat.Stats.decisions
       || st0.Sat.Stats.propagations <> st1.Sat.Stats.propagations
       || m0 <> m1
       || not !(mock.invariant_ok)
    then incr mismatches
  done;
  check
    (Printf.sprintf
       "regression: inert theory bit-identical to no theory over %d formulas (%d \
        mismatches)"
       n
       !mismatches)
    (!mismatches = 0)
;;

(* ------------------------------------------------------------------ *)

(* CONTRACT-LEMMA (adr-0005-contract-lemma-erratum): a theory-VALID implication lemma
   emitted as the signed clause (¬b ∨ cut) — the propagate-as-lemma shape a derived
   integer cut uses (z3's assign(cut, core)). With the antecedent b forced true, the
   clause unit-propagates the fresh atom cut to TRUE, with b as its reason. The contrast
   run (no lemma → cut decided false by phase-saving) proves the lemma is what propagated
   it, not the default. This is the runtime behavior CONTRACT-LEMMA relies on; it needs no
   seam change (a signed Split clause already clausifies to this via split_lit's
   Not-peeling). *)
let test_valid_lemma_propagates () =
  let setup ~with_lemma =
    let s = Sat.create () in
    let b = Sat.new_var s
    and cut = Sat.new_var s in
    let cfg =
      if with_lemma
      then { empty_config with final_splits = [ [ Sat.neg b; Sat.pos cut ] ] }
      else empty_config
    in
    let mock = make_mock s cfg in
    Sat.set_theory s (Some mock.theory);
    Sat.add_clause s [ Sat.pos b ] (* antecedent forced true *);
    let r = Sat.solve s in
    r, Sat.value s cut, mock
  in
  let r0, cut0, _ = setup ~with_lemma:false in
  check "lemma: baseline sat" (r0 = Sat.Sat);
  check "lemma: without lemma, cut decided false (phase-saving)" (not cut0);
  let r1, cut1, mock = setup ~with_lemma:true in
  check "lemma: sat with lemma" (r1 = Sat.Sat);
  check "lemma: valid lemma (¬b ∨ cut), b true ⇒ cut propagated TRUE" cut1;
  check "lemma: emitted exactly once" (!(mock.splits_emitted) = 1);
  check "lemma: push/pop invariant held" !(mock.invariant_ok)
;;

let () =
  test_conflict_at_depth ();
  test_conflict_learns_negated_premises ();
  test_propagate_then_conflict ();
  test_lazy_explain_called ();
  test_lazy_explain_not_called ();
  test_propagate_into_false ();
  test_explain_after_backjump ();
  test_reason_clause_1uip ();
  test_bad_explain_1uip ();
  test_bad_explain_final ();
  test_set_theory_after_assert_raises ();
  test_poisoned_not_pristine ();
  test_final_split ();
  test_final_split_empty_unsat ();
  test_final_conflict ();
  test_valid_lemma_propagates ();
  test_pushpop_stress ();
  test_no_theory_regression ();
  Printf.printf "seam_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
