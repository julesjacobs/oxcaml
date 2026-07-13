(* Cert step-1 emission wiring self-test (ADR-0013 §4.0). Drives the frozen Sat trace seam
   through {!Oxsmt_certificate.Recorder} and pins that each hook fires with the right data
   for the four Unsat exits + E3 Theory_prop materialization + the ordered-RUP antecedent
   order. Every test is DISCRIMINATING: it fails against the pre-wiring code (hooks that
   never fired, an antecedent order reversed by a stray List.rev, an analyze_final that
   did not materialize Theory_prop reasons). Stdlib-only; deterministic; nonzero exit on
   any failed check.

   The theory tests reuse the seam_test scripted-mock pattern: a dumb theory recognizing
   hard-coded conflict/implication sets, faithful to what a real adapter does at the seam. *)

module Sat = Oxsmt_solver.Sat
module Recorder = Oxsmt_certificate.Recorder

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

(* clause (lit array) as a sorted DIMACS-int set, for content-based identification *)
let dimacs_of_lit l =
  let v = Sat.var_of_lit l + 1 in
  if Sat.sign_of_lit l then v else -v
;;

let clause_set (c : Sat.lit array) =
  List.sort compare (List.map dimacs_of_lit (Array.to_list c))
;;

let show_ints xs = "[" ^ String.concat ";" (List.map string_of_int xs) ^ "]"

(* ------------------------------------------------------------------ *)
(* The scripted mock theory (a trimmed copy of seam_test's, enough for the E3 shape). *)

type mock_config =
  { conflicts : Sat.lit list list
  ; implications : (Sat.lit list * Sat.lit) list
  ; final_splits : Sat.lit list list
  }

let empty_config = { conflicts = []; implications = []; final_splits = [] }

let make_mock st config =
  let trail = ref [] in
  let is_true l = List.exists (fun (x, _) -> x = l) !trail in
  let on_assign l = trail := (l, Sat.decision_level st) :: !trail in
  let on_backtrack ~level = trail := List.filter (fun (_, lv) -> lv <= level) !trail in
  let all_true ls = List.for_all is_true ls in
  let pending_splits = ref config.final_splits in
  let check ~final =
    match List.find_opt all_true config.conflicts with
    | Some premises -> Sat.T_conflict premises
    | None ->
      let props =
        List.filter_map
          (fun (ants, cons) ->
             if all_true ants && not (is_true cons) then Some cons else None)
          config.implications
      in
      let props = List.sort_uniq compare props in
      if props <> []
      then Sat.T_consistent props
      else if final
      then (
        match !pending_splits with
        | s :: rest ->
          pending_splits := rest;
          Sat.T_lemma [ s ]
        | [] -> Sat.T_consistent [])
      else Sat.T_consistent []
  in
  let explain l =
    match List.find_opt (fun (_, cons) -> cons = l) config.implications with
    | Some (ants, _) -> ants
    | None -> []
  in
  { Sat.on_assign; on_backtrack; check; explain }
;;

(* ------------------------------------------------------------------ *)
(* on_input / on_unit fire, before level-0 filtering, with origin. *)

let test_on_input_and_unit () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s in
  let rec_ = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec_));
  Sat.add_clause s [ Sat.pos a ];
  (* unit: on_input + on_unit *)
  Sat.add_clause s [ Sat.pos a; Sat.neg b ];
  (* retained width-2 clause: on_input only *)
  let inputs = Recorder.inputs rec_ in
  let units = Recorder.units rec_ in
  check "input/unit: on_input fired for both clauses" (List.length inputs = 2);
  check
    (Printf.sprintf "input/unit: on_unit fired once (got %d)" (List.length units))
    (List.length units = 1);
  check
    "input/unit: first input is the raw unit clause [1] with Query origin"
    (match inputs with
     | i :: _ -> clause_set i.Recorder.clause = [ 1 ] && i.Recorder.origin = Sat.Query
     | [] -> false);
  check
    "input/unit: recorded unit lit is a"
    (match units with
     | [ u ] -> u.Recorder.lit = Sat.pos a
     | _ -> false);
  (* id-reuse: the retained width-2 clause's on_input id is stable & content-bearing *)
  check "input/unit: no unresolved citations" (Recorder.unresolved_citations rec_ = [])
;;

(* ------------------------------------------------------------------ *)
(* E1 — a Query input that filters to [] (H2): on_input fired for it BEFORE filtering, and
   the terminal Root_empty cites its id (which carries the Query origin). *)

let test_e1_root_empty () =
  let s = Sat.create () in
  let a = Sat.new_var s in
  let rec_ = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec_));
  Sat.add_clause s [ Sat.pos a ];
  (* a := true (unit) *)
  Sat.add_clause s [ Sat.neg a ];
  (* filters to [] under the level-0 unit ⇒ t.ok := false, no [||] among the inputs *)
  let r = Sat.solve s in
  check "e1: unsat" (r = Sat.Unsat);
  (match Recorder.conclusion rec_ with
   | Some (Sat.Root_empty { input_id }) ->
     check "e1: conclusion is Root_empty" true;
     let ev =
       List.find_opt
         (fun (i : Recorder.input_event) -> i.Recorder.id = input_id)
         (Recorder.inputs rec_)
     in
     check
       "e1: input_id resolves to an on_input event for the [-1] clause, Query origin"
       (match ev with
        | Some i -> clause_set i.Recorder.clause = [ -1 ] && i.Recorder.origin = Sat.Query
        | None -> false)
   | _ -> check "e1: conclusion is Root_empty" false);
  check "e1: no unresolved citations" (Recorder.unresolved_citations rec_ = [])
;;

(* ------------------------------------------------------------------ *)
(* E2 — a level-0 conflict discovered by BCP in solve (not at add time): a retained
   width-2 clause [a∨b] is falsified by two later level-0 units ¬a, ¬b. Level0_conflict
   cites the retained clause, and its id resolves to that clause's on_input event
   (id-reuse). *)

let test_e2_level0_conflict () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s in
  let rec_ = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec_));
  Sat.add_clause s [ Sat.pos a; Sat.pos b ];
  (* retained: neither literal decided yet *)
  Sat.add_clause s [ Sat.neg a ];
  (* ¬a unit *)
  Sat.add_clause s [ Sat.neg b ];
  (* ¬b unit; [a∨b] now false but undiscovered until solve's BCP *)
  let r = Sat.solve s in
  check "e2: unsat" (r = Sat.Unsat);
  (match Recorder.conclusion rec_ with
   | Some (Sat.Level0_conflict { conflict_id }) ->
     let ev =
       List.find_opt
         (fun (i : Recorder.input_event) -> i.Recorder.id = conflict_id)
         (Recorder.inputs rec_)
     in
     check
       "e2: conflict_id resolves to the on_input event for [1;2] (id-reuse)"
       (match ev with
        | Some i -> clause_set i.Recorder.clause = [ 1; 2 ]
        | None -> false)
   | _ -> check "e2: conclusion is Level0_conflict" false);
  check "e2: no unresolved citations" (Recorder.unresolved_citations rec_ = [])
;;

(* ------------------------------------------------------------------ *)
(* E3 — the universal session exit + the H1 Theory_prop materialization. Under assumption
   a the theory propagates c (Theory_prop, explain c = [a]); the Boolean clause [¬c∨¬b]
   then forces ¬b, so assumption b is found false ⇒ Failed_assumption. The forcing chain
   crosses c's Theory_prop reason, which analyze_final MUST materialize (H1) — else the
   emitted antecedents skip the theory-propagation ancestor and no Reason leaf is
   surfaced. *)

let test_e3_failed_assumption_theory_prop () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s
  and c = Sat.new_var s in
  let la = Sat.pos a
  and lb = Sat.pos b
  and lc = Sat.pos c in
  let mock = make_mock s { empty_config with implications = [ [ la ], lc ] } in
  Sat.set_theory s (Some mock);
  let rec_ = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec_));
  Sat.add_clause s [ Sat.neg c; Sat.neg b ];
  (* ¬c ∨ ¬b *)
  let r = Sat.solve ~assumptions:[ la; lb ] s in
  check "e3: unsat" (r = Sat.Unsat);
  let failed = List.sort compare (List.map dimacs_of_lit (Sat.failed_assumptions s)) in
  check
    (Printf.sprintf "e3: failed core = [1;2] (got %s)" (show_ints failed))
    (failed = [ 1; 2 ]);
  (match Recorder.conclusion rec_ with
   | Some (Sat.Failed_assumption { antecedents }) ->
     check "e3: conclusion is Failed_assumption" true;
     check "e3: antecedents non-empty" (antecedents <> []);
     (* H1 discrimination: some cited antecedent resolves to a materialized theory Reason
        leaf (c's lazy reason). Without the Theory_prop leg in analyze_final this leaf is
        never emitted nor cited, so this check goes RED. *)
     let theory_reason_ids =
       List.filter_map
         (fun (te : Recorder.theory_event) ->
            if te.Recorder.role = Sat.Reason then Some te.Recorder.id else None)
         (Recorder.theory_clauses rec_)
     in
     check "e3 (H1): a Theory_prop reason leaf was materialized" (theory_reason_ids <> []);
     check
       "e3 (H1): the failed-assumption antecedents cite a materialized Theory reason id"
       (List.exists (fun id -> List.mem id antecedents) theory_reason_ids)
   | _ -> check "e3: conclusion is Failed_assumption" false);
  check
    "e3: no unresolved citations (Theory_prop reason surfaced)"
    (Recorder.unresolved_citations rec_ = [])
;;

(* ------------------------------------------------------------------ *)
(* E4 — a Final-effort Theory_lemma (split) that filters to [] at level 0 sets result
   directly (H3): Root_empty citing the lemma, whose id carries the Theory_lemma origin
   (RR5 provenance split — a query input would carry Query). Units ¬p, ¬q; the split [p∨q]
   is falsified at level 0. *)

let test_e4_theory_lemma_empty () =
  let s = Sat.create () in
  let p = Sat.new_var s
  and q = Sat.new_var s in
  let lp = Sat.pos p
  and lq = Sat.pos q in
  let mock = make_mock s { empty_config with final_splits = [ [ lp; lq ] ] } in
  Sat.set_theory s (Some mock);
  let rec_ = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec_));
  Sat.add_clause s [ Sat.neg p ];
  Sat.add_clause s [ Sat.neg q ];
  let r = Sat.solve s in
  check "e4: unsat" (r = Sat.Unsat);
  (match Recorder.conclusion rec_ with
   | Some (Sat.Root_empty { input_id }) ->
     let ev =
       List.find_opt
         (fun (i : Recorder.input_event) -> i.Recorder.id = input_id)
         (Recorder.inputs rec_)
     in
     check
       "e4: input_id resolves to a Theory_lemma-origin on_input event for [p;q]"
       (match ev with
        | Some i ->
          clause_set i.Recorder.clause = [ 1; 2 ] && i.Recorder.origin = Sat.Theory_lemma
        | None -> false)
   | _ -> check "e4: conclusion is Root_empty" false);
  check "e4: no unresolved citations" (Recorder.unresolved_citations rec_ = [])
;;

(* ------------------------------------------------------------------ *)
(* on_theory_clause fires (role Conflict) for a theory conflict transient, so the learned
   clause's antecedents (which cite it) resolve. *)

let test_theory_conflict_surfaced () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s in
  let la = Sat.pos a
  and lb = Sat.pos b in
  let mock = make_mock s { empty_config with conflicts = [ [ la; lb ] ] } in
  Sat.set_theory s (Some mock);
  let rec_ = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec_));
  let r = Sat.solve ~assumptions:[ la; lb ] s in
  check "theory-confl: unsat" (r = Sat.Unsat);
  let confl_leaves =
    List.filter
      (fun (te : Recorder.theory_event) -> te.Recorder.role = Sat.Conflict)
      (Recorder.theory_clauses rec_)
  in
  check "theory-confl: a Conflict leaf was surfaced" (confl_leaves <> []);
  check
    "theory-confl: the negated-premise conflict clause [-2;-1] was surfaced"
    (List.exists
       (fun (te : Recorder.theory_event) -> clause_set te.Recorder.clause = [ -2; -1 ])
       confl_leaves);
  check "theory-confl: no unresolved citations" (Recorder.unresolved_citations rec_ = [])
;;

(* ------------------------------------------------------------------ *)
(* 1UIP theory-reason materialization (the analyze-path parallel of E3's H1). Under
   assumptions p, a the theory propagates BOTH c and d TRUE at level 2 (explain each =
   [p;a]); [¬c∨¬d] then conflicts and 1UIP resolves the two true theory literals against
   their lazy reasons — driving theory_reason_clause inside analyze. Those reason leaves
   are cited in the learned clause's antecedents, so they MUST be surfaced
   (on_theory_clause Reason) or the citation is unresolved. Discriminates
   note_theory_clause being wired into theory_reason_clause. *)

let test_analyze_theory_reason () =
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
  Sat.set_theory s (Some mock);
  let rec_ = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec_));
  Sat.add_clause s [ Sat.neg c; Sat.neg d ];
  (* ¬c ∨ ¬d *)
  let r = Sat.solve ~assumptions:[ lp; la ] s in
  check "analyze-reason: unsat" (r = Sat.Unsat);
  let reason_leaves =
    List.filter
      (fun (te : Recorder.theory_event) -> te.Recorder.role = Sat.Reason)
      (Recorder.theory_clauses rec_)
  in
  check "analyze-reason: a Reason leaf was surfaced during 1UIP" (reason_leaves <> []);
  (* the learned clause(s) cite the materialized theory reason id(s) *)
  let cited =
    List.concat_map
      (fun (le : Recorder.learned_event) -> le.Recorder.antecedents)
      (Recorder.learned rec_)
  in
  check
    "analyze-reason: a learned clause cites a materialized Reason leaf"
    (List.exists
       (fun (te : Recorder.theory_event) -> List.mem te.Recorder.id cited)
       reason_leaves);
  check "analyze-reason: no unresolved citations" (Recorder.unresolved_citations rec_ = [])
;;

(* ------------------------------------------------------------------ *)
(* Ordered-RUP antecedent order (ADR-0013 §1.4(a); the dropped List.rev). A PURE-BOOLEAN
   1UIP with a real resolution chain, so the conflict clause is an unambiguous retained
   input: assume a; [¬a∨b] propagates b; [¬b∨c] propagates c; [¬b∨¬c] conflicts. 1UIP
   seeds the accumulator with the conflict, then prepends each resolved reason, so the
   frozen contract order [rₙ..r₁; conflict] puts the CONFLICT clause LAST. The old
   [List.rev] returned [conflict; r₁..rₙ] (conflict FIRST) — so "the last antecedent is
   the conflict clause" is exactly the discriminator: it goes RED against the pre-fix
   core. *)

let test_antecedent_order_conflict_last () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s
  and c = Sat.new_var s in
  let la = Sat.pos a in
  let rec_ = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec_));
  Sat.add_clause s [ Sat.neg a; Sat.pos b ];
  (* ¬a ∨ b *)
  Sat.add_clause s [ Sat.neg b; Sat.pos c ];
  (* ¬b ∨ c *)
  Sat.add_clause s [ Sat.neg b; Sat.neg c ];
  (* ¬b ∨ ¬c : the conflict clause, set {-3;-2} *)
  ignore c;
  let r = Sat.solve ~assumptions:[ la ] s in
  check "order: unsat" (r = Sat.Unsat);
  (* map an antecedent id to its (input) clause set *)
  let clause_of_id id =
    List.find_map
      (fun (i : Recorder.input_event) ->
         if i.Recorder.id = id then Some (clause_set i.Recorder.clause) else None)
      (Recorder.inputs rec_)
  in
  (match Recorder.learned rec_ with
   | [ le ] ->
     check "order: exactly one learned clause" true;
     let ants = le.Recorder.antecedents in
     check
       (Printf.sprintf
          "order: >=2 antecedents (a reason was resolved); got %s"
          (show_ints ants))
       (List.length ants >= 2);
     check
       (Printf.sprintf
          "order: conflict clause {-3;-2} is the LAST antecedent (contract \
           [rₙ..r₁;confl]); got %s"
          (show_ints ants))
       (match List.rev ants with
        | last :: _ -> clause_of_id last = Some [ -3; -2 ]
        | [] -> false)
   | ls ->
     check
       (Printf.sprintf "order: exactly one learned clause (got %d)" (List.length ls))
       false);
  check "order: no unresolved citations" (Recorder.unresolved_citations rec_ = [])
;;

(* ------------------------------------------------------------------ *)
(* Side-channel soundness: installing a recorder never flips a VERDICT, over a batch of
   random formulas. It is NOT bit-identical to untraced any more — the frozen unminimized-
   clause contract means a traced solve bypasses minimization, so learned clauses (hence
   counters and the eventual model) legitimately differ (a weaker but still sound and
   complete solver, ADR-0013 §1.4(b)). The guarantee that matters is that emission is a
   pure side channel for the ANSWER: Sat stays Sat, Unsat stays Unsat. (Untraced
   bit-identicality to the pre-cert core is covered by sat_test / seam_test running
   untraced.) *)

let lcg = ref 0xC0FFEE123456

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

let build num_vars clauses ~trace =
  let s = Sat.create () in
  for _ = 0 to num_vars - 1 do
    ignore (Sat.new_var s : Sat.var)
  done;
  let rec_ = Recorder.create () in
  if trace then Sat.set_trace s (Some (Recorder.trace rec_));
  List.iter (fun cl -> Sat.add_clause s cl) clauses;
  s
;;

let test_traced_verdict_preserved () =
  lcg := 0xC0FFEE123456;
  let n = 3000 in
  let mismatches = ref 0 in
  for _ = 1 to n do
    let num_vars = 3 + rand_n 10 in
    let num_clauses = 1 + rand_n (num_vars * 4) in
    let clauses = List.init num_clauses (fun _ -> random_clause num_vars) in
    let plain = build num_vars clauses ~trace:false in
    let r0 = Sat.solve plain in
    let traced = build num_vars clauses ~trace:true in
    let r1 = Sat.solve traced in
    if r0 <> r1 then incr mismatches
  done;
  check
    (Printf.sprintf
       "side-channel: traced verdict == untraced verdict over %d formulas (%d mismatches)"
       n
       !mismatches)
    (!mismatches = 0)
;;

(* ------------------------------------------------------------------ *)
(* CRIT-1 (codex): a MINIMIZABLE conflict under an active trace must emit-and-store the
   UNMINIMIZED 1UIP clause (frozen sat.mli:156). Codex's trigger:
   [(¬a∨b); (¬c∨¬a∨d); (¬c∨¬b∨¬d)] under assumptions [a;c]. The 1UIP clause is (¬c∨¬a∨¬b);
   minimization would drop ¬b (its reason (¬a∨b) is subsumed) giving (¬c∨¬a). With the
   bypass the emitted clause keeps ¬b, and the emitted antecedent chain is the full chain
   that derives THAT clause. Discriminator: without the bypass the emitted clause is
   (¬c∨¬a) [set {-3;-1}]. *)

let test_crit1_unminimized_when_traced () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s
  and c = Sat.new_var s
  and d = Sat.new_var s in
  ignore (b, d);
  let rec_ = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec_));
  Sat.add_clause s [ Sat.neg a; Sat.pos b ];
  Sat.add_clause s [ Sat.neg c; Sat.neg a; Sat.pos d ];
  Sat.add_clause s [ Sat.neg c; Sat.neg b; Sat.neg d ];
  let r = Sat.solve ~assumptions:[ Sat.pos a; Sat.pos c ] s in
  check "crit1: unsat under [a;c]" (r = Sat.Unsat);
  (match Recorder.learned rec_ with
   | [ le ] ->
     check
       (Printf.sprintf
          "crit1: emitted-and-stored clause is UNMINIMIZED {-3;-2;-1} (got %s)"
          (show_ints (clause_set le.Recorder.clause)))
       (clause_set le.Recorder.clause = [ -3; -2; -1 ]);
     check
       "crit1: >=2 antecedents (full chain, not truncated to a minimized clause)"
       (List.length le.Recorder.antecedents >= 2)
   | ls ->
     check
       (Printf.sprintf "crit1: exactly one learned clause (got %d)" (List.length ls))
       false);
  check "crit1: no unresolved citations" (Recorder.unresolved_citations rec_ = [])
;;

(* ------------------------------------------------------------------ *)
(* CRIT-2 (codex): a theory propagation at LEVEL 0 feeding an E2 level-0 conflict must
   surface its reason clause, or the checker's level-0 closure (§1.3, over Input clauses)
   cannot derive it. Codex's trigger: theory a⇒c; level-0 inputs [a], [¬c∨x], [¬c∨y],
   [¬x∨¬y]. c is propagated at level 0, x,y follow, [¬x∨¬y] conflicts at level 0 (E2). The
   reason (c∨¬a) must be a surfaced Reason leaf. Discriminator: without the
   enqueue_theory_lits L0 surfacing, no Reason leaf exists for c. *)

let test_crit2_level0_theory_reason () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and c = Sat.new_var s
  and x = Sat.new_var s
  and y = Sat.new_var s in
  let la = Sat.pos a
  and lc = Sat.pos c
  and lx = Sat.pos x
  and ly = Sat.pos y in
  let mock = make_mock s { empty_config with implications = [ [ la ], lc ] } in
  Sat.set_theory s (Some mock);
  let rec_ = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec_));
  Sat.add_clause s [ la ];
  Sat.add_clause s [ Sat.neg c; lx ];
  Sat.add_clause s [ Sat.neg c; ly ];
  Sat.add_clause s [ Sat.neg x; Sat.neg y ];
  let r = Sat.solve s in
  check "crit2: unsat" (r = Sat.Unsat);
  check
    "crit2: conclusion is Level0_conflict (E2)"
    (match Recorder.conclusion rec_ with
     | Some (Sat.Level0_conflict _) -> true
     | _ -> false);
  let has_c_reason =
    List.exists
      (fun (te : Recorder.theory_event) ->
         te.Recorder.role = Sat.Reason && clause_set te.Recorder.clause = [ -1; 2 ])
      (Recorder.theory_clauses rec_)
  in
  check "crit2: level-0 theory reason (c ∨ ¬a) surfaced as a Reason leaf" has_c_reason;
  check "crit2: no unresolved citations" (Recorder.unresolved_citations rec_ = [])
;;

(* ------------------------------------------------------------------ *)
(* CRIT-3 (codex): a REPEATED solve on an already-unsat core must re-emit a checkable
   conclusion (no silent traced Unsat). E2 sets [t.ok] false; the second solve returns
   Unsat via the entry and must re-fire the persisted Level0_conflict. Discriminator:
   without terminal persistence the second solve emits nothing → count stays 1. *)

let counting_trace count last =
  { Sat.on_input = (fun ~id:_ ~clause:_ ~origin:_ -> ())
  ; on_unit = (fun ~id:_ ~lit:_ -> ())
  ; on_learned = (fun ~id:_ ~clause:_ ~antecedents:_ ~btlevel:_ -> ())
  ; on_theory_clause = (fun ~id:_ ~clause:_ ~role:_ -> ())
  ; on_unsat =
      (fun c ->
        incr count;
        last := Some c)
  }
;;

let test_crit3_repeated_solve_reemits () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s in
  let count = ref 0 in
  let last = ref None in
  Sat.set_trace s (Some (counting_trace count last));
  Sat.add_clause s [ Sat.pos a; Sat.pos b ];
  Sat.add_clause s [ Sat.neg a ];
  Sat.add_clause s [ Sat.neg b ];
  let r1 = Sat.solve s in
  let c1 = !count in
  let r2 = Sat.solve s in
  let c2 = !count in
  check "crit3: first solve unsat (E2)" (r1 = Sat.Unsat);
  check "crit3: second solve unsat" (r2 = Sat.Unsat);
  check "crit3: first solve emitted one conclusion" (c1 = 1);
  check
    (Printf.sprintf "crit3: repeated solve re-emitted a conclusion (total %d)" c2)
    (c2 = 2);
  check
    "crit3: re-emitted conclusion is Level0_conflict"
    (match !last with
     | Some (Sat.Level0_conflict _) -> true
     | _ -> false)
;;

(* ------------------------------------------------------------------ *)
(* HIGH-4 (codex): unresolved_citations must NOT false-clean an ambiguous id. Reuse ONE
   recorder across two solvers whose ids both restart from 0: solver 1 emits content id 0
   ([a]); solver 2 emits content id 0 ([b∨c]) and cites conflict id 0. The recorder cannot
   bind to a solver identity, so it rejects the ambiguity (id 0 in two content events →
   count 2 → unresolved). Discriminator: an IntSet-collapse implementation returns []. *)

let test_high4_ambiguous_id () =
  let rec_ = Recorder.create () in
  let s1 = Sat.create () in
  let a = Sat.new_var s1 in
  Sat.set_trace s1 (Some (Recorder.trace rec_));
  Sat.add_clause s1 [ Sat.pos a ];
  ignore (Sat.solve s1 : Sat.result);
  let s2 = Sat.create () in
  let b = Sat.new_var s2
  and c = Sat.new_var s2 in
  Sat.set_trace s2 (Some (Recorder.trace rec_));
  Sat.add_clause s2 [ Sat.pos b; Sat.pos c ];
  Sat.add_clause s2 [ Sat.neg b ];
  Sat.add_clause s2 [ Sat.neg c ];
  let r = Sat.solve s2 in
  check "high4: solver-2 unsat" (r = Sat.Unsat);
  check
    "high4: ambiguous cited id reported unresolved (not false-clean)"
    (Recorder.unresolved_citations rec_ <> [])
;;

(* ------------------------------------------------------------------ *)

let () =
  test_on_input_and_unit ();
  test_e1_root_empty ();
  test_e2_level0_conflict ();
  test_e3_failed_assumption_theory_prop ();
  test_e4_theory_lemma_empty ();
  test_theory_conflict_surfaced ();
  test_analyze_theory_reason ();
  test_antecedent_order_conflict_last ();
  test_crit1_unminimized_when_traced ();
  test_crit2_level0_theory_reason ();
  test_crit3_repeated_solve_reemits ();
  test_high4_ambiguous_id ();
  test_traced_verdict_preserved ();
  Printf.printf "cert_emit_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
