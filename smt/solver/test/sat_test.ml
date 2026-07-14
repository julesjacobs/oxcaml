module Sat = Oxsmt_solver.Sat
module Sh = Oxsmt_solver.Search_heuristics
module Dimacs = Oxsmt_dimacs.Dimacs

(* Unit + property self-test for the CDCL SAT core (TASKS.md M1-sat).

   Layers (DESIGN.md §8), cheapest first:
   - Unit: exact learned clause + backjump level + antecedent set on textbook conflicts
     (observed through the proof-readiness trace), assumption semantics, incremental
     add-after-solve.
   - Every Sat verdict is self-checked by evaluating all clauses under the model.
   - Property: thousands of random small CNFs cross-checked against the naive DPLL oracle
     ({!Dpll}); verdicts must agree.
   - Determinism (I6): the same formula solved twice yields identical stats and model.

   Stdlib-only; deterministic (fixed-seed LCG, no wall-clock). Nonzero exit on any failed
   check. *)

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

(* DIMACS-style rendering of a Sat literal (±(var+1)), so expected values in the unit
   tests read naturally. *)
let dimacs_of_lit l =
  let v = Sat.var_of_lit l + 1 in
  if Sat.sign_of_lit l then v else -v
;;

let sorted_dimacs lits = List.sort compare (List.map dimacs_of_lit (Array.to_list lits))
let show_ints xs = "[" ^ String.concat ";" (List.map string_of_int xs) ^ "]"

(* Build a solver from DIMACS clauses (±v, 1-based). *)
let build num_vars clauses = Dimacs.to_sat { Dimacs.num_vars; clauses }
let lit l = Dimacs.to_lit l

(* A trace collector: records (learned clause as sorted dimacs, sorted antecedent ids,
   btlevel) per learned clause. *)
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
(* Unit: exact conflict analysis on textbook cases. *)

let test_analyze_multi () =
  (* Clauses (c0..c3): ¬1∨2 ; ¬1∨3∨5 ; ¬2∨4 ; ¬3∨¬4. Under assumptions ¬5 then 1, unit
     propagation forces 2,3,4 and c3 conflicts. The 1UIP learned clause is (¬1 ∨ 5),
     backjumping to level 1. *)
  let s = build 5 [ [ -1; 2 ]; [ -1; 3; 5 ]; [ -2; 4 ]; [ -3; -4 ] ] in
  let learned = with_collector s in
  let r = Sat.solve ~assumptions:[ lit (-5); lit 1 ] s in
  let ls = learned () in
  check "multi: unsat under assumptions" (r = Sat.Unsat);
  check "multi: one learned clause" (List.length ls = 1);
  (match ls with
   | [ (clause, ants, bt) ] ->
     check
       (Printf.sprintf "multi: learned = [-1;5] (got %s)" (show_ints clause))
       (clause = [ -1; 5 ]);
     check "multi: backjump level = 1" (bt = 1);
     check
       (Printf.sprintf "multi: antecedents = [0;1;2;3] (got %s)" (show_ints ants))
       (ants = [ 0; 1; 2; 3 ])
   | _ -> ());
  let failed = List.sort compare (List.map dimacs_of_lit (Sat.failed_assumptions s)) in
  check
    (Printf.sprintf "multi: failed assumptions = [-5;1] (got %s)" (show_ints failed))
    (failed = [ -5; 1 ])
;;

let test_analyze_unit () =
  (* Clauses: ¬1∨2 ; ¬1∨3 ; ¬2∨¬3. Assuming 1 forces 2,3 and the last clause conflicts;
     the 1UIP is the unit (¬1), backjumping to level 0. *)
  let s = build 3 [ [ -1; 2 ]; [ -1; 3 ]; [ -2; -3 ] ] in
  let learned = with_collector s in
  let r = Sat.solve ~assumptions:[ lit 1 ] s in
  let ls = learned () in
  check "unit: unsat under assumption" (r = Sat.Unsat);
  check "unit: one learned clause" (List.length ls = 1);
  (match ls with
   | [ (clause, ants, bt) ] ->
     check
       (Printf.sprintf "unit: learned = [-1] (got %s)" (show_ints clause))
       (clause = [ -1 ]);
     check "unit: backjump level = 0" (bt = 0);
     check
       (Printf.sprintf "unit: antecedents = [0;1;2] (got %s)" (show_ints ants))
       (ants = [ 0; 1; 2 ])
   | _ -> ());
  let failed = List.map dimacs_of_lit (Sat.failed_assumptions s) in
  check "unit: failed assumptions = [1]" (failed = [ 1 ])
;;

(* ------------------------------------------------------------------ *)
(* Unit: propagation reaches the obvious fixpoint. *)

let test_propagation () =
  (* (1) ; (¬1∨2) ; (¬2∨3) forces 1,2,3 with no decisions. *)
  let s = build 3 [ [ 1 ]; [ -1; 2 ]; [ -2; 3 ] ] in
  let r = Sat.solve s in
  check "prop: sat" (r = Sat.Sat);
  check "prop: x1 true" (Sat.value s 0);
  check "prop: x2 true" (Sat.value s 1);
  check "prop: x3 true" (Sat.value s 2);
  let st = Sat.stats s in
  check "prop: no conflicts" (st.Sat.Stats.conflicts = 0)
;;

let test_level0_contradiction () =
  (* Units (1) and (¬1) make the instance unconditionally unsat at level 0. *)
  let s = build 1 [ [ 1 ]; [ -1 ] ] in
  check "level0: unsat" (Sat.solve s = Sat.Unsat);
  check "level0: still unsat on re-solve" (Sat.solve s = Sat.Unsat)
;;

let test_empty_clause () =
  let s = Sat.create () in
  Sat.add_clause s [];
  check "empty: unsat" (Sat.solve s = Sat.Unsat)
;;

let test_no_clauses () =
  let s = Sat.create () in
  ignore (Sat.new_var s : Sat.var);
  check "trivial: sat with no clauses" (Sat.solve s = Sat.Sat)
;;

(* ------------------------------------------------------------------ *)
(* Assumption semantics. *)

let test_assumptions () =
  let s = build 2 [ [ 1; 2 ] ] in
  check "assume: plain sat" (Sat.solve s = Sat.Sat);
  check "assume: sat under ¬1 (forces 2)" (Sat.solve ~assumptions:[ lit (-1) ] s = Sat.Sat);
  check "assume: x2 true under ¬1" (Sat.value s 1);
  let r = Sat.solve ~assumptions:[ lit (-1); lit (-2) ] s in
  check "assume: unsat under ¬1,¬2" (r = Sat.Unsat);
  let failed = List.sort compare (List.map dimacs_of_lit (Sat.failed_assumptions s)) in
  check
    (Printf.sprintf "assume: core = [-2;-1] (got %s)" (show_ints failed))
    (failed = [ -2; -1 ]);
  (* Solving again with no assumptions must recover sat (assumptions are per-call). *)
  check "assume: sat again with no assumptions" (Sat.solve s = Sat.Sat)
;;

(* ------------------------------------------------------------------ *)
(* Incrementality: add a clause after a solve; verdict must track. *)

let test_incremental () =
  let s = build 2 [ [ 1; 2 ] ] in
  check "incr: initially sat" (Sat.solve s = Sat.Sat);
  Sat.add_clause s [ lit (-1) ];
  Sat.add_clause s [ lit (-2) ];
  check "incr: unsat after adding ¬1, ¬2" (Sat.solve s = Sat.Unsat)
;;

(* ------------------------------------------------------------------ *)
(* Random-CNF property test cross-checked against the DPLL oracle, with every sat model
   self-checked by evaluation. *)

(* xorshift64*, fixed seed (same family as core_test's PRNG); all constants fit in OCaml's
   63-bit int and arithmetic overflow wraps deterministically. *)
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

let model_satisfies clauses model =
  List.for_all
    (fun cl ->
       List.exists
         (fun l ->
            let b = model.(abs l - 1) in
            if l > 0 then b else not b)
         cl)
    clauses
;;

let random_clause num_vars =
  let width = 1 + rand_n 3 in
  List.init width (fun _ ->
    let v = 1 + rand_n num_vars in
    if rand_n 2 = 0 then v else -v)
;;

(* Learned-clause entailment (§10 oracle strengthening, mutant sat-minimize-unsound):
   every learned clause L must be entailed by the original formula F, i.e. F ∧ ¬L is
   UNSAT. We check with the independent DPLL oracle (not the solver under test — that
   would be circular): assert the original clauses plus a unit [-l] per literal l of L,
   and require UNSAT. A too-strong learned clause (over-minimization dropping a needed
   literal) makes F ∧ ¬L SAT and is caught here even when the final verdict is unaffected
   — which is precisely the gap the mutant slipped through. *)
let learned_clause_entailed clauses num_vars learned_dimacs =
  let neg_units = List.map (fun l -> [ -l ]) learned_dimacs in
  not (Dpll.solve num_vars (List.rev_append neg_units clauses))
;;

(* Sparse mixed-width formulas: broad structural coverage. *)
let gen_sparse () =
  let num_vars = 3 + rand_n 10 in
  let num_clauses = 1 + rand_n (num_vars * 4) in
  num_vars, List.init num_clauses (fun _ -> random_clause num_vars)
;;

(* Dense 3-CNF near the 3-SAT phase transition (clause/var ratio ~4.3): mostly hard,
   conflict-heavy instances, so the solver actually learns (and minimizes) many clauses —
   this is what gives the entailment check real volume. *)
let gen_dense () =
  let num_vars = 6 + rand_n 7 in
  let num_clauses = (num_vars * 4) + rand_n num_vars in
  let clause () =
    List.init 3 (fun _ ->
      let v = 1 + rand_n num_vars in
      if rand_n 2 = 0 then v else -v)
  in
  num_vars, List.init num_clauses (fun _ -> clause ())
;;

let test_property label gen n =
  let disagreements = ref 0 in
  let bad_models = ref 0 in
  let n_learned = ref 0 in
  let unentailed = ref 0 in
  for _ = 1 to n do
    let num_vars, clauses = gen () in
    let expected = Dpll.solve num_vars clauses in
    let s = build num_vars clauses in
    (* Collect every learned clause (as DIMACS ints) for entailment checking. *)
    let learned = ref [] in
    Sat.set_trace
      s
      (Some
         { Sat.on_learned =
             (fun ~id ~clause ~antecedents ~btlevel ->
               ignore id;
               ignore antecedents;
               ignore btlevel;
               learned := List.map dimacs_of_lit (Array.to_list clause) :: !learned)
         ; on_input = (fun ~id:_ ~clause:_ ~origin:_ -> ())
         ; on_unit = (fun ~id:_ ~lit:_ -> ())
         ; on_theory_clause = (fun ~id:_ ~clause:_ ~role:_ -> ())
         ; on_unsat = (fun _ -> ())
         });
    let verdict = Sat.solve s in
    (match verdict with
     | Sat.Sat ->
       if not expected then incr disagreements;
       if not (model_satisfies clauses (Sat.model s)) then incr bad_models
     | Sat.Unsat -> if expected then incr disagreements);
    List.iter
      (fun l ->
         incr n_learned;
         if not (learned_clause_entailed clauses num_vars l) then incr unentailed)
      !learned
  done;
  check
    (Printf.sprintf
       "property[%s]: %d formulas agree with DPLL (%d disagreements)"
       label
       n
       !disagreements)
    (!disagreements = 0);
  check
    (Printf.sprintf "property[%s]: all sat models valid (%d bad)" label !bad_models)
    (!bad_models = 0);
  check
    (Printf.sprintf
       "property[%s]: all %d learned clauses entailed (%d unentailed)"
       label
       !n_learned
       !unentailed)
    (!unentailed = 0);
  Printf.printf
    "  (property[%s]: entailment-checked %d learned clauses across %d formulas)\n"
    label
    !n_learned
    n
;;

(* UNTRACED verdict oracle (cert-step-1 fix round): the sibling of {!test_property} that
   installs NO trace and cross-checks only the verdict + sat-model against the DPLL
   reference. This is the path that exercises local clause minimization: with a trace
   active minimization is bypassed (frozen sat.mli:156), so {!test_property} — which
   traces to observe learned clauses — no longer runs minimization, and the
   sat-minimize-unsound mutant is invisible there. An unsound minimization drops a needed
   literal from a learned clause, over-constraining the search into a spurious Unsat (or a
   model that fails evaluation) on some random instance, so it MUST surface here as a DPLL
   disagreement or a bad model. This is the mutant's kill site. *)
let test_property_untraced label gen n =
  let disagreements = ref 0 in
  let bad_models = ref 0 in
  for _ = 1 to n do
    let num_vars, clauses = gen () in
    let expected = Dpll.solve num_vars clauses in
    let s = build num_vars clauses in
    match Sat.solve s with
    | Sat.Sat ->
      if not expected then incr disagreements;
      if not (model_satisfies clauses (Sat.model s)) then incr bad_models
    | Sat.Unsat -> if expected then incr disagreements
  done;
  check
    (Printf.sprintf
       "property-untraced[%s]: %d formulas agree with DPLL (%d disagreements)"
       label
       n
       !disagreements)
    (!disagreements = 0);
  check
    (Printf.sprintf
       "property-untraced[%s]: all sat models valid (%d bad)"
       label
       !bad_models)
    (!bad_models = 0)
;;

(* ------------------------------------------------------------------ *)
(* Crafted conflicts that exercise 1UIP local (self-subsumption) minimization, the code
   the sat-minimize-unsound mutant corrupts. One case where minimization must NOT fire (a
   reasoned literal whose reason carries an out-of-clause, level>0 literal — correct keeps
   it, the mutant wrongly drops it) and one where it legitimately DOES fire (the reason is
   subsumed by clause literals). Each pins the exact learned clause and independently
   checks entailment. *)

let single_learned s assumptions =
  let learned = with_collector s in
  let r = Sat.solve ~assumptions s in
  r, learned ()
;;

let test_minimize_must_not_fire () =
  (* c0 a->e, c1 e->b, c2 c∧b->d, c3 c∧a->¬d (conflict). Decide a then c. 1UIP analysis
     yields [{¬c,¬a,¬b}]; ¬b is a reasoned literal whose reason (¬e ∨ b) carries ¬e —
     level 1, NOT in the clause — so it is not redundant. Under the cert-step-1 contract
     minimization is bypassed while [with_collector]'s trace is active, so the observed
     clause is the unminimized [{¬c,¬a,¬b}] — the same literals it would keep with
     minimization on, so this assertion is unchanged. *)
  let clauses = [ [ -1; 5 ]; [ -5; 2 ]; [ -3; -2; 4 ]; [ -3; -1; -4 ] ] in
  let s = build 5 clauses in
  let r, ls = single_learned s [ lit 1; lit 3 ] in
  check "min-keep: unsat under assumptions" (r = Sat.Unsat);
  check "min-keep: one learned clause" (List.length ls = 1);
  match ls with
  | [ (clause, _ants, bt) ] ->
    check
      (Printf.sprintf "min-keep: learned = [-3;-2;-1] (got %s)" (show_ints clause))
      (clause = [ -3; -2; -1 ]);
    check "min-keep: backjump level = 1" (bt = 1);
    check "min-keep: learned clause entailed" (learned_clause_entailed clauses 5 clause)
  | _ -> ()
;;

let test_minimize_bypassed_when_traced () =
  (* c0 a->b, c2 c∧a->d, c3 c∧b->¬d (conflict). Decide a then c. 1UIP analysis yields the
     UNMINIMIZED [{¬c,¬a,¬b}]; ¬b's reason (¬a ∨ b) carries only ¬a (in the clause), so ¬b
     would be dropped by minimization giving [{¬c,¬a}]. But the frozen sat.mli:156
     contract requires that WHEN A TRACE IS ACTIVE the emitted-and-stored clause is the
     UNMINIMIZED 1UIP clause (ADR-0013 §1.4(b): a hint-restricted ordered-RUP replay of
     the minimized clause would stall on the absent minimization reason). [with_collector]
     installs a trace, so minimization is bypassed and the observed clause is [{¬c,¬a,¬b}]
     (was [-3;-1] before the cert-step-1 fix — that assertion encoded the bug, codex
     CRITICAL-1). NOTE: minimization now runs only on UNTRACED solves, so the
     sat-minimize-unsound mutant is no longer observable through this trace-based path —
     see the cert-step1 report / master flag. *)
  let clauses = [ [ -1; 2 ]; [ -3; -1; 4 ]; [ -3; -2; -4 ] ] in
  let s = build 4 clauses in
  let r, ls = single_learned s [ lit 1; lit 3 ] in
  check "min-bypass: unsat under assumptions" (r = Sat.Unsat);
  check "min-bypass: one learned clause" (List.length ls = 1);
  match ls with
  | [ (clause, _ants, bt) ] ->
    check
      (Printf.sprintf
         "min-bypass: learned = [-3;-2;-1] (unminimized under trace; got %s)"
         (show_ints clause))
      (clause = [ -3; -2; -1 ]);
    check "min-bypass: backjump level = 1" (bt = 1);
    check "min-bypass: learned clause entailed" (learned_clause_entailed clauses 4 clause)
  | _ -> ()
;;

(* ------------------------------------------------------------------ *)
(* Determinism (I6): identical runs give identical stats and model. *)

let test_determinism () =
  let mk () =
    (* A formula with real search (uf-ish): mostly-3-CNF over 12 vars. *)
    lcg := 0xDEADBEEFCAFE;
    let clauses = List.init 40 (fun _ -> random_clause 12) in
    build 12 clauses, clauses
  in
  let s1, _ = mk () in
  let s2, _ = mk () in
  let r1 = Sat.solve s1
  and r2 = Sat.solve s2 in
  check "determinism: same verdict" (r1 = r2);
  let a = Sat.stats s1
  and b = Sat.stats s2 in
  check "determinism: same conflicts" (a.Sat.Stats.conflicts = b.Sat.Stats.conflicts);
  check "determinism: same decisions" (a.Sat.Stats.decisions = b.Sat.Stats.decisions);
  check
    "determinism: same propagations"
    (a.Sat.Stats.propagations = b.Sat.Stats.propagations);
  if r1 = Sat.Sat then check "determinism: same model" (Sat.model s1 = Sat.model s2)
;;

(* ------------------------------------------------------------------ *)
(* Pigeonhole PHP(n+1, n): n+1 pigeons into n holes is unsatisfiable, and drives the
   conflict machinery hard. Variable p(i,h) = pigeon i in hole h. *)

let test_pigeonhole n =
  let s = Sat.create () in
  let pv = Array.make_matrix (n + 1) n 0 in
  for i = 0 to n do
    for h = 0 to n - 1 do
      pv.(i).(h) <- Sat.new_var s
    done
  done;
  (* Each pigeon in at least one hole. *)
  for i = 0 to n do
    Sat.add_clause s (List.init n (fun h -> Sat.pos pv.(i).(h)))
  done;
  (* No hole holds two pigeons. *)
  for h = 0 to n - 1 do
    for i = 0 to n do
      for j = i + 1 to n do
        Sat.add_clause s [ Sat.neg pv.(i).(h); Sat.neg pv.(j).(h) ]
      done
    done
  done;
  check (Printf.sprintf "pigeonhole(%d): unsat" n) (Sat.solve s = Sat.Unsat);
  let st = Sat.stats s in
  check "pigeonhole: took some conflicts" (n < 2 || st.Sat.Stats.conflicts > 0)
;;

(* ------------------------------------------------------------------ *)
(* DIMACS reader strictness (sat-review item 11): a truncated file must be a loud reject,
   not a silently-shorter formula that can flip unsat->sat. *)

let write_tmp contents =
  let path = Filename.temp_file "dimacs_test" ".cnf" in
  let oc = open_out path in
  output_string oc contents;
  close_out oc;
  path
;;

let parses_to name contents expected_clauses =
  let path = write_tmp contents in
  (match Dimacs.parse_file path with
   | p -> check name (List.length p.Dimacs.clauses = expected_clauses)
   | exception e -> check (name ^ " (unexpected " ^ Printexc.to_string e ^ ")") false);
  Sys.remove path
;;

let rejects name contents =
  let path = write_tmp contents in
  (match Dimacs.parse_file path with
   | _ -> check (name ^ " (no reject)") false
   | exception Dimacs.Parse_error _ -> check name true
   | exception e -> check (name ^ " (wrong exn " ^ Printexc.to_string e ^ ")") false);
  Sys.remove path
;;

let test_dimacs_strict () =
  (* Complete file with no SATLIB "%" footer still parses (the final clause's own 0
     terminates it) — regression guard for the footer-independence the review checked. *)
  parses_to "dimacs: complete file, no % footer" "p cnf 3 2\n1 -2 0\n2 3 0\n" 2;
  (* "%" footer early-stops and its lone trailing 0 is not a phantom empty clause. *)
  parses_to "dimacs: % footer early-stop" "p cnf 1 1\n1 0\n%\n0\n" 1;
  (* Clauses may span lines / share a line; count is by 0-terminators, not lines. *)
  parses_to "dimacs: multiline clause" "p cnf 3 2\n1\n-2 0 2 3 0\n" 2;
  (* Truncation: header declares more clauses than are present -> loud reject. *)
  rejects "dimacs: truncated (fewer clauses than header)" "p cnf 3 3\n1 -2 0\n2 3 0\n";
  (* More clauses than the header declares -> also a mismatch, reject. *)
  rejects "dimacs: more clauses than header" "p cnf 3 1\n1 0\n2 0\n";
  (* Nonempty unterminated trailing clause (truncated mid-clause) -> reject. *)
  rejects "dimacs: unterminated trailing clause (with header)" "p cnf 3 2\n1 -2 0\n2 3\n";
  (* Same, with no header at all -> the trailing-clause rule still fires. *)
  rejects "dimacs: unterminated trailing clause (no header)" "1 2 3\n4 5\n"
;;

(* ------------------------------------------------------------------ *)

(* Board #60 / codex AP1 regression: a [Budget.Exceeded] raised at a decision tick must
   not permanently lose the decision variable. Repro shape (codex's): clause (a ∨ b) and a
   hook that raises on the FIRST tick of each solve. Pre-fix — the tick fired after
   [pick_branch] popped the var from the VSIDS heap but before [unchecked_enqueue] — two
   budgeted solves pop a then b off the heap and drop them (neither trailed, so
   [cancel_until 0] cannot restore them), and a third solve returns [Sat] with both false,
   FALSIFYING (a ∨ b). Post-fix the tick is after enqueue, so every raise leaves the
   decided var trailed and fully recoverable; the disarmed third solve is [Sat] with a
   satisfying model. *)
let test_budget_tick_exception_safety () =
  let module Local = struct
    exception Stop
  end
  in
  let s = Sat.create () in
  let a = Sat.new_var s in
  let b = Sat.new_var s in
  Sat.add_clause s [ Sat.pos a; Sat.pos b ];
  let ticks = ref 0 in
  Sat.set_budget_tick
    s
    (Some
       (fun () ->
         incr ticks;
         if !ticks = 1 then raise Local.Stop));
  let budgeted_solve () =
    ticks := 0;
    try ignore (Sat.solve s : Sat.result) with
    | Local.Stop -> ()
  in
  budgeted_solve ();
  budgeted_solve ();
  Sat.set_budget_tick s None;
  let r = Sat.solve s in
  check "AP1: disarmed solve after two budget exceptions is Sat" (r = Sat.Sat);
  check
    "AP1: recovered model satisfies (a ∨ b) (no lost variable)"
    (Sat.value s a || Sat.value s b)
;;

(* ------------------------------------------------------------------ *)
(* Modern-search heuristics (S3 LBD/reduceDB + #155 rephasing). The logic under test lives
   in the pure, stateless {!Oxsmt_solver.Search_heuristics} — the frozen sat.mli hides the
   solver internals, so this is where LBD, reduceDB selection, and the rephase schedule
   are hand-checked directly. *)

let test_lbd_of_levels () =
  (* LBD = number of distinct decision levels among the literals. *)
  check "lbd: empty clause is 0" (Sh.lbd_of_levels [||] = 0);
  check "lbd: single level is 1" (Sh.lbd_of_levels [| 0 |] = 1);
  check "lbd: all same level is 1" (Sh.lbd_of_levels [| 3; 3; 3 |] = 1);
  check
    (Printf.sprintf
       "lbd: {1,2,2,5,5,5} = 3 (got %d)"
       (Sh.lbd_of_levels [| 1; 2; 2; 5; 5; 5 |]))
    (Sh.lbd_of_levels [| 1; 2; 2; 5; 5; 5 |] = 3);
  check "lbd: {0,1,2,3} = 4" (Sh.lbd_of_levels [| 0; 1; 2; 3 |] = 4);
  check "lbd: order-independent" (Sh.lbd_of_levels [| 5; 1; 5; 2 |] = 3)
;;

let test_reduce_deletions_protects_glue () =
  (* Four glue clauses (LBD <= 2) and two non-glue (LBD 5, 4). n=6 => limit = n/2 = 3.
     Correct: glue is protected, so only the two removable (non-glue) clauses are deleted.
     The glue-deletion MUTANT (dropping the [lbd > glue_threshold] guard) treats glue as
     removable; with only two non-glue clauses it must then reach into glue to fill the
     third deletion slot, deleting glue clause index 1 — which this exact-array assertion
     catches. *)
  let s act lbd = { Sh.lbd; activity = act; protected_ = false } in
  let stats = [| s 0.0 1; s 0.0 2; s 0.0 2; s 0.0 1; s 3.0 5; s 4.0 4 |] in
  let del = Sh.reduce_deletions stats in
  let show = Array.to_list (Array.map (fun b -> if b then 1 else 0) del) in
  check
    (Printf.sprintf "reduce: glue protected, del=[0;0;0;0;1;1] (got %s)" (show_ints show))
    (del = [| false; false; false; false; true; true |])
;;

let test_reduce_deletions_worst_half_and_locked () =
  (* Worst-first order (LBD desc, ties activity asc) among removable, and locked/binary
     ([protected_]) never deleted. n=7 => limit=3. Removable =
     {2 ,3,4,6}
     ; index 5 locked. worst-first = [6(9); 4(8); 2(5,act1); 3(5,act2)] => delete top 3 =
     {2 ,4,6}
     , spare 3. *)
  let mk lbd act protected_ = { Sh.lbd; activity = act; protected_ } in
  let stats =
    [| mk 1 0.0 false (* glue *)
     ; mk 2 0.0 false (* glue *)
     ; mk 5 1.0 false
     ; mk 5 2.0 false
     ; mk 8 1.0 false
     ; mk 3 1.0 true (* locked: protected despite mid LBD *)
     ; mk 9 0.5 false
    |]
  in
  let del = Sh.reduce_deletions stats in
  check
    "reduce: worst-half by (lbd desc, act asc), glue+locked spared"
    (del = [| false; false; true; false; true; false; true |])
;;

let test_rephase_schedule () =
  (* The rephase cycle is a deterministic total function of the event count, front-loading
     the TRUE-flip (event 0). *)
  check "rephase: event 0 = Flipped_true" (Sh.rephase_mode 0 = Sh.Flipped_true);
  check "rephase: event 1 = Best_trail" (Sh.rephase_mode 1 = Sh.Best_trail);
  check "rephase: event 2 = Original_default" (Sh.rephase_mode 2 = Sh.Original_default);
  check "rephase: event 3 = Saved" (Sh.rephase_mode 3 = Sh.Saved);
  check "rephase: cycles at 4" (Sh.rephase_mode 4 = Sh.Flipped_true);
  check "rephase: cycles at 5" (Sh.rephase_mode 5 = Sh.Best_trail);
  (* Interval grows ~1.5x and is strictly increasing (backoff, no thrash). *)
  check "rephase: grow 1000 -> 1500" (Sh.grow_interval 1000 = 1500);
  check "rephase: grow 1500 -> 2250" (Sh.grow_interval 1500 = 2250);
  check "rephase: grow strictly increasing" (Sh.grow_interval 1000 > 1000)
;;

(* The modern machinery (LBD reduceDB, adaptive restarts, and the decision-interval
   rephase) only engages on a hard, decision-heavy instance. Pigeonhole PHP(n+1,n) is
   unsat and drives thousands of conflicts and decisions, so reduceDB fires (>2000
   conflicts), the LBD restart trigger fires, and — because it makes >1000 branch
   decisions — a rephase impulse fires too. This asserts all of that stays SOUND (still
   unsat) and DETERMINISTIC (I6: two identical runs, identical stats). *)
let php_solver n =
  let s = Sat.create () in
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
  done;
  s
;;

let test_search_machinery_determinism () =
  let n = 7 in
  let s1 = php_solver n
  and s2 = php_solver n in
  let r1 = Sat.solve s1
  and r2 = Sat.solve s2 in
  check "machinery: PHP(8,7) unsat" (r1 = Sat.Unsat && r2 = Sat.Unsat);
  let a = Sat.stats s1
  and b = Sat.stats s2 in
  check
    (Printf.sprintf
       "machinery: reduceDB engaged (>2000 conflicts, got %d)"
       a.Sat.Stats.conflicts)
    (a.Sat.Stats.conflicts > 2000);
  check
    "machinery: deterministic conflicts"
    (a.Sat.Stats.conflicts = b.Sat.Stats.conflicts);
  check
    "machinery: deterministic decisions"
    (a.Sat.Stats.decisions = b.Sat.Stats.decisions);
  check
    "machinery: deterministic propagations"
    (a.Sat.Stats.propagations = b.Sat.Stats.propagations)
;;

(* ENGAGEMENT tests (board #172 codex gap): pin that each mechanism actually FIRES in the
   solve path — a mutant that disables it must go RED. These run in the SHIPPED config,
   where adaptive restart is default-OFF (team-lead ruling B; see
   [adaptive_restart_enabled] in sat.ml). Verified RED by mutation: rephasing 4000→1500
   decisions; reduceDB 4141→3437 conflicts on PHP(8,7) (adaptive off); and re-enabling
   adaptive restart inflates the same instance 4141→31518. The reduceDB test's two-sided
   window pins BOTH: its lower bound catches reduceDB being disabled, its upper bound
   catches adaptive restart being turned back on (which would silently revert ruling B). *)

(* Rephasing engagement, ISOLATED: with N > rephase_base_interval free variables and NO
   clauses, the solve makes zero conflicts (so reduceDB and adaptive restart never fire —
   only rephasing can act) and would decide each var exactly once (decisions = N) but for
   the conflict-independent rephase interval, which fires a rephase+restart mid-descent
   and forces re-decisions. So decisions STRICTLY EXCEEDS N iff the decision-interval
   rephase engaged. (Disabling rephasing makes decisions = N — RED.) *)
let test_rephase_engagement () =
  let s = Sat.create () in
  let n = 1500 in
  for _ = 1 to n do
    ignore (Sat.new_var s : Sat.var)
  done;
  let r = Sat.solve s in
  let st = Sat.stats s in
  check "rephase-engage: sat" (r = Sat.Sat);
  check
    "rephase-engage: zero conflicts (only rephasing can act)"
    (st.Sat.Stats.conflicts = 0);
  check
    (Printf.sprintf
       "rephase-engage: decisions %d > nvars %d (interval rephase+restart fired)"
       st.Sat.Stats.decisions
       n)
    (st.Sat.Stats.decisions > n)
;;

(* reduceDB engagement + adaptive-restart-OFF guard, on PHP(8,7) (hard unsat). In the
   shipped config (adaptive off) this solves in 4141 conflicts. reduceDB fires ~7 times
   and its clause deletion reshapes the search: disabling reduceDB drops it to 3437 —
   below the LOWER bound (RED). Re-enabling adaptive restart inflates it to 31518 (the
   frequent restarting explodes pigeonhole) — above the UPPER bound (RED). So the window
   pins reduceDB engagement AND that adaptive restart stays off per ruling B; a mutant on
   either goes RED. (Direction is not "better": on pigeonhole both mechanisms inflate the
   conflict count; the test pins ENGAGEMENT / config, not benefit. This is why
   adaptive-restart engagement can't be pinned as a positive "it fires" bound here — the
   ruling makes it inert and the flag is not reachable behind the frozen sat.mli — so the
   upper bound guards the ruling instead.) *)
let test_reducedb_engagement () =
  let s = php_solver 7 in
  let r = Sat.solve s in
  let st = Sat.stats s in
  let c = st.Sat.Stats.conflicts in
  check "reduce-engage: PHP(8,7) unsat" (r = Sat.Unsat);
  check
    (Printf.sprintf "reduce-engage: conflicts %d > 3800 (reduceDB firing; off → 3437)" c)
    (c > 3800);
  check
    (Printf.sprintf
       "reduce-engage: conflicts %d < 10000 (adaptive restart OFF per ruling B; on → \
        31518)"
       c)
    (c < 10000)
;;

(* ------------------------------------------------------------------ *)
(* Branch-filter hook (sat.mli set_branch_filter). Two discriminating checks the relevancy
   lane rides on:
   (a) FIRING oracle — a filter that forbids a set of otherwise-free decision vars
       actually SUPPRESSES their decisions (the search stops branching once only forbidden
       vars remain) while the verdict is unchanged. Must be RED against a no-op filter
       (the vacuous-feature guard): a filter that changes nothing would leave the decision
       count equal, so the strict inequality below fails.
   (b) PARITY — installing an allow-all filter [fun _ -> true] reproduces the no-filter
       search exactly (same verdict + conflicts/decisions/propagations + model), so the
       [Some]-branch machinery (stash / re-insert) does not perturb search when nothing is
       filtered; combined with [pick_branch]'s structural [None] arm this is the
       bit-identical-when-unset contract. *)

(* Three unit clauses (1)(2)(3) force vars 1..3 at level 0 (0 branch-decisions); vars
   4,5,6 are allocated but appear in no clause, so the only branch-decisions are on
   {4 ,5,6}
   . *)
let build_free_var_instance () =
  let s = Sat.create () in
  for _ = 1 to 6 do
    ignore (Sat.new_var s : Sat.var)
  done;
  Sat.add_clause s [ lit 1 ];
  Sat.add_clause s [ lit 2 ];
  Sat.add_clause s [ lit 3 ];
  s
;;

let test_branch_filter_firing () =
  (* Baseline: no filter. The three free vars each get a branch-decision. *)
  let s0 = build_free_var_instance () in
  check "branch-filter firing: baseline sat" (Sat.solve s0 = Sat.Sat);
  let d0 = (Sat.stats s0).Sat.Stats.decisions in
  check "branch-filter firing: baseline decides the free vars" (d0 >= 3);
  (* Filter forbids the three free vars (0-based 3,4,5). They must never be decided, so
     the search stops branching and hands off with them unassigned. *)
  let s1 = build_free_var_instance () in
  Sat.set_branch_filter s1 (Some (fun v -> v < 3));
  check "branch-filter firing: still sat" (Sat.solve s1 = Sat.Sat);
  let d1 = (Sat.stats s1).Sat.Stats.decisions in
  check
    (Printf.sprintf "branch-filter firing: suppresses decisions (%d < %d)" d1 d0)
    (d1 < d0);
  check "branch-filter firing: forbidden vars undecided => zero branch-decisions" (d1 = 0)
;;

let test_branch_filter_parity () =
  let mk () =
    lcg := 0xC0FFEE1234;
    build 12 (List.init 40 (fun _ -> random_clause 12))
  in
  let s_none = mk () in
  let s_all = mk () in
  Sat.set_branch_filter s_all (Some (fun _ -> true));
  let r_none = Sat.solve s_none
  and r_all = Sat.solve s_all in
  check "branch-filter parity: same verdict" (r_none = r_all);
  let a = Sat.stats s_none
  and b = Sat.stats s_all in
  check
    "branch-filter parity: same conflicts"
    (a.Sat.Stats.conflicts = b.Sat.Stats.conflicts);
  check
    "branch-filter parity: same decisions"
    (a.Sat.Stats.decisions = b.Sat.Stats.decisions);
  check
    "branch-filter parity: same propagations"
    (a.Sat.Stats.propagations = b.Sat.Stats.propagations);
  if r_none = Sat.Sat
  then check "branch-filter parity: same model" (Sat.model s_none = Sat.model s_all)
;;

(* (c) EXCEPTION-SAFETY (codex S1). The filter is called mid-scan on a var already popped
   from the decision heap. If it RAISES, [pick_branch] must still re-insert every popped
   var — the stashed ones AND the one in flight — before the exception propagates;
   otherwise those vars are lost from the heap and, being untrailed, are NOT restored by
   [cancel_until 0], so a later filter-free solve on the same core can return a model that
   omits them and falsifies a clause over them (a wrong-SAT reachable from this public
   API). RED against the pre-fix core (which re-inserts only after [go] returns normally). *)
let test_branch_filter_exception_safe () =
  let s = Sat.create () in
  let a = Sat.new_var s in
  let b = Sat.new_var s in
  Sat.add_clause s [ Sat.pos a; Sat.pos b ];
  (* Stash the first var the filter is asked about, then raise on the second: against the
     unfixed core this loses BOTH (the stashed [a] and the in-flight [b]). *)
  let calls = ref 0 in
  Sat.set_branch_filter
    s
    (Some
       (fun _ ->
         incr calls;
         if !calls >= 2 then raise Exit else false));
  (match Sat.solve s with
   | (_ : Sat.result) ->
     check "branch-filter exn-safe: filter raise propagates out of solve" false
   | exception Exit -> ());
  (* Clear the filter and re-solve. With the fix [a] and [b] are back in the heap, so the
     core finds a genuine model of (a ∨ b). Against the unfixed core the heap is empty and
     the no-theory path returns Sat with a = b = false, falsifying the clause. *)
  Sat.set_branch_filter s None;
  check "branch-filter exn-safe: re-solve sat" (Sat.solve s = Sat.Sat);
  check "branch-filter exn-safe: model satisfies (a ∨ b)" (Sat.value s a || Sat.value s b)
;;

(* A10: with OXSMT_SATPRE off (the default this executable runs under), [set_eliminable]
   is inert — marking variables must not change the verdict, model, or the counter trio
   versus a run that never marks anything. Guards the "bit-identical when off" contract at
   the unit level; the firing / reconstruction behaviour with the gate ON lives in
   satpre_test.exe (run with OXSMT_SATPRE=1). *)
let test_eliminable_inert_when_off () =
  let clauses = [ [ -1; 2 ]; [ 1; -2; 3 ]; [ -3; 4 ]; [ 1; 2; -4 ]; [ -1; -3; 4 ] ] in
  let run mark =
    let s = build 4 clauses in
    if mark then List.iter (fun v -> Sat.set_eliminable s v) [ 0; 1; 2; 3 ];
    let r = Sat.solve s in
    let st = Sat.stats s in
    r, Array.to_list (Sat.model s), (st.conflicts, st.decisions, st.propagations)
  in
  let r0, m0, c0 = run false in
  let r1, m1, c1 = run true in
  check "eliminable-off: same verdict" (r0 = r1);
  check "eliminable-off: same model" (m0 = m1);
  check "eliminable-off: same counters" (c0 = c1)
;;

let () =
  test_eliminable_inert_when_off ();
  test_branch_filter_firing ();
  test_branch_filter_parity ();
  test_branch_filter_exception_safe ();
  test_lbd_of_levels ();
  test_rephase_engagement ();
  test_reducedb_engagement ();
  test_reduce_deletions_protects_glue ();
  test_reduce_deletions_worst_half_and_locked ();
  test_rephase_schedule ();
  test_search_machinery_determinism ();
  test_dimacs_strict ();
  test_budget_tick_exception_safety ();
  test_analyze_multi ();
  test_analyze_unit ();
  test_minimize_must_not_fire ();
  test_minimize_bypassed_when_traced ();
  test_propagation ();
  test_level0_contradiction ();
  test_empty_clause ();
  test_no_clauses ();
  test_assumptions ();
  test_incremental ();
  test_determinism ();
  test_pigeonhole 5;
  test_property "sparse" gen_sparse 20000;
  test_property "dense" gen_dense 20000;
  (* untraced verdict oracle: exercises local minimization (bypassed under trace) so the
     sat-minimize-unsound mutant has a kill site — cert-step-1 fix round *)
  test_property_untraced "sparse" gen_sparse 20000;
  test_property_untraced "dense" gen_dense 20000;
  Printf.printf "sat_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
