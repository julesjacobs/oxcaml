module Sat = Oxsmt_solver.Sat
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

let test_property n =
  let disagreements = ref 0 in
  let bad_models = ref 0 in
  for _ = 1 to n do
    let num_vars = 3 + rand_n 10 in
    let num_clauses = 1 + rand_n (num_vars * 4) in
    let clauses = List.init num_clauses (fun _ -> random_clause num_vars) in
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
       "property: %d formulas agree with DPLL (%d disagreements)"
       n
       !disagreements)
    (!disagreements = 0);
  check
    (Printf.sprintf "property: all sat models valid (%d bad)" !bad_models)
    (!bad_models = 0)
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

let () =
  test_dimacs_strict ();
  test_analyze_multi ();
  test_analyze_unit ();
  test_propagation ();
  test_level0_contradiction ();
  test_empty_clause ();
  test_no_clauses ();
  test_assumptions ();
  test_incremental ();
  test_determinism ();
  test_pigeonhole 5;
  test_property 20000;
  Printf.printf "sat_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
