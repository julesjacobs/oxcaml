module Sat = Oxsmt_solver.Sat

(* Chronological-backtracking (task #41 Stage 1) adversarial self-test. RUN WITH
   [OXSMT_CHRONO=1] (the Makefile [chrono-test] target sets it), so every [Sat.create]
   below builds a CB solver — [Sat.create] reads the gate once at construction and there
   is no in-process toggle. The test refuses to run if the gate is unset, so a broken
   Makefile wiring cannot silently degrade this to a redundant OFF run.

   What it proves:
   1. THE §10.2 CRUX, RED-verified: the watch-repair after a scattered (out-of-order)
      [cancel_until] is soundness-critical. If it is broken (a clause whose only
      satisfying literal was removed, with a surviving false partner watch, is not
      re-detected as unit), the solver reports a model that FALSIFIES a clause (wrong-Sat)
      or the wrong verdict. Both are caught below: every reported [Sat] model is evaluated
      against the formula, and every verdict is cross-checked against an INDEPENDENT DPLL
      oracle (a distinct recursive implementation, sharing no code with the CDCL core). A
      directed hazard family plus thousands of conflict-dense random CNFs give the crux
      real volume.
   2. Determinism (I6): the same formula solved twice yields the same verdict, model, and
      counter trio under CB.

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

(* ------------------------------------------------------------------ *)
(* Independent DPLL oracle. A naive recursive definition-of-satisfiability search over
   DIMACS clauses (±v, 1-based); no watches, no learning, no heuristics — deliberately
   shares nothing with the core under test. Correct, not fast; used only on small
   formulas. *)
module Oracle = struct
  let solve num_vars clauses =
    let assign = Array.make (num_vars + 1) 0 in
    let lit_false l =
      let a = assign.(abs l) in
      a <> 0 && Bool.equal (l > 0) (a = 1) = false
    in
    let falsified () = List.exists (List.for_all lit_false) clauses in
    let rec go v =
      if falsified ()
      then false
      else if v > num_vars
      then true
      else (
        assign.(v) <- 1;
        if go (v + 1)
        then true
        else (
          assign.(v) <- -1;
          let r = go (v + 1) in
          if not r then assign.(v) <- 0;
          r))
    in
    go 1
  ;;
end

(* ------------------------------------------------------------------ *)
(* SAT-core driver over DIMACS. *)

let lit_of_dimacs _s l =
  (* vars are pre-allocated by [build]/[build_conflict_mock] (and [add_clause]
     auto-allocates on demand), so this only maps a DIMACS literal to a [Sat.lit] — no
     fresh var. *)
  let v = abs l - 1 in
  if l > 0 then Sat.pos v else Sat.neg v
;;

let build num_vars clauses =
  let s = Sat.create () in
  for _ = 1 to num_vars do
    ignore (Sat.new_var s : int)
  done;
  List.iter (fun cl -> Sat.add_clause s (List.map (lit_of_dimacs s) cl)) clauses;
  s
;;

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

(* ------------------------------------------------------------------ *)
(* Deterministic PRNG (xorshift64*, fixed seed — same family as sat_test). *)
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

(* Dense 3-CNF near the phase transition (ratio ~4.3): conflict-heavy, so CB fires often
   and the out-of-order trail (hence the watch-repair crux) is exercised hard. *)
let gen_dense () =
  let num_vars = 6 + rand_n 8 in
  let num_clauses = (num_vars * 4) + rand_n num_vars in
  let clause () =
    List.init 3 (fun _ ->
      let v = 1 + rand_n num_vars in
      if rand_n 2 = 0 then v else -v)
  in
  num_vars, List.init num_clauses (fun _ -> clause ())
;;

(* Sparse mixed-width formulas: broad structural coverage, more sat instances (so the
   model self-check — the wrong-Sat detector for a broken watch-repair — gets volume). *)
let gen_sparse () =
  let num_vars = 4 + rand_n 10 in
  let num_clauses = 1 + rand_n (num_vars * 3) in
  let clause () =
    let width = 1 + rand_n 3 in
    List.init width (fun _ ->
      let v = 1 + rand_n num_vars in
      if rand_n 2 = 0 then v else -v)
  in
  num_vars, List.init num_clauses (fun _ -> clause ())
;;

let test_property label gen n =
  let disagreements = ref 0 in
  let bad_models = ref 0 in
  let sat_count = ref 0 in
  for _ = 1 to n do
    let num_vars, clauses = gen () in
    let expected = Oracle.solve num_vars clauses in
    let s = build num_vars clauses in
    match Sat.solve s with
    | Sat.Sat ->
      incr sat_count;
      if not expected then incr disagreements;
      if not (model_satisfies clauses (Sat.model s)) then incr bad_models
    | Sat.Unsat -> if expected then incr disagreements
  done;
  check
    (Printf.sprintf
       "property[%s]: %d formulas agree with DPLL oracle (%d disagreements)"
       label
       n
       !disagreements)
    (!disagreements = 0);
  check
    (Printf.sprintf "property[%s]: all sat models valid (%d bad)" label !bad_models)
    (!bad_models = 0);
  Printf.printf "  (property[%s]: %d formulas, %d sat)\n" label n !sat_count
;;

(* Directed hazard family: chains of clauses engineered so that conflict-dense solving
   produces deep out-of-order trails. Each is a random dense CNF over a slightly larger
   variable set with a couple of long clauses (whose satisfying literal is prone to being
   the removed-watch of the §10.2 hazard). Cross-checked and model-verified like the
   property runs; kept separate so a regression in this family is named distinctly. *)
let test_directed n =
  let disagreements = ref 0 in
  let bad_models = ref 0 in
  for _ = 1 to n do
    let num_vars = 8 + rand_n 6 in
    let three () =
      List.init 3 (fun _ ->
        let v = 1 + rand_n num_vars in
        if rand_n 2 = 0 then v else -v)
    in
    let wide () =
      List.init
        (4 + rand_n 3)
        (fun _ ->
           let v = 1 + rand_n num_vars in
           if rand_n 2 = 0 then v else -v)
    in
    let clauses =
      List.init ((num_vars * 4) + rand_n num_vars) (fun _ -> three ())
      @ List.init (2 + rand_n 3) (fun _ -> wide ())
    in
    let expected = Oracle.solve num_vars clauses in
    let s = build num_vars clauses in
    match Sat.solve s with
    | Sat.Sat ->
      if not expected then incr disagreements;
      if not (model_satisfies clauses (Sat.model s)) then incr bad_models
    | Sat.Unsat -> if expected then incr disagreements
  done;
  check
    (Printf.sprintf "directed-hazard: %d formulas agree with DPLL (%d)" n !disagreements)
    (!disagreements = 0);
  check
    (Printf.sprintf "directed-hazard: all sat models valid (%d bad)" !bad_models)
    (!bad_models = 0)
;;

(* Run-twice determinism under CB: verdict, model, and the counter trio must match. *)
let test_determinism n =
  let mismatches = ref 0 in
  for _ = 1 to n do
    let num_vars, clauses = gen_dense () in
    let run () =
      let s = build num_vars clauses in
      let v = Sat.solve s in
      let m = if v = Sat.Sat then Sat.model s else [||] in
      let st = Sat.stats s in
      v, m, (st.conflicts, st.decisions, st.propagations)
    in
    let v1, m1, c1 = run () in
    let v2, m2, c2 = run () in
    if not (v1 = v2 && m1 = m2 && c1 = c2) then incr mismatches
  done;
  check
    (Printf.sprintf
       "determinism: %d formulas reproduce exactly (%d mismatch)"
       n
       !mismatches)
    (!mismatches = 0)
;;

(* THEORY-SEAM REPLAY test (§3.6/§10.5 audit item 6), RED-verified. Under CB a scattered
   [cancel_until] is not a top-frame suffix, so the seam REBUILDS the theory to the
   surviving trail ([on_backtrack ~0] to base + replay [on_assign] for each survivor)
   instead of popping a frame suffix.

   The mock is a REAL conflict-emitting theory over a fixed set of binary implication
   constraints [a → b] (var indices; equivalently the clause [¬a ∨ b]). It keeps its own
   trail-synchronized assignment view via a FRAME STACK indexed by SAT decision level —
   exactly like the real cdclt adapter — pushing per level on [on_assign] and popping
   frames on [on_backtrack]. At each [check] it reports [T_conflict [a; ¬b]] for the first
   violated constraint (both literals currently asserted true), else consistent; it never
   propagates, so it needs no [explain]. The instance's verdict must therefore equal the
   DPLL oracle over [clauses ∧ {¬a∨b}], which we cross-check; every reported sat model
   must satisfy the clauses AND the constraints.

   RED against the naive frame-suffix [on_backtrack ~level] (no rebuild): the frame the
   mock pops by count no longer matches the scattered Boolean removal, so its assignment
   view goes stale; the core then either validates a stale [T_conflict] against the real
   trail and raises {!Sat.Theory_contract_violation} (caught here as a failure) or reaches
   a verdict that disagrees with the oracle. With the correct rebuild the view always
   matches the trail. *)
let random_constraints num_vars =
  (* a handful of a→b implications over distinct vars *)
  List.init
    (1 + rand_n 4)
    (fun _ ->
       let a = rand_n num_vars in
       let b = rand_n num_vars in
       a, b)
;;

let constraint_clauses constraints =
  (* a→b as the DIMACS clause [¬(a+1); (b+1)] for the oracle *)
  List.map (fun (a, b) -> [ -(a + 1); b + 1 ]) constraints
;;

let build_conflict_mock num_vars clauses constraints =
  let s = Sat.create () in
  for _ = 1 to num_vars do
    ignore (Sat.new_var s : int)
  done;
  let asg = Array.make num_vars 0 in
  (* frames.(k) = vars assigned while the mock was at level k+1; mock_level tracks the
     theory's own frame count, synced up to [Sat.decision_level] on each assign. *)
  let frames : int list array ref = ref (Array.make 0 []) in
  let mock_level = ref 0 in
  let ensure_frames k =
    if k > Array.length !frames
    then (
      let f = Array.make k [] in
      Array.blit !frames 0 f 0 (Array.length !frames);
      frames := f)
  in
  let on_assign l =
    let dl = Sat.decision_level s in
    ensure_frames dl;
    while !mock_level < dl do
      incr mock_level;
      !frames.(!mock_level - 1) <- []
    done;
    let v = Sat.var_of_lit l in
    asg.(v) <- (if Sat.sign_of_lit l then 1 else -1);
    if !mock_level > 0 then !frames.(!mock_level - 1) <- v :: !frames.(!mock_level - 1)
  in
  let on_backtrack ~level =
    while !mock_level > level do
      List.iter (fun v -> asg.(v) <- 0) !frames.(!mock_level - 1);
      !frames.(!mock_level - 1) <- [];
      decr mock_level
    done
  in
  let check ~final:_ =
    let rec go = function
      | [] -> Sat.T_consistent []
      | (a, b) :: rest ->
        if asg.(a) = 1 && asg.(b) = -1
        then Sat.T_conflict [ Sat.pos a; Sat.neg b ]
        else go rest
    in
    go constraints
  in
  Sat.set_theory s (Some { Sat.on_assign; on_backtrack; check; explain = (fun _ -> []) });
  List.iter (fun cl -> Sat.add_clause s (List.map (lit_of_dimacs s) cl)) clauses;
  s
;;

let test_seam_replay n =
  let disagreements = ref 0 in
  let bad_models = ref 0 in
  let raises = ref 0 in
  for _ = 1 to n do
    (* small so the augmented oracle is cheap and solving terminates within budget *)
    let num_vars = 5 + rand_n 6 in
    let clause () =
      List.init 3 (fun _ ->
        let v = 1 + rand_n num_vars in
        if rand_n 2 = 0 then v else -v)
    in
    let clauses = List.init ((num_vars * 4) + rand_n num_vars) (fun _ -> clause ()) in
    let constraints = random_constraints num_vars in
    let augmented = clauses @ constraint_clauses constraints in
    let expected = Oracle.solve num_vars augmented in
    try
      let s = build_conflict_mock num_vars clauses constraints in
      match Sat.solve s with
      | Sat.Sat ->
        if not expected then incr disagreements;
        if not (model_satisfies augmented (Sat.model s)) then incr bad_models
      | Sat.Unsat -> if expected then incr disagreements
    with
    | Sat.Theory_contract_violation _ -> incr raises
  done;
  check
    (Printf.sprintf
       "seam-replay: %d formulas agree with augmented DPLL (%d)"
       n
       !disagreements)
    (!disagreements = 0);
  check
    (Printf.sprintf
       "seam-replay: all sat models satisfy clauses+constraints (%d bad)"
       !bad_models)
    (!bad_models = 0);
  check
    (Printf.sprintf
       "seam-replay: no theory-contract violation from a stale view (%d)"
       !raises)
    (!raises = 0)
;;

(* Guard: this executable is meaningless unless CB is actually engaged. We cannot query
   the gate through the frozen [Sat] surface, so we assert the env directly — a green run
   then genuinely exercised the chrono paths. *)
let assert_chrono_gate () =
  match Sys.getenv_opt "OXSMT_CHRONO" with
  | Some ("1" | "true" | "yes" | "on") -> ()
  | _ ->
    prerr_endline
      "chrono_test: OXSMT_CHRONO is not set — this suite must run with the gate ON (see \
       `make chrono-test`).";
    exit 2
;;

let () =
  assert_chrono_gate ();
  Printf.printf "chrono_test: OXSMT_CHRONO on";
  (match Sys.getenv_opt "OXSMT_CHRONO_T" with
   | Some t -> Printf.printf " (T=%s)\n" t
   | None -> Printf.printf " (T=default)\n");
  test_property "sparse" gen_sparse 4000;
  test_property "dense" gen_dense 4000;
  test_directed 3000;
  test_seam_replay 4000;
  test_determinism 500;
  Printf.printf "chrono_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
