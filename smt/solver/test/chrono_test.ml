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

(* A learned clause L must be ENTAILED by the formula F: F ∧ ¬L is UNSAT (checked with the
   INDEPENDENT DPLL oracle, never the solver under test). CB rewires the learning path
   (conflict_level, the walk-skip), so this guards that the out-of-order 1UIP still
   derives only sound clauses — a too-strong learnt (dropping a needed literal) is caught
   here even when the final verdict is unaffected. Mirrors sat_test's
   [learned_clause_entailed]. *)
let learned_entailed num_vars clauses learned_dimacs =
  let neg_units = List.map (fun l -> [ -l ]) learned_dimacs in
  not (Oracle.solve num_vars (List.rev_append neg_units clauses))
;;

let dimacs_of_lit l =
  let v = Sat.var_of_lit l + 1 in
  if Sat.sign_of_lit l then v else -v
;;

let test_property label gen n =
  let disagreements = ref 0 in
  let bad_models = ref 0 in
  let sat_count = ref 0 in
  let n_learned = ref 0 in
  let unentailed = ref 0 in
  for _ = 1 to n do
    let num_vars, clauses = gen () in
    let expected = Oracle.solve num_vars clauses in
    let s = build num_vars clauses in
    (* Collect every learned clause (as DIMACS) under CB for entailment-checking. Trace is
       a pure side channel; it does not perturb the search. *)
    let learned = ref [] in
    Sat.set_trace
      s
      (Some
         { Sat.on_learned =
             (fun ~id:_ ~clause ~antecedents:_ ~btlevel:_ ->
               learned := List.map dimacs_of_lit (Array.to_list clause) :: !learned)
         ; on_input = (fun ~id:_ ~clause:_ ~origin:_ -> ())
         ; on_unit = (fun ~id:_ ~lit:_ -> ())
         ; on_theory_clause = (fun ~id:_ ~clause:_ ~role:_ -> ())
         ; on_unsat = (fun _ -> ())
         });
    (match Sat.solve s with
     | Sat.Sat ->
       incr sat_count;
       if not expected then incr disagreements;
       if not (model_satisfies clauses (Sat.model s)) then incr bad_models
     | Sat.Unsat -> if expected then incr disagreements);
    List.iter
      (fun l ->
        incr n_learned;
        if not (learned_entailed num_vars clauses l) then incr unentailed)
      !learned
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
  check
    (Printf.sprintf
       "property[%s]: all %d learned clauses entailed (%d unentailed)"
       label
       !n_learned
       !unentailed)
    (!unentailed = 0);
  Printf.printf
    "  (property[%s]: %d formulas, %d sat, %d learned entailment-checked)\n"
    label
    n
    !sat_count
    !n_learned
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
  (* This mock deliberately frames by [decision_level] (not the new [~level]) to keep
     testing the CB REBUILD path: under the rebuild the core replays every survivor at the
     backtrack target level, and the mock must track that. The delivered [~level] is
     ignored here; the true-level delivery itself is checked by
     [test_true_level_delivery]. *)
  let on_assign l ~level:_ =
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
  Sat.set_theory
    s
    (Some
       { Sat.on_assign
       ; on_backtrack
       ; check
       ; explain = (fun _ -> [])
       ; on_chrono_rewind = None
       });
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

(* F1 — theory-PROPAGATION provenance under CB, RED-verified. The conflict-mock above
   never propagates and its [explain = fun _ -> []] never runs, so it gave FALSE
   CONFIDENCE for Phase 2: it exercised the verdict path but NOT the explanation path that
   the real adapters (euf/lia/arr) take when [analyze] resolves through a
   theory-propagated literal. This mock closes that gap.

   The mock is a real theory over implication constraints [a → b] that also does UNIT
   PROPAGATION of them ([a] true, [b] unassigned ⇒ propagate [b] with reason [{a}]; [b]
   false, [a] unassigned ⇒ propagate [¬a] with reason [{¬b}]); it reports [T_conflict] for
   a violated constraint. It faithfully mirrors the real adapters' FRAME-SCOPED reason
   cache: a propagation's reason is snapshotted AT PROPAGATION TIME into the current
   [push] frame and dropped on the matching [pop]; [explain] serves the cached reason and
   — exactly like {!Euf_adapter.explain} / {!Lia_adapter.explain} — RAISES "no cached
   reason (frame was popped)" when the entry is gone (there is no precedence-valid
   ask-time recompute).

   RED against unfixed Phase 2: the chrono [cancel_until] theory rebuild
   ([on_backtrack ~level:0] + replay [on_assign] survivors) re-asserts a surviving
   theory-propagated literal as a FACT but never re-caches its reason, so a later
   [analyze] that resolves through it calls [explain] on an uncached literal ⇒ raise ⇒ the
   [raises] counter fires (or, if a stale reason slips the CONTRACT-EX guard, an oracle
   disagreement). With F1 — the chrono [cancel_until] SNAPSHOTS each surviving
   [Theory_prop] literal's reason (via [theory_premises]) into the SAT core just before
   the rebuild destroys the adapter cache, and serves it back afterwards — every surviving
   propagated literal stays explainable, so [explain] always finds a valid entry.

   PROBE INTERACTION (none): this mock runs on the THEORY-PLUGGED path. The SAT
   inprocessing probes that open their own decision level and call [cancel_until]
   (vivification, failed-literal probing, ELS — all now on trunk) are gated to
   [t.theory = None], so they are inert here and cannot interact with the theory snapshot;
   equivalently, F1's invariant is stated over [cancel_until] itself (any caller), not
   over any one probe. *)
exception Mock_frame_popped

let build_prop_mock num_vars clauses constraints =
  let s = Sat.create () in
  for _ = 1 to num_vars do
    ignore (Sat.new_var s : int)
  done;
  let asg = Array.make num_vars 0 in
  let frames : int list array ref = ref (Array.make 0 []) in
  let mock_level = ref 0 in
  (* reason cache: signed-lit key -> premises; [cache_frames.(k-1)] lists the keys cached
     while the mock was at level k, so [pop] drops exactly them (mirrors the adapters'
     per-frame [explain_cache] + [frames]). Level-0 reasons live in [base_cache] and are
     never popped — the base frame survives [on_backtrack ~level:0]. *)
  let reason_tbl : (int, Sat.lit list) Hashtbl.t = Hashtbl.create 64 in
  let cache_frames : int list array ref = ref (Array.make 0 []) in
  let key l = (Sat.var_of_lit l * 2) + if Sat.sign_of_lit l then 1 else 0 in
  let ensure k =
    if k > Array.length !frames
    then (
      let grow a =
        let f = Array.make k [] in
        Array.blit !a 0 f 0 (Array.length !a);
        a := f
      in
      grow frames;
      grow cache_frames)
  in
  let cache_reason l prem =
    let k = key l in
    if not (Hashtbl.mem reason_tbl k)
    then (
      Hashtbl.replace reason_tbl k prem;
      if !mock_level > 0
      then !cache_frames.(!mock_level - 1) <- k :: !cache_frames.(!mock_level - 1))
  in
  (* frames by [decision_level] (see the sibling mock): tests the rebuild + reason cache,
     so the new [~level] is ignored here. *)
  let on_assign l ~level:_ =
    let dl = Sat.decision_level s in
    ensure dl;
    while !mock_level < dl do
      incr mock_level;
      !frames.(!mock_level - 1) <- [];
      !cache_frames.(!mock_level - 1) <- []
    done;
    let v = Sat.var_of_lit l in
    asg.(v) <- (if Sat.sign_of_lit l then 1 else -1);
    if !mock_level > 0 then !frames.(!mock_level - 1) <- v :: !frames.(!mock_level - 1)
  in
  let on_backtrack ~level =
    while !mock_level > level do
      List.iter (fun v -> asg.(v) <- 0) !frames.(!mock_level - 1);
      !frames.(!mock_level - 1) <- [];
      List.iter (fun k -> Hashtbl.remove reason_tbl k) !cache_frames.(!mock_level - 1);
      !cache_frames.(!mock_level - 1) <- [];
      decr mock_level
    done
  in
  let check ~final:_ =
    let rec first_confl = function
      | [] -> None
      | (a, b) :: rest ->
        if asg.(a) = 1 && asg.(b) = -1 then Some (a, b) else first_confl rest
    in
    match first_confl constraints with
    | Some (a, b) -> Sat.T_conflict [ Sat.pos a; Sat.neg b ]
    | None ->
      let props = ref [] in
      List.iter
        (fun (a, b) ->
          if asg.(a) = 1 && asg.(b) = 0
          then (
            let l = Sat.pos b in
            cache_reason l [ Sat.pos a ];
            props := l :: !props)
          else if asg.(b) = -1 && asg.(a) = 0
          then (
            let l = Sat.neg a in
            cache_reason l [ Sat.neg b ];
            props := l :: !props))
        constraints;
      Sat.T_consistent !props
  in
  let explain l =
    match Hashtbl.find_opt reason_tbl (key l) with
    | Some prem -> prem
    | None -> raise Mock_frame_popped
  in
  Sat.set_theory
    s
    (Some { Sat.on_assign; on_backtrack; check; explain; on_chrono_rewind = None });
  List.iter (fun cl -> Sat.add_clause s (List.map (lit_of_dimacs s) cl)) clauses;
  s
;;

let test_prop_seam n =
  let disagreements = ref 0 in
  let bad_models = ref 0 in
  let raises = ref 0 in
  for _ = 1 to n do
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
      let s = build_prop_mock num_vars clauses constraints in
      match Sat.solve s with
      | Sat.Sat ->
        if not expected then incr disagreements;
        if not (model_satisfies augmented (Sat.model s)) then incr bad_models
      | Sat.Unsat -> if expected then incr disagreements
    with
    (* Either failure form: the mock's own "frame was popped" (mirrors euf/lia) or the
       core's CONTRACT-EX guard tripping on a stale reason the mock let through. *)
    | Mock_frame_popped | Sat.Theory_contract_violation _ -> incr raises
  done;
  check
    (Printf.sprintf
       "prop-seam: %d formulas agree with augmented DPLL (%d)"
       n
       !disagreements)
    (!disagreements = 0);
  check
    (Printf.sprintf
       "prop-seam: all sat models satisfy clauses+constraints (%d bad)"
       !bad_models)
    (!bad_models = 0);
  check
    (Printf.sprintf
       "prop-seam: every theory-propagated literal stays explainable after a chrono \
        backtrack (%d strandings)"
       !raises)
    (!raises = 0)
;;

(* TRUE-LEVEL DELIVERY (fabric S4.1 seam), RED-verified, PER-SITE. [on_assign] must hand
   the theory a literal's TRUE decision level from BOTH firing sites. Under CB the trail
   is non-monotone, so a delivered true level can be STRICTLY BELOW [decision_level]:
   - ENQUEUE site ([unchecked_enqueue]): a learned unit enqueued at its backjump level
     while the solver still sits at a higher level;
   - REPLAY site (the chrono [cancel_until] rebuild): a survivor whose true level is below
     the backtrack target, re-asserted while [decision_level] equals that (higher) target.
     A conforming seam delivers the true level at each; the pre-S4 pull
     ([Sat.decision_level]) delivers the current level at both.

   This observer attributes each delivery to a site WITHOUT a trail accessor: the chrono
   rebuild fires [on_backtrack ~level:0] then replays the surviving trail, so between a
   backtrack and the first NON-survivor delivery every fact is a replay of a literal held
   before the backtrack ([pre_bt]); the first delivery not in [pre_bt] (the freshly
   learned unit) ends the replay window and is an enqueue. It then requires a
   below-current delivery at EACH site independently, plus one delivery equal to a
   positive current level.

   RED, each mutation-killed INDEPENDENTLY: enqueue site -> [decision_level t] fails
   [saw_below_enqueue]; replay site -> [decision_level t] fails [saw_below_replay]; an
   always-zero delivery fails [saw_level_eq_current_pos] (0 never equals a positive
   current level). [level <= decision_level] (a true level is never ABOVE current) is also
   asserted. The observer never conflicts or propagates, so it cannot change the verdict.
   Own fixed seed so it is placement-independent and reproducible. *)
let test_true_level_delivery n =
  let saved_lcg = !lcg in
  lcg := 0x1E3779B97F4A7C16;
  let key l = (Sat.var_of_lit l * 2) + if Sat.sign_of_lit l then 1 else 0 in
  let saw_below_enqueue = ref false in
  let saw_below_replay = ref false in
  let saw_level_eq_current_pos = ref false in
  let level_above_current = ref 0 in
  for _ = 1 to n do
    let num_vars, clauses = gen_dense () in
    let s = Sat.create () in
    for _ = 1 to num_vars do
      ignore (Sat.new_var s : int)
    done;
    let view = ref (Hashtbl.create 64) in
    let pre_bt = ref (Hashtbl.create 64) in
    let replaying = ref false in
    let on_assign l ~level =
      let dl = Sat.decision_level s in
      (* a replay re-asserts a survivor held before the backtrack; the first delivery not
         in [pre_bt] is the freshly learned unit and ends the replay window *)
      let is_replay = !replaying && Hashtbl.mem !pre_bt (key l) in
      if !replaying && not (Hashtbl.mem !pre_bt (key l)) then replaying := false;
      if level > dl then incr level_above_current;
      if level = dl && dl > 0 then saw_level_eq_current_pos := true;
      if level < dl
      then if is_replay then saw_below_replay := true else saw_below_enqueue := true;
      Hashtbl.replace !view (key l) ()
    in
    let on_backtrack ~level:_ =
      pre_bt := !view;
      view := Hashtbl.create 64;
      replaying := true
    in
    let check ~final:_ =
      replaying := false;
      Sat.T_consistent []
    in
    Sat.set_theory
      s
      (Some
         { Sat.on_assign
         ; on_backtrack
         ; check
         ; explain = (fun _ -> [])
         ; on_chrono_rewind = None
         });
    List.iter (fun cl -> Sat.add_clause s (List.map (lit_of_dimacs s) cl)) clauses;
    ignore (Sat.solve s : Sat.result)
  done;
  lcg := saved_lcg;
  check
    (Printf.sprintf
       "true-level: ENQUEUE site delivered a below-current true level over %d CB \
        formulas (kills enqueue-site decision_level mutant)"
       n)
    !saw_below_enqueue;
  check
    "true-level: REPLAY site delivered a below-current true level (kills replay-site \
     decision_level mutant)"
    !saw_below_replay;
  check
    "true-level: some delivery equals a positive current level (kills always-zero mutant)"
    !saw_level_eq_current_pos;
  check
    (Printf.sprintf
       "true-level: delivered level is never above the current decision level (%d \
        violations)"
       !level_above_current)
    (!level_above_current = 0)
;;

(* ------------------------------------------------------------------ *)
(* S4.2 INCREMENTAL-UNDO OBS-EQ + frames-vs-watermark RED. The seam added
   [on_chrono_rewind]: with [Some rewind] the chrono [cancel_until] rewinds the theory to
   the earliest-removed watermark [w] and replays ONLY the survivors at compacted positions
   [w..trail_n-1], instead of [on_backtrack ~level:0] + replay-of-ALL-survivors. These
   tests drive that SAT-core dispatch DIRECTLY through mock theories — the env flag
   [OXSMT_CHRONO_INCR_UNDO] gates only the real cdclt install, whereas a mock sets
   [on_chrono_rewind] itself, so the core's [Some]/[None] arm is exercised regardless of the
   flag. Both mocks are conflict-only (they never propagate, so [explain] is never called);
   the verdict must equal the DPLL oracle over [clauses ∧ {¬a∨b}], and every sat model must
   satisfy clauses+constraints — the same augmented-oracle cross-check as {!test_seam_replay}.

   OBS-EQ ({!test_incr_undo_obs_eq}): the CORRECT mock keeps a FLAT, stream-indexed
   assignment log — [on_assign] appends the var at the next stream position (the seam
   contract fires exactly one [on_assign] per trail placement, so stream index == trail
   index); [on_chrono_rewind w] clears exactly the assertions at stream positions [>= w] and
   sets the log length back to [w]; the core then replays positions [w..]. Survivors [0,w)
   are never cleared and [w..] are re-asserted, so the assignment view after every backtrack
   equals a from-base rebuild's — observational equality BY CONSTRUCTION. Verified sound
   against the independent oracle over thousands of conflict-dense CB solves + all-models
   valid + zero stale [T_conflict] (no [Theory_contract_violation]).

   RED — two mutants, both through the deeper {!gen_deep_directed} family (see {!run_mutant}):
   - {!test_incr_undo_frame_pop_boundary}: the FAITHFUL frames-vs-watermark mutant
     ({!build_frame_pop_mock}) keeps a decision-level FRAME STACK and pops whole frames to
     the current level instead of rewinding to the absolute watermark [w]. Under CB a
     literal's frame need not match its true level, so frame-popping CAN discard the wrong
     assertions — but only when the trail is deep/out-of-order; on shallow trails frames ==
     watermark and it is observationally equivalent (measured [caught = 0] on random 3-CNF).
     Kept as a NON-gating DIAGNOSTIC that prints its catch count to document that boundary,
     NOT a gate (asserting [caught > 0] on it would be RED theater on this generator).
   - {!test_incr_undo_overkeep_red}: the LOAD-BEARING RED ({!build_overkeep_mock}) is the
     correct flat-stream rewind with an OFF-BY-ONE bug — [on_chrono_rewind w] clears only
     stream positions [>= w+1], KEEPING the earliest-removed literal (position [w]) as
     STALE-TRUE while the core replays [w..] anyway. When [check] cites that stale var in a
     [T_conflict] the core raises {!Sat.Theory_contract_violation} (or the stale view
     disagrees with the oracle). An off-by-one absolute rewind ALWAYS strands the
     earliest-removed literal, so this cannot be coincidence-lucky: it asserts [caught > 0]
     and proves the watermark's exact position is load-bearing. Mirrors the frame-suffix RED
     of {!test_seam_replay}, one level down (the incremental arm). *)
let build_rewind_mock num_vars clauses constraints =
  let s = Sat.create () in
  for _ = 1 to num_vars do
    ignore (Sat.new_var s : int)
  done;
  let asg = Array.make num_vars 0 in
  (* flat stream: the var asserted at each [on_assign] stream position (== trail index) *)
  let stream = ref (Array.make 16 0) in
  let stream_n = ref 0 in
  let push v =
    if !stream_n >= Array.length !stream
    then (
      let a = Array.make (2 * Array.length !stream) 0 in
      Array.blit !stream 0 a 0 (Array.length !stream);
      stream := a);
    !stream.(!stream_n) <- v;
    incr stream_n
  in
  let on_assign l ~level:_ =
    let v = Sat.var_of_lit l in
    push v;
    asg.(v) <- (if Sat.sign_of_lit l then 1 else -1)
  in
  (* Under CB every [cancel_until] takes the chrono arm, which with
     [Some on_chrono_rewind] calls the rewind and NEVER [on_backtrack]; a no-op is
     therefore correct here. *)
  let on_backtrack ~level:_ = () in
  (* CORRECT: sub-frame absolute rewind to stream position [w] — clear exactly [w..]. *)
  let on_chrono_rewind w =
    for i = w to !stream_n - 1 do
      asg.(!stream.(i)) <- 0
    done;
    stream_n := w
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
  Sat.set_theory
    s
    (Some
       { Sat.on_assign
       ; on_backtrack
       ; check
       ; explain = (fun _ -> [])
       ; on_chrono_rewind = Some on_chrono_rewind
       });
  List.iter (fun cl -> Sat.add_clause s (List.map (lit_of_dimacs s) cl)) clauses;
  s
;;

let build_frame_pop_mock num_vars clauses constraints =
  let s = Sat.create () in
  for _ = 1 to num_vars do
    ignore (Sat.new_var s : int)
  done;
  let asg = Array.make num_vars 0 in
  let frames : int list array ref = ref (Array.make 0 []) in
  let mock_level = ref 0 in
  let ensure_frames k =
    if k > Array.length !frames
    then (
      let f = Array.make k [] in
      Array.blit !frames 0 f 0 (Array.length !frames);
      frames := f)
  in
  let on_assign l ~level:_ =
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
  let on_backtrack ~level:_ = () in
  (* MUTANT: pop whole frames down to the CURRENT decision level, ignoring the watermark
     [w]. Frame-count semantics, not a sub-frame absolute rewind — the S4.1 hazard. *)
  let on_chrono_rewind _w =
    let dl = Sat.decision_level s in
    while !mock_level > dl do
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
  Sat.set_theory
    s
    (Some
       { Sat.on_assign
       ; on_backtrack
       ; check
       ; explain = (fun _ -> [])
       ; on_chrono_rewind = Some on_chrono_rewind
       });
  List.iter (fun cl -> Sat.add_clause s (List.map (lit_of_dimacs s) cl)) clauses;
  s
;;

(* Load-bearing RED mutant: OFF-BY-ONE over-keep. Same flat stream as [build_rewind_mock],
   but [on_chrono_rewind w] clears only stream positions [>= w+1] — it KEEPS the earliest-
   removed literal (stream position [w], the literal whose true level exceeded the target,
   i.e. the one the core actually removed from the trail). The core replays compacted
   positions [w..] regardless, so the kept position-[w] entry becomes STALE-TRUE: it names
   a var no longer on the real trail. When [check] then finds a constraint whose true side
   is that stale var, it returns a [T_conflict] citing a literal that is NOT currently
   true, and the core's premise-validity guard raises {!Sat.Theory_contract_violation} (or
   the stale view misses a real conflict and disagrees with the oracle / yields a bad
   model). Unlike frame-pop this cannot be watermark-coincidence-lucky: an
   absolute-position rewind that is off by one ALWAYS strands the earliest-removed
   literal, so the watermark must be EXACT. The [min] guard keeps the truncation in range
   for the [w = trail_n] corner. *)
let build_overkeep_mock num_vars clauses constraints =
  let s = Sat.create () in
  for _ = 1 to num_vars do
    ignore (Sat.new_var s : int)
  done;
  let asg = Array.make num_vars 0 in
  let stream = ref (Array.make 16 0) in
  let stream_n = ref 0 in
  let push v =
    if !stream_n >= Array.length !stream
    then (
      let a = Array.make (2 * Array.length !stream) 0 in
      Array.blit !stream 0 a 0 (Array.length !stream);
      stream := a);
    !stream.(!stream_n) <- v;
    incr stream_n
  in
  let on_assign l ~level:_ =
    let v = Sat.var_of_lit l in
    push v;
    asg.(v) <- (if Sat.sign_of_lit l then 1 else -1)
  in
  let on_backtrack ~level:_ = () in
  let on_chrono_rewind w =
    let keep = min (w + 1) !stream_n in
    for i = keep to !stream_n - 1 do
      asg.(!stream.(i)) <- 0
    done;
    stream_n := keep
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
  Sat.set_theory
    s
    (Some
       { Sat.on_assign
       ; on_backtrack
       ; check
       ; explain = (fun _ -> [])
       ; on_chrono_rewind = Some on_chrono_rewind
       });
  List.iter (fun cl -> Sat.add_clause s (List.map (lit_of_dimacs s) cl)) clauses;
  s
;;

(* Deep directed formula: wider clauses + more vars => deeper, more out-of-order CB trails
   (the regime where a scattered removal's earliest-removed sits below several higher
   frames, so frame-count and absolute-watermark undo diverge). Shared by the RED drivers
   below. *)
let gen_deep_directed () =
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
  num_vars, clauses
;;

let test_incr_undo_obs_eq n =
  let disagreements = ref 0 in
  let bad_models = ref 0 in
  let raises = ref 0 in
  for _ = 1 to n do
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
      let s = build_rewind_mock num_vars clauses constraints in
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
       "incr-undo obs-eq: %d formulas agree with augmented DPLL (%d)"
       n
       !disagreements)
    (!disagreements = 0);
  check
    (Printf.sprintf
       "incr-undo obs-eq: all sat models satisfy clauses+constraints (%d bad)"
       !bad_models)
    (!bad_models = 0);
  check
    (Printf.sprintf
       "incr-undo obs-eq: the incremental arm never emits a stale conflict (%d)"
       !raises)
    (!raises = 0)
;;

(* Drive a mutant [build] over [n] deep-directed formulas + their random constraints;
   return how many were CAUGHT (oracle disagreement, bad model, or a stale-conflict
   [Theory_contract_violation]). Shared by the boundary diagnostic and the load-bearing
   RED so both see the same (harder) formula distribution. *)
let run_mutant build n =
  let caught = ref 0 in
  for _ = 1 to n do
    let num_vars, clauses = gen_deep_directed () in
    let constraints = random_constraints num_vars in
    let augmented = clauses @ constraint_clauses constraints in
    let expected = Oracle.solve num_vars augmented in
    try
      let s = build num_vars clauses constraints in
      match Sat.solve s with
      | Sat.Sat ->
        if not expected
        then incr caught
        else if not (model_satisfies augmented (Sat.model s))
        then incr caught
      | Sat.Unsat -> if expected then incr caught
    with
    | Sat.Theory_contract_violation _ -> incr caught
  done;
  !caught
;;

(* Frame-pop boundary DIAGNOSTIC (not a gate). The faithful frames-vs-watermark mutant
   (pop whole decision-level frames instead of rewinding to the absolute watermark) is
   observationally EQUIVALENT to the correct rewind whenever the trail is shallow / has
   few out-of-order survivors — which is the common case even on deep directed 3-CNF — so
   it is frequently caught=0 and CANNOT be a hard gate without risking RED theater. We run
   it and PRINT the count to document where the equivalence boundary sits; the
   load-bearing discrimination is {!test_incr_undo_overkeep_red} below (an off-by-one
   absolute-position rewind, which can never be frame-coincidence-lucky). *)
let test_incr_undo_frame_pop_boundary n =
  let caught = run_mutant build_frame_pop_mock n in
  Printf.printf
    "  (incr-undo frame-pop boundary: caught=%d / %d deep-directed formulas — \
     frames==watermark on the rest; load-bearing RED is overkeep below)\n"
    caught
    n
;;

let test_incr_undo_overkeep_red n =
  let caught = run_mutant build_overkeep_mock n in
  (* RED: an off-by-one absolute-position rewind strands the earliest-removed literal as
     stale-true EVERY backtrack, so over a deep-directed run it MUST be caught (a stale
     [T_conflict] the core rejects, or an oracle disagreement). caught=0 would mean the
     watermark's exact position is not load-bearing in this harness — do NOT ship green;
     escalate (it would contradict the seam contract). *)
  check
    (Printf.sprintf
       "incr-undo overkeep-RED: off-by-one watermark mutant is caught (>=1 of %d; \
        caught=%d)"
       n
       caught)
    (caught > 0)
;;

(* F-core, RED-verified: [failed_assumptions] must be a SUBSET of the assumptions (frozen
   sat.mli contract). Under CB, [analyze_final]'s whole-trail walk must SKIP level-0
   literals; a level-0 UNIT is enqueued with reason [Decision] ([add_clause] / a learned
   unit), so without the skip the [Decision] arm appends it to the core, returning a
   SUPERSET.

   Trigger needs >= 2 assumptions (a single-assumption failure is at decision level 0, and
   [analyze_final] is guarded by [decision_level > 0], so it never walks): [pos a] forces
   [a] and [pos x] forces [x] (both level-0 units, reason [Decision]); assuming [pos a] is
   satisfied (a dummy level, raising the decision level to 1), then assuming [neg x]
   conflicts with the forced [x] and drives [analyze_final] at level 1. Buggy CB returns
   failed = [neg x; pos x] (the extra [pos x] is the level-0 unit wrongly added); fixed
   returns exactly [neg x] — a subset, and still a correct singleton core. *)
let test_failed_assumptions_subset () =
  let s = Sat.create () in
  let a = Sat.new_var s in
  let x = Sat.new_var s in
  Sat.add_clause s [ Sat.pos a ];
  Sat.add_clause s [ Sat.pos x ];
  let assumptions = [ Sat.pos a; Sat.neg x ] in
  let v = Sat.solve ~assumptions s in
  check "failed-assumptions: unsat under the conflicting assumption" (v = Sat.Unsat);
  let failed = Sat.failed_assumptions s in
  let key l = (Sat.var_of_lit l * 2) + if Sat.sign_of_lit l then 1 else 0 in
  let assumed = List.map key assumptions in
  let subset = List.for_all (fun l -> List.mem (key l) assumed) failed in
  check
    (Printf.sprintf
       "failed-assumptions: core is a SUBSET of the assumptions (|failed|=%d)"
       (List.length failed))
    subset;
  (* the core must still be non-empty and name the actually-failing assumption *)
  check
    "failed-assumptions: core names the conflicting assumption [neg x]"
    (List.exists (fun l -> key l = key (Sat.neg x)) failed)
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
  test_prop_seam 4000;
  test_true_level_delivery 4000;
  test_incr_undo_obs_eq 4000;
  test_incr_undo_frame_pop_boundary 4000;
  test_incr_undo_overkeep_red 4000;
  test_failed_assumptions_subset ();
  test_determinism 500;
  Printf.printf "chrono_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
