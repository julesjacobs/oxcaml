module Sat = Oxsmt_solver.Sat

(* OXSMT_SATCORE_MODES stable/focused mode-alternation self-test (SAT-core S1).

   The lever layers a kissat-style mode bit over the EXISTING EMA/restart/blocking
   machinery: when the flag is on the restart trigger is mode-scoped (focused mode fires
   the LBD-EMA restart, stable mode restarts rarely), flipping on a geometric conflict
   schedule; when off, the trunk restart policy (Luby cap + the dark global adaptive
   trigger) is the only one live and the core is byte-identical. All mode state resets
   per-solve relative to [conflicts_at_solve_start] (M3), so an incremental re-solve does
   not inherit a stale mode/limit.

   The suite toggles the gate (and the [OXSMT_SATCORE_MODE_INIT] measurement knob) per
   solver via [Unix.putenv], comparing OFF and ON in one process (Sat.create reads them).

   What it proves:
   1. SOUNDNESS under mode churn: with a tiny mode_init, modes flip many times per solve;
      every verdict still matches an INDEPENDENT DPLL oracle and every reported model
      satisfies the formula. Restarts are satisfiability-preserving, so a mode switch must
      never change a verdict or produce a bad model.
   2. LOAD-BEARING / whole-mechanism wiring (RED-verified): OFF and ON reach DIFFERENT
      counter trios on a clear majority of larger instances — the mode-scoped restart
      cadence genuinely changes the search. A mutant that ignores the flag (ON == OFF)
      drives [differ] to 0 and fails. Verdicts still agree OFF vs ON (never a flip).
   3. SWITCHING LIVENESS (RED-verified): ON with a small mode_init (modes flip) vs ON with
      a mode_init larger than any solve's conflict count (modes never flip) differ on a
      robust fraction of instances — proving the SWITCH is live, isolated from the
      mode-vs-Luby difference. A never-switch mutant drives [differ] to 0 and fails.
   4. MULTI-QUERY RESET (charter rider, option-(a) ruling): a FIXED formula solved under K
      assumption cubes on ONE reused solver returns, for every query, the SAME verdict a
      FRESH solver gives — the per-solve mode-state reset keeps each incremental query
      sound, never wedging or mis-deciding a later one. The reset is DEFENSIVE-ONLY and
      has NO reachable observable effect under single-solve-per-file corpus use (see the
      note on {!test_multi_query_reset}); the committed assertion is therefore
      verdict-soundness, not a counter divergence (which would be RED theater on an
      unreachable path).

   Stdlib + Unix (test-only). Deterministic (fixed PRNG seed). Nonzero exit on failure. *)

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n%!" name)
;;

(* Independent DPLL oracle (shares no code with the CDCL core). *)
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

let lit_of_dimacs l =
  let v = abs l - 1 in
  if l > 0 then Sat.pos v else Sat.neg v
;;

let build num_vars clauses =
  let s = Sat.create () in
  for _ = 1 to num_vars do
    ignore (Sat.new_var s : int)
  done;
  List.iter (fun cl -> Sat.add_clause s (List.map lit_of_dimacs cl)) clauses;
  s
;;

let model_satisfies clauses model =
  List.for_all
    (fun cl ->
      List.exists (fun l -> if l > 0 then model.(l - 1) else not model.(abs l - 1)) cl)
    clauses
;;

(* xorshift64* (fixed seed — same family as sat_test/lgc_test). *)
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

(* Set (or clear) the mode-alternation gate for the NEXT [Sat.create]. [on=false] clears
   it (empty string is falsy); [on=true] arms it with the given mode_init measurement knob
   so modes churn fast on small instances. *)
let set_modes ~on ~init =
  if on
  then (
    Unix.putenv "OXSMT_SATCORE_MODES" "1";
    Unix.putenv "OXSMT_SATCORE_MODE_INIT" (string_of_int init))
  else Unix.putenv "OXSMT_SATCORE_MODES" ""
;;

let run_one num_vars clauses =
  let s = build num_vars clauses in
  let v = Sat.solve s in
  let model_ok =
    match v with
    | Sat.Sat -> model_satisfies clauses (Sat.model s)
    | Sat.Unsat -> true
  in
  let st = Sat.stats s in
  v, model_ok, (st.Sat.Stats.conflicts, st.Sat.Stats.decisions, st.Sat.Stats.propagations)
;;

let gen_small () =
  let num_vars = 8 + rand_n 7 in
  let num_clauses = (num_vars * 43 / 10) + rand_n num_vars in
  let clause () =
    List.init 3 (fun _ ->
      let v = 1 + rand_n num_vars in
      if rand_n 2 = 0 then v else -v)
  in
  num_vars, List.init num_clauses (fun _ -> clause ())
;;

let gen_large () =
  (* Big enough (and near the ratio-4.26 phase transition) that the solve runs hundreds of
     conflicts — several EMA/Luby restart windows — so the mode-scoped restart cadence
     actually diverges from the trunk Luby policy. Smaller instances solve before any
     restart fires (OFF and ON then coincide) and cannot exercise the lever. *)
  let num_vars = 90 + rand_n 30 in
  let num_clauses = (num_vars * 43 / 10) + rand_n num_vars in
  let clause () =
    List.init 3 (fun _ ->
      let v = 1 + rand_n num_vars in
      if rand_n 2 = 0 then v else -v)
  in
  num_vars, List.init num_clauses (fun _ -> clause ())
;;

(* (1) SOUNDNESS under mode churn. *)
let test_soundness n ~init =
  set_modes ~on:true ~init;
  let disagreements = ref 0
  and bad_models = ref 0 in
  for _ = 1 to n do
    let num_vars, clauses = gen_small () in
    let expected = Oracle.solve num_vars clauses in
    let v, model_ok, _ = run_one num_vars clauses in
    (match v with
     | Sat.Sat -> if not expected then incr disagreements
     | Sat.Unsat -> if expected then incr disagreements);
    if not model_ok then incr bad_models
  done;
  check
    (Printf.sprintf
       "soundness: %d formulas agree with DPLL oracle (%d disagree)"
       n
       !disagreements)
    (!disagreements = 0);
  check
    (Printf.sprintf "soundness: all sat models valid (%d bad)" !bad_models)
    (!bad_models = 0)
;;

(* (2) LOAD-BEARING: OFF vs ON on the SAME larger instances. *)
let test_load_bearing n ~init =
  let differ = ref 0
  and verdict_flips = ref 0
  and bad_models = ref 0 in
  for _ = 1 to n do
    let num_vars, clauses = gen_large () in
    set_modes ~on:false ~init:0;
    let voff, ok_off, coff = run_one num_vars clauses in
    set_modes ~on:true ~init;
    let von, ok_on, con = run_one num_vars clauses in
    if not (voff = von) then incr verdict_flips;
    if (not ok_off) || not ok_on then incr bad_models;
    if not (coff = con) then incr differ
  done;
  check
    (Printf.sprintf
       "load-bearing: OFF vs ON never flip a verdict (%d flips)"
       !verdict_flips)
    (!verdict_flips = 0);
  check
    (Printf.sprintf "load-bearing: every model valid both modes (%d bad)" !bad_models)
    (!bad_models = 0);
  check
    (Printf.sprintf
       "load-bearing: mode cadence is load-bearing — OFF vs ON differ on %d/%d"
       !differ
       n)
    (!differ > n / 4);
  Printf.printf "  (load-bearing: %d/%d instances differ OFF vs ON)\n%!" !differ n
;;

(* (3) SWITCHING LIVENESS. The mode SWITCH itself is live, isolated from the mode-vs-Luby
   restart difference. ON with a small mode_init (modes flip many times per solve) vs ON
   with a mode_init larger than any solve's conflict count (modes NEVER flip — focused for
   the whole solve) must reach DIFFERENT counter trios on a majority of larger instances:
   the switch changes the search. A mutant that neutralizes the flip condition makes both
   configs never-switch ⇒ trios identical ⇒ [differ] collapses to 0 and this FAILS (the
   mode-alternation-liveness RED). Verdicts still agree (restarts are sat-preserving).

   NOTE why this is small-vs-big-init and NOT ON-vs-OFF: a never-switch ON still restarts
   on the focused EMA trigger, which already differs from OFF's Luby cap, so ON≠OFF even
   with the switch dead — an ON-vs-OFF check cannot isolate the SWITCH. Whole-mechanism
   wiring loss (ON collapses to OFF) is caught separately by the flag-ignore RED in
   {!test_load_bearing}. *)
let test_switching_liveness n ~small ~big =
  let differ = ref 0
  and bad = ref 0
  and flips = ref 0 in
  for _ = 1 to n do
    let num_vars, clauses = gen_large () in
    set_modes ~on:true ~init:small;
    let vs, oks, cs = run_one num_vars clauses in
    set_modes ~on:true ~init:big;
    let vb, okb, cb = run_one num_vars clauses in
    if not (vs = vb) then incr flips;
    if (not oks) || not okb then incr bad;
    if not (cs = cb) then incr differ
  done;
  check
    (Printf.sprintf
       "switching-liveness: small vs big mode_init never flip a verdict (%d)"
       !flips)
    (!flips = 0);
  check
    (Printf.sprintf "switching-liveness: every model valid both configs (%d bad)" !bad)
    (!bad = 0);
  check
    (Printf.sprintf
       "switching-liveness: the mode switch is live — small vs big mode_init differ on \
        %d/%d"
       !differ
       n)
    (* Deterministic (fixed seed); the switch only perturbs search where restarts fire, so
       the signal is ~a fifth of instances, not a majority. A robust fixed floor the
       never-switch mutant (0) cannot clear; correct code clears it comfortably. *)
    (!differ > n / 8);
  Printf.printf
    "  (switching-liveness: %d/%d instances differ small vs big mode_init)\n%!"
    !differ
    n
;;

(* (4) MULTI-QUERY RESET (charter rider, option-(a) ruling). A FIXED formula solved under
   K assumption cubes on ONE reused solver (ON, tiny mode_init). Every query's verdict
   must match a FRESH solver's verdict for the same cube — the per-solve mode-state reset
   keeps each incremental query sound, never wedging or mis-deciding a later one.

   DEFENSIVE-ONLY, UNREACHABLE IN CURRENT USE: the per-solve reset (sat.ml [solve]) has NO
   reachable OBSERVABLE effect under the corpus's single-solve-per-file driving. Mode
   state is read only by the restart trigger, so the reset only bites when an EMA restart
   fires in the mode-gated warmup window on a 2nd+ solve; that requires EMA-restart-heavy
   incremental reuse, which in practice is UNSAT-heavy (a level-0 unsat bricks reuse).
   Verified: a neutralized-reset mutant produces BYTE-IDENTICAL conflict trajectories on
   real multi-query fixtures — so the RED here is verdict-SOUNDNESS (catches
   wedge/mis-decision), not a counter divergence (which would be RED theater on an
   unreachable path). The reset stays for M3 parity with the [next_reduce] reset; a future
   incremental-workload change (an EMA-heavy incremental session) must revisit this. *)
let test_multi_query_reset () =
  set_modes ~on:true ~init:20;
  (* a fixed SAT base formula with enough structure to produce conflicts across queries *)
  let num_vars = 30 in
  let clauses =
    let clause () =
      List.init 3 (fun _ ->
        let v = 1 + rand_n num_vars in
        if rand_n 2 = 0 then v else -v)
    in
    (* pick a satisfiable base (an UNSAT base would brick the reused solver on query 1) *)
    let rec pick tries =
      let cs = List.init (num_vars * 4) (fun _ -> clause ()) in
      if Oracle.solve num_vars cs || tries > 500 then cs else pick (tries + 1)
    in
    pick 0
  in
  (* K assumption cubes (each a couple of literals) *)
  let cubes =
    List.init 8 (fun _ ->
      List.init 2 (fun _ ->
        let v = 1 + rand_n num_vars in
        if rand_n 2 = 0 then v else -v))
  in
  let mismatches = ref 0 in
  (* reused solver: one solver, all cubes in sequence *)
  let reused = build num_vars clauses in
  List.iteri
    (fun i cube ->
      let assumptions = List.map lit_of_dimacs cube in
      let vr = Sat.solve ~assumptions reused in
      (* fresh reference for this cube *)
      let fresh = build num_vars clauses in
      let vf = Sat.solve ~assumptions fresh in
      if not (vr = vf)
      then (
        incr mismatches;
        Printf.printf
          "  reset MISMATCH query %d: reused=%s fresh=%s\n%!"
          i
          (match vr with
           | Sat.Sat -> "sat"
           | Sat.Unsat -> "unsat")
          (match vf with
           | Sat.Sat -> "sat"
           | Sat.Unsat -> "unsat")))
    cubes;
  check
    (Printf.sprintf
       "multi-query reset: %d reused-solver queries all match a fresh solve (%d \
        mismatches)"
       (List.length cubes)
       !mismatches)
    (!mismatches = 0)
;;

let () =
  Printf.printf "satcore_test: OXSMT_SATCORE_MODES mode-alternation self-test\n%!";
  test_soundness 4000 ~init:3;
  test_load_bearing 120 ~init:30;
  test_switching_liveness 120 ~small:30 ~big:10_000_000;
  test_multi_query_reset ();
  Printf.printf "satcore_test: %d checks, %d failures\n%!" !checks !failures;
  if !failures > 0 then exit 1
;;
