module Sat = Oxsmt_solver.Sat

(* OXSMT_LGC_FIXED reduceDB-schedule self-test. The lever swaps the reduceDB TRIGGER from
   the conflict-count schedule ([next_reduce] + [reduce_inc]) to z3's LGC_FIXED scheme —
   fire on the learned-clause COUNT crossing a threshold, grown x1.1. The deletion policy
   ([Search_heuristics.reduce_deletions]) and the arena rebuild+remap ([reduce_db]) are
   unchanged; only WHEN reduceDB fires changes.

   The gate ([OXSMT_LGC_FIXED]) is read once at [Sat.create]; this suite toggles it (and
   the [OXSMT_LGC_INITIAL] threshold) per solver via [Unix.putenv], so it can compare OFF
   and ON in one process.

   What it proves:
   1. SOUNDNESS under aggressive GC (RED-verified): with a TINY initial threshold,
      reduceDB (arena rebuild + cref remap) fires many times during a single solve — far
      more than the default schedule would on these small instances. Every verdict still
      matches an INDEPENDENT DPLL oracle, every reported model satisfies the formula, and
      every learned clause stays entailed. This is the composition check the card called
      for: the flat arena's rebuild/remap must stay sound when driven on the lemma-count
      trigger.
   2. LOAD-BEARING (RED-verified): on larger, high-LBD instances where GC genuinely
      deletes clauses, OFF and ON reach DIFFERENT counter trios (the schedule changed the
      search). A mutant that ignores the flag (ON == OFF) makes the trios identical and
      fails this. Both verdicts remain sound (SAT models valid); deleting learned clauses
      is satisfiability-preserving, so ON must never flip a verdict.

   Stdlib + Unix (test-only). Deterministic (fixed PRNG seed). Nonzero exit on any
   failure. Runs OFF and ON internally, so — unlike chrono_test — it does NOT require the
   gate to be preset in the environment. *)

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

(* Independent DPLL oracle — a naive definition-of-satisfiability search over DIMACS
   clauses (1-based ±v). Shares no code with the CDCL core; correct, not fast; used only
   on small formulas. Copied from chrono_test/sat_test (the shared oracle idiom). *)
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
  (* [Sat.create] reads the gate/threshold from the env set by {!set_lgc}. *)
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
       List.exists
         (fun l ->
            let b = model.(abs l - 1) in
            if l > 0 then b else not b)
         cl)
    clauses
;;

let dimacs_of_lit l =
  let v = Sat.var_of_lit l + 1 in
  if Sat.sign_of_lit l then v else -v
;;

(* A learned clause must be ENTAILED by the formula: F ∧ ¬L is UNSAT (checked with the
   independent oracle, never the solver under test). *)
let learned_entailed num_vars clauses learned_dimacs =
  let neg_units = List.map (fun l -> [ -l ]) learned_dimacs in
  not (Oracle.solve num_vars (List.rev_append neg_units clauses))
;;

(* Deterministic PRNG (xorshift64*, fixed seed — same family as sat_test/chrono_test). *)
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

(* Set (or clear) the reduceDB-schedule gate for the NEXT [Sat.create]. [on=false] clears
   the flag (empty string is falsy per [lgc_fixed_from_env]); [on=true] arms it with the
   given initial threshold. *)
let set_lgc ~on ~initial =
  if on
  then (
    Unix.putenv "OXSMT_LGC_FIXED" "1";
    Unix.putenv "OXSMT_LGC_INITIAL" (string_of_int initial))
  else Unix.putenv "OXSMT_LGC_FIXED" ""
;;

(* Solve one formula in the CURRENT env; return verdict, whether a reported model is
   valid, the learned clauses (DIMACS), and the counter trio. Trace is a pure side
   channel. *)
let run_one num_vars clauses =
  let s = build num_vars clauses in
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
  let v = Sat.solve s in
  let model_ok =
    match v with
    | Sat.Sat -> model_satisfies clauses (Sat.model s)
    | Sat.Unsat -> true
  in
  let st = Sat.stats s in
  v, model_ok, !learned, (st.conflicts, st.decisions, st.propagations)
;;

(* Small conflict-dense 3-CNF near the phase transition (ratio ~4.3): the oracle stays
   cheap (<= 14 vars) and the solve produces dozens of conflicts, so with a tiny threshold
   reduceDB fires repeatedly. *)
let gen_small () =
  let num_vars = 8 + rand_n 7 in
  let num_clauses = (num_vars * 4) + rand_n num_vars in
  let clause () =
    List.init 3 (fun _ ->
      let v = 1 + rand_n num_vars in
      if rand_n 2 = 0 then v else -v)
  in
  num_vars, List.init num_clauses (fun _ -> clause ())
;;

(* Larger conflict-dense 3-CNF (too big for the oracle) where learned clauses span many
   decision levels (high LBD), so reduceDB genuinely DELETES rather than protecting
   everything as glue — the regime where the schedule change actually moves the search. *)
let gen_large () =
  let num_vars = 40 + rand_n 14 in
  let num_clauses = (num_vars * 45 / 10) + rand_n num_vars in
  let clause () =
    List.init 3 (fun _ ->
      let v = 1 + rand_n num_vars in
      if rand_n 2 = 0 then v else -v)
  in
  num_vars, List.init num_clauses (fun _ -> clause ())
;;

(* (1) SOUNDNESS under aggressive GC: flag ON with a tiny threshold, small
       oracle-checkable instances. Confirms reduceDB had real opportunity (total learned
       >> threshold) and that the scheduled arena rebuild/remap never corrupts a verdict,
       model, or learned clause. *)
let test_soundness_under_gc n ~initial =
  set_lgc ~on:true ~initial;
  let disagreements = ref 0 in
  let bad_models = ref 0 in
  let unentailed = ref 0 in
  let total_learned = ref 0 in
  for _ = 1 to n do
    let num_vars, clauses = gen_small () in
    let expected = Oracle.solve num_vars clauses in
    let v, model_ok, learned, _ = run_one num_vars clauses in
    (match v with
     | Sat.Sat -> if not expected then incr disagreements
     | Sat.Unsat -> if expected then incr disagreements);
    if not model_ok then incr bad_models;
    List.iter
      (fun l ->
         incr total_learned;
         if not (learned_entailed num_vars clauses l) then incr unentailed)
      learned
  done;
  check
    (Printf.sprintf
       "soundness-under-gc: %d formulas agree with DPLL oracle (%d disagreements)"
       n
       !disagreements)
    (!disagreements = 0);
  check
    (Printf.sprintf "soundness-under-gc: all sat models valid (%d bad)" !bad_models)
    (!bad_models = 0);
  check
    (Printf.sprintf
       "soundness-under-gc: all %d learned clauses entailed (%d unentailed)"
       !total_learned
       !unentailed)
    (!unentailed = 0);
  (* Confirm reduceDB actually had work: far more clauses were learned than the tiny
     threshold, so the lemma-count trigger fired many times. *)
  check
    (Printf.sprintf
       "soundness-under-gc: reduceDB was exercised (%d learned >> threshold %d)"
       !total_learned
       initial)
    (!total_learned > initial * 10);
  Printf.printf
    "  (soundness-under-gc: %d formulas, %d learned entailment-checked, threshold %d)\n"
    n
    !total_learned
    initial
;;

(* (2) LOAD-BEARING: OFF vs ON on the SAME larger instances. Both must be sound (SAT
   models valid, and OFF vs ON verdicts must agree — GC is satisfiability-preserving); and
   the counter trios must DIFFER on a clear majority, proving the flag changes the search.
   A flag-ignoring mutant collapses ON onto OFF, driving [differ] to 0 and failing here. *)
let test_load_bearing n ~initial =
  let differ = ref 0 in
  let verdict_flips = ref 0 in
  let bad_models = ref 0 in
  for _ = 1 to n do
    let num_vars, clauses = gen_large () in
    set_lgc ~on:false ~initial:0;
    let voff, ok_off, _, coff = run_one num_vars clauses in
    set_lgc ~on:true ~initial;
    let von, ok_on, _, con = run_one num_vars clauses in
    if not (voff = von) then incr verdict_flips;
    if (not ok_off) || not ok_on then incr bad_models;
    if not (coff = con) then incr differ
  done;
  check
    (Printf.sprintf
       "load-bearing: OFF and ON never flip a verdict (%d flips — GC is \
        satisfiability-preserving)"
       !verdict_flips)
    (!verdict_flips = 0);
  check
    (Printf.sprintf
       "load-bearing: every sat model valid in both modes (%d bad)"
       !bad_models)
    (!bad_models = 0);
  (* The discriminating assertion. A flag-ignoring mutant makes ON == OFF, driving
     [differ] to 0; the real schedule change moves the search on every instance that
     produces enough learned clauses to cross the tiny ON threshold before solving. The
     bar is [n/4] (not a majority): near the phase transition roughly half the instances
     are SAT and are found in fewer conflicts than the threshold, so they never fire GC
     and legitimately match — [n/4] is a robust floor that a no-op mutant (0) cannot
     clear. *)
  check
    (Printf.sprintf
       "load-bearing: the schedule is load-bearing — OFF and ON differ on %d/%d instances"
       !differ
       n)
    (!differ > n / 4);
  Printf.printf "  (load-bearing: %d/%d instances differ OFF vs ON)\n" !differ n
;;

let () =
  Printf.printf "lgc_test: OXSMT_LGC_FIXED reduceDB-schedule self-test\n";
  test_soundness_under_gc 4000 ~initial:3;
  test_load_bearing 250 ~initial:12;
  Printf.printf "lgc_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
