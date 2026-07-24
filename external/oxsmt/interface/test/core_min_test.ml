(* Core-minimization property test + benchmark (task #36).

   [Session.check_sat_assuming]'s documented guarantee is that [unsat_core] is a
   subset-minimal, duplicate-free subset of the assumptions in input order. This upgrade
   replaces the linear one-probe-per-member deletion loop with z3-style CLAUSE-SET
   REFINEMENT (mus.cpp:80 [get_mus1]): an Unsat deletion probe's own failed-assumption
   core becomes the new working set, dropping many redundant members per probe. This file
   proves the upgrade is a COST change, not a semantics change, and measures the win.

   Gadget (the [assuming-bool-min] session-cores test generalized from one decoy to [r]):
   one wide clause [c_1; ...; c_r; a; b] plus the four x/y clauses that already make
   [{~a, ~b}] unsatisfiable. Assuming every [~c_i], [~a], [~b], the wide clause is
   falsified immediately (all literals false), so the SAT core's first conflict names ALL
   of [{~c_1..~c_r, ~a, ~b}] — a loose over-approximation, because [{~a, ~b}] alone is
   unsat via the x/y clauses. The unique minimal core is therefore [{~a, ~b}]: every
   [~c_i] is redundant. Linear deletion pays one re-solve per decoy; refinement drops the
   whole decoy block in the single probe that first removes one, then confirms [~a]/[~b].

   Checks:
   1. SUBSET-MINIMALITY (randomized): over shuffled assumption orders and several decoy
      counts, the returned core re-solves Unsat and every one-literal deletion is Sat, and
      it is exactly [{~a, ~b}]. This is the documented guarantee, verified independently.
   2. EQUIVALENCE: the linear baseline ([OXSMT_CORE_MIN_LINEAR=1]) and refinement return
      the SAME minimal class (both pass check 1); refinement never spends MORE probes and,
      once there are >= 2 decoys, spends strictly FEWER.
   3. DISCRIMINATION: the minimality checker REJECTS a deliberately non-minimal core (the
      full assumption set) — deleting a decoy leaves it Unsat — so a broken minimizer that
      left decoys in (or dropped an essential) would fail check 1. *)

open Oxsmt_core
module Session = Oxsmt_interface.Session

let checks = ref 0
let failures = ref 0

let fail name msg =
  incr failures;
  Printf.printf "  FAIL %s: %s\n" name msg
;;

let ok _name = incr checks
let check_true name b = if b then ok name else fail name "expected true"
let assumption_equal (a, ap) (b, bp) = ap = bp && Term.equal a b
let assumption_mem x xs = List.exists (assumption_equal x) xs
let remove_assumption x xs = List.filter (fun y -> not (assumption_equal x y)) xs

(* Deterministic Fisher-Yates so a failure reproduces from its seed. *)
let shuffle seed xs =
  let a = Array.of_list xs in
  let st = Random.State.make [| seed |] in
  for i = Array.length a - 1 downto 1 do
    let j = Random.State.int st (i + 1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done;
  Array.to_list a
;;

(* Build the r-decoy gadget on a fresh session; return (essential MUS, redundant decoys). *)
let build_gadget s ~r ~tag =
  let ctx = Session.context s in
  let bv name = Context.const ctx (Session.declare_const s name Sort.bool) in
  let neg t = Context.not_ ctx t in
  let clause ts = Session.assert_term s (Context.or_ ctx ts) in
  let a = bv (tag ^ "_a") in
  let b = bv (tag ^ "_b") in
  let x = bv (tag ^ "_x") in
  let y = bv (tag ^ "_y") in
  let decoys = List.init r (fun i -> bv (Printf.sprintf "%s_c%d" tag i)) in
  clause (decoys @ [ a; b ]);
  clause [ a; b; x; y ];
  clause [ a; b; x; neg y ];
  clause [ a; b; neg x; y ];
  clause [ a; b; neg x; neg y ];
  let mus = [ a, false; b, false ] in
  let noise = List.map (fun c -> c, false) decoys in
  mus, noise
;;

(* [is_minimal_core session core] is the documented guarantee, checked independently: the
   core re-solves Unsat and every one-literal deletion re-solves Sat. Returns [false] (not
   an exception) on a violation so it can double as the discrimination oracle. *)
let is_minimal_core session core =
  let replay = Session.check_sat_assuming session core in
  replay.Session.verdict = Session.Unsat
  && List.for_all
       (fun lit ->
         let probe = Session.check_sat_assuming session (remove_assumption lit core) in
         probe.Session.verdict = Session.Sat)
       core
;;

let set_linear on = Unix.putenv "OXSMT_CORE_MIN_LINEAR" (if on then "1" else "0")

(* The property suite proves the full core contract and therefore runs with optional
   effort limiting disabled. The dedicated section below exercises the production cap. *)
let () = Unix.putenv "OXSMT_CORE_MIN_EFFORT_CAP" "0"

(* ------------------------------------------------------------------ 1 + 2 + 3 --------- *)

let property_case ~r ~seed =
  let name = Printf.sprintf "r=%d/seed=%d" r seed in
  let tag = Printf.sprintf "g_%d_%d" r seed in
  let cores =
    List.map
      (fun linear ->
        set_linear linear;
        let s = Session.create () in
        let mus, noise = build_gadget s ~r ~tag:(tag ^ if linear then "L" else "R") in
        let assumptions = shuffle seed (mus @ noise) in
        let result = Session.check_sat_assuming s assumptions in
        (* Capture the probe count BEFORE the minimality replays below, which re-enter
           [check_sat_assuming] on [s] and reset its counter. *)
        let probes = Session.minimize_probes s in
        (match result.Session.verdict with
         | Session.Unsat -> ok (name ^ ": unsat")
         | _ -> fail name "check_sat_assuming did not return Unsat");
        (match result.Session.unsat_core with
         | None -> fail name "unsat_core = None"
         | Some core ->
           (* the returned core is exactly the planted MUS {~a,~b} ... *)
           check_true
             (name ^ ": core = MUS")
             (List.length core = List.length mus
              && List.for_all (fun m -> assumption_mem m core) mus);
           (* ... and independently subset-minimal. Verify on the SAME session [s]: the
              core's atoms live in [s]'s context, so re-solving them elsewhere is invalid.
              Assumptions never persist, so these replays do not corrupt later checks. *)
           check_true (name ^ ": subset-minimal") (is_minimal_core s core));
        result.Session.unsat_core, probes, mus, noise)
      [ false (* refinement *); true (* linear *) ]
  in
  match cores with
  | [ (Some refine_core, refine_probes, mus, noise)
    ; (Some linear_core, linear_probes, _, _)
    ] ->
    (* EQUIVALENCE: both strategies land on the same minimal class. *)
    check_true
      (name ^ ": strategies agree")
      (List.length refine_core = List.length linear_core
       && List.for_all (fun m -> assumption_mem m refine_core) mus
       && List.for_all (fun m -> assumption_mem m linear_core) mus);
    (* COST: refinement never spends more; with >= 2 decoys it spends strictly fewer. *)
    check_true
      (Printf.sprintf
         "%s: refine probes (%d) <= linear (%d)"
         name
         refine_probes
         linear_probes)
      (refine_probes <= linear_probes);
    if List.length noise >= 2
    then
      check_true
        (Printf.sprintf
           "%s: refine probes (%d) < linear (%d)"
           name
           refine_probes
           linear_probes)
        (refine_probes < linear_probes);
    (* DISCRIMINATION: the checker must REJECT the non-minimal full set (deleting a decoy
       stays Unsat), so a minimizer that left decoys in would fail the minimality check. *)
    if List.length noise >= 1
    then (
      let disc = Session.create () in
      let dmus, dnoise = build_gadget disc ~r ~tag:(tag ^ "D") in
      check_true
        (name ^ ": checker rejects non-minimal full set")
        (not (is_minimal_core disc (dmus @ dnoise))))
  | _ -> fail name "a strategy returned None"
;;

let () =
  List.iter
    (fun r -> List.iter (fun seed -> property_case ~r ~seed) [ 1; 2; 3; 7; 42 ])
    [ 0; 1; 2; 5; 12 ]
;;

(* ------------------------------------------ verdict-first degradation (bugreport 02)
   ---- *)

(* The three degradation arms are unreachable naturally — the replay re-certifies a core
   the minimizer just derived — so drive them with the test-only fault-injection hooks.
   Each asserts the FLIPPED contract: the verdict stays [Unsat] (old code returned
   [Unknown] here), [unsat_core] drops to [None], and [last_unknown_reason] carries the
   diagnostic tag. The control below shows the same query returns a verified core with no
   injection, so the injection is exactly what exercises the arm. *)
let degradation_case ~name ~arm ~expected_reason =
  set_linear false;
  let s = Session.create () in
  let mus, noise = build_gadget s ~r:1 ~tag:name in
  (match arm with
   | `Deletion v -> Session.inject_deletion_verdict_for_test (Some v)
   | `Replay v -> Session.inject_replay_verdict_for_test (Some v));
  let result = Session.check_sat_assuming s (mus @ noise) in
  (match result.Session.verdict with
   | Session.Unsat -> ok (name ^ ": verdict stays Unsat (old: Unknown)")
   | _ -> fail name "verdict downgraded — verdict-first requires Unsat");
  check_true (name ^ ": unsat_core = None") (result.Session.unsat_core = None);
  check_true
    (name ^ ": diagnostic reason")
    (String.equal (Session.last_unknown_reason s) expected_reason);
  (* defensive disarm so a not-fired injection cannot leak into a later query *)
  Session.inject_deletion_verdict_for_test None;
  Session.inject_replay_verdict_for_test None
;;

let () =
  set_linear false;
  (* control: no injection ⇒ Unsat WITH a verified 2-literal core and no degradation tag. *)
  let s = Session.create () in
  let mus, noise = build_gadget s ~r:1 ~tag:"ctrl" in
  let result = Session.check_sat_assuming s (mus @ noise) in
  check_true "control: verdict Unsat" (result.Session.verdict = Session.Unsat);
  check_true
    "control: core = Some MUS"
    (match result.Session.unsat_core with
     | Some core -> List.length core = List.length mus
     | None -> false);
  check_true
    "control: no degradation reason"
    (String.equal (Session.last_unknown_reason s) "");
  degradation_case
    ~name:"replay-sat"
    ~arm:(`Replay Session.Sat)
    ~expected_reason:"assumption-core-recheck-sat";
  degradation_case
    ~name:"replay-unknown"
    ~arm:(`Replay Session.Unknown)
    ~expected_reason:"assumption-core-recheck-unknown";
  degradation_case
    ~name:"deletion-unknown"
    ~arm:(`Deletion Session.Unknown)
    ~expected_reason:"assumption-core-minimize-unknown"
;;

(* ------------------------------------------ sufficiency + empty-core semantics (bug 01)
   -- *)

let bool_atom s name =
  Context.const (Session.context s) (Session.declare_const s name Sort.bool)
;;

let () =
  (* Unsat with an empty core occurs ONLY when the active assertions alone are unsat:
     assert a base contradiction, query under an unrelated assumption, and confirm the
     empty core is itself sufficient (actives alone re-solve Unsat). *)
  let name = "empty-core-actives-unsat" in
  let s = Session.create () in
  let ctx = Session.context s in
  let p = bool_atom s "ec_p" in
  Session.assert_term s p;
  Session.assert_term s (Context.not_ ctx p);
  let q = bool_atom s "ec_q" in
  let result = Session.check_sat_assuming s [ q, true ] in
  check_true (name ^ ": verdict Unsat") (result.Session.verdict = Session.Unsat);
  check_true (name ^ ": empty core") (result.Session.unsat_core = Some []);
  check_true
    (name ^ ": empty core is sufficient (actives alone unsat)")
    ((Session.check_sat_assuming s []).Session.verdict = Session.Unsat)
;;

let () =
  (* Sufficiency on a nonempty core: (actives ∧ returned core) re-solves Unsat and every
     one-literal deletion is Sat — the replay-verified guarantee, checked on the producing
     session. *)
  let name = "sufficiency-nonempty" in
  set_linear false;
  let s = Session.create () in
  let mus, noise = build_gadget s ~r:3 ~tag:"suff" in
  let result = Session.check_sat_assuming s (mus @ noise) in
  match result.Session.unsat_core with
  | None -> fail name "expected Some core"
  | Some core ->
    check_true (name ^ ": nonempty") (core <> []);
    check_true (name ^ ": sufficient + subset-minimal") (is_minimal_core s core)
;;

(* ------------------------------------------ per-deletion effort cap ------------------ *)

let set_core_min_effort_cap on =
  Unix.putenv "OXSMT_CORE_MIN_EFFORT_CAP" (if on then "1" else "0")
;;

let () =
  let saved = Sys.getenv_opt "OXSMT_CORE_MIN_EFFORT_CAP" in
  let saved_limit = Sys.getenv_opt "OXSMT_CORE_MIN_INITIAL_EFFORT_LIMIT" in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "OXSMT_CORE_MIN_EFFORT_CAP" (Option.value ~default:"" saved);
      Unix.putenv
        "OXSMT_CORE_MIN_INITIAL_EFFORT_LIMIT"
        (Option.value ~default:"" saved_limit))
    (fun () ->
      (* Easy probes remain below max(32, initial-effort), so the ordinary verified,
         subset-minimal core is still returned. *)
      set_core_min_effort_cap true;
      let easy = Session.create () in
      let mus, noise = build_gadget easy ~r:3 ~tag:"effcap_easy" in
      let easy_result = Session.check_sat_assuming easy (mus @ noise) in
      check_true "effort-cap/easy: verdict Unsat" (easy_result.verdict = Session.Unsat);
      (match easy_result.unsat_core with
       | None -> fail "effort-cap/easy" "expected verified core"
       | Some core ->
         check_true "effort-cap/easy: core = MUS" (List.length core = 2);
         check_true "effort-cap/easy: subset-minimal" (is_minimal_core easy core));

      (* An initial proof over the configured threshold skips optional minimization
         without launching a deletion probe. The public Unsat verdict is already proved;
         only the unavailable core is dropped. A fresh easy query under the normal
         threshold below still returns its core, pinning that the skip is per-call. *)
      Unix.putenv "OXSMT_CORE_MIN_INITIAL_EFFORT_LIMIT" "0";
      let skipped = Session.create () in
      let skipped_mus, skipped_noise = build_gadget skipped ~r:3 ~tag:"effcap_skip" in
      let skipped_result =
        Session.check_sat_assuming skipped (skipped_mus @ skipped_noise)
      in
      check_true "effort-cap/skip: verdict stays Unsat" (skipped_result.verdict = Session.Unsat);
      check_true "effort-cap/skip: core is None" (skipped_result.unsat_core = None);
      check_true "effort-cap/skip: only initial solve ran" (Session.minimize_probes skipped = 1);
      check_true
        "effort-cap/skip: diagnostic"
        (String.equal
           (Session.last_unknown_reason skipped)
           "assumption-core-initial-effort-skip");
      Unix.putenv "OXSMT_CORE_MIN_INITIAL_EFFORT_LIMIT" "9";
      let after_skip = Session.create () in
      let after_mus, after_noise = build_gadget after_skip ~r:3 ~tag:"effcap_after_skip" in
      let after_result = Session.check_sat_assuming after_skip (after_mus @ after_noise) in
      check_true "effort-cap/skip-restore: verdict Unsat" (after_result.verdict = Session.Unsat);
      check_true
        "effort-cap/skip-restore: verified core"
        (Option.is_some after_result.unsat_core);

      (* The assumption [a] contradicts a root fact, so the initial Unsat costs almost
         nothing. Deleting [a] exposes 160 independent satisfiable clauses and exceeds the
         floor cap of 32. Only core extraction degrades: the established verdict remains
         Unsat. Disabling the cap and repeating the SAME query must then return its
         verified singleton core, proving the temporary Budget cap did not leak. *)
      let hard = Session.create () in
      let ctx = Session.context hard in
      let a = bool_atom hard "effcap_hard_a" in
      Session.assert_term hard (Context.not_ ctx a);
      for i = 0 to 159 do
        let x = bool_atom hard (Printf.sprintf "effcap_hard_x_%d" i) in
        let y = bool_atom hard (Printf.sprintf "effcap_hard_y_%d" i) in
        Session.assert_term hard (Context.or_ ctx [ x; y ])
      done;
      let first = Session.check_sat_assuming hard [ a, true ] in
      check_true "effort-cap/hard: verdict stays Unsat" (first.verdict = Session.Unsat);
      check_true "effort-cap/hard: core degrades to None" (first.unsat_core = None);
      check_true
        "effort-cap/hard: minimize diagnostic"
        (String.equal
           (Session.last_unknown_reason hard)
           "assumption-core-minimize-unknown");
      check_true "effort-cap/hard: cap fired" (Session.effort_exhausted hard);
      set_core_min_effort_cap false;
      let repeated = Session.check_sat_assuming hard [ a, true ] in
      check_true "effort-cap/restore: repeated verdict Unsat" (repeated.verdict = Session.Unsat);
      check_true
        "effort-cap/restore: repeated core available"
        (match repeated.unsat_core with
         | Some [ (atom, true) ] -> Term.equal atom a
         | Some _ | None -> false);
      check_true "effort-cap/restore: no exhaustion" (not (Session.effort_exhausted hard));

      (* A user-configured cap remains authoritative when the derived cap is larger.
         With initial effort near zero the derived floor is 32, and max_effort=16 must
         stop the deletion probe at tick 17. Turning the core cap off and repeating the
         query must still stop at 17, covering restoration of a strictly tighter existing
         [Some] cap rather than accidentally reusing the temporary derived cap. *)
      set_core_min_effort_cap true;
      let user_capped = Session.create ~max_effort:16 () in
      let user_ctx = Session.context user_capped in
      let ua = bool_atom user_capped "effcap_user_a" in
      Session.assert_term user_capped (Context.not_ user_ctx ua);
      for i = 0 to 159 do
        let x = bool_atom user_capped (Printf.sprintf "effcap_user_x_%d" i) in
        let y = bool_atom user_capped (Printf.sprintf "effcap_user_y_%d" i) in
        Session.assert_term user_capped (Context.or_ user_ctx [ x; y ])
      done;
      let capped = Session.check_sat_assuming user_capped [ ua, true ] in
      check_true "effort-cap/user: verdict stays Unsat" (capped.verdict = Session.Unsat);
      check_true "effort-cap/user: core degrades to None" (capped.unsat_core = None);
      check_true "effort-cap/user: configured cap is tighter" (Session.effort user_capped = 17);
      set_core_min_effort_cap false;
      let user_repeat = Session.check_sat_assuming user_capped [ ua, true ] in
      check_true
        "effort-cap/user-restore: repeated verdict Unsat"
        (user_repeat.verdict = Session.Unsat);
      check_true
        "effort-cap/user-restore: configured cap still fires"
        (user_repeat.unsat_core = None && Session.effort user_capped = 17))
;;

(* ------------------------------------------ multi-MUS (review rider 1)
   ------------------- *)

(* Overlapping MUSes: assume a, b, c all true with hard (~a | ~b) and (~a | ~c). Both
   [{a,b}] and [{a,c}] are minimal cores (a is necessary to both; [{b,c}] is Sat). The
   minimizer must return ONE genuine MUS — subset-minimal, holding the shared essential a
   and exactly one of b/c — never the non-minimal [{a,b,c}]. Both strategies must return a
   valid MUS (they may pick different ones). *)
let () =
  let run linear =
    set_linear linear;
    let tag = if linear then "mmusL" else "mmusR" in
    let name = "multi-mus/" ^ if linear then "linear" else "refine" in
    let s = Session.create () in
    let ctx = Session.context s in
    let a = bool_atom s (tag ^ "_a") in
    let b = bool_atom s (tag ^ "_b") in
    let c = bool_atom s (tag ^ "_c") in
    let neg t = Context.not_ ctx t in
    Session.assert_term s (Context.or_ ctx [ neg a; neg b ]);
    Session.assert_term s (Context.or_ ctx [ neg a; neg c ]);
    let result = Session.check_sat_assuming s [ a, true; b, true; c, true ] in
    match result.Session.unsat_core with
    | None -> fail name "expected Some core"
    | Some core ->
      check_true (name ^ ": size 2") (List.length core = 2);
      check_true (name ^ ": subset-minimal MUS") (is_minimal_core s core);
      check_true (name ^ ": holds shared essential a") (assumption_mem (a, true) core);
      check_true
        (name ^ ": exactly one of b/c")
        (assumption_mem (b, true) core <> assumption_mem (c, true) core)
  in
  run false;
  run true
;;

(* ------------------------------------------------------------------ benchmark --------- *)

(* Report old (linear) vs new (refinement) probe counts and wall time on assumption-heavy
   inputs: N total assumptions, a planted 2-literal MUS, N-2 redundant decoys. *)
let benchmark () =
  Printf.printf "\ncore-min benchmark (N assumptions, planted 2-literal MUS):\n";
  Printf.printf
    "%6s | %-11s | %-11s | %-9s | %-9s\n"
    "N"
    "probes(lin)"
    "probes(ref)"
    "ms(lin)"
    "ms(ref)";
  Printf.printf "%s\n" (String.make 60 '-');
  List.iter
    (fun n ->
      let r = n - 2 in
      let measure linear =
        set_linear linear;
        let s = Session.create () in
        let mus, noise = build_gadget s ~r ~tag:(Printf.sprintf "b%d%b" n linear) in
        let assumptions = shuffle 1 (mus @ noise) in
        let t0 = Unix.gettimeofday () in
        let result = Session.check_sat_assuming s assumptions in
        let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
        assert (result.Session.verdict = Session.Unsat);
        Session.minimize_probes s, ms
      in
      let lp, lms = measure true in
      let rp, rms = measure false in
      Printf.printf "%6d | %-11d | %-11d | %-9.2f | %-9.2f\n" n lp rp lms rms)
    [ 20; 100; 500 ]
;;

let () =
  benchmark ();
  if !failures > 0
  then (
    Printf.printf "\ncore_min_test: %d FAILURES / %d checks\n" !failures !checks;
    exit 1)
  else Printf.printf "\ncore_min_test: all %d checks passed\n" !checks
;;
