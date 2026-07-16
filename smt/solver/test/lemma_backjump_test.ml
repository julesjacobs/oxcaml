module Sat = Oxsmt_solver.Sat

(* OXSMT_LEMMA_BACKJUMP ON-path self-test (task #38 / #25 hedge H2).

   The dark seam delivers an ASSERTING Theory.Lemma by a PARTIAL backjump (to the max
   false-literal level of the lemma) + learn-install, instead of the OFF path's
   [cancel_until 0] restart. Real HNF cuts are non-asserting, so before this test the ON
   path had no permanent exerciser (only the throwaway OXSMT_LCG_PROBE, now stripped).
   This drives the seam directly through the public theory API with a scripted mock.

   Scenario (deterministic, no free decisions — assumptions pin the levels): variables
   a,b,c,h; assumptions [a;b;c] decide at levels 1,2,3. At the first Final check the mock
   emits ONE asserting lemma [h ∨ ¬a ∨ ¬b]: ¬a is false at level 1, ¬b false at level 2, h
   is a fresh UNASSIGNED variable ⇒ the clause is asserting with max-false-level = 2.

   ON (OXSMT_LEMMA_BACKJUMP=1): partial backjump to level 2 — c@3 is discarded, a@1/b@2
   are KEPT (trail NOT reset to 0) — and h is enqueued true at level 2 with the lemma as
   its reason; the cert leaf ([on_input ~origin:Theory_lemma]) is emitted. OFF:
   [cancel_until 0] ⇒ h is not force-enqueued at a partial level.

   What it proves (RED-verified by mutation, see the log):
   1. PARTIAL BACKJUMP: ON records an [on_backtrack ~level:2] (the max-false-level), and h
      is first asserted at level 2 — trail not reset to 0. A [bt = second-highest] mutant
      backjumps to level 1 (where ¬b is no longer false ⇒ not asserting) and FAILS this.
   2. ASSERTING DETECTION: ON delivers via the backjump path (level 2), not the fallback
      [cancel_until 0]. A mutant whose asserting-detection is broken falls back to level 0
      and FAILS (no level-2 backtrack, h not at level 2).
   3. CERT LEAF: ON emits exactly one [on_input] with [Theory_lemma] origin for the
      clause.
   4. SOUNDNESS: verdict is Sat and the model satisfies the lemma (both flag states).

   Stdlib + Unix (test-only; toggles the flag per solver via [Unix.putenv]). Nonzero exit
   on failure. *)

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n%!" name)
;;

(* A minimal scripted theory: no propagations/conflicts; at the first Final check it emits
   ONE asserting lemma clause, then accepts. Records every [on_backtrack] level and the
   level at which the head [h] is first asserted true. *)
let make_mock ~a ~b ~c ~h =
  let bt_levels = ref [] in
  let head_level = ref (-1) in
  let emitted = ref false in
  (* the mock's own view of currently-true vars (var -> unit), maintained from the seam
     notifications, so it can time the emission structurally (no [decision_level] peek). *)
  let true_vars = Hashtbl.create 8 in
  let on_assign l ~level =
    if Sat.sign_of_lit l then Hashtbl.replace true_vars (Sat.var_of_lit l) ();
    if Sat.var_of_lit l = h && Sat.sign_of_lit l && !head_level < 0
    then head_level := level
  in
  let on_backtrack ~level =
    bt_levels := level :: !bt_levels;
    (* drop the head record if it was unwound, so a re-assignment after the backjump is
       not mistaken for the delivery-time enqueue *)
    Hashtbl.reset true_vars
  in
  let check ~final:_ =
    (* Emit ONCE, at the first check where a,b,c are all true and h is still unassigned —
       i.e. after all three assumptions are placed (decision level 3) but before h is
       decided. The lemma [h ∨ ¬a ∨ ¬b] is then asserting with max-false-level 2 < 3, so
       ON must partial-backjump to level 2 (discarding c@3, keeping a@1/b@2). *)
    if (not !emitted)
       && Hashtbl.mem true_vars a
       && Hashtbl.mem true_vars b
       && Hashtbl.mem true_vars c
       && not (Hashtbl.mem true_vars h)
    then (
      emitted := true;
      Sat.T_lemma [ [ Sat.pos h; Sat.neg a; Sat.neg b ] ])
    else Sat.T_consistent []
  in
  let explain _ = [] in
  ( { Sat.on_assign; on_backtrack; check; explain }
  , bt_levels
  , head_level
  , fun () -> !emitted )
;;

(* Count [on_input ~origin:Theory_lemma] cert-leaf events. *)
let with_lemma_leaf_counter s =
  let n = ref 0 in
  Sat.set_trace
    s
    (Some
       { Sat.on_learned = (fun ~id:_ ~clause:_ ~antecedents:_ ~btlevel:_ -> ())
       ; on_input =
           (fun ~id:_ ~clause:_ ~origin ->
             match origin with
             | Sat.Theory_lemma -> incr n
             | _ -> ())
       ; on_unit = (fun ~id:_ ~lit:_ -> ())
       ; on_theory_clause = (fun ~id:_ ~clause:_ ~role:_ -> ())
       ; on_unsat = (fun _ -> ())
       });
  fun () -> !n
;;

let run ~backjump =
  Unix.putenv "OXSMT_LEMMA_BACKJUMP" (if backjump then "1" else "0");
  let s = Sat.create () in
  let a = Sat.new_var s in
  let b = Sat.new_var s in
  let c = Sat.new_var s in
  let h = Sat.new_var s in
  let theory, bt_levels, head_level, emitted = make_mock ~a ~b ~c ~h in
  Sat.set_theory s (Some theory);
  let leaves = with_lemma_leaf_counter s in
  let r = Sat.solve ~assumptions:[ Sat.pos a; Sat.pos b; Sat.pos c ] s in
  (* model check: the lemma clause h ∨ ¬a ∨ ¬b must hold; a,b are assumed true, so h true *)
  let model_ok =
    match r with
    | Sat.Sat ->
      let m = Sat.model s in
      m.(h) || (not m.(a)) || not m.(b)
    | Sat.Unsat -> false
  in
  r, model_ok, List.rev !bt_levels, !head_level, leaves (), emitted ()
;;

let () =
  Printf.printf "lemma_backjump_test: OXSMT_LEMMA_BACKJUMP ON-path self-test\n%!";
  (* ON: partial backjump to level 2. *)
  let r_on, ok_on, bts_on, hl_on, leaves_on, emitted_on = run ~backjump:true in
  check "ON: lemma was emitted at final" emitted_on;
  check "ON: verdict Sat" (r_on = Sat.Sat);
  check "ON: model satisfies the lemma" ok_on;
  check
    (Printf.sprintf
       "ON: partial backjump to level 2 (max-false-level) occurred; bt levels = [%s]"
       (String.concat ";" (List.map string_of_int bts_on)))
    (List.mem 2 bts_on);
  check
    (Printf.sprintf "ON: head h enqueued at level 2 (got %d), trail NOT reset to 0" hl_on)
    (hl_on = 2);
  check
    (Printf.sprintf "ON: exactly one Theory_lemma cert leaf (got %d)" leaves_on)
    (leaves_on = 1);
  (* OFF: cancel_until 0 restart — no partial backjump to level 2, head not forced at 2. *)
  let r_off, ok_off, bts_off, hl_off, _leaves_off, emitted_off = run ~backjump:false in
  check "OFF: lemma was emitted at final" emitted_off;
  check "OFF: verdict Sat" (r_off = Sat.Sat);
  check "OFF: model satisfies the lemma" ok_off;
  (* The DELIVERY-TIME discriminator is the backjump level, not the head's eventual level:
     under OFF the lemma clause is added permanently and its head still propagates by BCP
     at level 2 once a,b are re-assigned — the SAME eventual level, reached by a different
     route (a full [cancel_until 0] restart, no partial backjump). So we assert on the
     backtrack set: OFF never partial-backjumps to level 2. *)
  ignore hl_off;
  check
    (Printf.sprintf
       "OFF: no partial backjump to level 2 (cancel_until 0 path); bt levels = [%s]"
       (String.concat ";" (List.map string_of_int bts_off)))
    (not (List.mem 2 bts_off));
  Printf.printf "lemma_backjump_test: %d checks, %d failures\n%!" !checks !failures;
  if !failures > 0 then exit 1
;;
