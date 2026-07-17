(* Cert step-2 REPLAY CHECKER self-test (ADR-0013 step 2). Two halves:

   (1) POSITIVE — the same E1-E4 / theory-reason / ordered-RUP / crit / high4 solves the
       step-1 [cert_emit_test] drives, run through {!Oxsmt_certificate.Checker.check}:
       every honest recorded stream must be VALID. This is the "search -> check" gate on
       real solves.

   (2) DISCRIMINATION — one corruption per class (dropped hint, permuted hints where order
       matters, wrong antecedent set, forged citation KIND, ambiguous id, truncated
       stream), each verified to FLIP a VALID stream to INVALID (or UNSUPPORTED). A
       checker that passed the positives but rubber-stamped a corruption would be
       worthless; each negative is proven to reject the exact defect it targets.

   Plus board #153b: exact-antecedent-SET assertions (not length-only) on a real chain.

   Stdlib-only; deterministic; nonzero exit on any failed check. The scripted mock theory
   is the trimmed copy [cert_emit_test]/[seam_test] use. *)

module Sat = Oxsmt_solver.Sat
module Recorder = Oxsmt_certificate.Recorder
module Checker = Oxsmt_certificate.Checker
module Cdclt = Oxsmt_interface.Cdclt
module Session = Oxsmt_interface.Session
module Context = Oxsmt_core.Context
module Sort = Oxsmt_core.Sort
module Rational = Oxsmt_lia.Rational

let checks = ref 0
let failures = ref 0

let fail name msg =
  incr failures;
  Printf.printf "  FAIL %s: %s\n" name msg
;;

let check name cond =
  incr checks;
  if not cond then fail name "condition false"
;;

let dimacs_of_lit l =
  let v = Sat.var_of_lit l + 1 in
  if Sat.sign_of_lit l then v else -v
;;

let clause_set (c : Sat.lit array) =
  List.sort compare (List.map dimacs_of_lit (Array.to_list c))
;;

let show_ints xs = "[" ^ String.concat ";" (List.map string_of_int xs) ^ "]"
let show_sets sets = "[" ^ String.concat "; " (List.map show_ints sets) ^ "]"

(* Assert a verdict shape (Valid / Invalid / Unsupported), printing the actual verdict on
   a mismatch so a regression is legible. *)
let expect name kind ev =
  incr checks;
  let v = Checker.check ev in
  let ok =
    match kind, v with
    | `Valid, (Checker.Valid_modulo_theory_leaves | Checker.Valid) -> true
    | `Fully_valid, Checker.Valid -> true
    | `Modulo, Checker.Valid_modulo_theory_leaves -> true
    | `Invalid, Checker.Invalid _ -> true
    | `Unsupported, Checker.Unsupported _ -> true
    | _ -> false
  in
  if not ok
  then (
    incr failures;
    Printf.printf "  FAIL %s: got %s\n" name (Checker.string_of_verdict v))
;;

(* Assert VALID *and* that the learned-clause full-closure RUP fallback (task #56)
   actually FIRED — so these are positive tests of the fallback PATH, not vacuously-valid
   streams that might pass via the hinted chain. Resets the cumulative counter per case. *)
let expect_via_fallback name ev =
  incr checks;
  Checker.reset_fallback_firings ();
  let v = Checker.check ev in
  let fired = Checker.fallback_firing_count () in
  match v with
  | (Checker.Valid_modulo_theory_leaves | Checker.Valid) when fired > 0 -> ()
  | _ ->
    incr failures;
    Printf.printf
      "  FAIL %s: got %s (fallback fired=%d, expected VALID with fired>0)\n"
      name
      (Checker.string_of_verdict v)
      fired
;;

(* ------------------------------------------------------------------ *)
(* The scripted mock theory (trimmed copy of cert_emit_test's). *)

type mock_config =
  { conflicts : Sat.lit list list
  ; implications : (Sat.lit list * Sat.lit) list
  ; final_splits : Sat.lit list list
  }

let empty_config = { conflicts = []; implications = []; final_splits = [] }

let make_mock _st config =
  let trail = ref [] in
  let is_true l = List.exists (fun (x, _) -> x = l) !trail in
  let on_assign l ~level = trail := (l, level) :: !trail in
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
  { Sat.on_assign; on_backtrack; check; explain; on_chrono_rewind = None }
;;

(* ------------------------------------------------------------------ *)
(* Positive scenarios: drive a real solve and snapshot its stream as [Checker.events]. *)

let e1_root_empty () =
  let s = Sat.create () in
  let a = Sat.new_var s in
  let rec_ = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec_));
  Sat.add_clause s [ Sat.pos a ];
  Sat.add_clause s [ Sat.neg a ];
  let r = Sat.solve s in
  check "e1: unsat" (r = Sat.Unsat);
  Checker.of_recorder rec_ ~assumptions:[]
;;

let e2_level0_conflict () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s in
  let rec_ = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec_));
  Sat.add_clause s [ Sat.pos a; Sat.pos b ];
  Sat.add_clause s [ Sat.neg a ];
  Sat.add_clause s [ Sat.neg b ];
  let r = Sat.solve s in
  check "e2: unsat" (r = Sat.Unsat);
  Checker.of_recorder rec_ ~assumptions:[]
;;

let e3_failed_assumption_theory_prop () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s
  and c = Sat.new_var s in
  let la = Sat.pos a
  and lb = Sat.pos b
  and lc = Sat.pos c in
  ignore lc;
  let mock = make_mock s { empty_config with implications = [ [ la ], lc ] } in
  Sat.set_theory s (Some mock);
  let rec_ = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec_));
  Sat.add_clause s [ Sat.neg c; Sat.neg b ];
  let r = Sat.solve ~assumptions:[ la; lb ] s in
  check "e3: unsat" (r = Sat.Unsat);
  Checker.of_recorder rec_ ~assumptions:[ la; lb ]
;;

let e4_theory_lemma_empty () =
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
  Checker.of_recorder rec_ ~assumptions:[]
;;

let theory_conflict () =
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
  Checker.of_recorder rec_ ~assumptions:[ la; lb ]
;;

let analyze_theory_reason () =
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
  let r = Sat.solve ~assumptions:[ lp; la ] s in
  check "analyze-reason: unsat" (r = Sat.Unsat);
  Checker.of_recorder rec_ ~assumptions:[ lp; la ]
;;

(* the antecedent-order scenario: a pure-Boolean 1UIP with a real resolution chain.
   Returns both the events and the single learned event (for the exact-set assertion). *)
let antecedent_order () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s
  and c = Sat.new_var s in
  ignore (b, c);
  let la = Sat.pos a in
  let rec_ = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec_));
  Sat.add_clause s [ Sat.neg a; Sat.pos b ];
  Sat.add_clause s [ Sat.neg b; Sat.pos c ];
  Sat.add_clause s [ Sat.neg b; Sat.neg c ];
  let r = Sat.solve ~assumptions:[ la ] s in
  check "order: unsat" (r = Sat.Unsat);
  Checker.of_recorder rec_ ~assumptions:[ la ]
;;

let crit1_unminimized () =
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
  check "crit1: unsat" (r = Sat.Unsat);
  Checker.of_recorder rec_ ~assumptions:[ Sat.pos a; Sat.pos c ]
;;

let crit2_level0_theory_reason () =
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
  Checker.of_recorder rec_ ~assumptions:[]
;;

(* repeated solve on an already-unsat core re-emits its conclusion; both must check VALID. *)
let crit3_repeated_solve () =
  let s = Sat.create () in
  let a = Sat.new_var s
  and b = Sat.new_var s in
  let rec1 = Recorder.create () in
  Sat.set_trace s (Some (Recorder.trace rec1));
  Sat.add_clause s [ Sat.pos a; Sat.pos b ];
  Sat.add_clause s [ Sat.neg a ];
  Sat.add_clause s [ Sat.neg b ];
  let r1 = Sat.solve s in
  check "crit3: first unsat" (r1 = Sat.Unsat);
  let ev1 = Checker.of_recorder rec1 ~assumptions:[] in
  (* second solve on the same (now permanently-unsat) core re-emits the terminal into a
     fresh recorder; but the id-resolving content events were surfaced on the FIRST solve,
     so re-checking the second stream in isolation would dangle. The realistic re-emit
     check is that the SAME recorder, kept installed across both solves, still checks
     VALID. *)
  let r2 = Sat.solve s in
  check "crit3: second unsat" (r2 = Sat.Unsat);
  let ev2 = Checker.of_recorder rec1 ~assumptions:[] in
  ev1, ev2
;;

(* the HIGH-4 cross-solver ambiguity: one recorder over two solvers whose ids both restart
   from 0 -> a content id is emitted twice -> the cited id is ambiguous -> INVALID. *)
let high4_ambiguous () =
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
  Checker.of_recorder rec_ ~assumptions:[]
;;

(* End-to-end LIA leaf witness: [x <= 0] and [x >= 1] produce one Farkas-backed theory
   Conflict. Cdclt records the SAT-var->atom statement separately from the multiplier
   witness, Recorder binds the latter to the next frozen-seam Conflict id, and Checker
   independently recomputes the contradiction. *)
let lia_farkas_conflict () =
  let s = Session.create () in
  let rec_ = Recorder.create () in
  Session.install_cert_trace s (Some (Recorder.trace rec_));
  Session.install_lia_certificate_trace
    s
    (Some
       { Cdclt.on_theory_atom =
           (fun ~var ~atom -> Recorder.record_theory_atom rec_ ~var ~atom)
       ; on_lia_conflict =
           (fun ~premise_lits ~multipliers ->
             Recorder.record_lia_conflict rec_ ~premise_lits ~multipliers)
       });
  let ctx = Session.context s in
  let x = Context.const ctx (Session.declare_const s "cert_x" Sort.int) in
  Session.assert_term s (Context.le ctx x (Context.int_const ctx 0));
  Session.assert_term s (Context.le ctx (Context.int_const ctx 1) x);
  check "lia-farkas: solve unsat" (Session.check_sat s = Session.Unsat);
  Checker.of_recorder rec_ ~assumptions:(Session.cert_assumptions s)
;;

let strip_lia_witnesses (ev : Checker.events) =
  { ev with
    theory =
      List.map
        (fun (e : Recorder.theory_event) -> { e with Recorder.lia_witness = None })
        ev.theory
  }
;;

let zero_lia_multipliers (ev : Checker.events) =
  { ev with
    theory =
      List.map
        (fun (e : Recorder.theory_event) ->
           match e.Recorder.lia_witness with
           | None -> e
           | Some witness ->
             let premises =
               List.map
                 (fun (p : Recorder.lia_premise) ->
                    { p with Recorder.multiplier = Rational.zero })
                 witness.Recorder.premises
             in
             { e with
               Recorder.lia_witness = Some { Recorder.premises = premises }
             })
        ev.theory
  }
;;

(* ------------------------------------------------------------------ *)
(* A HAND-BUILT, order-sensitive learned-clause chain (full control for the ordered-RUP
   discrimination). Inputs (all Query): id10 [a∨b], id11 [¬a∨c], id12 [¬a∨¬c] entail b;
   the learned clause id20 [b] replays by ordered RUP [10;11;12] (b:=false; 10 forces a;
   11 forces c; 12 conflicts) — a chain where a permutation strands a non-unit hint. The
   refutation is closed as a Failed_assumption under [¬b] (b forced true by the learned
   clause contradicts the assumption). *)
let a_ = Sat.pos 0
and na_ = Sat.neg 0
and b_ = Sat.pos 1
and nb_ = Sat.neg 1
and c_ = Sat.pos 2
and nc_ = Sat.neg 2

let mk_input id clause : Recorder.input_event = { id; clause; origin = Sat.Query }

let mk_learned id clause antecedents : Recorder.learned_event =
  { id; clause; antecedents; btlevel = 0 }
;;

let handbuilt ?(learned_ants = [ 10; 11; 12 ]) ?conclusion () : Checker.events =
  { Checker.inputs =
      [ mk_input 10 [| a_; b_ |]; mk_input 11 [| na_; c_ |]; mk_input 12 [| na_; nc_ |] ]
  ; atoms = []
  ; units = []
  ; learned = [ mk_learned 20 [| b_ |] learned_ants ]
  ; theory = []
  ; conclusion =
      (match conclusion with
       | Some c -> Some c
       | None -> Some (Sat.Failed_assumption { antecedents = [ 20 ] }))
  ; assumptions = [ nb_ ]
  }
;;

(* task #56 rider 2: same inputs as [handbuilt], but the learned clause is [c], which
   those inputs do NOT entail (model a=false,b=true,c=false satisfies all three). So
   corrupting the cited chain fails ordered RUP AND the full-closure fallback finds no ⊥ →
   INVALID — the accept-invalid direction stays rejected under every hint-corruption
   shape. *)
let handbuilt_unentailed ?(learned_ants = [ 10; 11; 12 ]) () : Checker.events =
  { Checker.inputs =
      [ mk_input 10 [| a_; b_ |]; mk_input 11 [| na_; c_ |]; mk_input 12 [| na_; nc_ |] ]
  ; atoms = []
  ; units = []
  ; learned = [ mk_learned 20 [| c_ |] learned_ants ]
  ; theory = []
  ; conclusion = Some (Sat.Failed_assumption { antecedents = [ 20 ] })
  ; assumptions = [ nc_ ]
  }
;;

(* CRIT-1 (reviewer, logs/cert-step2-review.md): a learned-clause hint must resolve only
   to an ALREADY-VERIFIED learned clause, never itself or a later one. These two streams
   certify a trivially-SAT / SAT query as unsat pre-fix (accept-invalid, the north star);
   the growing verified-id set makes each INVALID. *)

(* exploit A: two SELF-citing learned clauses on an empty (trivially SAT) query. Each
   learned clause cites its own id as its sole hint, so negating the clause falsifies the
   cited (== same) clause and "verifies" it out of nothing. *)
let exploit_self_cite : Checker.events =
  let x = Sat.pos 3
  and nx = Sat.neg 3 in
  { Checker.inputs = []
  ; atoms = []
  ; units = []
  ; learned = [ mk_learned 20 [| x |] [ 20 ]; mk_learned 21 [| nx |] [ 21 ] ]
  ; theory = []
  ; conclusion = Some (Sat.Level0_conflict { conflict_id = 21 })
  ; assumptions = []
  }
;;

(* exploit B: MUTUALLY-referential learned clauses certify a SATISFIABLE query ([a] alone
   is sat, a=true) as unsat. id20 cites id21 and id21 cites id20; pre-fix both "verify". *)
let exploit_mutual : Checker.events =
  { Checker.inputs = [ mk_input 1 [| a_ |] ]
  ; atoms = []
  ; units = []
  ; learned = [ mk_learned 20 [| na_ |] [ 21 ]; mk_learned 21 [| na_ |] [ 20 ] ]
  ; theory = []
  ; conclusion = Some (Sat.Level0_conflict { conflict_id = 20 })
  ; assumptions = []
  }
;;

(* C2 (codex, CRITICAL): an EMPTY theory Reason clause is admitted into the axiom DB as ⊥
   (guard_theory_leaf guarded only the empty-Conflict role), so BCP finds an immediate
   conflict and a SAT query is certified unsat. A Reason must carry its implied literal at
   slot 0 — an empty Reason is malformed → INVALID. Query [{a}] is SAT. *)
let exploit_empty_reason : Checker.events =
  { Checker.inputs = [ mk_input 1 [| a_ |] ]
  ; atoms = []
  ; units = []
  ; learned = []
  ; theory =
      [ ({ id = 30; clause = [||]; role = Sat.Reason; lia_witness = None }
          : Recorder.theory_event)
      ]
  ; conclusion = Some (Sat.Failed_assumption { antecedents = [] })
  ; assumptions = []
  }
;;

(* H4 (codex, HIGH→CRITICAL): a spurious clause sharing an id with a real one is admitted
   to the axiom DB even though the ambiguity is NEVER cited, poisoning BCP. Here id 10
   carries both [a] (the real, SAT query [{a}]) and a spurious [¬a]; the E3 terminal cites
   nothing, so the ambiguity slips past citation-time resolution and BCP refutes a SAT
   query. Ambiguous content ids must be rejected at STREAM ADMISSION. Triggers ONLY on
   ambiguity (M6: a clean discriminator of the #153a admission guard). *)
let exploit_ambiguous_admission : Checker.events =
  { Checker.inputs = [ mk_input 10 [| a_ |]; mk_input 10 [| na_ |] ]
  ; atoms = []
  ; units = []
  ; learned = []
  ; theory = []
  ; conclusion = Some (Sat.Failed_assumption { antecedents = [] })
  ; assumptions = []
  }
;;

(* M5 (codex): duplicate raw literals defeat unit detection, OVER-rejecting a valid cert.
   Input id10 [a;a] is really the unit [a]; without dedup it is seen as 2-free and never
   propagated, so [¬a] (id11) is not falsified and Root_empty spuriously fails. Post-fix
   (dedup at ingest) this is VALID. *)
let overreject_dup_lit : Checker.events =
  { Checker.inputs = [ mk_input 10 [| a_; a_ |]; mk_input 11 [| na_ |] ]
  ; atoms = []
  ; units = []
  ; learned = []
  ; theory = []
  ; conclusion = Some (Sat.Root_empty { input_id = 11 })
  ; assumptions = []
  }
;;

(* CRITICAL (codex, this round): a RAW-EMPTY Theory_lemma INPUT clause fabricates ⊥.
   Theory lemmas arrive as input_events with origin=Theory_lemma; a raw-empty one has no
   Valid_lemma witness in any theory (ADR-0013 §4.0 E4 admits only a NONEMPTY lemma that
   FILTERS to [] under the level-0 closure). Pre-fix it is admitted to the axiom DB as a
   trusted [] that certifies the SAT query [{a}] unsat through ALL THREE terminals:
   Root_empty citing it, Level0_conflict citing it, and Failed_assumption (empty
   antecedents — never cites it, yet BCP over the poisoned DB refutes anything). Post-fix
   the admission guard keys on origin and rejects the empty lemma before it enters the DB,
   so all three go INVALID. Contrast [empty_query_input_ok]: an empty QUERY input is the
   legitimate E1 opposite and stays VALID. *)
let mk_lemma_input id clause : Recorder.input_event =
  { id; clause; origin = Sat.Theory_lemma }
;;

let exploit_empty_lemma_root_empty : Checker.events =
  { Checker.inputs = [ mk_input 1 [| a_ |]; mk_lemma_input 30 [||] ]
  ; atoms = []
  ; units = []
  ; learned = []
  ; theory = []
  ; conclusion = Some (Sat.Root_empty { input_id = 30 })
  ; assumptions = []
  }
;;

let exploit_empty_lemma_level0 : Checker.events =
  { Checker.inputs = [ mk_input 1 [| a_ |]; mk_lemma_input 30 [||] ]
  ; atoms = []
  ; units = []
  ; learned = []
  ; theory = []
  ; conclusion = Some (Sat.Level0_conflict { conflict_id = 30 })
  ; assumptions = []
  }
;;

let exploit_empty_lemma_failed_assumption : Checker.events =
  { Checker.inputs = [ mk_input 1 [| a_ |]; mk_lemma_input 30 [||] ]
  ; atoms = []
  ; units = []
  ; learned = []
  ; theory = []
  ; conclusion = Some (Sat.Failed_assumption { antecedents = [] })
  ; assumptions = []
  }
;;

(* Base #53 RED: a level-0 THEORY conflict now routes to an EMPTY-CORE E3
   [Failed_assumption { antecedents = [] }] (sat.ml [conclude_unsat], [~theory:true]).
   That route is NOT a free pass to "unsat" — it is gated by the checker's [refutes_under]
   over the whole clause DB. Here the theory conflict is BOGUS: the emitted theory leaf
   [b] does NOT contradict the SAT input [a], so the DB [{a}, {b}] is satisfiable and BCP
   over it derives NO ⊥. The empty-core E3 must therefore be REJECTED — proving a
   mis-emitted (or fabricated) level-0 theory conflict cannot launder a SAT query to
   unsat. Contrast the e3_failed_assumption_theory_prop positive, where the DB genuinely
   refutes. *)
let bogus_theory_conflict_empty_core_e3 : Checker.events =
  { Checker.inputs = [ mk_input 1 [| a_ |] ]
  ; atoms = []
  ; units = []
  ; learned = []
  ; theory =
      [ ({ id = 30; clause = [| b_ |]; role = Sat.Conflict; lia_witness = None }
          : Recorder.theory_event)
      ]
  ; conclusion = Some (Sat.Failed_assumption { antecedents = [] })
  ; assumptions = []
  }
;;

(* the E1 opposite the origin-keyed guard must NOT break: a raw-empty QUERY input asserts
   the empty clause = false, which is legitimately unsat. Stays VALID pre- and post-fix. *)
let empty_query_input_ok : Checker.events =
  { Checker.inputs = [ mk_input 1 [||] ]
  ; atoms = []
  ; units = []
  ; learned = []
  ; theory = []
  ; conclusion = Some (Sat.Root_empty { input_id = 1 })
  ; assumptions = []
  }
;;

(* task #42 SATISFIED-HINT SKIP — the rings id-162 pattern reproduced by hand: a redundant
   antecedent whose satisfying literal an EARLIER antecedent already forced sits
   MID-chain. Negate learned [b] → b:=false; 10=[a∨b] forces a; 11=[¬a∨c] forces c;
   12=[¬a∨c] is now SATISFIED (c true) → SKIP; 13=[¬a∨¬c] is falsified → conflict. Pre-fix
   the checker ERRORED on the satisfied hint 12 (INVALID); post-fix it skips it and the
   chain still closes on 13. VALID — the exact positive the fix unblocks. *)
let id162_satisfied_skip : Checker.events =
  { Checker.inputs =
      [ mk_input 10 [| a_; b_ |]
      ; mk_input 11 [| na_; c_ |]
      ; mk_input 12 [| na_; c_ |]
      ; mk_input 13 [| na_; nc_ |]
      ]
  ; atoms = []
  ; units = []
  ; learned = [ mk_learned 20 [| b_ |] [ 10; 11; 12; 13 ] ]
  ; theory = []
  ; conclusion = Some (Sat.Failed_assumption { antecedents = [ 20 ] })
  ; assumptions = [ nb_ ]
  }
;;

(* task #42 negative (i): a mid-chain hint that is NEITHER unit NOR satisfied (2 free
   literals) still ERRORS — the skip is narrow and the "refuses to search" contract stands
   for the non-satisfied cases. Negate [b] → b:=false; 10 forces a; 11=[c∨d] then has BOTH
   literals free → reject, never search. *)
let neither_unit_nor_satisfied : Checker.events =
  let d_ = Sat.pos 3 in
  { Checker.inputs = [ mk_input 10 [| a_; b_ |]; mk_input 11 [| c_; d_ |] ]
  ; atoms = []
  ; units = []
  ; learned = [ mk_learned 20 [| b_ |] [ 10; 11 ] ]
  ; theory = []
  ; conclusion = Some (Sat.Failed_assumption { antecedents = [ 20 ] })
  ; assumptions = [ nb_ ]
  }
;;

(* task #42 negative (ii): remove the load-bearing final hint (13) from the id-162 chain —
   the remaining hints derive NO conflict. Skipping the satisfied 12 must NOT launder an
   incomplete chain to valid: the chain is consumed without a conflict → ERROR. Proves the
   conflict-derivation requirement survives the skip. *)
let satisfied_but_no_conflict : Checker.events =
  { Checker.inputs =
      [ mk_input 10 [| a_; b_ |]; mk_input 11 [| na_; c_ |]; mk_input 12 [| na_; c_ |] ]
  ; atoms = []
  ; units = []
  ; learned = [ mk_learned 20 [| b_ |] [ 10; 11; 12 ] ]
  ; theory = []
  ; conclusion = Some (Sat.Failed_assumption { antecedents = [ 20 ] })
  ; assumptions = [ nb_ ]
  }
;;

(* task #47 E1/E2 CITED-CLAUSE FALLBACK. A Level0_conflict whose cited clause is NOT
   falsified by the closure, but the closure IS globally inconsistent → VALID via the
   [refutes_under []] fallback. Inputs [a] and [¬a] are UP-inconsistent; the terminal
   cites id10=[a], which the closure (a:=true) SATISFIES, not falsifies — mirroring the
   rings id-7866 shape where the emitter's cited witness is satisfied under the checker's
   batch closure yet the closure still derives ⊥. Pre-fix (no fallback) this is INVALID;
   the fix accepts it. *)
let e2_fallback_closure_inconsistent : Checker.events =
  { Checker.inputs = [ mk_input 10 [| a_ |]; mk_input 11 [| na_ |] ]
  ; atoms = []
  ; units = []
  ; learned = []
  ; theory = []
  ; conclusion = Some (Sat.Level0_conflict { conflict_id = 10 })
  ; assumptions = []
  }
;;

(* task #47 negative: cited clause NOT falsified AND the closure CONSISTENT → INVALID. The
   fallback is gated on GENUINE inconsistency, not a blanket pass. Query [a] alone is SAT;
   a Level0_conflict citing the satisfied [a] must be rejected — the accept-invalid north
   star. (Rejects pre- and post-fix; the point is that the fallback did NOT open a hole.) *)
let e2_cited_not_falsified_closure_consistent : Checker.events =
  { Checker.inputs = [ mk_input 10 [| a_ |] ]
  ; atoms = []
  ; units = []
  ; learned = []
  ; theory = []
  ; conclusion = Some (Sat.Level0_conflict { conflict_id = 10 })
  ; assumptions = []
  }
;;

(* task #56 LEARNED-CLAUSE FULL-CLOSURE RUP FALLBACK. Mirrors the rings id-6571/6572
   shape: a learned clause whose CITED antecedent is SATISFIED under the level-0 closure
   (so the hinted ordered chain is consumed without a conflict), yet the clause is
   ENTAILED by the verified closure. Inputs 10=[a∨b],11=[¬a∨c],12=[¬a∨¬c] entail b; 13=[d]
   forces d; 14=[d∨b] is satisfied by d. Learned 20=[b] cites ONLY the satisfied 14 →
   ordered RUP consumes the chain (14 skipped) → fails; the fallback seeds ¬b and BCP over
   the closure derives ⊥ (b:=false ⇒ 10 forces a ⇒ 11 forces c ⇒ 12 conflicts) → VALID.
   Failed_assumption under [¬b] closes it. Pre-fix (no fallback) → INVALID. *)
let learned_fallback_entailed : Checker.events =
  let d_ = Sat.pos 3 in
  { Checker.inputs =
      [ mk_input 10 [| a_; b_ |]
      ; mk_input 11 [| na_; c_ |]
      ; mk_input 12 [| na_; nc_ |]
      ; mk_input 13 [| d_ |]
      ; mk_input 14 [| d_; b_ |]
      ]
  ; atoms = []
  ; units = []
  ; learned = [ mk_learned 20 [| b_ |] [ 14 ] ]
  ; theory = []
  ; conclusion = Some (Sat.Failed_assumption { antecedents = [ 20 ] })
  ; assumptions = [ nb_ ]
  }
;;

(* task #56 negative: an UNENTAILED learned clause must still be REJECTED — the fallback
   is gated on genuine entailment, not a blanket pass (no accept-invalid). Query is a=true
   (SAT); learned 20 = the unit [b] (b unconstrained) cites the satisfied 10 = [a] →
   ordered RUP consumed → the fallback seeds b false and BCP over the single input derives
   NO conflict → INVALID. *)
let learned_unentailed : Checker.events =
  { Checker.inputs = [ mk_input 10 [| a_ |] ]
  ; atoms = []
  ; units = []
  ; learned = [ mk_learned 20 [| b_ |] [ 10 ] ]
  ; theory = []
  ; conclusion = Some (Sat.Failed_assumption { antecedents = [ 20 ] })
  ; assumptions = [ nb_ ]
  }
;;

(* ------------------------------------------------------------------ *)

let () =
  (* (1) POSITIVE — every honest stream is VALID. *)
  expect "positive: E1 Root_empty (query filter-to-[])" `Valid (e1_root_empty ());
  expect "positive: E2 Level0_conflict" `Valid (e2_level0_conflict ());
  expect
    "positive: E3 Failed_assumption + Theory_prop (H1)"
    `Valid
    (e3_failed_assumption_theory_prop ());
  expect
    "positive: E4 Root_empty (theory-lemma filter-to-[])"
    `Valid
    (e4_theory_lemma_empty ());
  expect "positive: theory conflict leaf" `Valid (theory_conflict ());
  expect "positive: 1UIP theory-reason materialization" `Valid (analyze_theory_reason ());
  expect "positive: ordered-RUP antecedent chain" `Valid (antecedent_order ());
  expect "positive: CRIT-1 unminimized learned clause" `Valid (crit1_unminimized ());
  expect "positive: CRIT-2 level-0 theory reason" `Valid (crit2_level0_theory_reason ());
  let ev1, ev2 = crit3_repeated_solve () in
  expect "positive: CRIT-3 first solve" `Valid ev1;
  expect "positive: CRIT-3 repeated-solve re-emit" `Valid ev2;
  expect "positive: hand-built order-sensitive chain" `Valid (handbuilt ());
  let lia_ev = lia_farkas_conflict () in
  check
    "positive: LIA stream carries a Farkas-witnessed Conflict leaf"
    (List.exists
       (fun (e : Recorder.theory_event) -> Option.is_some e.Recorder.lia_witness)
       lia_ev.Checker.theory);
  expect
    "positive: all theory leaves witnessed -> fully VALID"
    `Fully_valid
    lia_ev;
  expect
    "coverage: stripping a valid LIA witness stays conditional"
    `Modulo
    (strip_lia_witnesses lia_ev);
  expect
    "corrupt: zeroed Farkas multipliers -> INVALID"
    `Invalid
    (zero_lia_multipliers lia_ev);
  (* board #153b — EXACT antecedent-SET (and order) on a real chain, not length-only. The
     order scenario learns [¬b] from [¬b∨c] then the conflict [¬b∨¬c]; the frozen contract
     order [rₙ..r₁; conflict] is exactly [ {¬b,c}; {¬b,¬c} ] = [ [-2;3]; [-3;-2] ]. *)
  let () =
    let ev = antecedent_order () in
    let input_clause_of id =
      List.find_map
        (fun (i : Recorder.input_event) ->
           if i.Recorder.id = id then Some (clause_set i.Recorder.clause) else None)
        ev.Checker.inputs
    in
    match ev.Checker.learned with
    | [ le ] ->
      let ant_sets =
        List.map
          (fun id -> Option.value ~default:[ 0 ] (input_clause_of id))
          le.Recorder.antecedents
      in
      let expected = [ [ -2; 3 ]; [ -3; -2 ] ] in
      check
        (Printf.sprintf
           "exact-set: antecedent clause-sets = %s (got %s)"
           (show_sets expected)
           (show_sets ant_sets))
        (ant_sets = expected)
    | ls ->
      fail
        "exact-set"
        (Printf.sprintf "expected 1 learned clause, got %d" (List.length ls))
  in
  (* (2) DISCRIMINATION — each corruption FLIPS a VALID stream to INVALID/UNSUPPORTED.
     Every negative is paired with its honest VALID baseline above so the flip is proven. *)
  (* task #56 REFRAME (was three "corrupt hint chain -> INVALID" negatives): with the
     full-closure RUP fallback, a well-formed but incomplete/mis-ordered/duplicated hint
     chain on an ENTAILED learned clause is now VALID *via the fallback* — the cited chain
     is advisory, the ground truth is closure-entailment. handbuilt's inputs entail [b],
     so these three (dropped / permuted / wrong-set of well-formed cited ids) are now
     positive tests of the FALLBACK PATH; each asserts the fallback actually FIRED. The
     lost "corruption -> reject" discrimination is REPLACED below by the same shapes on an
     UNENTAILED clause (the only soundness-relevant direction). *)
  expect_via_fallback
    "fallback: dropped hint on entailed clause -> VALID (fallback fired)"
    (handbuilt ~learned_ants:[ 10; 11 ] ());
  expect_via_fallback
    "fallback: permuted hints on entailed clause -> VALID (fallback fired)"
    (handbuilt ~learned_ants:[ 11; 10; 12 ] ());
  expect_via_fallback
    "fallback: wrong antecedent set on entailed clause -> VALID (fallback fired)"
    (handbuilt ~learned_ants:[ 10; 11; 11 ] ());
  (* task #56 rider 2 — the REPLACEMENT discrimination: the SAME hint-corruption shapes on
     an UNENTAILED clause still REJECT. [handbuilt_unentailed] learns [c], which
     handbuilt's inputs do NOT entail (a=false,b=true,c=false is a model), so ordered RUP
     fails AND the fallback finds no ⊥ → INVALID. Proves corruption + non-entailment (the
     accept-invalid direction — the only soundness direction) stays fully rejected. *)
  expect
    "corrupt: dropped hint on UNENTAILED clause -> INVALID"
    `Invalid
    (handbuilt_unentailed ~learned_ants:[ 10; 11 ] ());
  expect
    "corrupt: permuted hints on UNENTAILED clause -> INVALID"
    `Invalid
    (handbuilt_unentailed ~learned_ants:[ 11; 10; 12 ] ());
  expect
    "corrupt: wrong antecedent set on UNENTAILED clause -> INVALID"
    `Invalid
    (handbuilt_unentailed ~learned_ants:[ 10; 11; 11 ] ());
  (* forged citation KIND (board #153a): a Root_empty whose input_id is actually a LEARNED
     event's id. The recorder's occurrence-count resolver false-cleans this; the
     kind-keyed checker rejects it. *)
  expect
    "corrupt: forged citation kind (Root_empty -> learned id) -> INVALID"
    `Invalid
    (handbuilt ~conclusion:(Sat.Root_empty { input_id = 20 }) ());
  (* ambiguous id (HIGH-4): one id, two content clauses (cross-solver reuse). *)
  expect "corrupt: ambiguous cross-solver id -> INVALID" `Invalid (high4_ambiguous ());
  (* truncated stream: the terminal conclusion is missing. *)
  expect
    "corrupt: truncated stream (no conclusion) -> INVALID"
    `Invalid
    { (handbuilt ()) with conclusion = None };
  (* dangling citation: an antecedent id that resolves to nothing. *)
  expect
    "corrupt: dangling antecedent id -> INVALID"
    `Invalid
    (handbuilt ~learned_ants:[ 10; 11; 999 ] ());
  (* CRIT-1 (reviewer accept-invalid north star): a learned-clause hint that cites an
     unverified learned id — itself, or a later/mutually-referential one. Both certify a
     (trivially-)SAT query as unsat pre-fix; the verified-id gate makes them INVALID. *)
  expect
    "corrupt: self-citing learned clauses (empty query) -> INVALID"
    `Invalid
    exploit_self_cite;
  expect
    "corrupt: mutually-referential learned clauses (sat query) -> INVALID"
    `Invalid
    exploit_mutual;
  (* H3 (codex): ordered_rup must validate the FULL antecedent list even after an early
     falsification — a forged/dangling id in the TAIL (here 999 after the 12 that already
     conflicts) must not slip through. *)
  expect
    "corrupt: dangling antecedent AFTER early conflict -> INVALID"
    `Invalid
    (handbuilt ~learned_ants:[ 10; 11; 12; 999 ] ());
  (* C2 (codex, CRITICAL): empty theory Reason clause admitted as ⊥ -> a SAT query VALID. *)
  expect "corrupt: empty theory Reason clause -> INVALID" `Invalid exploit_empty_reason;
  (* base #53: a bogus level-0 theory conflict routed to empty-core E3 must still fail
     [refutes_under] — the route is not a free pass to unsat. *)
  expect
    "corrupt: bogus theory conflict empty-core E3 (non-refuting DB) -> INVALID"
    `Invalid
    bogus_theory_conflict_empty_core_e3;
  (* CRITICAL (codex, this round): a raw-empty Theory_lemma INPUT fabricates ⊥ and
     certifies the SAT query [{a}] unsat through all THREE terminals. Each must go
     INVALID; the empty QUERY input opposite must stay VALID (the origin-keyed guard, not
     a blanket empty ban). *)
  expect
    "corrupt: empty Theory_lemma input (Root_empty) -> INVALID"
    `Invalid
    exploit_empty_lemma_root_empty;
  expect
    "corrupt: empty Theory_lemma input (Level0_conflict) -> INVALID"
    `Invalid
    exploit_empty_lemma_level0;
  expect
    "corrupt: empty Theory_lemma input (Failed_assumption, uncited) -> INVALID"
    `Invalid
    exploit_empty_lemma_failed_assumption;
  expect
    "positive: empty QUERY input stays legitimately unsat (E1) -> VALID"
    `Valid
    empty_query_input_ok;
  (* task #42 satisfied-hint SKIP: the rings id-162 pattern (redundant satisfied hint
     mid-chain) now REPLAYS, and the two narrow-guard negatives (a hint neither unit nor
     satisfied still errors; skipping a satisfied hint does not launder a chain that
     derives no conflict). *)
  expect
    "positive: id-162 satisfied hint mid-chain skipped -> VALID"
    `Valid
    id162_satisfied_skip;
  expect
    "corrupt: hint neither unit nor satisfied (2 free) -> INVALID"
    `Invalid
    neither_unit_nor_satisfied;
  expect
    "corrupt: satisfied hint but chain derives no conflict -> INVALID"
    `Invalid
    satisfied_but_no_conflict;
  (* task #47 E1/E2 cited-clause fallback: cited clause not falsified but closure globally
     inconsistent -> VALID; and the gate — cited not falsified AND closure consistent (a
     SAT query) -> INVALID (fallback fires only on genuine inconsistency). *)
  expect
    "positive: E2 cited clause satisfied but closure inconsistent -> VALID"
    `Valid
    e2_fallback_closure_inconsistent;
  expect
    "corrupt: E2 cited not falsified AND closure consistent (sat query) -> INVALID"
    `Invalid
    e2_cited_not_falsified_closure_consistent;
  (* task #56 learned-clause full-closure RUP fallback: a learned clause whose cited
     antecedent is satisfied (hinted chain consumed) but which the verified closure
     entails -> VALID; and the gate — an unentailed learned clause -> INVALID (fallback
     gated on genuine entailment, no accept-invalid). *)
  expect
    "positive: learned clause cited-antecedent satisfied but closure-entailed -> VALID"
    `Valid
    learned_fallback_entailed;
  expect
    "corrupt: unentailed learned clause (fallback finds no ⊥) -> INVALID"
    `Invalid
    learned_unentailed;
  (* H4 (codex, HIGH->CRITICAL): ambiguous content id admitted to the DB (never cited) ->
     a SAT query VALID. Must be rejected at stream admission. Also the M6 clean
     discriminator of the #153a ambiguity guard (only ambiguity triggers). *)
  expect
    "corrupt: ambiguous content id at admission -> INVALID"
    `Invalid
    exploit_ambiguous_admission;
  (* M5 (codex): duplicate raw literals must NOT over-reject a valid cert (dedup at
     ingest). *)
  expect "positive: duplicate raw literals do not over-reject" `Valid overreject_dup_lit;
  (* UNSUPPORTED extension point: an empty theory Conflict leaf (unconditional T_conflict
     [], ADR-0013 Rev 6) has no v1 leaf witness for ⊥-from-∅ — the checker fails closed to
     UNSUPPORTED, never VALID. Hand-built: the terminal cites the empty theory conflict. *)
  let unsupported_empty_conflict : Checker.events =
    { Checker.inputs = [ mk_input 10 [| a_ |] ]
    ; atoms = []
    ; units = []
    ; learned = []
    ; theory =
        [ ({ id = 30; clause = [||]; role = Sat.Conflict; lia_witness = None }
            : Recorder.theory_event)
        ]
    ; conclusion = Some (Sat.Level0_conflict { conflict_id = 30 })
    ; assumptions = []
    }
  in
  expect
    "ext-point: empty theory Conflict -> UNSUPPORTED"
    `Unsupported
    unsupported_empty_conflict;
  Printf.printf "checker_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
