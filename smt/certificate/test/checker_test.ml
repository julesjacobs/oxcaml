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
    (* today's good-cert verdict is the theory-leaf-conditional one (MED-1); plain [Valid]
       is reserved for the leaf-checker tranche. *)
    | `Valid, Checker.Valid_modulo_theory_leaves -> true
    | `Invalid, Checker.Invalid _ -> true
    | `Unsupported, Checker.Unsupported _ -> true
    | _ -> false
  in
  if not ok
  then (
    incr failures;
    Printf.printf "  FAIL %s: got %s\n" name (Checker.string_of_verdict v))
;;

(* ------------------------------------------------------------------ *)
(* The scripted mock theory (trimmed copy of cert_emit_test's). *)

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
  ; units = []
  ; learned = [ mk_learned 20 [| na_ |] [ 21 ]; mk_learned 21 [| na_ |] [ 20 ] ]
  ; theory = []
  ; conclusion = Some (Sat.Level0_conflict { conflict_id = 20 })
  ; assumptions = []
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
  (* dropped hint: the chain can no longer reach a conflict. *)
  expect
    "corrupt: dropped hint -> INVALID"
    `Invalid
    (handbuilt ~learned_ants:[ 10; 11 ] ());
  (* permuted hints (order matters): [11;10;12] strands hint 11 (2 free literals) before
     the propagations that would make it unit. *)
  expect
    "corrupt: permuted hints -> INVALID"
    `Invalid
    (handbuilt ~learned_ants:[ 11; 10; 12 ] ());
  (* wrong antecedent SET:
     {10 ,11,11}
     (12 replaced by a duplicate) -> the second 11 is already satisfied, not unit. *)
  expect
    "corrupt: wrong antecedent set -> INVALID"
    `Invalid
    (handbuilt ~learned_ants:[ 10; 11; 11 ] ());
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
  (* UNSUPPORTED extension point: an empty theory Conflict leaf (unconditional T_conflict
     [], ADR-0013 Rev 6) has no v1 leaf witness for ⊥-from-∅ — the checker fails closed to
     UNSUPPORTED, never VALID. Hand-built: the terminal cites the empty theory conflict. *)
  let unsupported_empty_conflict : Checker.events =
    { Checker.inputs = [ mk_input 10 [| a_ |] ]
    ; units = []
    ; learned = []
    ; theory =
        [ ({ id = 30; clause = [||]; role = Sat.Conflict } : Recorder.theory_event) ]
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
