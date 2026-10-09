module Sat = Oxsmt_solver.Sat

(* Discriminating self-test for CNF preprocessing / bounded variable elimination
   (DESIGN.md A10; Jacobs 2021). The feature is env-gated (OXSMT_SATPRE) and read at
   [Sat.create], so this executable is meaningful only with the gate ON —
   [make satpre-test] runs it with OXSMT_SATPRE=1. Run without the gate it SKIPS (exit 0),
   so a bare [dune exec] is not a spurious failure.

   Two properties, both of which FAIL against a broken build:
   - FIRING: a formula of K "blocked triangles" whose clauses mention eliminable aux vars.
     Solved twice in this same (gate-ON) process — aux vars marked eliminable vs not —
     elimination empties the pivots' clauses, so the marked run does strictly fewer trail
     propagations than the unmarked run. Equal counts (no elimination) fails.
   - RECONSTRUCTION: every reported model must satisfy the ORIGINAL clauses (the note's
     Lemma 1 flip-to-satisfy). A cases where the eliminated var MUST be flipped to true is
     forced, so a "never flip" / wrong-pivot reconstruction is caught. UNSAT preservation
     is checked too (elimination must not turn UNSAT into SAT).

   Stdlib-only, deterministic. Nonzero exit on any failed check. *)

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

(* DIMACS literal (±v, 1-based) -> Sat literal (0-based var). *)
let to_lit l = if l > 0 then Sat.pos (l - 1) else Sat.neg (-l - 1)

(* Build a solver from dimacs clauses; mark the given 0-based vars eliminable. Vars are
   allocated up to [nvars] first so [set_eliminable] and the clauses agree on numbering. *)
let build nvars ~eliminable clauses =
  let s = Sat.create () in
  for _ = 1 to nvars do
    ignore (Sat.new_var s : Sat.var)
  done;
  List.iter (fun v -> Sat.set_eliminable s v) eliminable;
  List.iter (fun cl -> Sat.add_clause s (List.map to_lit cl)) clauses;
  s
;;

(* Does [model] (var-indexed) satisfy dimacs clause [cl]? *)
let clause_sat model cl =
  List.exists (fun l -> if l > 0 then model.(l - 1) else not model.(-l - 1)) cl
;;

let all_sat model clauses = List.for_all (clause_sat model) clauses

(* ---- FIRING: marked run does strictly fewer trail propagations than the unmarked run
   (elimination removes the pivots' clauses). Both must be SAT with a model satisfying the
   original clauses. ---- *)
let test_firing_fewer_propagations () =
  let k = 8 in
  let clauses = ref [] in
  let elim = ref [] in
  for i = 0 to k - 1 do
    let a = (3 * i) + 1
    and x = (3 * i) + 2
    and y = (3 * i) + 3 in
    (* (a∨x) (a∨y) (¬a∨¬x∨¬y): both resolvents on a are tautological, so a is blocked and
       all three clauses vanish; x,y then appear in no clause. *)
    clauses := [ a; x ] :: [ a; y ] :: [ -a; -x; -y ] :: !clauses;
    elim := (a - 1) :: !elim
  done;
  let clauses = List.rev !clauses in
  let run eliminable =
    let s = build (3 * k) ~eliminable clauses in
    let r = Sat.solve s in
    r, Sat.model s, (Sat.stats s).propagations
  in
  let r0, m0, p0 =
    run []
    (* unmarked: nothing eliminable *)
  in
  let r1, m1, p1 =
    run !elim
    (* marked *)
  in
  check "firing: unmarked sat" (r0 = Sat.Sat);
  check "firing: marked sat" (r1 = Sat.Sat);
  check "firing: unmarked model satisfies originals" (all_sat m0 clauses);
  check "firing: marked model satisfies originals" (all_sat m1 clauses);
  check (Printf.sprintf "firing: marked propagations < unmarked (%d < %d)" p1 p0) (p1 < p0)
;;

(* ---- RECONSTRUCTION (general BVE + forced flip). var1 is eliminated by adding the two
   resolvents; the rest of the formula forces the reduced model into the branch where var1
   MUST be reconstructed to true, so a broken reconstruction violates an original clause.
   -- *)
let test_reconstruction_forced_flip () =
  (* originals over var1(=1): (1∨2) (¬1∨3) (¬1∨4). Extra clauses force 2=false, so
     {2 ,3}
     and
     {2 ,4}
     (the resolvents) force 3=4=true, and var1's only positive clause (1∨2) is unsatisfied
     unless var1 is flipped true. *)
  let originals = [ [ 1; 2 ]; [ -1; 3 ]; [ -1; 4 ] ] in
  let forcing =
    [ [ -2; 5 ]; [ -2; -5 ] ]
    (* => 2 = false *)
  in
  let clauses = originals @ forcing in
  let s = build 5 ~eliminable:[ 0 ] clauses in
  let r = Sat.solve s in
  let model = Sat.model s in
  check "reconstruct: sat" (r = Sat.Sat);
  check "reconstruct: var1 reconstructed true (flip fired)" model.(0);
  check "reconstruct: model satisfies all original clauses" (all_sat model clauses)
;;

(* ---- PURE-literal elimination + reconstruction. var1 only positive; forced context
   makes its clauses unsatisfied unless var1 is set true by reconstruction. ---- *)
let test_pure_literal () =
  let originals = [ [ 1; 2 ]; [ 1; 3 ] ] in
  (* force 2=false and 3=false *)
  let forcing = [ [ -2; 4 ]; [ -2; -4 ]; [ -3; 5 ]; [ -3; -5 ] ] in
  let clauses = originals @ forcing in
  let s = build 5 ~eliminable:[ 0 ] clauses in
  let r = Sat.solve s in
  let model = Sat.model s in
  check "pure: sat" (r = Sat.Sat);
  check "pure: var1 reconstructed true" model.(0);
  check "pure: model satisfies all original clauses" (all_sat model clauses)
;;

(* ---- UNSAT preservation: a blocked var eliminates cleanly, but the rest is unsat; the
   verdict must stay UNSAT (elimination never manufactures a model). ---- *)
let test_unsat_preserved () =
  (* x(1),z(2) unsatisfiable; blocked triangle on a(3),p(4),q(5) that fully eliminates. *)
  let xz = [ [ 1; 2 ]; [ -1; 2 ]; [ 1; -2 ]; [ -1; -2 ] ] in
  let triangle = [ [ 3; 4 ]; [ 3; 5 ]; [ -3; -4; -5 ] ] in
  let s = build 5 ~eliminable:[ 2 ] (xz @ triangle) in
  check "unsat-preserved: unsat" (Sat.solve s = Sat.Unsat)
;;

(* ---- Self-subsuming resolution (strengthening) soundness. All eight 3-literal clauses
   over three variables forbid every assignment => UNSAT. Every clause is 3-literal, so
   strengthening fires heavily (each pair like (a∨b∨c)/(¬a∨b∨c) strengthens to (b∨c)); an
   UNSOUND literal drop would delete a constraint and flip this to SAT. No var is marked
   eliminable, so this isolates strengthening from BVE. Also a SAT companion (drop one
   clause) that must stay SAT with a model satisfying the originals. ---- *)
let all8 =
  [ [ 1; 2; 3 ]
  ; [ 1; 2; -3 ]
  ; [ 1; -2; 3 ]
  ; [ 1; -2; -3 ]
  ; [ -1; 2; 3 ]
  ; [ -1; 2; -3 ]
  ; [ -1; -2; 3 ]
  ; [ -1; -2; -3 ]
  ]
;;

let test_strengthening_unsat () =
  let s = build 3 ~eliminable:[] all8 in
  check "strengthen: all-8 3-var stays unsat" (Sat.solve s = Sat.Unsat)
;;

let test_strengthening_sat () =
  (* drop the last clause: now the assignment a=b=c=true is the unique model. *)
  let clauses = List.filteri (fun i _ -> i < 7) all8 in
  let s = build 3 ~eliminable:[] clauses in
  let r = Sat.solve s in
  let model = Sat.model s in
  check "strengthen: 7-of-8 sat" (r = Sat.Sat);
  check "strengthen: model satisfies originals" (all_sat model clauses)
;;

(* ---- PHASE-2 inprocessing integration. A pigeonhole PHP(6,5) is UNSAT and takes enough
   conflicts to cross the first restart, at which a restart-boundary inprocessing ROUND
   fires (make satpre-test sets OXSMT_SATPRE_INPROC_FIRST=1 so the first restart triggers
   one). Eliminable "blocked triangle" gadgets are added so the round actually rebuilds
   the clause DB (eliminates the gadget vars) and re-attaches the PHP learned clauses
   mid-search — exercising the learn/forget + re-attach path. The verdict must stay UNSAT:
   a round that dropped a needed ORIGINAL clause or corrupted the watch state would flip
   or crash. ---- *)
let php pigeons holes =
  (* var (i in hole j), 1-based: (i-1)*holes + j, for i in 1..pigeons, j in 1..holes. *)
  let v i j = ((i - 1) * holes) + j in
  let clauses = ref [] in
  for i = 1 to pigeons do
    clauses := List.init holes (fun j -> v i (j + 1)) :: !clauses
  done;
  for j = 1 to holes do
    for i = 1 to pigeons do
      for i' = i + 1 to pigeons do
        clauses := [ -v i j; -v i' j ] :: !clauses
      done
    done
  done;
  List.rev !clauses, pigeons * holes
;;

let test_inprocessing_unsat_preserved () =
  let php_clauses, nphp = php 6 5 in
  (* three blocked triangles on fresh vars nphp+1.., all eliminable *)
  let gadget = ref [] in
  let elim = ref [] in
  for k = 0 to 2 do
    let a = nphp + (3 * k) + 1
    and x = nphp + (3 * k) + 2
    and y = nphp + (3 * k) + 3 in
    gadget := [ a; x ] :: [ a; y ] :: [ -a; -x; -y ] :: !gadget;
    elim := (a - 1) :: !elim
  done;
  let clauses = php_clauses @ List.rev !gadget in
  let s = build (nphp + 9) ~eliminable:!elim clauses in
  let r = Sat.solve s in
  let st = Sat.stats s in
  check "inproc: PHP(6,5)+gadgets stays unsat" (r = Sat.Unsat);
  check
    (Printf.sprintf "inproc: search was nontrivial (conflicts=%d > 100)" st.conflicts)
    (st.conflicts > 100)
;;

(* ---- VIVIFICATION soundness. PHP(6,5) with the last mutual-exclusion clause dropped is
   SATISFIABLE (two pigeons may now share the freed hole) but still near-unsat, so it
   takes many conflicts → restart-boundary rounds fire, and with no theory plugged (raw
   SAT API) the learned clauses are VIVIFIED. An over-shortened (non-entailed) vivified
   clause would exclude the tight satisfying model → wrong UNSAT, or a returned model that
   violates a clause. So SAT + model-satisfies-originals is the discriminator. ---- *)
let test_vivification_sat () =
  (* PHP(8,7) minus its last exclusion clause: SAT (two pigeons may share the freed hole),
     but near-unsat, so it clears the first restart (>100 conflicts) and rounds fire → the
     learned clauses are vivified before the model is found. *)
  let php_clauses, nphp = php 8 7 in
  let m = List.length php_clauses in
  let clauses = List.filteri (fun i _ -> i < m - 1) php_clauses in
  let s = build nphp ~eliminable:[] clauses in
  let r = Sat.solve s in
  let model = Sat.model s in
  let st = Sat.stats s in
  check "vivify: PHP(8,7)-1 is sat" (r = Sat.Sat);
  check "vivify: model satisfies all clauses" (r = Sat.Sat && all_sat model clauses);
  check
    (Printf.sprintf "vivify: search cleared a restart (conflicts=%d > 100)" st.conflicts)
    (st.conflicts > 100)
;;

(* ---- EQUIVALENT-LITERAL SUBSTITUTION (ELS). x(1) ↔ y(2) via the two binaries; x marked
   eliminable, y frozen ⇒ ELS substitutes x→y (rep frozen) at solve entry. The (2∨4)(2∨¬4)
   pair forces y=true, so y's representative value is TRUE and x MUST be reconstructed to
   true — a broken definitional reconstruction (x left at its default false) violates the
   original (1∨3). So model.(0)=true + model satisfies originals + x=y discriminate. ---- *)
let test_els_sat () =
  let clauses =
    [ [ -1; 2 ]
    ; [ 1; -2 ] (* x ↔ y *)
    ; [ 1; 3 ] (* x ∨ z : needs x's reconstructed value *)
    ; [ 2; 4 ]
    ; [ 2; -4 ] (* force y = true *)
    ]
  in
  let s = build 4 ~eliminable:[ 0 ] clauses in
  let r = Sat.solve s in
  let model = Sat.model s in
  check "els: sat" (r = Sat.Sat);
  check "els: y forced true" (r = Sat.Sat && model.(1));
  check
    "els: x reconstructed = y (true)"
    (r = Sat.Sat && model.(0) = model.(1) && model.(0));
  check "els: model satisfies all original clauses" (r = Sat.Sat && all_sat model clauses)
;;

(* ---- ELS UNSAT preservation. x ↔ y, x forced true, y forced false ⇒ unsat; whether ELS
   substitutes or its dry run skips (exposed-unsat), the verdict must stay UNSAT. ---- *)
let test_els_unsat () =
  let clauses =
    [ [ -1; 2 ]
    ; [ 1; -2 ] (* x ↔ y *)
    ; [ 1; 5 ]
    ; [ 1; -5 ] (* force x = true *)
    ; [ -2; 6 ]
    ; [ -2; -6 ] (* force y = false *)
    ]
  in
  let s = build 6 ~eliminable:[ 0 ] clauses in
  check "els: x↔y with x=true,y=false stays unsat" (Sat.solve s = Sat.Unsat)
;;

(* ---- FAILED-LITERAL PROBING. Assuming x(1) triggers (¬x∨y)∧(¬x∨¬y) ⇒ y and ¬y ⇒
   conflict, so x is a FAILED literal and ¬x is entailed; FLP enqueues x=false at level 0.
   The (x∨z) clause then forces z=true. A broken FLP that forced x=true instead would
   violate (¬x∨y)/(¬x∨¬y). So model.(0)=false + model satisfies originals discriminate.
   ---- *)
let test_flp_sat () =
  let clauses = [ [ -1; 2 ]; [ -1; -2 ] (* x failed ⇒ ¬x *); [ 1; 3 ] (* x ∨ z *) ] in
  let s = build 3 ~eliminable:[] clauses in
  let r = Sat.solve s in
  let model = Sat.model s in
  check "flp: sat" (r = Sat.Sat);
  check "flp: x forced false by probing" (r = Sat.Sat && not model.(0));
  check "flp: model satisfies all original clauses" (r = Sat.Sat && all_sat model clauses)
;;

(* ---- FLP UNSAT: both polarities of x are failed literals (assuming x conflicts via
   (¬x∨y)(¬x∨¬y); assuming ¬x conflicts via (x∨a)(x∨¬a)) ⇒ unsat. ---- *)
let test_flp_unsat () =
  let clauses = [ [ -1; 2 ]; [ -1; -2 ]; [ 1; 4 ]; [ 1; -4 ] ] in
  let s = build 4 ~eliminable:[] clauses in
  check "flp: both-polarities-failed stays unsat" (Sat.solve s = Sat.Unsat)
;;

(* ---- F1 REGRESSION (the adjudication gadget, a decisive-blocker golden). ELS collapses
   three x↔y equivalences whose OR-clauses rewrite to the forced units {var2,var4,var6}=true
   (each enqueued by [els_pass] WITHOUT being propagated); those units falsify (¬2∨¬4∨¬6),
   so the formula is genuinely UNSAT. In the unfixed core, failed-literal probing (probing
   the free var7) ran its assume/propagate/cancel_until from the STALE qhead: its [propagate]
   processed the pending ELS units first, mis-attributed their level-0 conflict to the probe
   decision, and [cancel_until 0] advanced qhead PAST the units — orphaning the conflict —
   so search returned SAT with a model (var2/4/6 true) violating (¬2∨¬4∨¬6). The round-level
   fix closes the level-0 trail under BCP after [els_pass] and concludes Unsat on that
   conflict. RED against the unfixed core (returns Sat with an original-clause-violating
   model); GREEN after. ---- *)
let test_els_flp_orphan_unsat () =
  let clauses =
    [ [ -1; 2 ]
    ; [ 1; -2 ]
    ; [ 1; 2 ] (* var1 ↔ var2, and (var1 ∨ var2) ⇒ both forced true *)
    ; [ -3; 4 ]
    ; [ 3; -4 ]
    ; [ 3; 4 ]
    ; [ -5; 6 ]
    ; [ 5; -6 ]
    ; [ 5; 6 ]
    ; [ -2; -4; -6 ] (* falsified by the three forced units ⇒ UNSAT *)
    ]
  in
  (* 7 vars: var1/3/5 eliminable (substituted onto frozen var2/4/6); var7 is a free
     variable that failed-literal probing will probe (the trigger in the unfixed core). *)
  let s = build 7 ~eliminable:[ 0; 2; 4 ] clauses in
  let r = Sat.solve s in
  check "els+flp: forced-unit orphan gadget stays unsat" (r = Sat.Unsat);
  (* on the buggy path [r = Sat] with a model that violates (¬2∨¬4∨¬6) — surface both *)
  if r = Sat.Sat
  then
    check
      "els+flp: (buggy) model violates an original clause"
      (all_sat (Sat.model s) clauses)
;;

(* ---- F2 REGRESSION (cross-round ELS reconstruction chain). [t.equiv] persists across
   solves, so a variable chosen as an ELS representative in one solve can itself be
   ELS-eliminated in a later solve, forming a chain B→A→C. Two solves on ONE core:
   - solve 1: A(var0) ↔ B(var2), both eliminable ⇒ rep is the lowest lit A; B is
     eliminated onto A (equiv[B]=+A). A stays live (a representative is not itself
     eliminated).
   - solve 2: add A ↔ C (C=var1 frozen) ⇒ rep is the frozen C, so A is now eliminated onto
     C (equiv[A]=+C). Chain: equiv[B]=+A, equiv[A]=+C. C is forced TRUE only via
     failed-literal probing ((C∨w) with w a failed literal), never a level-0 unit at round
     entry, so A stays free for ELS to eliminate this round. B ↔ A ↔ C, so B and C must
     agree in the model. The unfixed [save_model] did a SINGLE-HOP read under an unordered
     [Hashtbl.iter]: for key B it read A's snapshot value, but [iter] visits key B(=var2)
     before key A(=var0) (deterministic, non-randomized Hashtbl), so B read A's DEFAULT
     (false) while C=true ⇒ B=false, a model violating the B↔C equivalence. The fix
     chain-resolves each var to its ultimate representative. RED against the unfixed core
     (B≠C); GREEN after. ---- *)
let test_els_cross_round_chain () =
  let s = Sat.create () in
  for _ = 1 to 6 do
    ignore (Sat.new_var s : Sat.var)
  done;
  (* A(0), B(2) eliminable; C(1), w(3), u(4), e(5) frozen *)
  Sat.set_eliminable s 0;
  Sat.set_eliminable s 2;
  let add cls = List.iter (fun cl -> Sat.add_clause s (List.map to_lit cl)) cls in
  (* solve 1: A ↔ B, plus (A∨e)(¬A∨e) as A's "home" clauses. They survive the B→A
     substitution (they name no B); their only resolvent on A is the UNIT [e], which
     disqualifies A from bounded variable elimination, so A stays live (un-eliminated) as
     the representative after solve 1 — ready to be re-eliminated in solve 2. Without a
     home clause BVE would drop A as an absent variable and no chain could form. *)
  add [ [ -1; 3 ]; [ 1; -3 ]; [ 1; 6 ]; [ -1; 6 ] ];
  let r1 = Sat.solve s in
  check "chain: solve1 sat" (r1 = Sat.Sat);
  add [ [ -1; 2 ]; [ 1; -2 ]; [ 2; 4 ]; [ -4; 5 ]; [ -4; -5 ] ];
  (* solve 2: A ↔ C, plus (C∨w) and w a failed literal ⇒ probing forces C=true *)
  let r2 = Sat.solve s in
  let m = Sat.model s in
  check "chain: solve2 sat" (r2 = Sat.Sat);
  check "chain: C(var1) forced true" (r2 = Sat.Sat && m.(1));
  check
    "chain: B(var2) agrees with C(var1) via chain-resolved reconstruction"
    (r2 = Sat.Sat && Bool.equal m.(2) m.(1))
;;

(* ---- F2 chain, COMPLEMENTED edge (R2 hardening). Same two-solve shape as above, but
   solve 2 links A to C with OPPOSITE polarity (A ↔ ¬C), so the recorded chain is
   equiv[B]=+A, equiv[A]=¬C. This exercises the SIGN-recursion branch of [save_model]'s
   [resolve] (the positive-only chain above never does): B must resolve to ¬C, i.e. B and
   C DISAGREE. C is forced FALSE (probing: C=true triggers (¬C∨p)(¬C∨¬p)), so B must be
   true. The unfixed single-hop reconstruction, visiting key B before key A, reads A's
   default (false) → B=false while ¬C=true — wrong. (Still order-sensitive like any
   single-hop reproducer, but it additionally guards the negative [resolve] edge.) ---- *)
let test_els_cross_round_chain_complemented () =
  let s = Sat.create () in
  for _ = 1 to 6 do
    ignore (Sat.new_var s : Sat.var)
  done;
  (* A(0), B(2) eliminable; C(1), p(3), q(4), e(5) frozen *)
  Sat.set_eliminable s 0;
  Sat.set_eliminable s 2;
  let add cls = List.iter (fun cl -> Sat.add_clause s (List.map to_lit cl)) cls in
  add [ [ -1; 3 ]; [ 1; -3 ]; [ 1; 6 ]; [ -1; 6 ] ] (* solve 1: A ↔ B, + A home clauses *);
  let r1 = Sat.solve s in
  check "chain¬: solve1 sat" (r1 = Sat.Sat);
  (* solve 2: A ↔ ¬C via (A∨C)(¬A∨¬C) ⇒ rep is frozen ¬C, equiv[A]=¬C; (¬C∨p)(¬C∨¬p) make
     C=true a failed literal ⇒ probing forces C=false. *)
  add [ [ 1; 2 ]; [ -1; -2 ]; [ -2; 4 ]; [ -2; -4 ] ];
  let r2 = Sat.solve s in
  let m = Sat.model s in
  check "chain¬: solve2 sat" (r2 = Sat.Sat);
  check "chain¬: C(var1) forced false" (r2 = Sat.Sat && not m.(1));
  check
    "chain¬: B(var2) = ¬C(var1) via sign-correct chain reconstruction"
    (r2 = Sat.Sat && Bool.equal m.(2) (not m.(1)))
;;

(* ---- R1 REGRESSION (no-occurrence BVE restore). A zero-occurrence eliminable var is
   marked eliminated by the [np = 0 && nn = 0] BVE branch. On an incremental re-add of a
   clause naming it, the restore hook must UN-ELIMINATE it — else it stays frozen out of
   [pick_branch], the re-added clause can never be satisfied, and the solver reports a
   wrong Sat with the var defaulted false. Both x,y eliminable, solved with NO clauses
   (both become zero-occurrence ⇒ eliminated), then (x∨y) is added and re-solved: the
   model must satisfy (x∨y). RED against the unfixed branch (returns Sat with x=y=false,
   violating (x∨y)); GREEN after recording an empty restore-map entry so restore
   un-eliminates. ---- *)
let test_bve_no_occurrence_restore () =
  let s = Sat.create () in
  for _ = 1 to 2 do
    ignore (Sat.new_var s : Sat.var)
  done;
  Sat.set_eliminable s 0;
  Sat.set_eliminable s 1;
  let r1 = Sat.solve s in
  check "bve-noocc: empty solve sat" (r1 = Sat.Sat);
  Sat.add_clause s (List.map to_lit [ 1; 2 ]) (* (x ∨ y), names both eliminated vars *);
  let r2 = Sat.solve s in
  let m = Sat.model s in
  check "bve-noocc: re-add (x∨y) still sat" (r2 = Sat.Sat);
  check "bve-noocc: model satisfies the re-added (x∨y)" (r2 = Sat.Sat && (m.(0) || m.(1)))
;;

let () =
  match Sys.getenv_opt "OXSMT_SATPRE" with
  | Some ("1" | "true" | "yes" | "on") ->
    test_firing_fewer_propagations ();
    test_els_sat ();
    test_els_unsat ();
    test_els_flp_orphan_unsat ();
    test_els_cross_round_chain ();
    test_els_cross_round_chain_complemented ();
    test_bve_no_occurrence_restore ();
    test_flp_sat ();
    test_flp_unsat ();
    test_reconstruction_forced_flip ();
    test_pure_literal ();
    test_unsat_preserved ();
    test_strengthening_unsat ();
    test_strengthening_sat ();
    test_inprocessing_unsat_preserved ();
    test_vivification_sat ();
    Printf.printf "satpre_test: %d checks, %d failures\n" !checks !failures;
    if !failures > 0 then exit 1
  | Some _ | None ->
    Printf.printf "satpre_test: SKIP (set OXSMT_SATPRE=1 to run; make satpre-test)\n"
;;
