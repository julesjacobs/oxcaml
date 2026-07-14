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

let () =
  match Sys.getenv_opt "OXSMT_SATPRE" with
  | Some ("1" | "true" | "yes" | "on") ->
    test_firing_fewer_propagations ();
    test_reconstruction_forced_flip ();
    test_pure_literal ();
    test_unsat_preserved ();
    test_strengthening_unsat ();
    test_strengthening_sat ();
    Printf.printf "satpre_test: %d checks, %d failures\n" !checks !failures;
    if !failures > 0 then exit 1
  | Some _ | None ->
    Printf.printf "satpre_test: SKIP (set OXSMT_SATPRE=1 to run; make satpre-test)\n"
;;
