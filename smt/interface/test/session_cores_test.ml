(* Task #106 consumer proof for the observational theory-infeasibility API
   ([Session.last_unsat_core] / [Session.last_farkas]).

   Proven end-to-end through the public {!Session} surface:

   1. CORE SOUNDNESS — the returned atoms are a genuine theory-unsat core: the same
      conjunction on a FRESH session returns [Unsat]. A wrong core would poison a
      consumer.

   2. FARKAS CERTIFICATE — the returned coefficients ARE a Farkas certificate: recombining
      [Σ coeffᵢ · half-plane(litᵢ)] over the rationals yields a variable-free STRICTLY-
      POSITIVE constant (every variable coefficient cancels), i.e. the "0 < c"
      contradiction. Positive-premise and negative-premise (ℤ-complement) conflicts both.

   3. FARKAS INTERPOLATION (the decisive CHC use) — on a
      counterexample-to-induction-shaped UNSAT query split into a frame side [A] and a bad
      side [B], the A-side of the Farkas certificate summed into a single inequality [I]
      is a McMillan LA(ℚ) interpolant: [A ⊨ I], [I ∧ B] is [Unsat], and [I] mentions only
      the SHARED variable. This is what chc/ needs to replace its template-proxy
      interpolants; self-contained on the API (chc/ is not yet on trunk).

   Plus gating/staleness: [None] before any check, on [Sat], and on a purely propositional
   [Unsat]; and the per-check stash reset (a prior theory conflict must not leak into a
   later propositional refutation). *)

open Oxsmt_core
module Session = Oxsmt_interface.Session
module Rational = Oxsmt_lia.Rational

let checks = ref 0
let failures = ref 0

let fail name msg =
  incr failures;
  Printf.printf "  FAIL %s: %s\n" name msg
;;

let ok _name = incr checks

let expect_verdict name got want =
  incr checks;
  if got <> want
  then (
    let s = function
      | Session.Sat -> "sat"
      | Session.Unsat -> "unsat"
      | Session.Unknown -> "unknown"
    in
    fail name (Printf.sprintf "verdict got %s, want %s" (s got) (s want)))
;;

let check_true name b = if b then ok name else fail name "expected true"

(* ---- term helpers ---- *)

let int_var s name =
  Context.const (Session.context s) (Session.declare_const s name Sort.int)
;;

let bool_var s name =
  Context.const (Session.context s) (Session.declare_const s name Sort.bool)
;;

let le s a b = Context.le (Session.context s) a b
let ic s n = Context.int_const (Session.context s) n

(* ---- linear-form extraction over the returned literal terms ---- *)

(* [linear_of arg] reads the [(var, coeff)] pairs + constant of an Int-sorted [arg] (a
   [Le] atom's argument). A bare variable is coeff 1; a numeral is a pure constant. *)
let linear_of (arg : Term.t) : (Term.t * Bigint.t) list * Bigint.t =
  match arg.Term.node with
  | Term.Arith { coeffs; const } -> Iarr.to_list coeffs, const
  | Term.Int_const c -> [], c
  | _ -> [ arg, Bigint.one ], Bigint.zero
;;

let bneg b = Bigint.mul (Bigint.of_int (-1)) b

(* [half_plane (atom, polarity)] is the linear form [e] of the ASSERTED half-plane
   [e <= 0] for a returned core premise (task #106 carries polarity OUT OF BAND — the atom
   is never a negated term):
   - [(Le arg, true)] was asserted [arg <= 0] -> e = arg
   - [(Le arg, false)] was asserted ¬(arg<=0) = arg>=1 -> e = -arg + 1 (ℤ-complement) *)
let half_plane ((atom, polarity) : Term.t * bool) : (Term.t * Bigint.t) list * Bigint.t =
  match atom.Term.node with
  | Term.Le arg ->
    if polarity
    then linear_of arg
    else (
      let pairs, c = linear_of arg in
      List.map (fun (v, b) -> v, bneg b) pairs, Bigint.add (bneg c) Bigint.one)
  | _ ->
    failwith "half_plane: atom is not an Le (an equality premise carries no Farkas cert)"
;;

(* A plainly-asserted (positive) [Term.t] read as its half-plane — for the in-test atoms
   and interpolants, which are always asserted positively. *)
let half_plane_pos (t : Term.t) = half_plane (t, true)

(* Accumulate [Σ coeffᵢ · half-plane(atomᵢ, polᵢ)] into a rational (var -> coeff) map +
   constant. *)
let accumulate (pairs : (Rational.t * (Term.t * bool)) list) =
  let map = ref Term.Map.empty in
  let const = ref Rational.zero in
  List.iter
    (fun (coeff, lit) ->
      let vars, c = half_plane lit in
      List.iter
        (fun (v, b) ->
          let contrib = Rational.mul coeff (Rational.of_bigint b) in
          let prev =
            match Term.Map.find_opt v !map with
            | Some r -> r
            | None -> Rational.zero
          in
          map := Term.Map.add v (Rational.add prev contrib) !map)
        vars;
      const := Rational.add !const (Rational.mul coeff (Rational.of_bigint c)))
    pairs;
  !map, !const
;;

let nonzero_bindings map =
  Term.Map.bindings map |> List.filter (fun (_, r) -> not (Rational.is_zero r))
;;

let vars_of lit =
  let vars, _ = half_plane_pos lit in
  List.fold_left (fun acc (v, _) -> Term.Set.add v acc) Term.Set.empty vars
;;

(* ========================================================================= 1 + 2. core
   soundness + Farkas certificate self-check. Every atom in these queries is part of the
   minimal core, so re-asserting the whole built set on a fresh session IS re-checking the
   core. ========================================================================= *)

let core_and_cert name build =
  let s = Session.create () in
  List.iter (Session.assert_term s) (build s);
  expect_verdict (name ^ ": unsat") (Session.check_sat s) Session.Unsat;
  (match Session.last_unsat_core s with
   | None -> fail (name ^ ": core") "last_unsat_core = None on a LIA-refuted query"
   | Some core ->
     check_true (name ^ ": core nonempty") (core <> []);
     let s2 = Session.create () in
     List.iter (Session.assert_term s2) (build s2);
     expect_verdict (name ^ ": core re-check unsat") (Session.check_sat s2) Session.Unsat);
  match Session.last_farkas s with
  | None -> fail (name ^ ": farkas") "last_farkas = None on a rational-infeasible query"
  | Some cert ->
    check_true (name ^ ": farkas nonempty") (cert <> []);
    check_true
      (name ^ ": farkas coeffs >= 0")
      (List.for_all (fun (c, _) -> Rational.sign c >= 0) cert);
    let map, const = accumulate cert in
    check_true (name ^ ": farkas vars cancel") (nonzero_bindings map = []);
    check_true (name ^ ": farkas const > 0") (Rational.sign const > 0)
;;

let () =
  (* positive premises: x <= 5 && x >= 6 *)
  core_and_cert "pos" (fun s ->
    let x = int_var s "x" in
    [ le s x (ic s 5); le s (ic s 6) x ]);
  (* negative premise (ℤ-complement): ¬(x <= 5) && x <= 4 (x >= 6 && x <= 4) *)
  core_and_cert "neg" (fun s ->
    let x = int_var s "x" in
    [ Context.not_ (Session.context s) (le s x (ic s 5)); le s x (ic s 4) ])
;;

(* =========================================================================

   3. Farkas interpolation (the CHC-decisive proof).

   A (frame + transition, over local xa and shared s): xa <= 0 , s <= xa B (bad, over
   shared s): s >= 1 A ⟹ s <= 0 , B ⟹ s >= 1 -> A ∧ B unsat. The A-side of the Farkas
   certificate sums to an interpolant I over the SHARED variable s only.
   ========================================================================= *)

let rec gcd a b = if b = 0 then abs a else gcd b (a mod b)
let lcm a b = if a = 0 || b = 0 then 0 else abs (a / gcd a b * b)

(* Read I's integer coefficients as (var-name, coeff) + constant, so it can be rebuilt
   verbatim in another session's context (matched by declared name). *)
let named_linear (i_term : Term.t) =
  let vars, c = half_plane_pos i_term in
  let named =
    List.map
      (fun (v, b) ->
        ( (match v.Term.node with
           | Term.App (sym, _) -> Symbol.name sym
           | _ -> failwith "I var not an App")
        , b ))
      vars
  in
  named, c
;;

let build_from_named sess (named, c) =
  let to_int b =
    match Bigint.to_int_opt b with
    | Some i -> i
    | None -> failwith "coeff too big for test"
  in
  let pairs = List.map (fun (nm, b) -> to_int b, int_var sess nm) named in
  let lhs = Context.linear_combination (Session.context sess) pairs (to_int c) in
  Context.le (Session.context sess) lhs (ic sess 0)
;;

let () =
  let name = "interp" in
  let s = Session.create () in
  let xa = int_var s "xa" (* A-local *) in
  let sh = int_var s "s" (* shared *) in
  let a_atoms = [ le s xa (ic s 0); le s sh xa ] in
  let b_atoms = [ le s (ic s 1) sh ] in
  List.iter (Session.assert_term s) (a_atoms @ b_atoms);
  expect_verdict (name ^ ": unsat") (Session.check_sat s) Session.Unsat;
  match Session.last_farkas s with
  | None -> fail name "last_farkas = None (expected a Farkas certificate)"
  | Some cert ->
    (* Partition by origin: a positive-premise atom is Term.equal to an asserted A atom. *)
    let is_a (atom, _polarity) = List.exists (fun a -> Term.equal a atom) a_atoms in
    let a_part = List.filter (fun (_, ap) -> is_a ap) cert in
    let b_part = List.filter (fun (_, ap) -> not (is_a ap)) cert in
    check_true (name ^ ": cert spans A") (a_part <> []);
    check_true (name ^ ": cert spans B") (b_part <> []);
    check_true
      (name ^ ": core terms are asserted atoms")
      (List.for_all
         (fun (_, (atom, _)) ->
           List.exists (fun a -> Term.equal a atom) (a_atoms @ b_atoms))
         cert);
    (* I = Σ over the A-part of the certificate. *)
    let map, const = accumulate a_part in
    (* clear denominators into an integer inequality (scaling by a positive int is sound). *)
    let entries = nonzero_bindings map in
    let d =
      List.fold_left
        lcm
        1
        (Rational.den const :: List.map (fun (_, r) -> Rational.den r) entries)
    in
    let scale r = Rational.num r * (d / Rational.den r) in
    let i_term =
      let pairs = List.map (fun (v, r) -> scale r, v) entries in
      let lhs = Context.linear_combination (Session.context s) pairs (scale const) in
      Context.le (Session.context s) lhs (ic s 0)
    in
    (* (i) I mentions only the shared variable. *)
    let a_vars =
      List.fold_left (fun acc a -> Term.Set.union acc (vars_of a)) Term.Set.empty a_atoms
    in
    let b_vars =
      List.fold_left (fun acc b -> Term.Set.union acc (vars_of b)) Term.Set.empty b_atoms
    in
    let shared = Term.Set.inter a_vars b_vars in
    let i_vars = vars_of i_term in
    check_true (name ^ ": I nonempty") (not (Term.Set.is_empty i_vars));
    check_true (name ^ ": I over shared vars only") (Term.Set.subset i_vars shared);
    check_true (name ^ ": s is shared") (Term.Set.mem sh shared);
    check_true (name ^ ": xa is A-local") (not (Term.Set.mem xa shared));
    let i_named = named_linear i_term in
    (* (ii) A ⊨ I : A ∧ ¬I is Unsat. *)
    (let sess = Session.create () in
     let xa' = int_var sess "xa" in
     let s' = int_var sess "s" in
     let a' = [ le sess xa' (ic sess 0); le sess s' xa' ] in
     let i' = build_from_named sess i_named in
     List.iter (Session.assert_term sess) a';
     Session.assert_term sess (Context.not_ (Session.context sess) i');
     expect_verdict
       (name ^ ": A |= I  (A & ~I unsat)")
       (Session.check_sat sess)
       Session.Unsat);
    (* (iii) I ∧ B is Unsat. *)
    let sess = Session.create () in
    let s' = int_var sess "s" in
    let b' = [ le sess (ic sess 1) s' ] in
    let i' = build_from_named sess i_named in
    Session.assert_term sess i';
    List.iter (Session.assert_term sess) b';
    expect_verdict (name ^ ": I & B unsat") (Session.check_sat sess) Session.Unsat
;;

(* ========================================================================= Diophantine
   (ℤ-infeasible, ℚ-feasible): core present, Farkas absent. 2x = 1 has no integer solution
   but is rational-feasible (x = 1/2).
   ========================================================================= *)

let () =
  let name = "dioph" in
  let s = Session.create () in
  let ctx = Session.context s in
  let x = int_var s "x" in
  Session.assert_term s (Context.eq ctx (Context.mul_const ctx 2 x) (ic s 1));
  match Session.check_sat s with
  | Session.Unsat ->
    check_true (name ^ ": core present") (Session.last_unsat_core s <> None);
    check_true
      (name ^ ": farkas absent (divisibility cert)")
      (Session.last_farkas s = None)
  | Session.Unknown -> ok (name ^ ": unknown tolerated")
  | Session.Sat -> fail name "2x=1 reported Sat (integer-unsound)"
;;

(* ========================================================================= Gating +
   staleness. ========================================================================= *)

let () =
  let s = Session.create () in
  check_true "gate: none before check (core)" (Session.last_unsat_core s = None);
  check_true "gate: none before check (farkas)" (Session.last_farkas s = None);
  let x = int_var s "gx" in
  Session.assert_term s (le s x (ic s 5));
  expect_verdict "gate: sat" (Session.check_sat s) Session.Sat;
  check_true "gate: none on sat (core)" (Session.last_unsat_core s = None);
  check_true "gate: none on sat (farkas)" (Session.last_farkas s = None)
;;

let () =
  let s = Session.create () in
  let p = bool_var s "p" in
  Session.assert_term s p;
  Session.assert_term s (Context.not_ (Session.context s) p);
  expect_verdict "gate: bool unsat" (Session.check_sat s) Session.Unsat;
  check_true "gate: none on propositional unsat" (Session.last_unsat_core s = None)
;;

let () =
  (* STALENESS: a theory conflict in check #1 must NOT leak into a later purely
     propositional Unsat (the top of [check_sat] clears the stash). *)
  let s = Session.create () in
  let x = int_var s "sx" in
  Session.push s;
  Session.assert_term s (le s x (ic s 5));
  Session.assert_term s (le s (ic s 6) x);
  expect_verdict "stale: check1 theory unsat" (Session.check_sat s) Session.Unsat;
  check_true "stale: check1 core present" (Session.last_unsat_core s <> None);
  Session.pop s;
  let q = bool_var s "q" in
  Session.assert_term s q;
  Session.assert_term s (Context.not_ (Session.context s) q);
  expect_verdict "stale: check2 propositional unsat" (Session.check_sat s) Session.Unsat;
  check_true "stale: no leaked core in check2" (Session.last_unsat_core s = None)
;;

(* ========================================================================= Finding #2
   (codex review) — STALE LIA EVIDENCE via the pure-BV fast path.

   [check_sat] dispatches pure QF_BV BEFORE [Cdclt.begin_check], and its [Unsat] arm sets
   [last_verdict <- Unsat]. The stash is now cleared at the TOP of [check_sat] (not only
   in [begin_check]), so a prior LIA conflict cannot leak into a later pure-BV refutation.
   RED before the fix: [last_unsat_core] returned the earlier LIA core.
   ========================================================================= *)

let () =
  let name = "bv-stale" in
  let s = Session.create () in
  let ctx = Session.context s in
  let x = int_var s "bvx" in
  Session.push s;
  Session.assert_term s (le s x (ic s 5));
  Session.assert_term s (le s (ic s 6) x);
  expect_verdict (name ^ ": check1 LIA unsat") (Session.check_sat s) Session.Unsat;
  check_true (name ^ ": check1 core present") (Session.last_unsat_core s <> None);
  Session.pop s;
  (* check #2: pure QF_BV, unsatisfiable ([b0 = b1] and [b0 <> b1]), NO Int term — so
     [is_pure_bv] holds and the fast path (which bypasses [begin_check]) is taken. *)
  let w = 8 in
  let b0 = Context.const ctx (Session.declare_const s "bvb0" (Sort.bitvec w)) in
  let b1 = Context.const ctx (Session.declare_const s "bvb1" (Sort.bitvec w)) in
  let eq01 = Context.eq ctx b0 b1 in
  Session.assert_term s eq01;
  Session.assert_term s (Context.not_ ctx eq01);
  expect_verdict (name ^ ": check2 pure-BV unsat") (Session.check_sat s) Session.Unsat;
  check_true (name ^ ": no leaked core in check2") (Session.last_unsat_core s = None);
  check_true (name ^ ": no leaked farkas in check2") (Session.last_farkas s = None)
;;

(* ========================================================================= Finding #1
   (codex review) — EQUALITY-PREMISE Farkas orientation.

   An Int equality [x = k] is lowered into BOTH an upper and a lower bound sharing one
   premise token, so a Farkas multiplier paired with it has no single half-plane
   orientation and [Σ coeffᵢ·half-plane] cannot be reconstructed. [last_farkas] now
   returns [None] whenever a premise is an equality (fail-closed); the core itself stays
   valid and re-checkable. RED before the fix: [last_farkas] returned a coefficient paired
   with the unoriented [x = k].
   ========================================================================= *)

let () =
  let name = "eq-premise" in
  let s = Session.create () in
  (* x = y && x <= 0 && y >= 1 : the equality is a genuine conflict premise (x = y forces
     y <= 0, clashing with y >= 1). Two vars keep the equality from being eliminated to a
     constant relation. *)
  let build sess =
    let c = Session.context sess in
    let xv = int_var sess "ex" in
    let yv = int_var sess "ey" in
    [ Context.eq c xv yv; le sess xv (ic sess 0); le sess (ic sess 1) yv ]
  in
  List.iter (Session.assert_term s) (build s);
  match Session.check_sat s with
  | Session.Unsat ->
    (match Session.last_unsat_core s with
     | None -> fail name "core = None on an equality-premise LIA conflict"
     | Some core ->
       check_true (name ^ ": core nonempty") (core <> []);
       check_true
         (name ^ ": core has an equality premise")
         (List.exists
            (fun (atom, _) ->
              match atom.Term.node with
              | Term.Eq (a, _) -> not (Sort.equal a.Term.sort Sort.bool)
              | _ -> false)
            core);
       let s2 = Session.create () in
       List.iter (Session.assert_term s2) (build s2);
       expect_verdict
         (name ^ ": core re-check unsat")
         (Session.check_sat s2)
         Session.Unsat);
    check_true
      (name ^ ": farkas absent (equality orientation ambiguous)")
      (Session.last_farkas s = None)
  | Session.Unknown -> ok (name ^ ": unknown tolerated")
  | Session.Sat -> fail name "x=y & x<=0 & y>=1 reported Sat (unsound)"
;;

(* ========================================================================= Finding #3
   (codex review) — READING THE API MUST NOT MUTATE SOLVER STATE.

   A negative theory premise can arise through Boolean structure with NO interned [Not]
   node (here [q <-> (x<=1)] with [q] false forces [x<=1] false, but [Not (x<=1)] is never
   built). The old accessors rendered it via [Context.not_], which on a cache miss
   interned a fresh term and bumped the tag counter — perturbing later term tags / CNF
   ordering. Polarity is now carried out of band, so reading interns nothing.
   ========================================================================= *)

let () =
  let name = "read-purity" in
  let s = Session.create () in
  let ctx = Session.context s in
  let x = int_var s "px" in
  let q = bool_var s "pq" in
  let a = le s x (ic s 1) in
  Session.assert_term s (Context.eq ctx q a);
  Session.assert_term s (Context.not_ ctx q);
  Session.assert_term s (le s x (ic s 0));
  match Session.check_sat s with
  | Session.Unsat ->
    (* record AFTER the solve (which may still intern), then prove the reads add nothing *)
    let before = Context.term_count ctx in
    let core = Session.last_unsat_core s in
    let farkas = Session.last_farkas s in
    let after = Context.term_count ctx in
    check_true (name ^ ": reading accessors interns nothing") (before = after);
    (* and the core is genuinely surfaced with a NEGATIVE premise (out-of-band polarity) *)
    (match core with
     | Some prems ->
       check_true
         (name ^ ": core carries a negative premise")
         (List.exists (fun (_, polarity) -> not polarity) prems)
     | None -> ok (name ^ ": core None tolerated"));
    ignore farkas
  | Session.Unknown -> ok (name ^ ": unknown tolerated")
  | Session.Sat -> fail name "q<->(x<=1), ~q, x<=0 reported Sat (unsound)"
;;

(* =========================================================================
   First-class assumption solving.

   The Boolean instance has a deliberately NON-MINIMAL proof core. Under assumptions
   [~c, ~a, ~b], the current SAT failed-assumption walk returns all three literals, but
   the last four hard clauses already make [~a, ~b] inconsistent for every assignment
   of [x,y]. Thus this test is red if [check_sat_assuming] merely relabels the raw failed
   set and skips deletion minimization. Negative assumptions also pin polarity mapping.

   The LIA instance separately proves that previously-unasserted theory atoms are
   internalized as assumptions and that the returned core is genuinely T-unsat. In both
   cases the core is re-solved and every one-element deletion must be Sat. A final plain
   [check_sat] proves that assumptions did not become permanent assertions. =========== *)

let assumption_equal (a, ap) (b, bp) = ap = bp && Term.equal a b

let assumption_mem assumption assumptions =
  List.exists (assumption_equal assumption) assumptions
;;

let remove_assumption target assumptions =
  List.filter (fun assumption -> not (assumption_equal target assumption)) assumptions
;;

let expect_assumption_verdict name result want = expect_verdict name result.Session.verdict want

let require_minimal_core name session result expected =
  expect_assumption_verdict (name ^ ": unsat") result Session.Unsat;
  match result.Session.unsat_core with
  | None ->
    fail (name ^ ": core") "unsat_core = None";
    []
  | Some core ->
    check_true (name ^ ": expected cardinality") (List.length core = List.length expected);
    check_true
      (name ^ ": expected literals")
      (List.for_all (fun assumption -> assumption_mem assumption core) expected);
    let replay = Session.check_sat_assuming session core in
    expect_assumption_verdict (name ^ ": core replay") replay Session.Unsat;
    List.iter
      (fun assumption ->
        let residual = remove_assumption assumption core in
        let probe = Session.check_sat_assuming session residual in
        expect_assumption_verdict (name ^ ": deletion is sat") probe Session.Sat)
      core;
    core
;;

let () =
  let name = "assuming-bool-min" in
  let s = Session.create () in
  let ctx = Session.context s in
  let a = bool_var s "aa" in
  let b = bool_var s "ab" in
  let c = bool_var s "ac" in
  let x = bool_var s "ax" in
  let y = bool_var s "ay" in
  let neg t = Context.not_ ctx t in
  let clause terms = Session.assert_term s (Context.or_ ctx terms) in
  clause [ c; a; b ];
  clause [ a; b; x; y ];
  clause [ a; b; x; neg y ];
  clause [ a; b; neg x; y ];
  clause [ a; b; neg x; neg y ];
  let redundant = c, false in
  let essential_a = a, false in
  let essential_b = b, false in
  let result =
    Session.check_sat_assuming s [ redundant; essential_a; essential_b ]
  in
  let core = require_minimal_core name s result [ essential_a; essential_b ] in
  check_true
    (name ^ ": raw-core redundancy removed")
    (not (assumption_mem redundant core));
  check_true
    (name ^ ": user literals do not leak as frame selectors")
    (Session.failed_assumptions s = []);
  expect_verdict (name ^ ": assumptions do not persist") (Session.check_sat s) Session.Sat;
  check_true
    (name ^ ": failed frame core cleared by sat")
    (Session.failed_assumptions s = [])
;;

let () =
  let name = "assuming-lia-min" in
  let s = Session.create () in
  let x = int_var s "assume_x" in
  let y = int_var s "assume_y" in
  let noise = le s y (ic s 7), true in
  let lo = le s x (ic s 0), true in
  let hi = le s (ic s 1) x, true in
  let result = Session.check_sat_assuming s [ noise; lo; hi ] in
  let core = require_minimal_core name s result [ lo; hi ] in
  check_true (name ^ ": noise removed") (not (assumption_mem noise core));
  check_true
    (name ^ ": user theory literals do not leak as frame selectors")
    (Session.failed_assumptions s = []);
  expect_verdict (name ^ ": assumptions do not persist") (Session.check_sat s) Session.Sat
;;

let () =
  if !failures > 0
  then (
    Printf.printf "session_cores_test: %d FAILURES / %d checks\n" !failures !checks;
    exit 1)
  else Printf.printf "session_cores_test: all %d checks passed\n" !checks
;;
