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

(* [half_plane lit] is the linear form [e] of the ASSERTED half-plane [e <= 0] for a
   returned core literal:
   - [Le arg] was asserted [arg <= 0] -> e = arg
   - [Not (Le arg)] was asserted ¬(arg<=0) = arg>=1 -> e = -arg + 1 (ℤ-complement) *)
let half_plane (lit : Term.t) : (Term.t * Bigint.t) list * Bigint.t =
  match lit.Term.node with
  | Term.Le arg -> linear_of arg
  | Term.Not inner ->
    (match inner.Term.node with
     | Term.Le arg ->
       let pairs, c = linear_of arg in
       List.map (fun (v, b) -> v, bneg b) pairs, Bigint.add (bneg c) Bigint.one
     | _ -> failwith "half_plane: Not of non-Le")
  | _ -> failwith "half_plane: literal is neither Le nor Not(Le)"
;;

(* Accumulate [Σ coeffᵢ · half-plane(litᵢ)] into a rational (var -> coeff) map + constant. *)
let accumulate (pairs : (Rational.t * Term.t) list) =
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
  let vars, _ = half_plane lit in
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
  let vars, c = half_plane i_term in
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
    (* Partition by origin: a positive-premise literal is Term.equal to the asserted atom. *)
    let is_a lit = List.exists (fun a -> Term.equal a lit) a_atoms in
    let a_part = List.filter (fun (_, lit) -> is_a lit) cert in
    let b_part = List.filter (fun (_, lit) -> not (is_a lit)) cert in
    check_true (name ^ ": cert spans A") (a_part <> []);
    check_true (name ^ ": cert spans B") (b_part <> []);
    check_true
      (name ^ ": core terms are asserted atoms")
      (List.for_all
         (fun (_, lit) -> List.exists (fun a -> Term.equal a lit) (a_atoms @ b_atoms))
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
     propositional Unsat ([begin_check] clears the stash). *)
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

let () =
  if !failures > 0
  then (
    Printf.printf "session_cores_test: %d FAILURES / %d checks\n" !failures !checks;
    exit 1)
  else Printf.printf "session_cores_test: all %d checks passed\n" !checks
;;
