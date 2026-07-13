(* Unit + property tests for smt/theories/lia (M3-lia). This exercises the LIA decision
   procedure (Dutertre-de Moura simplex + branch-and-bound) adversarially:

   - exact rational/δ-rational arithmetic incl. overflow guards;
   - hand cases: DdM-style feasible/infeasible systems, infeasible bound chains with the
     EXACT expected Farkas multipliers, gcd-tightening interaction, strict-vs-nonstrict
     bounds via δ, unbounded problems;
   - a brute-force cross-check: thousands of random small BOUNDED systems, LIA's
     sat/unsat + returned model checked against exhaustive integer enumeration over the
     box;
   - an INDEPENDENT Farkas verifier (from the certificate definition, N-version) run on
     EVERY conflict, plus a mutant demonstration (a tampered certificate is rejected);
   - overflow: near-max_int coefficients raise cleanly, fresh solver still works;
   - determinism (I6): identical runs -> identical verdict/model/pivot count.

   Stdlib-only (I3). Deterministic: fixed-seed xorshift PRNG, no wall-clock. *)

open Oxsmt_core
open Oxsmt_lia

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let check_raises name f =
  incr checks;
  match f () with
  | exception _ -> ()
  | _ ->
    incr failures;
    Printf.printf "  FAIL %s (expected an exception)\n" name
;;

(* ---- fixed-seed PRNG (xorshift64-star) ---- *)
let rng = ref 0x1E3779B97F4A7C15

let rand_bits () =
  let x = !rng in
  let x = x lxor (x lsr 12) in
  let x = x lxor (x lsl 25) in
  let x = x lxor (x lsr 27) in
  rng := x;
  x * 0x2545F4914F6CDD1D land max_int
;;

let rand_int n = rand_bits () mod n
let rand_range lo hi = lo + rand_int (hi - lo + 1)
let reset_rng () = rng := 0x1E3779B97F4A7C15

(* ================================================================== *)
(* Rational + Delta unit tests. *)

let q = Rational.of_int
let qf = Rational.of_frac

let test_rational () =
  print_endline "Rational:";
  check "1/2 + 1/3 = 5/6" (Rational.equal (Rational.add (qf 1 2) (qf 1 3)) (qf 5 6));
  check "2/4 normalizes to 1/2" (Rational.equal (qf 2 4) (qf 1 2));
  check "-1/-2 normalizes to 1/2" (Rational.equal (qf (-1) (-2)) (qf 1 2));
  check "3/6 - 1/6 = 1/3" (Rational.equal (Rational.sub (qf 3 6) (qf 1 6)) (qf 1 3));
  check "2/3 * 3/4 = 1/2" (Rational.equal (Rational.mul (qf 2 3) (qf 3 4)) (qf 1 2));
  check "(1/2) / (3/4) = 2/3" (Rational.equal (Rational.div (qf 1 2) (qf 3 4)) (qf 2 3));
  check "floor(7/2)=3" (Rational.floor (qf 7 2) = 3);
  check "floor(-7/2)=-4" (Rational.floor (qf (-7) 2) = -4);
  check "ceil(7/2)=4" (Rational.ceil (qf 7 2) = 4);
  check "ceil(-7/2)=-3" (Rational.ceil (qf (-7) 2) = -3);
  check "floor(4)=4 (integral)" (Rational.floor (q 4) = 4);
  check "compare 1/3 < 1/2" (Rational.compare (qf 1 3) (qf 1 2) < 0);
  check "sign(-3/4) = -1" (Rational.sign (qf (-3) 4) = -1);
  check "is_int 6/3" (Rational.is_int (qf 6 3));
  check_raises "of_frac zero denom" (fun () -> ignore (qf 1 0));
  (* core-bignum W2: arithmetic that used to overflow int63 now PROMOTES to Big and
     returns the exact value (internal ops never raise; only the num/floor/ceil projection
     does). max_int = 2^62-1 = 4611686018427387903; max_int+1 = -min_int = 2^62. *)
  check
    "add promotes: max_int + 1 = 2^62"
    (Rational.equal
       (Rational.add (q max_int) (q 1))
       (Rational.of_string "4611686018427387904"));
  check
    "neg promotes: -min_int = 2^62"
    (Rational.equal (Rational.neg (q min_int)) (Rational.of_string "4611686018427387904"));
  check
    "mul promotes: max_int * 2 = 9223372036854775806"
    (Rational.equal
       (Rational.mul (q max_int) (q 2))
       (Rational.of_string "9223372036854775806"));
  check
    "compare promotes (no raise): max_int/1 > 1/max_int"
    (Rational.compare (qf max_int 1) (qf 1 max_int) > 0)
;;

let test_delta () =
  print_endline "Delta:";
  let d c k = Delta.make (q c) (q k) in
  check "3 < 3+δ" (Delta.lt (d 3 0) (d 3 1));
  check "3-δ < 3" (Delta.lt (d 3 (-1)) (d 3 0));
  check "3-δ < 3+δ" (Delta.lt (d 3 (-1)) (d 3 1));
  check "2+5δ < 3-9δ (rational part dominates)" (Delta.lt (d 2 5) (d 3 (-9)));
  check "equal 3+2δ" (Delta.equal (d 3 2) (d 3 2));
  check "add: (1+δ)+(2-δ)=3" (Delta.equal (Delta.add (d 1 1) (d 2 (-1))) (d 3 0));
  check "scale 2·(1+δ)=2+2δ" (Delta.equal (Delta.scale (q 2) (d 1 1)) (d 2 2));
  check "is_rational (3+0δ)" (Delta.is_rational (d 3 0));
  check "not is_rational (3+δ)" (not (Delta.is_rational (d 3 1)))
;;

(* ================================================================== *)
(* A test fixture: a Context, a set of Int variables, a LIA solver over int tokens, and a
   record of each asserted atom's <=0 half-plane (for the independent Farkas verifier). *)

type fixture =
  { ctx : Context.t
  ; vars : Term.t array
  ; solver : int Lia.t
  ; hp : (int, (int * int) list * int) Hashtbl.t (* token -> (Σ cᵢ·varᵢ + const) <= 0 *)
  ; mutable next_tok : int
  }

let make_fixture n =
  let env = Env.create () in
  let vars =
    Array.init n (fun i -> Printf.sprintf "x%d" i)
    |> Array.map (fun name -> Env.declare_fun env name (Rank.create [] Sort.int))
  in
  let ctx = Context.create env in
  let vterms = Array.map (fun s -> Context.const ctx s) vars in
  { ctx; vars = vterms; solver = Lia.create ctx; hp = Hashtbl.create 64; next_tok = 0 }
;;

(* Build the atom [Σ cᵢ·x_i + const <= 0] (coeffs by variable index). *)
let mk_le fx coeffs const =
  let pairs = List.map (fun (i, c) -> c, fx.vars.(i)) coeffs in
  let lhs = Context.linear_combination fx.ctx pairs const in
  Context.le fx.ctx lhs (Context.int_const fx.ctx 0)
;;

let idx_of fx (tm : Term.t) =
  let r = ref (-1) in
  Array.iteri (fun i v -> if Term.equal v tm then r := i) fx.vars;
  if !r < 0 then failwith "idx_of: unknown var term";
  !r
;;

(* The NORMALIZED <=0 half-plane of an [Le] atom's inner term, keyed by variable index.
   LIA reasons over this (gcd-tightened) form, so the independent Farkas verifier must
   too. *)
let bi_to_int b = Option.get (Bigint.to_int_opt b)

let inner_halfplane fx (inner : Term.t) =
  match inner.Term.node with
  | Term.Arith l ->
    let coeffs =
      Iarr.fold (fun acc (tm, c) -> (idx_of fx tm, bi_to_int c) :: acc) [] l.Term.coeffs
    in
    coeffs, bi_to_int l.Term.const
  | Term.Int_const k -> [], bi_to_int k
  | _ -> [ idx_of fx inner, 1 ], 0
;;

(* Assert [Σ cᵢ·x_i + const <op> 0] with [polarity], recording the resulting NORMALIZED
   <=0 half-plane (read back from the built term) for the independent Farkas check. *)
let assert_le fx coeffs const ~polarity =
  let atom = mk_le fx coeffs const in
  let tok = fx.next_tok in
  fx.next_tok <- tok + 1;
  let ic, ik =
    match atom.Term.node with
    | Term.Le inner -> inner_halfplane fx inner
    | _ -> failwith "assert_le: constructed atom is not an Le (constant-folded?)"
  in
  let hp =
    if polarity
    then ic, ik (* inner <= 0 *)
    else List.map (fun (i, c) -> i, -c) ic, 1 - ik (* complement: 1 - inner <= 0 *)
  in
  Hashtbl.replace fx.hp tok hp;
  Lia.assert_atom fx.solver atom ~polarity ~premise:tok;
  tok
;;

(* Independent Farkas verifier (from the definition, DESIGN.md §7): Σ farkasᵢ ·
   half-planeᵢ must cancel every variable and leave a strictly positive constant. Returns
   the combined (var-coeff map, constant) so callers can also inspect it. *)
let farkas_combination fx premises farkas =
  let acc = Hashtbl.create 16 in
  let const = ref Rational.zero in
  List.iter2
    (fun tok mult ->
       let coeffs, k = Hashtbl.find fx.hp tok in
       List.iter
         (fun (i, c) ->
            let cur =
              try Hashtbl.find acc i with
              | Not_found -> Rational.zero
            in
            Hashtbl.replace acc i (Rational.add cur (Rational.mul mult (q c))))
         coeffs;
       const := Rational.add !const (Rational.mul mult (q k)))
    premises
    farkas;
  acc, !const
;;

let farkas_valid fx premises farkas =
  (* all multipliers nonnegative *)
  List.for_all (fun m -> Rational.sign m >= 0) farkas
  &&
  let acc, const = farkas_combination fx premises farkas in
  Hashtbl.fold (fun _ c ok -> ok && Rational.is_zero c) acc true
  && Rational.sign const > 0
;;

let expect_conflict fx name =
  match Lia.check fx.solver with
  | Lia.Sat_candidate ->
    incr checks;
    incr failures;
    Printf.printf "  FAIL %s (expected Conflict, got Sat_candidate)\n" name;
    None
  | Lia.Conflict c ->
    check
      (name ^ ": farkas self-check (independent)")
      (farkas_valid fx c.premises c.farkas);
    Some c
;;

let expect_sat fx name =
  match Lia.check fx.solver with
  | Lia.Sat_candidate -> ()
  | Lia.Conflict _ ->
    incr checks;
    incr failures;
    Printf.printf "  FAIL %s (expected Sat_candidate, got Conflict)\n" name
;;

(* ================================================================== *)
(* Hand cases. *)

let test_hand_cases () =
  print_endline "hand cases:";
  (* --- classic two-bound infeasible chain: x <= 0 and x >= 1. Farkas [1;1]. --- *)
  (let fx = make_fixture 1 in
   let t1 = assert_le fx [ 0, 1 ] 0 ~polarity:true in
   (* x <= 0 *)
   let t2 = assert_le fx [ 0, 1 ] (-1) ~polarity:false in
   (* ¬(x-1<=0) = x>=1 *)
   match expect_conflict fx "x<=0 ∧ x>=1" with
   | Some c ->
     check
       "chain premises = {t1,t2}"
       (List.sort compare c.premises = List.sort compare [ t1; t2 ]);
     check
       "chain multipliers all 1"
       (List.for_all (fun m -> Rational.equal m (q 1)) c.farkas)
   | None -> ());
  (* --- feasible system: x >= 1, y >= 1, x + y <= 5. --- *)
  (let fx = make_fixture 2 in
   ignore (assert_le fx [ 0, -1 ] 1 ~polarity:true);
   (* -x + 1 <= 0 ==> x >= 1 *)
   ignore (assert_le fx [ 1, -1 ] 1 ~polarity:true);
   (* y >= 1 *)
   ignore (assert_le fx [ 0, 1; 1, 1 ] (-5) ~polarity:true);
   (* x + y - 5 <= 0 *)
   expect_sat fx "x>=1 ∧ y>=1 ∧ x+y<=5");
  (* --- infeasible with a NON-UNIT Farkas multiplier: 2x+y<=0, x>=1, y>=1. Certificate:
     1·(2x+y) + 2·(1-x) + 1·(1-y) = 3 > 0. --- *)
  (let fx = make_fixture 2 in
   let ta = assert_le fx [ 0, 2; 1, 1 ] 0 ~polarity:true in
   (* 2x + y <= 0 *)
   let tb = assert_le fx [ 0, -1 ] 1 ~polarity:true in
   (* x >= 1 *)
   let tc = assert_le fx [ 1, -1 ] 1 ~polarity:true in
   (* y >= 1 *)
   match expect_conflict fx "2x+y<=0 ∧ x>=1 ∧ y>=1" with
   | Some c ->
     check
       "non-unit multiplier present"
       (List.exists (fun m -> not (Rational.equal m (q 1))) c.farkas);
     ignore ta;
     ignore tb;
     ignore tc
   | None -> ());
  (* --- DdM-style: three vars, mutually contradictory. x-y<=-1, y-z<=-1, z-x<=-1. Sum = 0
     <= -3. Farkas [1;1;1]. --- *)
  (let fx = make_fixture 3 in
   ignore (assert_le fx [ 0, 1; 1, -1 ] 1 ~polarity:true);
   (* x - y + 1 <= 0 *)
   ignore (assert_le fx [ 1, 1; 2, -1 ] 1 ~polarity:true);
   (* y - z + 1 <= 0 *)
   ignore (assert_le fx [ 2, 1; 0, -1 ] 1 ~polarity:true);
   (* z - x + 1 <= 0 *)
   ignore (expect_conflict fx "cyclic x<y<z<x"));
  (* --- gcd interaction: 2x <= 3 gcd-tightens to x <= 1 over ℤ; with x >= 2 => unsat. --- *)
  (let fx = make_fixture 1 in
   let atom = mk_le fx [ 0, 2 ] (-3) in
   (* 2x - 3 <= 0 *)
   (* verify the constructed atom is gcd-normalized to x <= 1 (coeff 1, const -1) *)
   (match atom.Term.node with
    | Term.Le inner ->
      (match inner.Term.node with
       | Term.Arith l ->
         check "2x<=3 gcd-tightens: single coeff 1" (Iarr.length l.Term.coeffs = 1);
         check
           "2x<=3 gcd-tightens: coeff = 1"
           (Bigint.equal (snd (Iarr.get l.Term.coeffs 0)) Bigint.one);
         check "2x<=3 gcd-tightens: const = -1" (bi_to_int l.Term.const = -1)
       | _ -> check "2x<=3 unexpected inner (want Arith x-1)" false)
    | _ -> check "2x<=3 not an Le atom" false);
   ignore (assert_le fx [ 0, 2 ] (-3) ~polarity:true);
   (* x <= 1 (after tightening) *)
   ignore (assert_le fx [ 0, -1 ] 2 ~polarity:true);
   (* -x + 2 <= 0 ==> x >= 2 *)
   ignore (expect_conflict fx "2x<=3 ∧ x>=2 (gcd)"));
  (* --- unbounded: x <= 5, no lower bound: feasible (rational and integer). --- *)
  (let fx = make_fixture 1 in
   ignore (assert_le fx [ 0, 1 ] (-5) ~polarity:true);
   expect_sat fx "x<=5 unbounded below";
   match Lia.solve_integer fx.solver with
   | Lia.Int_sat _ -> check "x<=5 integer sat" true
   | _ -> check "x<=5 integer sat" false);
  (* --- branch-and-bound MUST do real work: x = y and x + y = 1. Rational-feasible (x = y
     = 1/2) but integer-infeasible; the ℚ simplex says SAT, B&B refutes it. The x+y slack
     is coprime, so gcd tightening does NOT pre-empt the branching. --- *)
  let fx = make_fixture 2 in
  ignore (assert_le fx [ 0, 1; 1, -1 ] 0 ~polarity:true);
  (* x - y <= 0 *)
  ignore (assert_le fx [ 1, 1; 0, -1 ] 0 ~polarity:true);
  (* y - x <= 0 (with the above: x = y) *)
  ignore (assert_le fx [ 0, 1; 1, 1 ] (-1) ~polarity:true);
  (* x + y <= 1 *)
  ignore (assert_le fx [ 0, -1; 1, -1 ] 1 ~polarity:true);
  (* -(x+y) + 1 <= 0 ==> x + y >= 1 *)
  expect_sat fx "x=y ∧ x+y=1 rational-feasible (x=y=1/2)";
  (match Lia.solve_integer fx.solver with
   | Lia.Int_unsat _ -> check "x=y ∧ x+y=1 integer-UNSAT via B&B" true
   | Lia.Int_sat _ -> check "x=y ∧ x+y=1 integer-UNSAT via B&B" false
   | Lia.Int_unknown -> check "x=y ∧ x+y=1 integer-UNSAT via B&B (got unknown)" false);
  check "B&B actually branched (pivots > 0)" (Lia.pivot_count fx.solver > 0)
;;

(* ================================================================== *)
(* Strict bounds via δ, exercised at the simplex level directly. *)

let test_strict_delta () =
  print_endline "strict δ bounds:";
  let d c k = Delta.make (q c) (q k) in
  (* x > 0 and x < 1: rational-feasible (x = 1/2). *)
  (let s = Simplex.create () in
   let x = Simplex.new_problem_var s in
   ignore (Simplex.assert_lower s x (d 0 1) "x>0");
   (* x >= 0 + δ *)
   ignore (Simplex.assert_upper s x (d 1 (-1)) "x<1");
   (* x <= 1 - δ *)
   check "0<x<1 rational feasible" (Simplex.check s = None));
  (* x >= 1 and x < 1: infeasible. *)
  (let s = Simplex.create () in
   let x = Simplex.new_problem_var s in
   ignore (Simplex.assert_lower s x (d 1 0) "x>=1");
   let conf = Simplex.assert_upper s x (d 1 (-1)) "x<1" in
   check "x>=1 ∧ x<1 infeasible (immediate)" (conf <> None));
  (* x > 0 (strict) alone is feasible. *)
  let s = Simplex.create () in
  let x = Simplex.new_problem_var s in
  ignore (Simplex.assert_lower s x (d 0 1) "x>0");
  check "x>0 feasible" (Simplex.check s = None)
;;

(* ================================================================== *)
(* Mutant demonstration: a valid Farkas certificate is accepted; a tampered one (dropped
   premise / perturbed multiplier / flipped sign) is rejected by the independent verifier.
   This is the tripwire that a flipped simplex comparison would trip (DESIGN.md §10). *)

let test_farkas_mutant () =
  print_endline "Farkas mutant demo:";
  let fx = make_fixture 2 in
  let ta = assert_le fx [ 0, 2; 1, 1 ] 0 ~polarity:true in
  let tb = assert_le fx [ 0, -1 ] 1 ~polarity:true in
  let tc = assert_le fx [ 1, -1 ] 1 ~polarity:true in
  match Lia.check fx.solver with
  | Lia.Sat_candidate -> check "mutant demo: expected conflict" false
  | Lia.Conflict c ->
    check "valid certificate accepted" (farkas_valid fx c.premises c.farkas);
    (* tamper 1: drop a premise *)
    (match c.premises, c.farkas with
     | _ :: pr, _ :: fr ->
       check "dropped-premise certificate REJECTED" (not (farkas_valid fx pr fr))
     | _ -> ());
    (* tamper 2: zero out all multipliers (degenerate) *)
    check
      "zeroed multipliers REJECTED"
      (not (farkas_valid fx c.premises (List.map (fun _ -> q 0) c.farkas)));
    (* tamper 3: negate one multiplier *)
    (match c.farkas with
     | m :: rest ->
       check
         "negated multiplier REJECTED"
         (not (farkas_valid fx c.premises (Rational.neg m :: rest)))
     | [] -> ());
    ignore ta;
    ignore tb;
    ignore tc
;;

(* ================================================================== *)
(* Propagation: register atoms; a tighter asserted bound should theory-imply a looser one. *)

let test_propagation () =
  print_endline "propagation:";
  let fx = make_fixture 1 in
  let a_le5 = mk_le fx [ 0, 1 ] (-5) in
  (* x <= 5 *)
  let a_le3 = mk_le fx [ 0, 1 ] (-3) in
  (* x <= 3 *)
  Lia.register_atom fx.solver a_le5;
  Lia.register_atom fx.solver a_le3;
  (* assert x <= 3; then x <= 5 is theory-implied. *)
  ignore (assert_le fx [ 0, 1 ] (-3) ~polarity:true);
  let props = Lia.propagate fx.solver in
  check
    "x<=3 propagates x<=5 (true)"
    (List.exists (fun (a, pol, _) -> Term.equal a a_le5 && pol) props)
;;

(* ================================================================== *)
(* Brute-force cross-check: random bounded systems vs exhaustive enumeration. *)

let box = 7

(* Evaluate whether integer assignment [asg] satisfies constraint (coeffs, const,
   polarity): L = Σ cᵢ·asgᵢ + const; polarity true => L <= 0, false => L >= 1 (integer
   complement). *)
let sat_constraint asg (coeffs, const, polarity) =
  let l = List.fold_left (fun acc (i, c) -> acc + (c * asg.(i))) const coeffs in
  if polarity then l <= 0 else l >= 1
;;

let enumerate n constraints =
  let asg = Array.make n 0 in
  let rec go i =
    if i = n
    then List.for_all (sat_constraint asg) constraints
    else (
      let rec try_v v =
        if v > box
        then false
        else (
          asg.(i) <- v;
          if go (i + 1) then true else try_v (v + 1))
      in
      try_v (-box))
  in
  go 0
;;

let test_bruteforce () =
  print_endline "brute-force cross-check:";
  reset_rng ();
  let systems = 3000 in
  let mismatches = ref 0 in
  let unknowns = ref 0 in
  let total_pivots = ref 0 in
  (* Bromberger-Fleury cube-test arm, cross-checked against the same exhaustive oracle. *)
  let cube_mismatches = ref 0 in
  let cube_hits = ref 0 in
  for _ = 1 to systems do
    let n = rand_range 1 3 in
    let fx = make_fixture n in
    let constraints = ref [] in
    (* box bounds on every variable: -box <= x_i <= box (guarantees finite feasible set). *)
    for i = 0 to n - 1 do
      ignore (assert_le fx [ i, 1 ] (-box) ~polarity:true);
      constraints := ([ i, 1 ], -box, true) :: !constraints;
      ignore (assert_le fx [ i, -1 ] (-box) ~polarity:true);
      constraints := ([ i, -1 ], -box, true) :: !constraints
    done;
    (* random constraints *)
    let m = rand_range 1 5 in
    for _ = 1 to m do
      let ncoeff = rand_range 1 n in
      let used = Array.make n false in
      let coeffs = ref [] in
      for _ = 1 to ncoeff do
        let i = rand_int n in
        if not used.(i)
        then (
          used.(i) <- true;
          let c = rand_range (-5) 5 in
          if c <> 0 then coeffs := (i, c) :: !coeffs)
      done;
      if !coeffs <> []
      then (
        let const = rand_range (-15) 15 in
        let polarity = rand_int 2 = 0 in
        ignore (assert_le fx !coeffs const ~polarity);
        constraints := (!coeffs, const, polarity) :: !constraints)
    done;
    let expected = enumerate n !constraints in
    (* Independent cube arm on the SAME system, BEFORE b&b (cube_model restores the
       simplex bounds it shrinks, so solve_integer below is unaffected). Any model it
       returns MUST satisfy every constraint AND the system MUST be genuinely SAT (the
       exhaustive [enumerate] is the oracle). Returning [None] on a real (but thin) SAT
       region is allowed — the test is sufficient, not necessary; only a wrong model, or a
       model on an unsat system, is a bug. Discriminates a broken shrink and a broken
       re-verification. *)
    ignore (Lia.check fx.solver : int Lia.result);
    (match Lia.cube_model fx.solver with
     | None -> ()
     | Some model ->
       incr cube_hits;
       if not expected then incr cube_mismatches;
       let asg = Array.make n 0 in
       List.iter
         (fun (term, v) ->
            Array.iteri (fun i vt -> if Term.equal vt term then asg.(i) <- v) fx.vars)
         model;
       if not (List.for_all (sat_constraint asg) !constraints) then incr cube_mismatches);
    (match Lia.solve_integer fx.solver with
     | Lia.Int_unknown -> incr unknowns
     | Lia.Int_sat model ->
       if not expected then incr mismatches;
       (* verify the returned model actually satisfies every constraint *)
       let asg = Array.make n 0 in
       List.iter
         (fun (term, v) ->
            Array.iteri (fun i vt -> if Term.equal vt term then asg.(i) <- v) fx.vars)
         model;
       if not (List.for_all (sat_constraint asg) !constraints) then incr mismatches
     | Lia.Int_unsat _ -> if expected then incr mismatches);
    total_pivots := !total_pivots + Lia.pivot_count fx.solver
  done;
  check "brute-force: no sat/unsat mismatches" (!mismatches = 0);
  check "brute-force: no unknowns on bounded systems" (!unknowns = 0);
  check "brute-force cube: no wrong/unsound cube models" (!cube_mismatches = 0);
  (* Non-vacuity: the cube test must actually fire (return a model) on a real fraction of
     the random systems, else the arm above proves nothing. *)
  check "brute-force cube: fired on a meaningful set" (!cube_hits > systems / 20);
  Printf.printf "    (cube: %d hits, %d mismatches)\n" !cube_hits !cube_mismatches;
  Printf.printf
    "    (%d systems, %d mismatches, %d unknowns, %d total pivots)\n"
    systems
    !mismatches
    !unknowns
    !total_pivots
;;

(* ================================================================== *)
(* core-bignum W2: coefficient growth that used to overflow int63 now PROMOTES to Big, so
   the ℚ-simplex [check] completes (never raises, never poisons). The residual native-int
   ceiling is only the OUTPUT projection (R1): this system's ℤ model binds y = -2·max_int,
   which exceeds int63, so [solve_integer] degrades to [Int_unknown] AT MODEL EXTRACTION
   (Rational.num), counting it and poisoning exactly at that sink. This doubles as the R1
   Big-model-value acceptance fixture at the Lia layer (never a truncated model). *)

let test_overflow () =
  print_endline "overflow (W2 promote + R1 model-value sink):";
  let big = max_int in
  let mk_overflowing () =
    let fx = make_fixture 2 in
    ignore (assert_le fx [ 0, big; 1, 1 ] 0 ~polarity:true);
    (* big·x + y <= 0 *)
    ignore (assert_le fx [ 0, -1 ] 2 ~polarity:true);
    (* -x + 2 <= 0 ==> x >= 2 *)
    fx
  in
  (* [check] PROMOTES rather than raising: the ℚ-simplex is feasible (y unbounded below),
     so it returns Sat_candidate and does NOT poison — the pre-W2 Rational.Overflow is
     gone. *)
  (let fx = mk_overflowing () in
   check
     "check on near-max_int PROMOTES to Sat_candidate (no overflow)"
     (match Lia.check fx.solver with
      | Lia.Sat_candidate -> true
      | _ -> false
      | exception _ -> false);
   check
     "check did not poison (internal growth promotes, I8)"
     (not (Lia.is_poisoned fx.solver));
   check
     "reuse check after promote is still Sat_candidate (live, not bricked)"
     (match Lia.check fx.solver with
      | Lia.Sat_candidate -> true
      | _ -> false
      | exception _ -> false));
  (* [solve_integer]: the ℤ model binds y = -2·max_int (Big); extracting it hits the R1
     int-projection sink -> Int_unknown, counted, and the instance is poisoned there. *)
  (let fx = mk_overflowing () in
   let r = Lia.solve_integer fx.solver in
   check
     "solve_integer degrades Big model VALUE to Int_unknown (R1 sink)"
     (r = Lia.Int_unknown);
   check
     "overflow_count attributes the projection degrade"
     (Lia.overflow_count fx.solver = 1);
   check "poisoned at the projection sink" (Lia.is_poisoned fx.solver));
  let is_poisoned name f =
    incr checks;
    match f () with
    | _ ->
      incr failures;
      Printf.printf "  FAIL %s (expected Lia.Poisoned, no exception)\n" name
    | exception Lia.Poisoned -> ()
    | exception e ->
      incr failures;
      Printf.printf
        "  FAIL %s (expected Lia.Poisoned, got %s)\n"
        name
        (Printexc.to_string e)
  in
  (* BRICK SEMANTICS retained at the projection sink: once the model-extraction overflow
     is caught, the instance is poisoned; REUSE must raise Lia.Poisoned, not a spurious
     verdict. *)
  (let fx = mk_overflowing () in
   ignore (Lia.solve_integer fx.solver);
   is_poisoned "reuse check after projection-poison -> Poisoned" (fun () ->
     Lia.check fx.solver);
   is_poisoned "reuse solve_integer after projection-poison -> Poisoned" (fun () ->
     Lia.solve_integer fx.solver);
   is_poisoned "reuse assert_atom after projection-poison -> Poisoned" (fun () ->
     assert_le fx [ 1, 1 ] 0 ~polarity:true));
  (* state-safe (I8): a fresh solver on a small problem is unaffected. *)
  let fx2 = make_fixture 1 in
  ignore (assert_le fx2 [ 0, 1 ] (-3) ~polarity:true);
  expect_sat fx2 "fresh solver after overflow works";
  (* direct rational arithmetic promotes to the exact Big value (no raise). 4·max_int. *)
  check
    "rational mul near max_int promotes to the exact Big value"
    (Rational.equal
       (Rational.mul (q big) (q 4))
       (Rational.of_string "18446744073709551612"))
;;

(* ================================================================== *)
(* R1 fixture (b) — the B&B BRANCH-BOUND int-projection sink (design R1 acceptance
   prerequisite (b)). Pairs with test_overflow's (a), the SAT-Big-MODEL-value sink. Here
   the OTHER R1 sink fires: [Rational.floor] at the branch point. Construction — pin x0 =
   0; promote x1 = x0 + min_int = -2^62 and x2 = x1 + min_int = -2^63 (Big integers, via
   the min_int-const equality promotion, same mechanism as the L5 case); then 2·x3 + 1 =
   x2, so x3 = -(2^63+1)/2 = -2^62 - 1/2 — a Big NON-integer whose floor (-2^62 - 1) is
   BELOW min_int, i.e. does not fit int63. B&B must branch on x3 (the only non-integer),
   and flooring it hits the R1 branch-bound sink → degrade to [Int_unknown], never a
   truncated bound or wrong sat/unsat. *)

let test_bb_big_branch_bound () =
  print_endline "B&B Big branch-bound (R1 fixture b):";
  let fx = make_fixture 4 in
  let assert_eq lhs rhs_var =
    let eq = Context.eq fx.ctx lhs fx.vars.(rhs_var) in
    Lia.assert_atom fx.solver eq ~polarity:true ~premise:fx.next_tok;
    fx.next_tok <- fx.next_tok + 1
  in
  (* pin x0 = 0 *)
  ignore (assert_le fx [ 0, 1 ] 0 ~polarity:true);
  ignore (assert_le fx [ 0, -1 ] 0 ~polarity:true);
  (* x1 = x0 + min_int = -2^62 (promotes to Big) *)
  assert_eq (Context.linear_combination fx.ctx [ 1, fx.vars.(0) ] min_int) 1;
  (* x2 = x1 + min_int = -2^63 (Big) *)
  assert_eq (Context.linear_combination fx.ctx [ 1, fx.vars.(1) ] min_int) 2;
  (* 2·x3 + 1 = x2 ⇒ x3 = -(2^63+1)/2, a Big non-integer, floor below min_int *)
  assert_eq (Context.linear_combination fx.ctx [ 2, fx.vars.(3) ] 1) 2;
  (* the ℚ relaxation is feasible: internal growth PROMOTES (never raises, never poisons). *)
  check
    "R1(b): feasible relaxation, internal growth promotes (not poisoned)"
    (match Lia.check fx.solver with
     | Lia.Sat_candidate -> not (Lia.is_poisoned fx.solver)
     | _ -> false
     | exception _ -> false);
  (* B&B branches on the Big non-integer x3; flooring it hits the R1 branch-bound sink. *)
  let r = Lia.solve_integer fx.solver in
  check
    "R1(b): Big branch bound degrades to Int_unknown (no truncation, no wrong verdict)"
    (r = Lia.Int_unknown);
  check
    "R1(b): the branch-bound projection overflow is counted"
    (Lia.overflow_count fx.solver >= 1);
  (* state-safe (I8): a fresh solver on a small problem is unaffected by the degrade. *)
  let fx2 = make_fixture 1 in
  ignore (assert_le fx2 [ 0, 1 ] (-3) ~polarity:true);
  expect_sat fx2 "R1(b): fresh solver after branch-bound degrade works"
;;

(* ================================================================== *)
(* Determinism (I6): identical inputs -> identical verdict, model, and pivot count. *)

let build_sample_system () =
  let fx = make_fixture 3 in
  ignore (assert_le fx [ 0, 1 ] (-6) ~polarity:true);
  ignore (assert_le fx [ 0, -1 ] (-6) ~polarity:true);
  ignore (assert_le fx [ 1, 1 ] (-6) ~polarity:true);
  ignore (assert_le fx [ 1, -1 ] (-6) ~polarity:true);
  ignore (assert_le fx [ 2, 1 ] (-6) ~polarity:true);
  ignore (assert_le fx [ 2, -1 ] (-6) ~polarity:true);
  ignore (assert_le fx [ 0, 2; 1, -3; 2, 1 ] 1 ~polarity:true);
  ignore (assert_le fx [ 0, -1; 1, 1; 2, -2 ] (-2) ~polarity:true);
  fx
;;

let test_determinism () =
  print_endline "determinism:";
  let run () =
    let fx = build_sample_system () in
    let r = Lia.solve_integer fx.solver in
    let verdict =
      match r with
      | Lia.Int_sat m ->
        "sat:" ^ String.concat "," (List.map (fun (_, v) -> string_of_int v) m)
      | Lia.Int_unsat _ -> "unsat"
      | Lia.Int_unknown -> "unknown"
    in
    verdict, Lia.pivot_count fx.solver
  in
  let v1, p1 = run () in
  let v2, p2 = run () in
  check "determinism: same verdict+model" (v1 = v2);
  check "determinism: same pivot count" (p1 = p2);
  Printf.printf "    (verdict=%s, pivots=%d)\n" v1 p1
;;

(* Big-tier determinism (I6): a system that FORCES Big promotion inside the ℚ-simplex must
   produce bit-identical results across independent runs — same verdict, same pivot count,
   and (at the R1 model-value sink) the same overflow attribution and poison state. The
   tier is an implementation detail; promotion must not perturb pivot order or the degrade
   decision, else the corpus verdict would depend on native-int reachability. This is the
   [Small]-tier {!test_determinism} arm re-run on the promoting path. *)

let build_promoting_system () =
  (* [max_int·x + y <= 0] with [x >= 2] drives [max_int·x] into the tableau (promotes to
     Big); the ℚ system is feasible (y unbounded below), and the ℤ model binds y =
     -2·max_int (Big), so [solve_integer] degrades at the R1 projection sink. Same shape
     as [test_overflow]'s [mk_overflowing]. *)
  let fx = make_fixture 2 in
  ignore (assert_le fx [ 0, max_int; 1, 1 ] 0 ~polarity:true);
  ignore (assert_le fx [ 0, -1 ] 2 ~polarity:true);
  fx
;;

let test_determinism_big () =
  print_endline "determinism (Big tier):";
  let run_check () =
    let fx = build_promoting_system () in
    let v =
      match Lia.check fx.solver with
      | Lia.Sat_candidate -> "sat"
      | Lia.Conflict _ -> "conflict"
    in
    v, Lia.pivot_count fx.solver, Lia.is_poisoned fx.solver
  in
  let c1 = run_check () in
  let c2 = run_check () in
  check "Big determinism: check verdict/pivots/poison identical across runs" (c1 = c2);
  let run_solve () =
    let fx = build_promoting_system () in
    let v =
      match Lia.solve_integer fx.solver with
      | Lia.Int_sat _ -> "sat"
      | Lia.Int_unsat _ -> "unsat"
      | Lia.Int_unknown -> "unknown"
    in
    v, Lia.pivot_count fx.solver, Lia.overflow_count fx.solver, Lia.is_poisoned fx.solver
  in
  let s1 = run_solve () in
  let s2 = run_solve () in
  check
    "Big determinism: solve_integer verdict/pivots/overflow/poison identical across runs"
    (s1 = s2);
  let cv, _, _ = c1 in
  let sv, sp, so, _ = s1 in
  Printf.printf "    (check=%s; solve=%s, pivots=%d, overflow=%d)\n" cv sv sp so
;;

(* ================================================================== *)
(* push/pop incrementality: assert, push a contradiction, pop it, remain sat. *)

let test_push_pop () =
  print_endline "push/pop:";
  let fx = make_fixture 1 in
  ignore (assert_le fx [ 0, 1 ] (-5) ~polarity:true);
  (* x <= 5 *)
  expect_sat fx "before push";
  Lia.push fx.solver;
  ignore (assert_le fx [ 0, -1 ] 6 ~polarity:true);
  (* -x + 6 <= 0 ==> x >= 6, contradicts x <= 5 *)
  (match Lia.check fx.solver with
   | Lia.Conflict _ -> check "conflict inside pushed frame" true
   | Lia.Sat_candidate -> check "conflict inside pushed frame" false);
  Lia.pop fx.solver 1;
  expect_sat fx "after pop, sat again"
;;

(* ================================================================== *)
(* Cross-model (codex) review findings L1-L5: each test encodes the exact reproduction. *)

let test_codex_findings () =
  print_endline "codex L1-L5:";
  (* L1 (false-unsat): new_slack must SUM a repeated var's coeffs, not overwrite. s =
     1·x + (-1)·x is identically 0, so s>=1 is UNSAT. The overwrite bug builds s=-x, which
     is feasible (x=-1) => a Farkas-"certified" false verdict. *)
  (let s = Simplex.create () in
   let x = Simplex.new_problem_var s in
   let sl = Simplex.new_slack s [ x, Rational.of_int 1; x, Rational.of_int (-1) ] in
   let _ = Simplex.assert_lower s sl (Delta.of_rat (Rational.of_int 1)) "sl>=1" in
   check "L1: [(x,1);(x,-1)] slack = 0, so sl>=1 is UNSAT" (Simplex.check s <> None));
  (let s = Simplex.create () in
   let x = Simplex.new_problem_var s in
   let sl = Simplex.new_slack s [ x, Rational.of_int 1; x, Rational.of_int (-1) ] in
   let _ = Simplex.assert_upper s sl (Delta.of_rat Rational.zero) "sl<=0" in
   let _ = Simplex.assert_lower s sl (Delta.of_rat Rational.zero) "sl>=0" in
   check "L1: sl=0 is feasible with no x-dependence" (Simplex.check s = None));
  (* L3 (false-sat): a conflict recorded at root must SURVIVE a push/pop that does not
     undo its triggering bound. x<=0 ∧ x>=1 (same var => pending), then push, pop. *)
  (let fx = make_fixture 1 in
   ignore (assert_le fx [ 0, 1 ] 0 ~polarity:true);
   (* x <= 0 *)
   ignore (assert_le fx [ 0, 1 ] 0 ~polarity:false);
   (* ¬(x<=0) => x >= 1, same var => pending conflict at root *)
   (match Lia.check fx.solver with
    | Lia.Conflict _ -> ()
    | Lia.Sat_candidate -> check "L3 precondition: root conflict present" false);
   Lia.push fx.solver;
   Lia.pop fx.solver 1;
   (match Lia.check fx.solver with
    | Lia.Conflict _ -> check "L3: root conflict survives a push/pop above it" true
    | Lia.Sat_candidate -> check "L3: root conflict survives a push/pop above it" false);
   match Lia.solve_integer fx.solver with
   | Lia.Int_unsat _ -> check "L3: solve_integer still UNSAT after push/pop" true
   | _ -> check "L3: solve_integer still UNSAT after push/pop" false);
  (* L3 control: a conflict raised INSIDE a pushed scope IS cleared when that scope pops
     (its triggering bound is undone) — the fix must not over-retain. *)
  (let fx = make_fixture 1 in
   ignore (assert_le fx [ 0, 1 ] 0 ~polarity:true);
   (* x <= 0 at root *)
   Lia.push fx.solver;
   ignore (assert_le fx [ 0, 1 ] 0 ~polarity:false);
   (* x >= 1 inside scope => pending here *)
   (match Lia.check fx.solver with
    | Lia.Conflict _ -> ()
    | Lia.Sat_candidate -> check "L3 control precondition: conflict in scope" false);
   Lia.pop fx.solver 1;
   expect_sat fx "L3 control: in-scope conflict clears on pop");
  (* L2/L4/L5 (originally false-sat via silent wrap). core-bignum W2 (term layer): every
     coefficient/const now stays exact arbitrary-precision [Rational] through the ingest —
     none projects back to native int at translation — so these atoms PROMOTE to exact
     Big-backed bounds/coefficients and are asserted precisely. The soundness property the
     originals guarded (no wrap to a bogus bound) holds by exactness; the assertions below
     pin the exact bound (feasible where it should be, Conflict where it should be). *)
  (* L2 (core-bignum W2, term-layer): [x + min_int <= 0] ==> [x <= -min_int = 2^63]. The
     [-const] bound now PROMOTES to an exact Big-backed bound instead of wrapping/raising
     — translates cleanly, not poisoned, and feasible (x = 0 works). Discriminating: a
     wrapped [x <= min_int] would make x = 0 a Conflict, not Sat. *)
  (let fx = make_fixture 1 in
   let atom = mk_le fx [ 0, 1 ] min_int in
   Lia.assert_atom fx.solver atom ~polarity:true ~premise:0;
   check "L2: x<=2^63 promotes exactly (not poisoned)" (not (Lia.is_poisoned fx.solver));
   check
     "L2: exact bound is feasible (x=0 Sat, not a wrapped Conflict)"
     (match Lia.check fx.solver with
      | Lia.Sat_candidate -> true
      | _ -> false));
  (* L4: [¬(x + min_int <= 0)] ==> [x >= 1 - min_int = 1 + 2^63]. The [1-const] bound
     promotes exactly; translates cleanly, not poisoned, feasible (large x works). *)
  (let fx = make_fixture 1 in
   let atom = mk_le fx [ 0, 1 ] min_int in
   Lia.assert_atom fx.solver atom ~polarity:false ~premise:0;
   check "L4: x>=1+2^63 promotes exactly (not poisoned)" (not (Lia.is_poisoned fx.solver));
   check
     "L4: exact lower bound is feasible (Sat)"
     (match Lia.check fx.solver with
      | Lia.Sat_candidate -> true
      | _ -> false));
  (* L5 rhs const (was: min_int rhs => Overflow => poison). core-bignum W2: the min_int
     equality const PROMOTES to a Big-backed bound; x0 - x1 = -min_int = 2^62 is asserted
     SOUNDLY — not the pre-W2 wrap-to-min_int false-sat. Verified two ways: the pinned
     bound is the correct POSITIVE 2^62 (s := x0 - x1 >= 1 stays SAT, whereas a
     wrapped-negative rhs would Conflict), and contradicting it (x0 = x1 = 0, i.e. 0 =
     2^62) is a Conflict. *)
  (let fx = make_fixture 2 in
   let a = Context.linear_combination fx.ctx [ 1, fx.vars.(0) ] min_int in
   let eq = Context.eq fx.ctx a fx.vars.(1) in
   Lia.assert_atom fx.solver eq ~polarity:true ~premise:0;
   check
     "L5 rhs: (x0+min_int=x1) asserts via promotion (no overflow, not poisoned)"
     (not (Lia.is_poisoned fx.solver));
   ignore (assert_le fx [ 0, -1; 1, 1 ] 1 ~polarity:true);
   (* s >= 1 *)
   check
     "L5 rhs: promoted bound is the correct +2^62 (s>=1 SAT, not wrapped-negative)"
     (match Lia.check fx.solver with
      | Lia.Sat_candidate -> true
      | _ -> false));
  (let fx = make_fixture 2 in
   let a = Context.linear_combination fx.ctx [ 1, fx.vars.(0) ] min_int in
   let eq = Context.eq fx.ctx a fx.vars.(1) in
   Lia.assert_atom fx.solver eq ~polarity:true ~premise:0;
   ignore (assert_le fx [ 0, 1 ] 0 ~polarity:true);
   ignore (assert_le fx [ 0, -1 ] 0 ~polarity:true);
   ignore (assert_le fx [ 1, 1 ] 0 ~polarity:true);
   ignore (assert_le fx [ 1, -1 ] 0 ~polarity:true);
   check
     "L5 rhs: x0=x1=0 contradicts x0-x1=2^62 (Conflict, sound promotion)"
     (match Lia.check fx.solver with
      | Lia.Conflict _ -> true
      | _ -> false));
  (* L5 (core-bignum W2): [max_int·x0 = -x0] ==> [(max_int+1)·x0 = 0] ==> x0 = 0. The
     coefficient sum [max_int + 1 = 2^63] is now an EXACT Big merge (never wraps to a
     bogus coefficient), so it translates cleanly, is not poisoned, and pins x0 = 0.
     Discriminating: adding x0 >= 1 must Conflict (a wrapped coeff would not). *)
  (let fx = make_fixture 1 in
   let a = Context.mul_const fx.ctx max_int fx.vars.(0) in
   let b = Context.neg fx.ctx fx.vars.(0) in
   let eq = Context.eq fx.ctx a b in
   Lia.assert_atom fx.solver eq ~polarity:true ~premise:0;
   check
     "L5: (max_int+1)·x0=0 exact merge (not poisoned)"
     (not (Lia.is_poisoned fx.solver));
   ignore (assert_le fx [ 0, -1 ] 1 ~polarity:true);
   (* x0 >= 1, contradicting x0 = 0 *)
   check
     "L5: x0>=1 contradicts x0=0 (Conflict, exact merge)"
     (match Lia.check fx.solver with
      | Lia.Conflict _ -> true
      | _ -> false));
  (* R1 (re-verify HIGH, false-SAT): an earlier-scope l>u contradiction must not be lost
     when a LATER assert (in a pushed scope) records its own contradiction and that scope
     is then popped. The old single-scalar `pending` was overwritten by the second
     conflict and dropped on pop; `check` now detects the empty bound interval
     structurally. *)
  (* Simplex level. *)
  (let s = Simplex.create () in
   let x = Simplex.new_problem_var s in
   let y = Simplex.new_problem_var s in
   let d k = Delta.of_rat (Rational.of_int k) in
   ignore (Simplex.assert_upper s x (d 0) "x<=0");
   ignore (Simplex.assert_lower s x (d 1) "x>=1");
   (* c1: x empty interval, at root *)
   Simplex.push s;
   ignore (Simplex.assert_upper s y (d 0) "y<=0");
   ignore (Simplex.assert_lower s y (d 1) "y>=1");
   (* c2: y empty interval, overwrites the old scalar pending *)
   Simplex.pop s 1;
   (* undoes y's bounds; x's still asserted *)
   check "R1 simplex: earlier x-conflict survives overwrite+pop" (Simplex.check s <> None));
  (* Lia level, check AND solve_integer. *)
  let fx = make_fixture 2 in
  ignore (assert_le fx [ 0, 1 ] 0 ~polarity:true);
  (* x <= 0 *)
  ignore (assert_le fx [ 0, 1 ] 0 ~polarity:false);
  (* x >= 1 => c1 on x at root *)
  Lia.push fx.solver;
  ignore (assert_le fx [ 1, 1 ] 0 ~polarity:true);
  (* y <= 0 *)
  ignore (assert_le fx [ 1, 1 ] 0 ~polarity:false);
  (* y >= 1  => c2 on y, "overwrites" *)
  Lia.pop fx.solver 1;
  (match Lia.check fx.solver with
   | Lia.Conflict _ -> check "R1 Lia check: x-conflict survives overwrite+pop" true
   | Lia.Sat_candidate -> check "R1 Lia check: x-conflict survives overwrite+pop" false);
  match Lia.solve_integer fx.solver with
  | Lia.Int_unsat _ -> check "R1 Lia solve_integer: still UNSAT after overwrite+pop" true
  | _ -> check "R1 Lia solve_integer: still UNSAT after overwrite+pop" false
;;

(* ================================================================== *)
let () =
  print_endline "lia self-test:";
  test_rational ();
  test_delta ();
  test_hand_cases ();
  test_strict_delta ();
  test_farkas_mutant ();
  test_propagation ();
  test_push_pop ();
  test_codex_findings ();
  test_bruteforce ();
  test_overflow ();
  test_bb_big_branch_bound ();
  test_determinism ();
  test_determinism_big ();
  Printf.printf "\nlia self-test: %d checks, %d failure(s)\n" !checks !failures;
  if !failures > 0 then exit 1
;;
