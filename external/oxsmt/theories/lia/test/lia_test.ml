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
  check "equal rationals hash equally" (Rational.hash (qf 2 4) = Rational.hash (qf 1 2));
  let big_int = Rational.of_string "4611686018427387904" in
  let big_int_again = Rational.add (q max_int) Rational.one in
  check
    "equal Big integers hash equally"
    (Rational.equal big_int big_int_again
     && Rational.hash big_int = Rational.hash big_int_again);
  let big_frac = Rational.of_string "4611686018427387904/3" in
  let big_frac_again = Rational.div big_int (q 3) in
  check
    "equal Big fractions hash equally"
    (Rational.equal big_frac big_frac_again
     && Rational.hash big_frac = Rational.hash big_frac_again);
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
    (Rational.compare (qf max_int 1) (qf 1 max_int) > 0);
  (* Integer (den=1) fast path == general result, cross-checked against an INDEPENDENT
     Bigint oracle over a matrix that stays Small and one that overflows to Big (#116). A
     fast path that dropped the gcd/promotion or mis-shaped the value would diverge here. *)
  let bi = Bigint.of_int in
  let oracle_add a b = Bigint.to_string (Bigint.add (bi a) (bi b)) in
  let oracle_sub a b = Bigint.to_string (Bigint.sub (bi a) (bi b)) in
  let oracle_mul a b = Bigint.to_string (Bigint.mul (bi a) (bi b)) in
  let ints = [ 0; 1; -1; 2; -3; 7; 1000; -1000; max_int; min_int; max_int - 1 ] in
  List.iter
    (fun a ->
      List.iter
        (fun b ->
          check
            (Printf.sprintf "fastpath add %d+%d exact" a b)
            (Rational.equal
               (Rational.add (q a) (q b))
               (Rational.of_string (oracle_add a b)));
          check
            (Printf.sprintf "fastpath sub %d-%d exact" a b)
            (Rational.equal
               (Rational.sub (q a) (q b))
               (Rational.of_string (oracle_sub a b)));
          check
            (Printf.sprintf "fastpath mul %d*%d exact" a b)
            (Rational.equal
               (Rational.mul (q a) (q b))
               (Rational.of_string (oracle_mul a b)));
          check
            (Printf.sprintf "fastpath compare %d?%d matches int" a b)
            (Int.compare (Rational.compare (q a) (q b)) 0
             = Int.compare (Bigint.compare (bi a) (bi b)) 0))
        ints)
    ints
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

(* core-bignum W2 cube/fabric migration (existing_combo / existing_combo_var): the fabric
   fixed-value path was migrated off native-int coeffs onto arbitrary-precision Rational
   (Rational.of_bigint), with lookup using the same canonical coefficient vector the real
   ingest records. This pins that it STILL FIRES on a small-coefficient combo (the
   Bromberger more_slacked cube anchor is small-coeff/den=1): a 2-var combo [x+y] pinned
   to a single integer by two ACTIVE USER bounds must be found by [fixed_bounds].
   Discriminating: if lookup keying diverged from ingest, the lookup would miss and
   [fixed_bounds] would return [None] — i.e. the cube win would be silently disabled.
   (Corpus confirmation: Bromberger more_slacked cut_lemmas solve fast, logged in
   bignum-log.md.) *)
let test_cube_still_fires () =
  print_endline "cube/fabric fix-path still fires (W2 cube migration):";
  let fx = make_fixture 2 in
  let xpy = Context.linear_combination fx.ctx [ 1, fx.vars.(0); 1, fx.vars.(1) ] 0 in
  ignore (assert_le fx [ 0, 1; 1, 1 ] (-3) ~polarity:true);
  (* x + y <= 3 *)
  ignore (assert_le fx [ 0, -1; 1, -1 ] 3 ~polarity:true);
  (* -(x + y) + 3 <= 0 ==> x + y >= 3 *)
  ignore (Lia.check fx.solver);
  check
    "small-coeff combo x+y pinned to 3 => fixed_bounds fires (slack key preserved)"
    (match Lia.fixed_bounds fx.solver xpy with
     | Some (v, _, _) -> Rational.equal v (Rational.of_int 3)
     | None -> false)
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
  (* An explicit zero in an otherwise sorted list must take the fallback and disappear:
     [s = 0·x + y], so [s <= 0 ∧ y >= 1] is inconsistent. *)
  (let s = Simplex.create () in
   let x = Simplex.new_problem_var s in
   let y = Simplex.new_problem_var s in
   let sl = Simplex.new_slack s [ x, Rational.zero; y, Rational.one ] in
   let _ = Simplex.assert_upper s sl (Delta.of_rat Rational.zero) "sl<=0" in
   let _ = Simplex.assert_lower s y (Delta.of_rat Rational.one) "y>=1" in
   check "L1: explicit zero coefficient is dropped" (Simplex.check s <> None));
  (* The initial-row fast path copies an all-nonbasic definition directly. Its row must
     still enforce the same equation as the general expansion: x>=1, y>=1, x+y<=1 is
     inconsistent. This also exercises the copied row through feasibility restoration. *)
  (let s = Simplex.create () in
   let x = Simplex.new_problem_var s in
   let y = Simplex.new_problem_var s in
   let sum = Simplex.new_slack s [ x, Rational.one; y, Rational.one ] in
   let _ = Simplex.assert_lower s x (Delta.of_rat Rational.one) "x>=1" in
   let _ = Simplex.assert_lower s y (Delta.of_rat Rational.one) "y>=1" in
   let _ = Simplex.assert_upper s sum (Delta.of_rat Rational.one) "x+y<=1" in
   check "initial slack row copy preserves x+y" (Simplex.check s <> None));
  (* A copied row must not alias its immutable definition. This inconsistent pair of
     copied rows drives pivot substitution through [diff.row]; if that mutation also
     changed [diff.def], Farkas construction would see a slack id in a problem-variable
     definition and fail instead of producing the conflict. *)
  (let s = Simplex.create () in
   let x = Simplex.new_problem_var s in
   let y = Simplex.new_problem_var s in
   let sum = Simplex.new_slack s [ x, Rational.one; y, Rational.one ] in
   let diff = Simplex.new_slack s [ x, Rational.one; y, q (-1) ] in
   let _ = Simplex.assert_lower s sum (Delta.of_rat Rational.one) "x+y>=1" in
   let _ = Simplex.assert_upper s y Delta.zero "y<=0" in
   let _ = Simplex.assert_upper s diff Delta.zero "x-y<=0" in
   check "copied slack row is independent of its definition" (Simplex.check s <> None));
  (* Once a pivot makes a referenced problem variable basic, [new_slack] must take the
     general expansion path. The second x+y row then expands through the existing [sum]
     row, so [sum>=1] and [again<=0] conflict. *)
  (let s = Simplex.create () in
   let x = Simplex.new_problem_var s in
   let y = Simplex.new_problem_var s in
   let sum = Simplex.new_slack s [ x, Rational.one; y, Rational.one ] in
   let _ = Simplex.assert_lower s sum (Delta.of_rat Rational.one) "x+y>=1" in
   check "post-pivot slack precondition is feasible" (Simplex.check s = None);
   check "post-pivot slack precondition performed a pivot" (Simplex.pivot_count s > 0);
   let again = Simplex.new_slack s [ x, Rational.one; y, Rational.one ] in
   let _ = Simplex.assert_upper s again Delta.zero "x+y<=0" in
   check "post-pivot slack takes the expansion fallback" (Simplex.check s <> None));
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
(* ================================================================== *)
(* notify_equality (ADR-0014 Stage 2 fabric new_eq) exactness — the review center of
   gravity: a re-notified equality whose variable combination cancels must be handled
   EXACTLY by sub-case. 0=0 (tautology) is a sound NO-OP; 0=k (k<>0) is UNSATISFIABLE and
   must be CAUGHT, never a silent no-op (that would be a wrong-verdict hole). Under the
   task #78 default-ON [trivial_eq_fix_on] the catch is a {!Lia.check} [Conflict] (the
   query keeps its verdict instead of poisoning to [unknown]); with
   [OXSMT_LIA_TRIVIAL_EQ=0] it is the pre-fix [Unsupported] raise. These in-process tests
   exercise the default-ON path (the flag is read once at module load); the OFF raise is
   covered by the manual wisa OFF run in logs/unknown-census-followups.md. A genuine
   equality is asserted as usual. *)
let test_notify_equality () =
  print_endline "notify_equality exactness:";
  (* 0=k contradiction: x0 = x0 + 3. This is the case the lazy-split prototype
     manufactured; it MUST raise, not silently vanish. (Discriminating: a version that
     no-ops every constant-relation equality would NOT raise here.) *)
  (let fx = make_fixture 1 in
   let x0 = fx.vars.(0) in
   let x0p3 = Context.linear_combination fx.ctx [ 1, x0 ] 3 in
   let eq_false = Context.eq fx.ctx x0 x0p3 in
   check
     "notify: x0 = x0+3 is a real Eq atom (not constant-folded away)"
     (match eq_false.Term.node with
      | Term.Eq _ -> true
      | _ -> false);
   Lia.notify_equality fx.solver eq_false ~premise:0;
   check
     "notify: 0=3 contradiction caught as a check Conflict (default-ON; not dropped, not \
      poison)"
     (match Lia.check fx.solver with
      | Lia.Conflict _ -> true
      | Lia.Sat_candidate -> false));
  (* genuine equality x0=x1 IS asserted (not skipped): pin x0=5, x1=7, notify x0=x1,
     expect the tableau to become infeasible. *)
  (let fx = make_fixture 2 in
   let x0 = fx.vars.(0)
   and x1 = fx.vars.(1) in
   ignore (assert_le fx [ 0, 1 ] (-5) ~polarity:true : int);
   (* x0 <= 5 *)
   ignore (assert_le fx [ 0, -1 ] 5 ~polarity:true : int);
   (* x0 >= 5 *)
   ignore (assert_le fx [ 1, 1 ] (-7) ~polarity:true : int);
   (* x1 <= 7 *)
   ignore (assert_le fx [ 1, -1 ] 7 ~polarity:true : int);
   (* x1 >= 7 *)
   Lia.notify_equality fx.solver (Context.eq fx.ctx x0 x1) ~premise:99;
   match Lia.check fx.solver with
   | Lia.Conflict _ ->
     check "notify: genuine x0=x1 asserted (x0=5 & x1=7 => conflict)" true
   | Lia.Sat_candidate ->
     check "notify: genuine x0=x1 asserted (x0=5 & x1=7 => conflict)" false);
  (* FOLDED contradiction: Context.eq folds two unequal constants to [Bool_const false],
     so [0 = 1] arrives already folded. It is UNSATISFIABLE and must fail closed,
     symmetric with the unfolded 0=k case. (Discriminating: RED against a
     [Bool_const _ -> ()] arm that no-ops every boolean constant — the codex-caught
     folded-path hole.) *)
  (let fx = make_fixture 1 in
   let eq01 =
     Context.eq fx.ctx (Context.int_const fx.ctx 0) (Context.int_const fx.ctx 1)
   in
   check
     "notify: Context.eq(0,1) folds to Bool_const false"
     (match eq01.Term.node with
      | Term.Bool_const false -> true
      | _ -> false);
   Lia.notify_equality fx.solver eq01 ~premise:0;
   check
     "notify: folded 0=1 contradiction caught as a check Conflict (default-ON; not \
      dropped)"
     (match Lia.check fx.solver with
      | Lia.Conflict _ -> true
      | Lia.Sat_candidate -> false));
  (* a TRUE-folded identity (Context.eq of a term with itself -> Bool_const true) is a
     no-op: it does not raise. *)
  (let fx = make_fixture 1 in
   let x0 = fx.vars.(0) in
   let eq_true = Context.eq fx.ctx x0 x0 in
   check
     "notify: Context.eq(x0,x0) folds to Bool_const true"
     (match eq_true.Term.node with
      | Term.Bool_const true -> true
      | _ -> false);
   check
     "notify: true-folded identity is a no-op (no raise)"
     (match Lia.notify_equality fx.solver eq_true ~premise:0 with
      | () -> true
      | exception _ -> false));
  (* H2 (review census-followups): [solve_integer] must honor a frame-scoped
     [Trivially_false] equality too. Such an equality records a [false_frames] premise but
     adds NO simplex bound, so the driver's simplex-only DFS would return a wrong
     [Int_sat] (x0=0). The entry guard (symmetric with {!Lia.check}) reports [Int_unsat]
     instead. Discriminating: RED against the pre-guard driver (Int_sat) — the whole point
     of the hedge. *)
  (let fx = make_fixture 1 in
   let x0 = fx.vars.(0) in
   let x0p3 = Context.linear_combination fx.ctx [ 1, x0 ] 3 in
   Lia.notify_equality fx.solver (Context.eq fx.ctx x0 x0p3) ~premise:0;
   check
     "notify: solve_integer honors false_frames (0=3 -> Int_unsat, not wrong Int_sat)"
     (match Lia.solve_integer fx.solver with
      | Lia.Int_unsat _ -> true
      | Lia.Int_sat _ | Lia.Int_unknown -> false));
  (* tautology re-notification is a NO-OP (does not raise, does not perturb feasibility):
     a syntactic identity x0=x0 folds to a Bool constant and is skipped; the query stays
     sat. *)
  let fx = make_fixture 1 in
  let x0 = fx.vars.(0) in
  ignore (assert_le fx [ 0, 1 ] (-5) ~polarity:true : int);
  ignore (assert_le fx [ 0, -1 ] 5 ~polarity:true : int);
  (* x0 = 5 *)
  Lia.notify_equality fx.solver (Context.eq fx.ctx x0 x0) ~premise:0;
  match Lia.check fx.solver with
  | Lia.Sat_candidate -> check "notify: x0=x0 tautology is a no-op (still sat)" true
  | Lia.Conflict _ -> check "notify: x0=x0 tautology is a no-op (still sat)" false
;;

(* GCD / Diophantine integer-feasibility test. Each case is ℚ-FEASIBLE (so [Lia.check]
   returns [Sat_candidate]); the conflict is purely integer, exactly the state b&b would
   otherwise wander on. Discriminating throughout: a solver WITHOUT the test returns
   [Sat_candidate]/[None] on every infeasible case here. *)
let test_diophantine () =
  print_endline "diophantine (gcd) integer-feasibility:";
  let pin fx i v =
    ignore (assert_le fx [ i, 1 ] (-v) ~polarity:true : int);
    (* xi <= v *)
    ignore (assert_le fx [ i, -1 ] v ~polarity:true : int)
    (* xi >= v *)
  in
  let assert_eq fx lhs rhs =
    Lia.assert_atom
      fx.solver
      (Context.eq fx.ctx lhs rhs)
      ~polarity:true
      ~premise:fx.next_tok;
    fx.next_tok <- fx.next_tok + 1
  in
  let is_some = function
    | Some _ -> true
    | None -> false
  in
  (* (a) DIRECT: pin x0=6; assert 4·x1 + 4·x2 = x0. ℚ-feasible (x1=1.5), ℤ-infeasible
         (gcd(4,4)=4 ∤ 6). *)
  (let fx = make_fixture 3 in
   pin fx 0 6;
   assert_eq
     fx
     (Context.linear_combination fx.ctx [ 4, fx.vars.(1); 4, fx.vars.(2) ] 0)
     fx.vars.(0);
   check
     "dio: 4x1+4x2=6 is ℚ-feasible (rational check does NOT catch it)"
     (match Lia.check fx.solver with
      | Lia.Sat_candidate -> true
      | Lia.Conflict _ -> false);
   check
     "dio: 4x1+4x2=6 (gcd 4 ∤ 6) ⇒ conflict"
     (is_some (Lia.diophantine_conflict fx.solver)));
  (* (b) FEASIBLE control: pin x0=8; 4·x1 + 4·x2 = 8 has integer solutions (gcd 4 | 8) ⇒
     the test must NOT fire (no over-firing / wrong unsat). *)
  (let fx = make_fixture 3 in
   pin fx 0 8;
   assert_eq
     fx
     (Context.linear_combination fx.ctx [ 4, fx.vars.(1); 4, fx.vars.(2) ] 0)
     fx.vars.(0);
   check
     "dio: 4x1+4x2=8 (gcd 4 | 8) ⇒ NO conflict (feasible, no over-firing)"
     (not (is_some (Lia.diophantine_conflict fx.solver))));
  (* (c) TRANSITIVE closure (the crux — the family's real shape): pin x0=0 directly;
     assert x1 = x0 + 6 (so x1 is fixed to 6 THROUGH the equation, NOT a direct bound);
     assert 4·x2 + 4·x3 = x1. Only the fixed-point closure over the equation system
     substitutes x1; a direct-bounds-only test would miss it. *)
  (let fx = make_fixture 4 in
   pin fx 0 0;
   assert_eq fx (Context.linear_combination fx.ctx [ 1, fx.vars.(0) ] 6) fx.vars.(1);
   assert_eq
     fx
     (Context.linear_combination fx.ctx [ 4, fx.vars.(2); 4, fx.vars.(3) ] 0)
     fx.vars.(1);
   check
     "dio: transitive x1=x0+6 (x0=0) then 4x2+4x3=x1 ⇒ conflict via closure"
     (is_some (Lia.diophantine_conflict fx.solver)));
  (* (e) PREMISE EXACTNESS (soundness): the conflict must cite EXACTLY the literals whose
     conjunction is ℤ-unsatisfiable — here the equation atom plus BOTH oriented bounds
     that pin x0=6 — nothing less (an under-cited premise set is a wrong-unsat generator:
     the conjunction of a strict subset is satisfiable, so learning it would refute a
     satisfiable branch) and nothing more (an over-wide set weakens learning). We capture
     the exact tokens and compare the premise set. Discriminating: an implementation that
     dropped the fixed-variable bound tokens (citing only the equation) would leave
     premises = [{eq}], and [4x1+4x2=x0] alone is satisfiable — this check fails, and such
     a conflict would be unsound. *)
  (let fx = make_fixture 3 in
   let t_le = assert_le fx [ 0, 1 ] (-6) ~polarity:true in
   (* x0 <= 6 *)
   let t_ge = assert_le fx [ 0, -1 ] 6 ~polarity:true in
   (* x0 >= 6 *)
   let t_eq = fx.next_tok in
   assert_eq
     fx
     (Context.linear_combination fx.ctx [ 4, fx.vars.(1); 4, fx.vars.(2) ] 0)
     fx.vars.(0);
   match Lia.diophantine_conflict fx.solver with
   | None -> check "dio: premise-exactness case produces a conflict" false
   | Some c ->
     let got = List.sort_uniq Int.compare c.Lia.premises in
     let want = List.sort_uniq Int.compare [ t_le; t_ge; t_eq ] in
     check
       "dio: conflict cites EXACTLY {eq, x0<=6, x0>=6} (no under/over-citing)"
       (List.equal Int.equal got want));
  (* (d) push/pop: the infeasible equation asserted inside a pushed scope is dropped on
     [pop], so the test no longer fires (eq_frames framing). *)
  let fx = make_fixture 3 in
  pin fx 0 6;
  Lia.push fx.solver;
  assert_eq
    fx
    (Context.linear_combination fx.ctx [ 4, fx.vars.(1); 4, fx.vars.(2) ] 0)
    fx.vars.(0);
  check
    "dio: conflict present inside pushed scope"
    (is_some (Lia.diophantine_conflict fx.solver));
  Lia.pop fx.solver 1;
  check
    "dio: after pop, the scoped equation is gone ⇒ NO conflict"
    (not (is_some (Lia.diophantine_conflict fx.solver)))
;;

(* ================================================================== *)
(* Stage B HNF integer cut (Lia.hnf_cut): a MULTI-ROW integer-lattice cut over the tight
   constraint rows (asserted equalities AND active one-sided bound inequalities). The
   oracle is INDEPENDENT and brute-force: for every cut the producer emits, enumerate the
   integer box and verify that EVERY point satisfying all the asserted tight constraints
   also satisfies the cut [f·x <= k] — i.e. the cut removes no integer solution of the
   antecedent tight constraints (validity). This is the mutation-testing tripwire for a
   corrupt cut (a flipped coefficient / wrong rounding / wrong multiplier that produced an
   UNSOUND cut would exclude a real integer point and fail here). Also checks the producer
   fires on a hand case single-row gcd cannot see, and never emits on integer-SAT systems. *)

(* assert [Σ coeffs·x = rhs] (coeffs by var index), recording it for the brute-force
   oracle *)
let assert_eq_rec fx eqs coeffs rhs =
  let lhs =
    Context.linear_combination fx.ctx (List.map (fun (i, c) -> c, fx.vars.(i)) coeffs) 0
  in
  let eq = Context.eq fx.ctx lhs (Context.int_const fx.ctx rhs) in
  Lia.assert_atom fx.solver eq ~polarity:true ~premise:fx.next_tok;
  fx.next_tok <- fx.next_tok + 1;
  eqs := (coeffs, rhs) :: !eqs
;;

(* parse the cut atom [f·x <= k], returned as [Le inner] with [inner = f·x - k <= 0], into
   (coeffs-by-index, const) meaning [Σ coeffs·x + const <= 0]. *)
let parse_cut fx (cut : Term.t) =
  match cut.Term.node with
  | Term.Le inner -> inner_halfplane fx inner
  | _ -> failwith "hnf_cut: cut atom is not an Le"
;;

(* enumerate every integer point in [-b,b]^n, applying [f] *)
let iter_box n b f =
  let p = Array.make n (-b) in
  let rec go i =
    if i = n
    then f p
    else
      for v = -b to b do
        p.(i) <- v;
        go (i + 1)
      done
  in
  go 0
;;

let sum_at coeffs const p =
  List.fold_left (fun acc (i, c) -> acc + (c * p.(i))) const coeffs
;;

(* A recorded antecedent constraint, keyed by its premise token so a cut's
   cited-antecedent set can be reconstructed from the tokens it returns.
   [CEq (coeffs, rhs)] is [Σ coeffs·x = rhs]; [CLe (coeffs, const)] is
   [Σ coeffs·x + const <= 0]. *)
type cons =
  | CEq of (int * int) list * int
  | CLe of (int * int) list * int

let sat_cons p = function
  | CEq (co, r) -> sum_at co 0 p = r
  | CLe (co, k) -> sum_at co k p <= 0
;;

(* Shared hardened soundness sweep for a cut producer ([Lia.hnf_cut] or [Lia.cg_cut]).
   Random mixed systems of integer equalities + one-sided inequalities; for every emitted
   cut [f·x <= k] it runs TWO independent brute-force oracles over the integer box, and
   hardens the three #51 H4 gaps:

   1. FULL-system oracle (as before): no integer point of [eqs ∧ les] violates the cut.
   2. CITED-ANTECEDENT oracle (stronger, lemma-faithful): the cut is emitted as the lemma
      [(cut) ∨ ¬ant_k …], i.e. it claims validity given EXACTLY its cited antecedents. So
      no integer point of the cut's OWN cited antecedent set may violate it — a strictly
      stronger obligation than the full system (the full system has more constraints,
      hence fewer models, hence is easier to satisfy the cut under). This catches an
      UNDER-CITED lemma (valid only because of a non-cited constraint), which the
      full-system oracle cannot. Every cited token must map to an asserted premise
      ([missing_cited]).
   3. NON-VACUITY: counts cuts whose cited antecedents actually admit an integer box
      point, so the oracle is provably exercising real points rather than passing on empty
      antecedent sets (the vacuity of the ℤ-infeasible hand case, whose antecedents have
      NO integer solutions). Asserted > 0.
   4. EXCEPTIONS un-silenced: any exception from [check]/the producer is counted and fails
      the test (these small, small-coefficient systems must not raise — Overflow needs
      >int63, Poisoned needs a bricked instance; neither occurs here). The old sweep
      swallowed all exceptions with [exception _ -> ()], which would have masked a
      producer crash. (The box is bounded, so this is a bounded — not exhaustive —
      soundness witness; the two oracles together are the "un-box-only" hardening.) *)
let run_cut_sweep ~label ~seed ~producer =
  let rng = Random.State.make seed in
  let systems = 3000 in
  let fired = ref 0
  and unsound_full = ref 0
  and unsound_cited = ref 0
  and nonvacuous = ref 0
  and missing_cited = ref 0
  and exns = ref 0 in
  for _ = 1 to systems do
    let n = 2 + Random.State.int rng 2 in
    let neq = 1 + Random.State.int rng 2 in
    let nle = 1 + Random.State.int rng 3 in
    let fx = make_fixture n in
    let eqs = ref [] in
    let les = ref [] in
    let tbl : (int, cons) Hashtbl.t = Hashtbl.create 16 in
    let rand_coeffs () =
      let c =
        List.init n (fun i -> i, -3 + Random.State.int rng 7)
        |> List.filter (fun (_, c) -> c <> 0)
      in
      if c = [] then [ 0, 1 ] else c
    in
    for _ = 1 to neq do
      let coeffs = rand_coeffs () in
      let rhs = -5 + Random.State.int rng 11 in
      let tok = fx.next_tok in
      assert_eq_rec fx eqs coeffs rhs;
      Hashtbl.replace tbl tok (CEq (coeffs, rhs))
    done;
    for _ = 1 to nle do
      let coeffs = rand_coeffs () in
      let const = -6 + Random.State.int rng 13 in
      let tok = assert_le fx coeffs const ~polarity:true in
      les := (coeffs, const) :: !les;
      Hashtbl.replace tbl tok (CLe (coeffs, const))
    done;
    match Lia.check fx.solver with
    | exception _ -> incr exns
    | Lia.Conflict _ -> () (* rationally infeasible: no cut sought *)
    | Lia.Sat_candidate ->
      (match producer fx.solver with
       | exception _ -> incr exns
       | None -> ()
       | Some (cut, ants) ->
         incr fired;
         let cc, ck = parse_cut fx cut in
         (* (1) full-system oracle *)
         iter_box n 6 (fun p ->
           let sat_all =
             List.for_all (fun (co, r) -> sum_at co 0 p = r) !eqs
             && List.for_all (fun (co, k) -> sum_at co k p <= 0) !les
           in
           if sat_all && sum_at cc ck p > 0 then incr unsound_full);
         (* (2) cited-antecedent oracle: reconstruct the lemma's OWN premise set *)
         let cited =
           List.sort_uniq compare ants
           |> List.map (fun tok ->
             match Hashtbl.find_opt tbl tok with
             | Some c -> c
             | None ->
               incr missing_cited;
               CLe ([], 0)
             (* placeholder; the missing_cited=0 check fails loud *))
         in
         let saw = ref false in
         iter_box n 6 (fun p ->
           if List.for_all (sat_cons p) cited
           then (
             saw := true;
             if sum_at cc ck p > 0 then incr unsound_cited));
         if !saw then incr nonvacuous)
  done;
  Printf.printf
    "    (%s: %d systems; fired=%d unsound_full=%d unsound_cited=%d nonvacuous=%d \
     missing_cited=%d exns=%d)\n"
    label
    systems
    !fired
    !unsound_full
    !unsound_cited
    !nonvacuous
    !missing_cited
    !exns;
  check
    (label ^ ": no cut removes an integer point of the FULL polyhedron")
    (!unsound_full = 0);
  check
    (label ^ ": no cut removes an integer point of its CITED antecedents (lemma-faithful)")
    (!unsound_cited = 0);
  check (label ^ ": every cited token maps to an asserted premise") (!missing_cited = 0);
  check (label ^ ": no unexpected exception from check/producer") (!exns = 0);
  check
    (label ^ ": cited-antecedent oracle non-vacuous (validated real integer points)")
    (!nonvacuous > 0);
  check (label ^ ": cuts fired on a meaningful set") (!fired > systems / 50)
;;

(* A NON-VACUOUS hand cut: [x0>=0, x1>=0, 2·x0+2·x1<=3] — ℚ-feasible with a fractional
   vertex, and (unlike the ℤ-infeasible lattice hand case, whose antecedents have NO
   integer points so any validity oracle over them is vacuous) it HAS integer points
   (0,0),(1,0), (0,1). A CG/HNF cut here must genuinely PRESERVE those points, so the
   validity oracle is non-vacuous. [producer] is the cut under test. A [None] from
   [producer] here is acceptable (it is not obligated to cut this one system); the
   randomized sweep carries the fired-count floor, so this hand check gates cut VALIDITY
   only, not firing. *)
let check_nonvacuous_hand ~label ~producer =
  let fx = make_fixture 2 in
  ignore (assert_le fx [ 0, -1 ] 0 ~polarity:true : int) (* -x0 <= 0 i.e. x0 >= 0 *);
  ignore (assert_le fx [ 1, -1 ] 0 ~polarity:true : int) (* x1 >= 0 *);
  ignore (assert_le fx [ 0, 2; 1, 2 ] (-3) ~polarity:true : int) (* 2x0+2x1 <= 3 *);
  match Lia.check fx.solver with
  | Lia.Sat_candidate ->
    (match producer fx.solver with
     | None -> () (* not obligated to cut here; the sweep carries the fired-count floor *)
     | Some (cut, _) ->
       let cc, ck = parse_cut fx cut in
       let pts = ref 0
       and bad = ref 0 in
       iter_box 2 6 (fun p ->
         let feasible = p.(0) >= 0 && p.(1) >= 0 && (2 * p.(0)) + (2 * p.(1)) <= 3 in
         if feasible
         then (
           incr pts;
           if sum_at cc ck p > 0 then incr bad));
       check
         (label ^ " nonvacuous hand: integer points exist (oracle non-vacuous)")
         (!pts > 0);
       check
         (label ^ " nonvacuous hand: cut preserves every integer point (valid)")
         (!bad = 0))
  | _ -> check (label ^ " nonvacuous hand: rational relaxation feasible") false
;;

let test_hnf_cut () =
  print_endline "HNF cut (Lia.hnf_cut) soundness:";
  (* Hand case: x0 + 2·x1 = 0, 2·x0 + x1 = 1 — ℚ-feasible (x0=2/3, x1=-1/3) but
     ℤ-infeasible; every SINGLE-row gcd passes (gcd(1,2)=1|0, gcd(2,1)=1|1), so
     diophantine_conflict cannot see it — the multi-row lattice cut must. *)
  let fx = make_fixture 2 in
  let eqs = ref [] in
  assert_eq_rec fx eqs [ 0, 1; 1, 2 ] 0;
  assert_eq_rec fx eqs [ 0, 2; 1, 1 ] 1;
  (match Lia.check fx.solver with
   | Lia.Sat_candidate ->
     check "hnf hand: ℚ-feasible (rational relaxation ok)" true;
     (match Lia.hnf_cut fx.solver with
      | Some (cut, ants) ->
        check "hnf hand: a cut is emitted on the multi-row ℤ-infeasible lattice" true;
        check "hnf hand: cut cites >=1 antecedent" (List.length ants >= 1);
        let cc, ck = parse_cut fx cut in
        (* This lattice is ℤ-INFEASIBLE, so a validity oracle over its integer points is
           VACUOUS (there are none) — the honest property here is that the cut fires where
           single-row gcd is blind, and that the antecedents really have no integer point
           (so the cut is a sound refutation witness, not an exclusion of a real
           solution). NON-vacuous validity is carried by {!check_nonvacuous_hand} +
           {!run_cut_sweep}. *)
        let eq_pts = ref 0
        and viol = ref 0 in
        iter_box 2 8 (fun p ->
          if List.for_all (fun (co, r) -> sum_at co 0 p = r) !eqs
          then (
            incr eq_pts;
            if sum_at cc ck p > 0 then incr viol));
        check
          "hnf hand: antecedent system is ℤ-infeasible (validity oracle vacuous here)"
          (!eq_pts = 0);
        check
          "hnf hand: cut excludes no integer point of the antecedents (vacuously)"
          (!viol = 0)
      | None -> check "hnf hand: cut emitted (multi-row lattice infeasibility)" false)
   | _ -> check "hnf hand: rational relaxation feasible" false);
  (* Guard RED (deterministic, permanent): an integer-FEASIBLE equality system
     [x0 + x1 = 2, x0 - x1 = 0] (solution x0=x1=1) must yield NO cut. This is the direct
     tripwire for the β-non-integer gate + the μ-recheck self-check: emitting here would
     be a SPURIOUS cut that could exclude the real solution (the unsoundness vector). If
     the guard is bypassed (verified: the sweep below goes RED), a cut is emitted and this
     fires. (Carried from Stage B; still valid under B2 — an equality-only tight system
     with an integer solution has integer β, so no cut is produced.) *)
  let fxs = make_fixture 2 in
  let _ = assert_eq_rec fxs (ref []) [ 0, 1; 1, 1 ] 2 in
  let _ = assert_eq_rec fxs (ref []) [ 0, 1; 1, -1 ] 0 in
  (match Lia.check fxs.solver with
   | Lia.Sat_candidate ->
     check
       "hnf guard: NO cut on an integer-feasible equality system (β-gate + self-check)"
       (Lia.hnf_cut fxs.solver = None)
   | _ -> check "hnf guard: feasible relaxation" false);
  (* A NON-VACUOUS hand cut: integer points exist and must be preserved (fixes the vacuity
     of the ℤ-infeasible case above). *)
  check_nonvacuous_hand ~label:"hnf" ~producer:Lia.hnf_cut;
  (* Random MIXED sweep (B2): small systems of integer EQUALITIES and one-sided
     INEQUALITIES, through the shared hardened sweep (full-system AND cited-antecedent
     oracles, non-vacuity, un-silenced exceptions). The mutation-testing tripwire: a
     mutant that drops the per-row μ≥0 sign discipline (or the β/integrality checks) emits
     an invalid cut and fails the oracle(s). *)
  run_cut_sweep ~label:"hnf sweep" ~seed:[| 0xB5C2; 7; 31 |] ~producer:Lia.hnf_cut
;;

(* Stage B3 CG-separation cut (Lia.cg_cut): the same MULTI-ROW tight-constraint
   Chvátal–Gomory cut as {!Lia.hnf_cut}, but where B2 rejects an HNF-row multiplier that
   is negative on an inequality row, B3 shifts it into the tight cone by an integer,
   nonnegative amount. The soundness obligation is IDENTICAL and checked by the same
   independent brute-force oracle: every emitted cut [f·x ≤ k] removes NO integer point of
   the FULL tight polyhedron (equalities ∧ inequalities). Because the shift preserves the
   multiplier's integer image and the fractional part of the rhs while forcing [μ' ≥ 0] on
   every inequality row, an emitted cut stays T-valid. The LOAD-BEARING soundness guard is
   the INDEPENDENT sign tripwire: {!Lia.cg_cut} re-verifies [μ' ≥ 0] on every inequality
   row from the ORIGINAL A/c (the [restricted && sign bigW < 0] recheck) and returns None
   on any violation, so a buggy shift cannot emit an unsound cut — it is caught and
   dropped. Verified RED, TWO variants: bypassing the shift ALONE (keep the tripwire)
   emits 0 unsound cuts (the tripwire drops the sign-invalid candidates) — the shift is a
   productivity/ completeness mechanism, NOT the soundness guard; disabling the sign
   DISCIPLINE (shift AND tripwire together) makes this sweep emit 1436 unsound cuts and
   FAIL (documented in logs/lia-cuts-b3-log.md). B3 fires on strictly MORE systems than B2
   (the sign discipline shifts rather than rejects), so the fired-count floor is higher. *)
let test_cg_cut () =
  print_endline "CG-separation cut (Lia.cg_cut) soundness:";
  (* Hand case (multi-row ℤ-infeasible, single-row gcd blind): B3 must also emit a valid
     cut. *)
  let fx = make_fixture 2 in
  let eqs = ref [] in
  assert_eq_rec fx eqs [ 0, 1; 1, 2 ] 0;
  assert_eq_rec fx eqs [ 0, 2; 1, 1 ] 1;
  (match Lia.check fx.solver with
   | Lia.Sat_candidate ->
     (match Lia.cg_cut fx.solver with
      | Some (cut, ants) ->
        check "cg hand: a cut is emitted on the multi-row ℤ-infeasible lattice" true;
        check "cg hand: cut cites >=1 antecedent" (List.length ants >= 1);
        let cc, ck = parse_cut fx cut in
        (* ℤ-INFEASIBLE lattice: the validity oracle over its integer points is VACUOUS
           (there are none). Assert exactly that (a sound refutation witness); non-vacuous
           validity is carried by {!check_nonvacuous_hand} + {!run_cut_sweep}. *)
        let eq_pts = ref 0
        and viol = ref 0 in
        iter_box 2 8 (fun p ->
          if List.for_all (fun (co, r) -> sum_at co 0 p = r) !eqs
          then (
            incr eq_pts;
            if sum_at cc ck p > 0 then incr viol));
        check
          "cg hand: antecedent system is ℤ-infeasible (validity oracle vacuous here)"
          (!eq_pts = 0);
        check
          "cg hand: cut excludes no integer point of the antecedents (vacuously)"
          (!viol = 0)
      | None -> check "cg hand: cut emitted (multi-row lattice infeasibility)" false)
   | _ -> check "cg hand: rational relaxation feasible" false);
  (* Guard (deterministic, permanent): an integer-FEASIBLE equality system has an integer
     LP vertex, so every HNF row has integer β and NO separating cut exists — B3 must emit
     none (the β-non-integer gate; a spurious cut here could exclude the real solution). *)
  let fxs = make_fixture 2 in
  let _ = assert_eq_rec fxs (ref []) [ 0, 1; 1, 1 ] 2 in
  let _ = assert_eq_rec fxs (ref []) [ 0, 1; 1, -1 ] 0 in
  (match Lia.check fxs.solver with
   | Lia.Sat_candidate ->
     check
       "cg guard: NO cut on an integer-feasible equality system (β-gate + self-check)"
       (Lia.cg_cut fxs.solver = None)
   | _ -> check "cg guard: feasible relaxation" false);
  (* task #60 SPARSITY-GATE plumbing: [?cut_gate] filters the selected best cut. On the
     same multi-row ℤ-infeasible lattice that emits a cut by default, a reject-all gate
     must suppress it (the adapter then branches — soundness-neutral); an accept-all gate
     is identical to the default. cg_cut is read-only, so both calls share one fixture. *)
  let fxg = make_fixture 2 in
  let eqsg = ref [] in
  assert_eq_rec fxg eqsg [ 0, 1; 1, 2 ] 0;
  assert_eq_rec fxg eqsg [ 0, 2; 1, 1 ] 1;
  (match Lia.check fxg.solver with
   | Lia.Sat_candidate ->
     check
       "cg gate: accept-all gate emits (= default behaviour)"
       (match Lia.cg_cut ~cut_gate:(fun ~nnz:_ ~ants:_ ~m:_ ~n:_ -> true) fxg.solver with
        | Some _ -> true
        | None -> false);
     check
       "cg gate: reject-all gate suppresses the cut (branch fallback)"
       (Lia.cg_cut ~cut_gate:(fun ~nnz:_ ~ants:_ ~m:_ ~n:_ -> false) fxg.solver = None);
     check
       "cg gate: default (no gate arg) still emits"
       (match Lia.cg_cut fxg.solver with
        | Some _ -> true
        | None -> false)
   | _ -> check "cg gate: feasible relaxation for gate fixture" false);
  (* NON-VACUOUS hand cut: integer points exist and must be preserved. *)
  check_nonvacuous_hand ~label:"cg" ~producer:Lia.cg_cut;
  (* Random MIXED sweep through the shared hardened sweep (full-system AND
     cited-antecedent oracles, non-vacuity, un-silenced exceptions). Every emitted CG cut
     must remove no integer point of the full polyhedron NOR of its own cited antecedents
     — the sign-shift tripwire. B3 fires on strictly MORE systems than B2. *)
  run_cut_sweep ~label:"cg sweep" ~seed:[| 0xC63B; 11; 43 |] ~producer:Lia.cg_cut
;;

(* Slack-key discrimination (soundness-critical): distinct canonical coefficient vectors
   must compare unequal, while permutations of one vector compare equal and hash equal.
   A broken equality could conflate distinct forms onto one slack (wrong reuse ⇒ possible
   wrong verdict); a broken equal⇒same-hash contract could miss legitimate dedup. Test the
   production table operations directly, including a randomized differential. *)
let test_slack_key () =
  let equal = Lia.For_testing.slack_key_equal in
  let hash = Lia.For_testing.slack_key_hash in
  (* Order-invariance / dedup: same canonical combo, permuted input → identical key. *)
  let ordered = [ 3, q 4; 1, q 2; 40, q (-7) ] in
  let permuted = [ 40, q (-7); 1, q 2; 3, q 4 ] in
  check
    "slack key order-invariant"
    (equal ordered permuted && hash ordered = hash permuted);
  (* Adversarial vector boundaries that are genuinely different combos. *)
  check
    "digit run-on (1,23) vs (12,3)"
    (not (equal [ 1, q 23 ] [ 12, q 3 ]));
  check
    "multi-pair boundary (1,2)(3,4) vs (12,34)"
    (not (equal [ 1, q 2; 3, q 4 ] [ 12, q 34 ]));
  check
    "coeff-length boundary (1,2)(3,45) vs (1,23)(4,5)"
    (not (equal [ 1, q 2; 3, q 45 ] [ 1, q 23; 4, q 5 ]));
  check "fraction vs integer" (not (equal [ 1, qf 1 2 ] [ 1, q 1 ]));
  check
    "negative vs positive coeff"
    (not (equal [ 5, q (-3) ] [ 5, q 3 ]));
  check
    "duplicate coefficients remain distinct from their sum"
    (not (equal [ 1, q 1; 1, q 1 ] [ 1, q 2 ]));
  check "explicit zero remains distinct from empty" (not (equal [ 1, q 0 ] []));
  check
    "duplicate-id order remains significant"
    (not (equal [ 1, q 1; 1, q 2 ] [ 1, q 2; 1, q 1 ]));
  (* Regression for the former linear mixer: these singletons all collided at hash 0. *)
  let crafted_hashes =
    List.init 32 (fun i ->
      let var = i + 2 in
      hash [ var, q ((31 * var) lxor 31) ])
  in
  check
    "crafted singleton family does not collapse into one hash bucket"
    (List.length (List.sort_uniq Int.compare crafted_hashes) >= 30);
  (* Randomized differential against a canonical string oracle. Hash collisions are
     legal, so compare every pair sharing a bucket and require the production equality to
     agree with the oracle. Also reverse every generated vector to pin equal⇒same-hash. *)
  let st = Random.State.make [| 0x51AC; 0x9E37; 7 |] in
  let by_hash : (int, ((int * Rational.t) list * (int * string) list) list) Hashtbl.t =
    Hashtbl.create 4096
  in
  let mismatches = ref 0 in
  for _ = 1 to 20000 do
    (* 1..4 distinct varids in [0,30], coeffs int in [-15,15]\{0} or a small fraction. *)
    let n = 1 + Random.State.int st 4 in
    let rec pick_vars acc k =
      if k = 0
      then acc
      else (
        let v = Random.State.int st 31 in
        if List.mem_assoc v acc
        then pick_vars acc k
        else pick_vars ((v, ()) :: acc) (k - 1))
    in
    let vars = List.map fst (pick_vars [] n) in
    let mk_coeff () =
      if Random.State.bool st
      then (
        let c = Random.State.int st 31 - 15 in
        q (if c = 0 then 1 else c))
      else qf (Random.State.int st 9 + 1) (Random.State.int st 8 + 2)
    in
    let combo = List.map (fun v -> v, mk_coeff ()) vars in
    let canon =
      List.sort (fun (a, _) (b, _) -> Int.compare a b) combo
      |> List.map (fun (v, c) -> v, Rational.to_string c)
    in
    let h = hash combo in
    if not (equal combo (List.rev combo) && h = hash (List.rev combo))
    then incr mismatches;
    let bucket =
      match Hashtbl.find_opt by_hash h with
      | Some entries -> entries
      | None -> []
    in
    List.iter
      (fun (previous, previous_canon) ->
        if not (Bool.equal (equal combo previous) (canon = previous_canon))
        then incr mismatches)
      bucket;
    Hashtbl.replace by_hash h ((combo, canon) :: bucket)
  done;
  check
    "slack key randomized differential: 0 mismatches over 20000 combos"
    (!mismatches = 0)
;;

(* H6 (fabric S4.2 train foundation fix): [checkpoint]/[rewind_to_checkpoint] must retract
   a [Trivially_false] equality's premise exactly as [pop] does. The foundation checkpoint
   predated [false_frames] (the default-ON trivial_eq fix, task #78) and auto-merged over
   it, so a stale tautologically-false premise survived a rewind and [check] kept
   reporting the retracted conflict — completeness-only (over-report, never a wrong
   verdict), but it broke the primitive's OBS-EQ-with-[pop] contract. RED: assert
   [x0 + 1 = x0] (which cancels to [1 = 0] -> [Trivially_false], recording a
   [false_frames] premise) inside a checkpoint window; after rewind [check] must be
   [Sat_candidate], matching the pop-path. Pre-fix the rewind left the premise and [check]
   stayed [Conflict]. *)
let test_h6_false_frames_checkpoint () =
  let fx = make_fixture 2 in
  let x = fx.vars.(0) in
  let eq_false = Context.eq fx.ctx (Context.linear_combination fx.ctx [ 1, x ] 1) x in
  check
    "h6: x0+1=x0 is a live Eq atom (not constant-folded)"
    (match eq_false.Term.node with
     | Term.Eq _ -> true
     | _ -> false);
  check "h6: fresh solver -> Sat_candidate" (Lia.check fx.solver = Lia.Sat_candidate);
  let cp = Lia.checkpoint fx.solver in
  Lia.assert_atom fx.solver eq_false ~polarity:true ~premise:0;
  check
    "h6: trivially-false eq asserted -> Conflict"
    (match Lia.check fx.solver with
     | Lia.Conflict _ -> true
     | Lia.Sat_candidate -> false);
  Lia.rewind_to_checkpoint fx.solver cp;
  (* THE RED: rewind must retract the false premise (pop-path parity) -> Sat_candidate.
     Pre-fix: [false_frames] not restored -> stale premise -> still Conflict. *)
  check
    "h6: rewind retracts the false premise -> Sat_candidate (checkpoint/pop parity)"
    (Lia.check fx.solver = Lia.Sat_candidate);
  (* Cross-check the reference behaviour on the [pop] path: the SAME trivially-false eq
     inside a push/pop window ends [Sat_candidate], confirming rewind now matches pop. *)
  let fx2 = make_fixture 2 in
  let x2 = fx2.vars.(0) in
  let eq2 = Context.eq fx2.ctx (Context.linear_combination fx2.ctx [ 1, x2 ] 1) x2 in
  Lia.push fx2.solver;
  Lia.assert_atom fx2.solver eq2 ~polarity:true ~premise:0;
  check
    "h6: pop-path trivially-false eq -> Conflict"
    (match Lia.check fx2.solver with
     | Lia.Conflict _ -> true
     | Lia.Sat_candidate -> false);
  Lia.pop fx2.solver 1;
  check
    "h6: pop retracts the false premise -> Sat_candidate"
    (Lia.check fx2.solver = Lia.Sat_candidate)
;;

(* A [model_find] snapshot covers both the asserted bounds and the problem-variable
   domain. Lazy interface disequality handling can internalize a previously unseen leaf
   without asserting a bound; that domain growth must invalidate the snapshot. *)
let test_modelfind_domain_growth () =
  let fx = make_fixture 2 in
  (* 2*x0 >= 1 and x0 <= 1 has the fractional LP vertex x0=1/2 and the integer model
     x0=1, so [model_find] records a nontrivial integral snapshot. x1 remains unseen. *)
  ignore (assert_le fx [ 0, 2 ] 0 ~polarity:false);
  ignore (assert_le fx [ 0, 1 ] (-1) ~polarity:true);
  check
    "model-domain: rational constraints are feasible"
    (Lia.check fx.solver = Lia.Sat_candidate);
  check "model-domain: model_find records an integer model" (Lia.model_find fx.solver);
  let before = Lia.model_bigint fx.solver in
  check
    "model-domain: cached model initially omits unseen x1"
    (not (List.exists (fun (tm, _) -> Term.equal tm fx.vars.(1)) before));
  let pivots_before_internalize = Lia.pivot_count fx.solver in
  Lia.internalize_term fx.solver fx.vars.(1);
  let after = Lia.model_bigint fx.solver in
  check
    "model-domain: internalization invalidates cache and includes x1"
    (List.exists (fun (tm, _) -> Term.equal tm fx.vars.(1)) after);
  check
    "model-domain: re-derived model preserves x0=1"
    (match List.find_opt (fun (tm, _) -> Term.equal tm fx.vars.(0)) after with
     | Some (_, value) -> Bigint.equal value Bigint.one
     | None -> false);
  check
    "model-domain: cache extension does not re-run the model finder"
    (Lia.pivot_count fx.solver = pivots_before_internalize);
  (* A subsequent real bound mutation must invalidate the zero extension. *)
  ignore (assert_le fx [ 1, -1 ] 2 ~polarity:true);
  check
    "model-domain: subsequent x1>=2 bound remains feasible"
    (Lia.check fx.solver = Lia.Sat_candidate);
  let bounded = Lia.model_bigint fx.solver in
  check
    "model-domain: asserted bound invalidates zero and re-derives x1>=2"
    (match List.find_opt (fun (tm, _) -> Term.equal tm fx.vars.(1)) bounded with
     | Some (_, value) -> Bigint.compare value (Bigint.of_int 2) >= 0
     | None -> false)
;;

let () =
  print_endline "lia self-test:";
  test_rational ();
  test_slack_key ();
  test_delta ();
  test_hand_cases ();
  test_strict_delta ();
  test_farkas_mutant ();
  test_propagation ();
  test_push_pop ();
  test_cube_still_fires ();
  test_codex_findings ();
  test_bruteforce ();
  test_overflow ();
  test_bb_big_branch_bound ();
  test_determinism ();
  test_determinism_big ();
  test_notify_equality ();
  test_h6_false_frames_checkpoint ();
  test_modelfind_domain_growth ();
  test_diophantine ();
  test_hnf_cut ();
  test_cg_cut ();
  Printf.printf "\nlia self-test: %d checks, %d failure(s)\n" !checks !failures;
  if !failures > 0 then exit 1
;;
