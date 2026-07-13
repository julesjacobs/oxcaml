(* Adversarial self-test for the QF_BV bit-blaster (smt/bitblast).

   Three layers, in increasing coverage:

   1. EXHAUSTIVE SMALL-WIDTH ORACLE. For every operator, at widths 3 and 4, enumerate ALL
      input assignments and check the Tseitin circuit against {!Bv_eval} — an INDEPENDENT
      value-arithmetic evaluator that shares none of the circuit machinery. Two solves per
      input combo prove the circuit computes EXACTLY the evaluator:
      - "wrong answer is UNSAT": inputs pinned AND output != expected => Unsat (the
        circuit forces output = expected: rules out under-constraint);
      - "right answer is SAT": inputs pinned AND output = expected => Sat (rules out
        over-constraint). For a Bool-result op the same two directions are run on its
        literal.

   2. END-TO-END through {!Bv_solve}: hand sat/unsat formulas; every Sat model is checked
      by re-evaluating the formula under it (also enforced inside the driver as a
      fail-closed net).

   3. FAIL-CLOSED door: a formula with an unencoded construct (here an uninterpreted
      function over bit-vectors, outside QF_BV) returns [Unknown], never a verdict.

   Stdlib-only (I3); deterministic (full enumeration, no PRNG, no wall-clock). Nonzero
   exit on any failed check. *)

open Oxsmt_core
module Sat = Oxsmt_solver.Sat
open Oxsmt_bitblast

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let big = Bigint.of_int

(* one fresh registry/context per test group *)
let fresh () =
  let env = Env.create () in
  let ctx = Context.create env in
  let reg = Bv_defs_stub.create env in
  reg, ctx
;;

let assert_eq ctx reg blaster x v w =
  Blast.assert_term blaster (Context.eq ctx x (Bv_defs_stub.const reg ctx (big v) w))
;;

let is_sat blaster =
  match Sat.solve (Blast.sat blaster) with
  | Sat.Sat -> true
  | Sat.Unsat -> false
;;

(* {2 Layer 1 — exhaustive oracle} *)

(* Check a bit-vector-valued binary op [make x y] of result width [wr], inputs width [w]. *)
let oracle_bv2 name w wr make =
  let reg, ctx = fresh () in
  let defs = Bv_defs_stub.defs reg in
  let x = Bv_defs_stub.var reg ctx (name ^ "_x") w in
  let y = Bv_defs_stub.var reg ctx (name ^ "_y") w in
  let term = make reg ctx x y in
  let n = 1 lsl w in
  for a = 0 to n - 1 do
    for b = 0 to n - 1 do
      let lookup t =
        if Term.equal t x
        then Some (big a)
        else if Term.equal t y
        then Some (big b)
        else None
      in
      let expected, _ = Bv_eval.eval_bv defs ~lookup term in
      let exp_const = Bv_defs_stub.const reg ctx expected wr in
      (* wrong-is-unsat *)
      let b1 = Blast.create defs in
      assert_eq ctx reg b1 x a w;
      assert_eq ctx reg b1 y b w;
      Blast.assert_term b1 (Context.not_ ctx (Context.eq ctx term exp_const));
      check (Printf.sprintf "%s w=%d wrong-unsat a=%d b=%d" name w a b) (not (is_sat b1));
      (* right-is-sat *)
      let b2 = Blast.create defs in
      assert_eq ctx reg b2 x a w;
      assert_eq ctx reg b2 y b w;
      Blast.assert_term b2 (Context.eq ctx term exp_const);
      check (Printf.sprintf "%s w=%d right-sat a=%d b=%d" name w a b) (is_sat b2)
    done
  done
;;

let oracle_bv1 name w wr make =
  let reg, ctx = fresh () in
  let defs = Bv_defs_stub.defs reg in
  let x = Bv_defs_stub.var reg ctx (name ^ "_x") w in
  let term = make reg ctx x in
  for a = 0 to (1 lsl w) - 1 do
    let lookup t = if Term.equal t x then Some (big a) else None in
    let expected, _ = Bv_eval.eval_bv defs ~lookup term in
    let exp_const = Bv_defs_stub.const reg ctx expected wr in
    let b1 = Blast.create defs in
    assert_eq ctx reg b1 x a w;
    Blast.assert_term b1 (Context.not_ ctx (Context.eq ctx term exp_const));
    check (Printf.sprintf "%s w=%d wrong-unsat a=%d" name w a) (not (is_sat b1));
    let b2 = Blast.create defs in
    assert_eq ctx reg b2 x a w;
    Blast.assert_term b2 (Context.eq ctx term exp_const);
    check (Printf.sprintf "%s w=%d right-sat a=%d" name w a) (is_sat b2)
  done
;;

(* Bool-valued binary predicate. *)
let oracle_pred name w make =
  let reg, ctx = fresh () in
  let defs = Bv_defs_stub.defs reg in
  let x = Bv_defs_stub.var reg ctx (name ^ "_x") w in
  let y = Bv_defs_stub.var reg ctx (name ^ "_y") w in
  let term = make reg ctx x y in
  let n = 1 lsl w in
  for a = 0 to n - 1 do
    for b = 0 to n - 1 do
      let lookup t =
        if Term.equal t x
        then Some (big a)
        else if Term.equal t y
        then Some (big b)
        else None
      in
      let expected = Bv_eval.eval_bool defs ~lookup term in
      let wrong = if expected then Context.not_ ctx term else term in
      let right = if expected then term else Context.not_ ctx term in
      let b1 = Blast.create defs in
      assert_eq ctx reg b1 x a w;
      assert_eq ctx reg b1 y b w;
      Blast.assert_term b1 wrong;
      check (Printf.sprintf "%s w=%d wrong-unsat a=%d b=%d" name w a b) (not (is_sat b1));
      let b2 = Blast.create defs in
      assert_eq ctx reg b2 x a w;
      assert_eq ctx reg b2 y b w;
      Blast.assert_term b2 right;
      check (Printf.sprintf "%s w=%d right-sat a=%d b=%d" name w a b) (is_sat b2)
    done
  done
;;

let bin op reg ctx x y = Bv_defs_stub.op reg ctx op [ x; y ]
let un op reg ctx x = Bv_defs_stub.op reg ctx op [ x ]

let run_oracle () =
  print_endline "layer 1: exhaustive small-width oracle";
  List.iter
    (fun w ->
       List.iter
         (fun (name, op) -> oracle_bv2 name w w (bin op))
         [ "bvand", Bv_op.And
         ; "bvor", Bv_op.Or
         ; "bvxor", Bv_op.Xor
         ; "bvadd", Bv_op.Add
         ; "bvsub", Bv_op.Sub
         ; "bvmul", Bv_op.Mul
         ; "bvshl", Bv_op.Shl
         ; "bvlshr", Bv_op.Lshr
         ; "bvashr", Bv_op.Ashr
         ; "bvudiv", Bv_op.Udiv
         ; "bvurem", Bv_op.Urem
         ];
       List.iter
         (fun (name, op) -> oracle_bv1 name w w (un op))
         [ "bvnot", Bv_op.Not; "bvneg", Bv_op.Neg ];
       List.iter
         (fun (name, op) -> oracle_pred name w (bin op))
         [ "bvult", Bv_op.Ult
         ; "bvule", Bv_op.Ule
         ; "bvugt", Bv_op.Ugt
         ; "bvuge", Bv_op.Uge
         ; "bvslt", Bv_op.Slt
         ; "bvsle", Bv_op.Sle
         ; "bvsgt", Bv_op.Sgt
         ; "bvsge", Bv_op.Sge
         ])
    [ 3; 4 ];
  (* division is the subtle case (the mul-wraparound spurious-quotient trap): a wider
     exhaustive pass over all inputs, including divide-by-zero. *)
  List.iter
    (fun w ->
       oracle_bv2 "bvudiv" w w (bin Bv_op.Udiv);
       oracle_bv2 "bvurem" w w (bin Bv_op.Urem))
    [ 5 ];
  (* width-changing ops *)
  oracle_bv1 "zero_extend" 3 5 (fun reg ctx x ->
    Bv_defs_stub.op reg ctx ~result_width:5 (Bv_op.Zero_extend 2) [ x ]);
  oracle_bv1 "sign_extend" 3 5 (fun reg ctx x ->
    Bv_defs_stub.op reg ctx ~result_width:5 (Bv_op.Sign_extend 2) [ x ]);
  oracle_bv1 "extract" 4 2 (fun reg ctx x ->
    Bv_defs_stub.op reg ctx ~result_width:2 (Bv_op.Extract { hi = 2; lo = 1 }) [ x ]);
  (* concat: distinct widths per arg *)
  let reg, ctx = fresh () in
  let defs = Bv_defs_stub.defs reg in
  let x = Bv_defs_stub.var reg ctx "cc_x" 2 in
  let y = Bv_defs_stub.var reg ctx "cc_y" 3 in
  let term = Bv_defs_stub.op reg ctx ~result_width:5 Bv_op.Concat [ x; y ] in
  for a = 0 to 3 do
    for b = 0 to 7 do
      let lookup t =
        if Term.equal t x
        then Some (big a)
        else if Term.equal t y
        then Some (big b)
        else None
      in
      let expected, _ = Bv_eval.eval_bv defs ~lookup term in
      let exp_const = Bv_defs_stub.const reg ctx expected 5 in
      let b1 = Blast.create defs in
      Blast.assert_term b1 (Context.eq ctx x (Bv_defs_stub.const reg ctx (big a) 2));
      Blast.assert_term b1 (Context.eq ctx y (Bv_defs_stub.const reg ctx (big b) 3));
      Blast.assert_term b1 (Context.not_ ctx (Context.eq ctx term exp_const));
      check (Printf.sprintf "concat wrong-unsat a=%d b=%d" a b) (not (is_sat b1))
    done
  done
;;

(* {2 Layer 2 — end-to-end via Bv_solve} *)

let run_e2e () =
  print_endline "layer 2: end-to-end solve + model check";
  (* unsat: x + 1 = x (width 4) *)
  let reg, ctx = fresh () in
  let defs = Bv_defs_stub.defs reg in
  let x = Bv_defs_stub.var reg ctx "e_x" 4 in
  let one = Bv_defs_stub.const reg ctx (big 1) 4 in
  let f = Context.eq ctx (Bv_defs_stub.op reg ctx Bv_op.Add [ x; one ]) x in
  check
    "unsat x+1=x"
    (match Bv_solve.solve defs [ f ] with
     | Bv_solve.Unsat -> true
     | _ -> false);
  (* sat: 3*x = 6 (width 4) -> x=2, and the driver's model check must pass *)
  let reg, ctx = fresh () in
  let defs = Bv_defs_stub.defs reg in
  let x = Bv_defs_stub.var reg ctx "s_x" 4 in
  let three = Bv_defs_stub.const reg ctx (big 3) 4 in
  let six = Bv_defs_stub.const reg ctx (big 6) 4 in
  let f = Context.eq ctx (Bv_defs_stub.op reg ctx Bv_op.Mul [ three; x ]) six in
  (match Bv_solve.solve defs [ f ] with
   | Bv_solve.Sat model ->
     check "sat 3x=6 found" true;
     (match List.assoc_opt x model with
      | Some (v, _) -> check "sat 3x=6 x=2" (Bigint.equal v (big 2))
      | None -> check "sat 3x=6 has x" false)
   | _ -> check "sat 3x=6 found" false);
  (* unsat: x <u y AND y <u x *)
  let reg, ctx = fresh () in
  let defs = Bv_defs_stub.defs reg in
  let x = Bv_defs_stub.var reg ctx "l_x" 4 in
  let y = Bv_defs_stub.var reg ctx "l_y" 4 in
  let lt1 = Bv_defs_stub.op reg ctx Bv_op.Ult [ x; y ] in
  let lt2 = Bv_defs_stub.op reg ctx Bv_op.Ult [ y; x ] in
  check
    "unsat x<y & y<x"
    (match Bv_solve.solve defs [ Context.and_ ctx [ lt1; lt2 ] ] with
     | Bv_solve.Unsat -> true
     | _ -> false);
  (* sat: (x <u 3) with a genuine model *)
  let reg, ctx = fresh () in
  let defs = Bv_defs_stub.defs reg in
  let x = Bv_defs_stub.var reg ctx "b_x" 4 in
  let three = Bv_defs_stub.const reg ctx (big 3) 4 in
  let f = Bv_defs_stub.op reg ctx Bv_op.Ult [ x; three ] in
  match Bv_solve.solve defs [ f ] with
  | Bv_solve.Sat model ->
    (match List.assoc_opt x model with
     | Some (v, _) -> check "sat x<3 model valid" (Bigint.compare v (big 3) < 0)
     | None -> check "sat x<3 has x" false)
  | _ -> check "sat x<3 found" false
;;

(* {2 Layer 3 — fail-closed} *)

let run_fail_closed () =
  print_endline "layer 3: fail-closed on unencoded construct";
  (* an uninterpreted function over bit-vectors is out of QF_BV: it must degrade to
     Unknown, never a verdict. *)
  let env = Env.create () in
  let ctx = Context.create env in
  let reg = Bv_defs_stub.create env in
  let defs = Bv_defs_stub.defs reg in
  let x = Bv_defs_stub.var reg ctx "d_x" 4 in
  let bv4 = Bv_defs_stub.sort reg 4 in
  let f = Env.declare_fun env "uf_f" (Rank.create [ bv4 ] bv4) in
  let fx = Context.app ctx f [ x ] in
  let g = Context.eq ctx fx x in
  check
    "uninterpreted BV function -> Unknown"
    (match Bv_solve.solve defs [ g ] with
     | Bv_solve.Unknown _ -> true
     | _ -> false)
;;

let () =
  print_endline "bv-blast self-test:";
  run_oracle ();
  run_e2e ();
  run_fail_closed ();
  Printf.printf "\nbv-blast self-test: %d checks, %d failure(s)\n" !checks !failures;
  if !failures > 0 then exit 1
;;
