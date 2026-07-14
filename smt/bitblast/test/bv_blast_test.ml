(* Adversarial self-test for the QF_BV bit-blaster (smt/bitblast), driven through
   bv-front's real term vocabulary ({!Oxsmt_core.Bv}) and the {!Bv_adapter} classifier.

   Three layers, in increasing coverage:

   1. EXHAUSTIVE SMALL-WIDTH ORACLE. For every operator, at widths 3 and 4, enumerate ALL
      input assignments and check the Tseitin circuit against {!Bv_eval} — an INDEPENDENT
      value-arithmetic evaluator that shares none of the circuit machinery. Two solves per
      input combo prove the circuit computes EXACTLY the evaluator:
      - "wrong answer is UNSAT": inputs pinned AND output != expected => Unsat (rules out
        under-constraint);
      - "right answer is SAT": inputs pinned AND output = expected => Sat (rules out
        over-constraint). For a Bool-result op the same two directions run on its literal.
        Division (the mul-wraparound spurious-quotient trap) also gets a wider width-5
        pass, including divide-by-zero.

   2. END-TO-END through {!Bv_solve}: hand sat/unsat formulas; every Sat model is checked
      by re-evaluating the formula under it (also enforced inside the driver as a
      fail-closed net).

   3. FAIL-CLOSED door: a formula with an unencoded construct (an uninterpreted function
      over bit-vectors, outside QF_BV) returns [Unknown], never a verdict.

   Stdlib-only (I3); deterministic (full enumeration, no PRNG, no wall-clock). Nonzero
   exit on any failed check. *)

open Oxsmt_core
module Sat = Oxsmt_solver.Sat
open Oxsmt_bitblast

let defs = Bv_adapter.defs
let big = Bigint.of_int
let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

(* A capped env plus its reserved-namespace minter: the bit-vector builders mint their
   [.oxsmt.bv.*] symbols through [mint] (board #58), while [bvvar] declares an ordinary
   user-named bit-vector variable through the public [Env] door. *)
let fresh () =
  let env, cap = Env.create_with_cap () in
  env, Context.create env, Env.declare_reserved cap env
;;

let bvvar env ctx name w =
  let sym = Env.declare_fun env name (Rank.create [] (Sort.bitvec w)) in
  Context.const ctx sym
;;

let bvconst ctx mint v w = Bv.const ctx mint ~value:v ~width:w
let bvconst_i ctx mint i w = bvconst ctx mint (big i) w

let assert_eq ctx mint blaster x i w =
  Blast.assert_term blaster (Context.eq ctx x (bvconst_i ctx mint i w))
;;

let is_sat blaster =
  match Sat.solve (Blast.sat blaster) with
  | Sat.Sat -> true
  | Sat.Unsat -> false
;;

(* {2 Layer 1 — exhaustive oracle} *)

(* bit-vector-valued binary op [make mint ctx x y], operands width [w], result width [wr]. *)
let oracle_bv2 name w wr make =
  let env, ctx, mint = fresh () in
  let x = bvvar env ctx (name ^ "_x") w in
  let y = bvvar env ctx (name ^ "_y") w in
  let term = make mint ctx x y in
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
      let exp_const = bvconst ctx mint expected wr in
      let b1 = Blast.create defs in
      assert_eq ctx mint b1 x a w;
      assert_eq ctx mint b1 y b w;
      Blast.assert_term b1 (Context.not_ ctx (Context.eq ctx term exp_const));
      check (Printf.sprintf "%s w=%d wrong-unsat a=%d b=%d" name w a b) (not (is_sat b1));
      let b2 = Blast.create defs in
      assert_eq ctx mint b2 x a w;
      assert_eq ctx mint b2 y b w;
      Blast.assert_term b2 (Context.eq ctx term exp_const);
      check (Printf.sprintf "%s w=%d right-sat a=%d b=%d" name w a b) (is_sat b2)
    done
  done
;;

let oracle_bv1 name w wr make =
  let env, ctx, mint = fresh () in
  let x = bvvar env ctx (name ^ "_x") w in
  let term = make mint ctx x in
  for a = 0 to (1 lsl w) - 1 do
    let lookup t = if Term.equal t x then Some (big a) else None in
    let expected, _ = Bv_eval.eval_bv defs ~lookup term in
    let exp_const = bvconst ctx mint expected wr in
    let b1 = Blast.create defs in
    assert_eq ctx mint b1 x a w;
    Blast.assert_term b1 (Context.not_ ctx (Context.eq ctx term exp_const));
    check (Printf.sprintf "%s w=%d wrong-unsat a=%d" name w a) (not (is_sat b1));
    let b2 = Blast.create defs in
    assert_eq ctx mint b2 x a w;
    Blast.assert_term b2 (Context.eq ctx term exp_const);
    check (Printf.sprintf "%s w=%d right-sat a=%d" name w a) (is_sat b2)
  done
;;

(* Bool-valued binary predicate. *)
let oracle_pred name w make =
  let env, ctx, mint = fresh () in
  let x = bvvar env ctx (name ^ "_x") w in
  let y = bvvar env ctx (name ^ "_y") w in
  let term = make mint ctx x y in
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
      assert_eq ctx mint b1 x a w;
      assert_eq ctx mint b1 y b w;
      Blast.assert_term b1 wrong;
      check (Printf.sprintf "%s w=%d wrong-unsat a=%d b=%d" name w a b) (not (is_sat b1));
      let b2 = Blast.create defs in
      assert_eq ctx mint b2 x a w;
      assert_eq ctx mint b2 y b w;
      Blast.assert_term b2 right;
      check (Printf.sprintf "%s w=%d right-sat a=%d b=%d" name w a b) (is_sat b2)
    done
  done
;;

let binop op mint ctx x y = Bv.binop ctx mint op x y
let unop op mint ctx x = Bv.unop ctx mint op x

let run_oracle () =
  print_endline "layer 1: exhaustive small-width oracle";
  List.iter
    (fun w ->
       List.iter
         (fun (name, op) -> oracle_bv2 name w w (binop op))
         [ "bvand", Bv.Bvand
         ; "bvor", Bv.Bvor
         ; "bvxor", Bv.Bvxor
         ; "bvadd", Bv.Bvadd
         ; "bvsub", Bv.Bvsub
         ; "bvmul", Bv.Bvmul
         ; "bvshl", Bv.Bvshl
         ; "bvlshr", Bv.Bvlshr
         ; "bvashr", Bv.Bvashr
         ; "bvudiv", Bv.Bvudiv
         ; "bvurem", Bv.Bvurem
         ];
       List.iter
         (fun (name, op) -> oracle_bv1 name w w (unop op))
         [ "bvnot", Bv.Bvnot; "bvneg", Bv.Bvneg ];
       List.iter
         (fun (name, op) -> oracle_pred name w (binop op))
         [ "bvult", Bv.Bvult; "bvule", Bv.Bvule; "bvslt", Bv.Bvslt; "bvsle", Bv.Bvsle ])
    [ 3; 4 ];
  (* division is the subtle case (the mul-wraparound spurious-quotient trap): a wider
     exhaustive pass over all inputs, including divide-by-zero. *)
  List.iter
    (fun w ->
       oracle_bv2 "bvudiv" w w (binop Bv.Bvudiv);
       oracle_bv2 "bvurem" w w (binop Bv.Bvurem))
    [ 5 ];
  (* width-changing ops *)
  oracle_bv1 "zero_extend" 3 5 (fun mint ctx x -> Bv.zero_extend ctx mint ~n:2 x);
  oracle_bv1 "sign_extend" 3 5 (fun mint ctx x -> Bv.sign_extend ctx mint ~n:2 x);
  oracle_bv1 "extract" 4 2 (fun mint ctx x -> Bv.extract ctx mint ~i:2 ~j:1 x);
  (* concat: distinct widths per arg (hi=2, lo=3, result 5) *)
  let env, ctx, mint = fresh () in
  let x = bvvar env ctx "cc_x" 2 in
  let y = bvvar env ctx "cc_y" 3 in
  let term = Bv.concat ctx mint x y in
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
      let exp_const = bvconst ctx mint expected 5 in
      let b1 = Blast.create defs in
      Blast.assert_term b1 (Context.eq ctx x (bvconst_i ctx mint a 2));
      Blast.assert_term b1 (Context.eq ctx y (bvconst_i ctx mint b 3));
      Blast.assert_term b1 (Context.not_ ctx (Context.eq ctx term exp_const));
      check (Printf.sprintf "concat wrong-unsat a=%d b=%d" a b) (not (is_sat b1))
    done
  done
;;

(* {2 Layer 2 — end-to-end via Bv_solve} *)

let run_e2e () =
  print_endline "layer 2: end-to-end solve + model check";
  (* unsat: x + 1 = x (width 4) *)
  let env, ctx, mint = fresh () in
  let x = bvvar env ctx "e_x" 4 in
  let f = Context.eq ctx (Bv.binop ctx mint Bv.Bvadd x (bvconst_i ctx mint 1 4)) x in
  check
    "unsat x+1=x"
    (match Bv_solve.solve defs [ f ] with
     | Bv_solve.Unsat -> true
     | _ -> false);
  (* sat: 3*x = 6 (width 4) -> x=2; the driver's model check must pass *)
  let env, ctx, mint = fresh () in
  let x = bvvar env ctx "s_x" 4 in
  let f =
    Context.eq
      ctx
      (Bv.binop ctx mint Bv.Bvmul (bvconst_i ctx mint 3 4) x)
      (bvconst_i ctx mint 6 4)
  in
  (match Bv_solve.solve defs [ f ] with
   | Bv_solve.Sat (model, _) ->
     check "sat 3x=6 found" true;
     (match List.assoc_opt x model with
      | Some (v, _) -> check "sat 3x=6 x=2" (Bigint.equal v (big 2))
      | None -> check "sat 3x=6 has x" false)
   | _ -> check "sat 3x=6 found" false);
  (* unsat: x <u y AND y <u x *)
  let env, ctx, mint = fresh () in
  let x = bvvar env ctx "l_x" 4 in
  let y = bvvar env ctx "l_y" 4 in
  let lt1 = Bv.binop ctx mint Bv.Bvult x y in
  let lt2 = Bv.binop ctx mint Bv.Bvult y x in
  check
    "unsat x<y & y<x"
    (match Bv_solve.solve defs [ Context.and_ ctx [ lt1; lt2 ] ] with
     | Bv_solve.Unsat -> true
     | _ -> false);
  (* sat: x <u 3 with a genuine model *)
  let env, ctx, mint = fresh () in
  let x = bvvar env ctx "b_x" 4 in
  let f = Bv.binop ctx mint Bv.Bvult x (bvconst_i ctx mint 3 4) in
  match Bv_solve.solve defs [ f ] with
  | Bv_solve.Sat (model, _) ->
    (match List.assoc_opt x model with
     | Some (v, _) -> check "sat x<3 model valid" (Bigint.compare v (big 3) < 0)
     | None -> check "sat x<3 has x" false)
  | _ -> check "sat x<3 found" false
;;

(* {2 Layer 3 — fail-closed} *)

let run_fail_closed () =
  print_endline "layer 3: fail-closed on unencoded construct";
  let env, ctx, _mint = fresh () in
  let x = bvvar env ctx "d_x" 4 in
  let bv4 = Sort.bitvec 4 in
  let f = Env.declare_fun env "uf_f" (Rank.create [ bv4 ] bv4) in
  let fx = Context.app ctx f [ x ] in
  let g = Context.eq ctx fx x in
  check
    "uninterpreted BV function -> Unknown"
    (match Bv_solve.solve defs [ g ] with
     | Bv_solve.Unknown _ -> true
     | _ -> false)
;;

(* {2 Layer 4 — word-level simplifier equivalence oracle}

   The pre-blast rewrite {!Bv_simplify} must be equivalence-preserving. For a battery of
   additive expressions over free variables, prove [e = simplify e] through the real
   blaster: assert the NEGATION and require Unsat, so the blaster certifies the two forms
   agree on ALL assignments (an exhaustive symbolic check for these widths). A [Sat] here
   is an unsound rewrite (the whole point of the oracle, since the bv model re-check
   validates only the rewritten formula). *)
let simplify1 ctx mint e =
  match Bv_simplify.simplify ctx mint [ e ] with
  | [ e' ] -> e'
  | _ -> failwith "simplify returned wrong arity"
;;

let equiv name w build =
  let env, ctx, mint = fresh () in
  let x = bvvar env ctx (name ^ "_x") w in
  let y = bvvar env ctx (name ^ "_y") w in
  let z = bvvar env ctx (name ^ "_z") w in
  let e = build ctx mint x y z in
  let e' = simplify1 ctx mint e in
  let b = Blast.create defs in
  Blast.assert_term b (Context.not_ ctx (Context.eq ctx e e'));
  check (Printf.sprintf "simplify-equiv %s w=%d" name w) (not (is_sat b))
;;

let run_simplify_equiv () =
  print_endline "layer 4: word-level simplifier equivalence (assert negation -> unsat)";
  let a ctx m p q = Bv.binop ctx m Bv.Bvadd p q in
  let s ctx m p q = Bv.binop ctx m Bv.Bvsub p q in
  let n ctx m p = Bv.unop ctx m Bv.Bvneg p in
  let band ctx m p q = Bv.binop ctx m Bv.Bvand p q in
  let k ctx m v w = Bv.const ctx m ~value:(big v) ~width:w in
  List.iter
    (fun w ->
       equiv "cancel" w (fun ctx m x y _ -> s ctx m (a ctx m x y) x);
       equiv "cancel2" w (fun ctx m x y z ->
         s ctx m (a ctx m (a ctx m x y) z) (a ctx m y x));
       equiv "const_fold" w (fun ctx m x _ _ ->
         a ctx m (a ctx m x (k ctx m 3 w)) (k ctx m 5 w));
       equiv "coeff2" w (fun ctx m x _ _ -> a ctx m x x);
       equiv "coeff3" w (fun ctx m x _ _ -> a ctx m (a ctx m x x) x);
       equiv "neg" w (fun ctx m x y _ -> n ctx m (s ctx m x y));
       equiv "sub_chain" w (fun ctx m x y z -> s ctx m (s ctx m x y) z);
       equiv "shared_atom" w (fun ctx m x y _ ->
         let g = band ctx m x y in
         a ctx m g g);
       equiv "mixed_zero" w (fun ctx m x y _ -> s ctx m (a ctx m x y) (a ctx m y x));
       equiv "wrap_const" w (fun ctx m x _ _ -> a ctx m x (k ctx m ((1 lsl w) - 1) w)))
    [ 3; 4; 8 ]
;;

let () =
  print_endline "bv-blast self-test:";
  run_oracle ();
  run_e2e ();
  run_fail_closed ();
  run_simplify_equiv ();
  Printf.printf "\nbv-blast self-test: %d checks, %d failure(s)\n" !checks !failures;
  if !failures > 0 then exit 1
;;
