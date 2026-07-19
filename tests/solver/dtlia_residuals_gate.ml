(* DT residuals gate (bugreport 03 fail-closed residuals, dtlia-residuals lane).

   Residual 1 (F2/G2, this file): a scalar fixed ONLY by an equality (e.g. [k = 2]) is
   removed by presolve equality-elimination, so it reaches neither the theory model nor
   the SAT skeleton; the DT checker re-evaluates the ORIGINAL assertions (still mentioning
   [k]) and fails closed. Fix: [Session.complete_dt_elim_scalars] reconstructs each
   eliminated def's value (the presolve witness, over surviving scalars in elimination
   order) into the DT checker model. Default ON; [OXSMT_DTLIA_ELIM_COMPLETE]=0 opts out
   (the RED baseline).

   Drives .smt2 through the SHARED loader with presolve — the consumer path that triggers
   equality-elimination. Recovery cases degrade to [unknown] with the fix off; soundness
   cases (contradictions) stay [unsat] in both configs. *)

module Session = Oxsmt_interface.Session
module Parser = Oxsmt_smtlib_parser.Parser

let failures = ref 0

let vstr = function
  | Session.Sat -> "sat"
  | Session.Unsat -> "unsat"
  | Session.Unknown -> "unknown"
;;

let solve src =
  let s = Session.create () in
  match
    Parser.parse_into
      ~internal_mint:(Session.parse_minter s)
      (Session.env s)
      (Session.context s)
      src
  with
  | exception e -> failwith ("parse: " ^ Printexc.to_string e)
  | parsed ->
    if Oxsmt_query_loader.assert_all ~presolve:true s parsed
    then Session.check_sat s
    else Session.Unknown
;;

let expect name src want =
  let got = solve src in
  if got = want
  then Printf.printf "  ok   %s: %s\n%!" name (vstr got)
  else (
    incr failures;
    Printf.printf "  FAIL %s: got %s, want %s\n%!" name (vstr got) (vstr want))
;;

(* Soundness: the verdict must never be [Sat]. [unsat] (refuted) and [unknown] (sound
   degrade) are both acceptable; only a wrong [Sat] fails. *)
let expect_not_sat name src =
  match solve src with
  | Session.Sat ->
    incr failures;
    Printf.printf "  FAIL %s: got sat (must not be sat)\n%!" name
  | v -> Printf.printf "  ok   %s: %s (not sat)\n%!" name (vstr v)
;;

let () =
  Printf.printf "dtlia residuals gate (F2/G2 presolve-eliminated scalars):\n%!";
  (* F2/G2 recovery: enum + Int scalar fixed only by an equality. RED with ELIM off. *)
  expect
    "F2 enum + k=2 (elim scalar)"
    "(declare-datatypes ((Color 0)) (((Red) (Green) (Blue))))\n\
     (declare-const c Color)\n\
     (declare-const k Int)\n\
     (assert (= c Red))\n\
     (assert (= k 2))\n\
     (check-sat)\n"
    Session.Sat;
  expect
    "G2 enum + k=5 (elim scalar)"
    "(declare-datatypes ((Color 0)) (((Red) (Green) (Blue))))\n\
     (declare-const c Color)\n\
     (declare-const k Int)\n\
     (assert (= c Red))\n\
     (assert (= k 5))\n\
     (check-sat)\n"
    Session.Sat;
  (* Value-correctness: an eliminated k compared against a selector output. SAT requires
     the reconstructed k to equal the tree's key (2); a wrong/absent value degrades to
     unknown. *)
  expect
    "V key t = k, k=2, key is 2 (value correct)"
    "(declare-datatypes ((Tree 0)) (((Node (left Tree) (key Int) (right Tree)) (Empty))))\n\
     (declare-const t Tree)\n\
     (declare-const k Int)\n\
     (assert (= t (Node Empty 2 Empty)))\n\
     (assert (= k 2))\n\
     (assert (= (key t) k))\n\
     (check-sat)\n"
    Session.Sat;
  (* Soundness: reconstruction must use the PRESOLVE WITNESS (k=2), not a value that makes
     the query pass. key t = 5 but k = 2 with (= (key t) k) is 5=2 -> the model-check must
     reject (a buggy reconstruction reading k=5 would wrongly pass). Never [Sat]. *)
  expect_not_sat
    "S1 key t=5, k=2, key t=k (wrong-value guard)"
    "(declare-datatypes ((Tree 0)) (((Node (left Tree) (key Int) (right Tree)) (Empty))))\n\
     (declare-const t Tree)\n\
     (declare-const k Int)\n\
     (assert (= t (Node Empty 5 Empty)))\n\
     (assert (= k 2))\n\
     (assert (= (key t) k))\n\
     (check-sat)\n";
  (* Soundness: enum + k=2 /\ k>5 -> UNSAT (must not become a wrong sat). *)
  expect
    "S2 enum + k=2 /\\ k>5 -> UNSAT"
    "(declare-datatypes ((Color 0)) (((Red) (Green) (Blue))))\n\
     (declare-const c Color)\n\
     (declare-const k Int)\n\
     (assert (= c Red))\n\
     (assert (= k 2))\n\
     (assert (> k 5))\n\
     (check-sat)\n"
    Session.Unsat;
  Printf.printf "\ndtlia residuals gate (applied uninterpreted predicates, QF_UFDT):\n%!";
  (* P recovery: applied predicate over a selector output. RED with PRED off. *)
  expect
    "P p(key t), t=Node(_,3,_) -> sat"
    "(declare-datatypes ((Tree 0)) (((Node (left Tree) (key Int) (right Tree)) (Empty))))\n\
     (declare-fun p (Int) Bool)\n\
     (declare-const t Tree)\n\
     (assert (= t (Node Empty 3 Empty)))\n\
     (assert (p (key t)))\n\
     (check-sat)\n"
    Session.Sat;
  (* P2 recovery: applied predicate over a plain Int, datatype present. *)
  expect
    "P2 p(x), enum present -> sat"
    "(declare-datatypes ((Color 0)) (((Red) (Green) (Blue))))\n\
     (declare-fun p (Int) Bool)\n\
     (declare-const c Color)\n\
     (declare-const x Int)\n\
     (assert (= c Red))\n\
     (assert (p x))\n\
     (check-sat)\n"
    Session.Sat;
  (* P3 mixed polarity, distinct selector values -> sat (p(key t1)=T, p(key t2)=F, keys
     DIFFER so no functionality conflict). *)
  expect
    "P3 p(key t1) /\\ ~p(key t2), keys 1 vs 2 -> sat"
    "(declare-datatypes ((Tree 0)) (((Node (left Tree) (key Int) (right Tree)) (Empty))))\n\
     (declare-fun p (Int) Bool)\n\
     (declare-const t1 Tree)\n\
     (declare-const t2 Tree)\n\
     (assert (= t1 (Node Empty 1 Empty)))\n\
     (assert (= t2 (Node Empty 2 Empty)))\n\
     (assert (p (key t1)))\n\
     (assert (not (p (key t2))))\n\
     (check-sat)\n"
    Session.Sat;
  (* PCONG soundness: congruence violation p(key t1) /\ ~p(key t2) /\ key t1 = key t2 ->
     UNSAT (solver refutes via p-congruence; must never become sat). *)
  expect_not_sat
    "PCONG p(key t1) /\\ ~p(key t2) /\\ key t1=key t2 (never sat)"
    "(declare-datatypes ((Tree 0)) (((Node (left Tree) (key Int) (right Tree)) (Empty))))\n\
     (declare-fun p (Int) Bool)\n\
     (declare-const t1 Tree)\n\
     (declare-const t2 Tree)\n\
     (assert (= (key t1) (key t2)))\n\
     (assert (p (key t1)))\n\
     (assert (not (p (key t2))))\n\
     (check-sat)\n";
  if !failures > 0
  then (
    Printf.printf "dtlia-residuals gate: %d failure(s)\n%!" !failures;
    exit 1)
  else Printf.printf "dtlia-residuals gate: all required checks passed\n%!"
;;
