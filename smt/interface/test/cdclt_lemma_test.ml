(* H1 seam coverage for CONTRACT-LEMMA (adr-0005-contract-lemma-erratum, review r2 H1):
   the cdclt-level [split_lit] desugar and the both-efforts delivery, exercised through
   the REAL clausifier via {!Cdclt.desugar_result_for_test} (a thin re-export of the exact
   function {!Cdclt.check} uses — no re-implemented copy).

   The prior coverage (seam_test's [test_valid_lemma_propagates]) built the desugared
   [Sat.T_lemma] clause directly and never reached [Cdclt.check] / [split_lit]. These
   tests close that gap:
   - [test_lemma_desugar_final]: a multi-antecedent Lemma clausifies to ONE clause whose
     head is the positive interned head atom and whose antecedents are the NEGATIVE
     interned antecedent atoms (per-disjunct sign), and a negative-HEAD variant flips the
     head.
   - [test_lemma_not_peeling]: a disjunct term that is [Not p] peels through [split_lit]'s
     parity tracking to the OPPOSITE literal of [p] (interning [Not p] as a fresh positive
     atom would be the wrong clause the desugar must avoid).
   - [test_lemma_vs_split_at_propagate]: the load-bearing distinction — a [Lemma] is
     desugared (NOT dropped) at [Propagate] (T_lemma), whereas a [Split] is dropped there
     (T_consistent []) and clausified only at [Final]. This is the arm that decides Option
     A over B.

   Stdlib-only over the interface stack (I3 firewall): oxsmt_core + oxsmt_interface +
   oxsmt_solver. Deterministic (hand cases). Run via `make cdclt-lemma-test`. *)

open Oxsmt_core
open Oxsmt_interface
module Sat = Oxsmt_solver.Sat

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

(* A fresh cdclt over a handful of Int vars + Bool predicates, plus their atom builders. *)
type fx =
  { ctx : Context.t
  ; cdclt : Cdclt.t
  ; xs : Term.t array
  ; ps : Term.t array
  }

let make () =
  let env, cap = Env.create_with_cap () in
  let xsyms =
    Array.init 3 (fun i ->
      Env.declare_fun env (Printf.sprintf "x%d" i) (Rank.create [] Sort.int))
  in
  let psyms =
    Array.init 2 (fun i ->
      Env.declare_fun env (Printf.sprintf "p%d" i) (Rank.create [] Sort.bool))
  in
  let ctx = Context.create env in
  let sat = Sat.create () in
  let budget = Budget.create () in
  let registry = ref Oxsmt_core.Datatype_defs.empty in
  let array_registry = ref Oxsmt_core.Array_defs.empty in
  let cdclt =
    Cdclt.create ctx env sat ~split_budget:10_000 ~budget ~registry ~array_registry ~cap
  in
  { ctx
  ; cdclt
  ; xs = Array.map (Context.const ctx) xsyms
  ; ps = Array.map (Context.const ctx) psyms
  }
;;

(* the atom [x_i <= c] *)
let le fx i c =
  Context.le
    fx.ctx
    (Context.linear_combination fx.ctx [ 1, fx.xs.(i) ] (-c))
    (Context.int_const fx.ctx 0)
;;

let one_clause name = function
  | Sat.T_lemma [ clause ] -> Some clause
  | _ ->
    check (name ^ ": T_lemma with exactly one clause") false;
    None
;;

(* ------------------------------------------------------------------ *)

(* Multi-antecedent + per-disjunct sign (positive head, negative antecedents), and a
   negative-head variant. The head/antecedent atoms are DISTINCT (CONTRACT-LEMMA). *)
let test_lemma_desugar_final () =
  let fx = make () in
  let head = le fx 0 5
  and a1 = le fx 1 3
  and a2 = le fx 2 7 in
  (* intern independently to compare vars (idempotent, hash-consed) *)
  let vh = Cdclt.intern_atom fx.cdclt head
  and va1 = Cdclt.intern_atom fx.cdclt a1
  and va2 = Cdclt.intern_atom fx.cdclt a2 in
  check "distinct atoms: head<>a1<>a2" (vh <> va1 && vh <> va2 && va1 <> va2);
  let r =
    Cdclt.desugar_result_for_test
      fx.cdclt
      ~final:true
      (Theory.Lemma [ head, true; a1, false; a2, false ])
  in
  (match one_clause "final" r with
   | Some [ l0; l1; l2 ] ->
     check "final: 3-literal clause" true;
     check "final: head desugars to pos(head)" (l0 = Sat.pos vh);
     check "final: antecedent-1 desugars to neg(a1)" (l1 = Sat.neg va1);
     check "final: antecedent-2 desugars to neg(a2)" (l2 = Sat.neg va2)
   | Some _ -> check "final: clause has 3 literals" false
   | None -> ());
  (* negative-head variant: (head,false) -> neg(head) *)
  let r2 =
    Cdclt.desugar_result_for_test
      fx.cdclt
      ~final:true
      (Theory.Lemma [ head, false; a1, false ])
  in
  match one_clause "neg-head" r2 with
  | Some [ l0; l1 ] ->
    check "neg-head: head (sign false) desugars to neg(head)" (l0 = Sat.neg vh);
    check "neg-head: antecedent desugars to neg(a1)" (l1 = Sat.neg va1)
  | Some _ | None -> check "neg-head: 2-literal clause" false
;;

(* [Not p] as a disjunct term peels through split_lit's parity tracking: [(Not p, true)]
   is the literal [¬p] (= [Sat.neg (intern p)]), NOT a fresh positive atom for the Not
   node; [(Not p, false)] double-negates to [p]. *)
let test_lemma_not_peeling () =
  let fx = make () in
  let notp = Context.not_ fx.ctx fx.ps.(0) in
  (* precondition: the builder actually produced a [Not] node (else the peel is untested) *)
  check
    "not-peel: Context.not_ built a Not node"
    (match notp.Term.node with
     | Term.Not _ -> true
     | _ -> false);
  let vp = Cdclt.intern_atom fx.cdclt fx.ps.(0) in
  let r1 =
    Cdclt.desugar_result_for_test fx.cdclt ~final:true (Theory.Lemma [ notp, true ])
  in
  (match one_clause "not-peel-true" r1 with
   | Some [ l ] -> check "not-peel: (Not p, true) => neg(p)" (l = Sat.neg vp)
   | Some _ | None -> check "not-peel-true: single-literal clause" false);
  let r2 =
    Cdclt.desugar_result_for_test fx.cdclt ~final:true (Theory.Lemma [ notp, false ])
  in
  match one_clause "not-peel-false" r2 with
  | Some [ l ] ->
    check "not-peel: (Not p, false) => pos(p) (double negation)" (l = Sat.pos vp)
  | Some _ | None -> check "not-peel-false: single-literal clause" false
;;

(* The load-bearing arm (Option A decider): a Lemma is clausified at BOTH efforts and is
   NEVER dropped at Propagate; a Split is dropped at Propagate and clausified only at
   Final. *)
let test_lemma_vs_split_at_propagate () =
  let fx = make () in
  let head = le fx 0 5
  and a1 = le fx 1 3 in
  let lemma = Theory.Lemma [ head, true; a1, false ] in
  let split = Theory.Split [ head; a1 ] in
  check
    "propagate: Lemma NOT dropped (=> T_lemma)"
    (match Cdclt.desugar_result_for_test fx.cdclt ~final:false lemma with
     | Sat.T_lemma [ _ ] -> true
     | _ -> false);
  check
    "propagate: Split dropped (=> T_consistent [])"
    (match Cdclt.desugar_result_for_test fx.cdclt ~final:false split with
     | Sat.T_consistent [] -> true
     | _ -> false);
  check
    "final: Split clausified (=> T_lemma)"
    (match Cdclt.desugar_result_for_test fx.cdclt ~final:true split with
     | Sat.T_lemma [ _ ] -> true
     | _ -> false);
  check
    "final: Lemma clausified identically (=> T_lemma)"
    (match Cdclt.desugar_result_for_test fx.cdclt ~final:true lemma with
     | Sat.T_lemma [ _ ] -> true
     | _ -> false)
;;

let () =
  test_lemma_desugar_final ();
  test_lemma_not_peeling ();
  test_lemma_vs_split_at_propagate ();
  Printf.printf "cdclt_lemma_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
