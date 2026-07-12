(* Lemma-tier tranche-1 acceptance honeypots (ADR-0012 §2/§7), end-to-end through the real
   Session stack. Tranche 1 ships a TRIVIAL matcher (the manual-instances path,
   [Session.instantiate], §8), so these exercise the store + THE SOUNDNESS RULE + the
   frame-scoped provenance + the dedup lifetime + the assert-side qvar gate — NOT
   trigger-finding (that arrives with the matcher in tranche 2, at which point H-REFUTE /
   H-REPEAT-REFUTE become .smt2 matcher tests). A tranche-1 green must not be over-read as
   validating a matcher that does not yet exist (§7 M5).

   Determinism (I6): no wall-clock; every verdict is a pure function of the input. *)

open Oxsmt_core
module Session = Oxsmt_interface.Session
module Qvar = Oxsmt_ematch.Qvar

let failures = ref 0
let passes = ref 0

let check name cond =
  if cond
  then (
    incr passes;
    Printf.printf "ok   %s\n" name)
  else (
    incr failures;
    Printf.printf "FAIL %s\n" name)
;;

let verdict_str = function
  | Session.Sat -> "sat"
  | Session.Unsat -> "unsat"
  | Session.Unknown -> "unknown"
;;

let int_to_int = Rank.create [ Sort.int ] Sort.int
let int_to_bool = Rank.create [ Sort.int ] Sort.bool

(* ------------------------------------------------------------------ *)
(* H-SOUND (§2): a live lemma over a ground-satisfiable core whose instance does NOT
   refute must degrade [Sat] -> [Unknown]. Adapted from the ADR's UFLIA example to a
   table-free LIA fragment so the ground check yields a GENUINE [Sat] (a reconstructable
   const model), making THE SOUNDNESS RULE — not a model-reconstruction failure — the
   thing that forces [Unknown]. (The UFLIA .smt2 form lands in tranche 2 with QF_UFLIA
   models + the matcher.) Guard under test: the live-lemma liveness check in check_sat's
   single Sat exit. Removing it reports [Sat] here -> RED. *)
let h_sound () =
  let s = Session.create () in
  let ctx = Session.context s in
  let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
  (* lemma: forall x. x + 0 = x (always true; kept live) *)
  let lemma =
    Session.assert_lemma
      s
      ~qvars:[ "x", Sort.int ]
      ~build:(fun qv ->
        let x = Qvar.to_term qv.(0) in
        let body = Context.eq ctx (Context.add ctx x (Context.int_const ctx 0)) x in
        { Session.body; triggers = [] })
  in
  (* ground: a >= 0 (table-free, satisfiable) *)
  Session.assert_term s (Context.ge ctx a (Context.int_const ctx 0));
  (* a consistent instance: x |-> a (yields a+0 = a, i.e. true — no refutation) *)
  Session.instantiate s lemma [| a |];
  let v = Session.check_sat s in
  check
    (Printf.sprintf "H-SOUND: live lemma degrades sat->unknown (got %s)" (verdict_str v))
    (v = Session.Unknown)
;;

(* ------------------------------------------------------------------ *)
(* H-REFUTE (§2): the dual — an instance that DOES close the goal must survive as [Unsat],
   proving the rule is not "always unknown when a quantifier is present". Guard under
   test: the instance actually flows through the pipeline to the theory core. Remove the
   seed / the instance-assert and this goes [Unknown] -> RED. *)
let h_refute () =
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" int_to_int in
  let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
  let fa = Context.app ctx f [ a ] in
  (* lemma: forall x. f(x) > 0 *)
  let lemma =
    Session.assert_lemma
      s
      ~qvars:[ "x", Sort.int ]
      ~build:(fun qv ->
        let x = Qvar.to_term qv.(0) in
        let body = Context.gt ctx (Context.app ctx f [ x ]) (Context.int_const ctx 0) in
        { Session.body; triggers = [ [ Context.app ctx f [ x ] ] ] })
  in
  (* ground: f(a) < 0 (contradicts the instance f(a) > 0) *)
  Session.assert_term s (Context.lt ctx fa (Context.int_const ctx 0));
  Session.instantiate s lemma [| a |];
  let v = Session.check_sat s in
  check
    (Printf.sprintf "H-REFUTE: instance closes goal -> unsat (got %s)" (verdict_str v))
    (v = Session.Unsat)
;;

(* ------------------------------------------------------------------ *)
(* H-PUSHPOP (C1, §7): a lemma asserted in a push, an instance drawn from it, then pop,
   then a ground fact contradicting the (now-retracted) instance. The final check must NOT
   be [unsat] — the instance retracts WITH its lemma's frame. Guard under test:
   assert_instance_at_frame guards by the LEMMA's frame selector, not the innermost/base.
   If it used the base selector the instance would survive the pop -> [unsat] -> RED (the
   C1 wrong-unsat). *)
let h_pushpop () =
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" int_to_int in
  let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
  let fa = Context.app ctx f [ a ] in
  Session.push s;
  let lemma =
    Session.assert_lemma
      s
      ~qvars:[ "x", Sort.int ]
      ~build:(fun qv ->
        let x = Qvar.to_term qv.(0) in
        let body = Context.eq ctx (Context.app ctx f [ x ]) (Context.int_const ctx 5) in
        { Session.body; triggers = [ [ Context.app ctx f [ x ] ] ] })
  in
  Session.assert_term s (Context.gt ctx fa (Context.int_const ctx 0));
  Session.instantiate s lemma [| a |];
  (* f(a)=5 drawn under the pushed lemma's frame *)
  let v1 = Session.check_sat s in
  check
    (Printf.sprintf "H-PUSHPOP: live pushed lemma -> unknown (got %s)" (verdict_str v1))
    (v1 = Session.Unknown);
  Session.pop s;
  (* f(a)=7 alone is sat; the retracted f(a)=5 must NOT strand *)
  Session.assert_term s (Context.eq ctx fa (Context.int_const ctx 7));
  let v2 = Session.check_sat s in
  check
    (Printf.sprintf "H-PUSHPOP: post-pop must NOT be unsat (got %s)" (verdict_str v2))
    (v2 <> Session.Unsat)
;;

(* ------------------------------------------------------------------ *)
(* H-REPEAT-REFUTE (R2, §7): an instance drawn under a PUSHED lemma (so its clause + dedup
   entry deactivate on pop), then a NEW equivalent lemma in the base frame must RE-derive
   it. Guard under test: the dedup cache is scoped to active-clause lifetime (dropped on
   the owning frame's pop). A permanent dedup suppresses L2's regeneration -> the base
   goal goes unrefuted -> [unknown] -> RED (distinct degenerate from H-PUSHPOP). *)
let h_repeat_refute () =
  let s = Session.create () in
  let ctx = Session.context s in
  let p = Session.declare_fun s "p" int_to_bool in
  let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
  let pa = Context.app ctx p [ a ] in
  (* base goal, survives pops *)
  Session.assert_term s (Context.not_ ctx pa);
  Session.push s;
  let l1 =
    Session.assert_lemma
      s
      ~qvars:[ "x", Sort.int ]
      ~build:(fun qv ->
        { Session.body = Context.app ctx p [ Qvar.to_term qv.(0) ]; triggers = [] })
  in
  Session.instantiate s l1 [| a |];
  let v1 = Session.check_sat s in
  check
    (Printf.sprintf "H-REPEAT-REFUTE: L1 refutes -> unsat (got %s)" (verdict_str v1))
    (v1 = Session.Unsat);
  Session.pop s;
  (* L2: new equivalent lemma, base frame; must re-derive p(a) (dedup entry was dropped) *)
  let l2 =
    Session.assert_lemma
      s
      ~qvars:[ "x", Sort.int ]
      ~build:(fun qv ->
        { Session.body = Context.app ctx p [ Qvar.to_term qv.(0) ]; triggers = [] })
  in
  Session.instantiate s l2 [| a |];
  let v2 = Session.check_sat s in
  check
    (Printf.sprintf "H-REPEAT-REFUTE: L2 re-derives -> unsat (got %s)" (verdict_str v2))
    (v2 = Session.Unsat)
;;

(* ------------------------------------------------------------------ *)
(* Assert-side qvar gate (R1 POINT 4, §1.1): a coerced placeholder reaching [assert_term]
   degrades to a clean [Unknown] (via the I8 Unsupported discipline), NEVER a crash and
   never registered. This closes the [Qvar.t]-coercion escape that the private alias alone
   does not. Guard under test: the [Qvar.term_contains_qvar] gate at the top of
   assert_term. Remove it and the placeholder reaches the solver. *)
let coercion_gate () =
  let s = Session.create () in
  let ctx = Session.context s in
  let f = Session.declare_fun s "f" int_to_int in
  (* mint a placeholder directly and coerce it into an asserted term *)
  let q = Qvar.to_term (Qvar.mint (Session.env s) ctx ~lemma_id:999 ~index:0 Sort.int) in
  Session.assert_term
    s
    (Context.eq ctx (Context.app ctx f [ q ]) (Context.int_const ctx 0));
  let v = Session.check_sat s in
  check
    (Printf.sprintf "GATE: coerced placeholder -> clean unknown (got %s)" (verdict_str v))
    (v = Session.Unknown);
  (* and it did not crash: reaching here at all is the no-[Failure] half of the check *)
  check "GATE: no crash on coerced placeholder" true
;;

(* ------------------------------------------------------------------ *)
(* Determinism smoke: the honeypots run twice byte-identically (I6). A tight-budget
   verdict-affecting determinism regression is a tranche-3 test (R7); this is the
   tranche-1 floor. *)
let determinism () =
  let run () =
    let s = Session.create () in
    let ctx = Session.context s in
    let f = Session.declare_fun s "f" int_to_int in
    let a = Context.const ctx (Session.declare_const s "a" Sort.int) in
    let lemma =
      Session.assert_lemma
        s
        ~qvars:[ "x", Sort.int ]
        ~build:(fun qv ->
          let x = Qvar.to_term qv.(0) in
          { Session.body =
              Context.gt ctx (Context.app ctx f [ x ]) (Context.int_const ctx 0)
          ; triggers = []
          })
    in
    Session.assert_term
      s
      (Context.lt ctx (Context.app ctx f [ a ]) (Context.int_const ctx 0));
    Session.instantiate s lemma [| a |];
    let v = Session.check_sat s in
    let st = Session.lemma_stats s in
    verdict_str v, st.instances, st.rounds
  in
  check "DETERMINISM: two runs identical" (run () = run ())
;;

let () =
  h_sound ();
  h_refute ();
  h_pushpop ();
  h_repeat_refute ();
  coercion_gate ();
  determinism ();
  Printf.printf "\n%d passed, %d failed\n" !passes !failures;
  if !failures > 0 then exit 1
;;
