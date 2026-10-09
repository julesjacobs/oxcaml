(* Codex C1/C2 wrong-Unsat reproducers (task #101 lemma tranche 1), kept durable for the
   SCOPED CONFIRM round. Drop into smt/ematch/test/ with a matching dune executable
   stanza:

   (executable (name crit_repro) (modules crit_repro) (libraries oxsmt_core oxsmt_solver
   oxsmt_ematch oxsmt_interface))

   Discrimination standard: against the UNFIXED tip (1620caec22) BOTH print "unsat" (RED,
   wrong-Unsat). Against a correct fix each must print NOT-unsat (Unknown), or the guarded
   entry must RAISE (a rejection) — both are treated here as PASS. Confirmed RED at
   1620caec22 by lemma-reviewer-2. *)

open Oxsmt_core
module Session = Oxsmt_interface.Session
module Qvar = Oxsmt_ematch.Qvar

let failures = ref 0

let verdict_str = function
  | Session.Sat -> "sat"
  | Session.Unsat -> "unsat"
  | Session.Unknown -> "unknown"
;;

(* PASS iff the guarded run does NOT yield a client-visible unsat: either a sound Unknown
   verdict, or the fixed code raised at the injection/assert point (a rejection). *)
let expect_not_unsat name thunk =
  match thunk () with
  | v ->
    let bad = v = Session.Unsat in
    if bad then incr failures;
    Printf.printf "%s %s: got %s\n" (if bad then "RED " else "ok  ") name (verdict_str v)
  | exception e -> Printf.printf "ok   %s: rejected (%s)\n" name (Printexc.to_string e)
;;

let int_to_bool = Rank.create [ Sort.int ] Sort.bool

(* C1: the assert gate rejects only .oxsmt.qvar.*, but Symbol.intern is PUBLIC. A user
   interns a PREPROCESSING WITNESS name (.oxsmt.ite.0) and asserts a value colliding with
   the witness's internal constraint -> wrong Unsat. Fix: gate the whole .oxsmt.* prefix. *)
let c1 () =
  expect_not_unsat "C1 witness-capture-via-public-intern" (fun () ->
    let s = Session.create () in
    let ctx = Session.context s in
    let c = Context.const ctx (Session.declare_const s "c" Sort.bool) in
    Session.assert_term s c;
    Session.assert_term
      s
      (Context.ge
         ctx
         (Context.ite ctx c (Context.int_const ctx 10) (Context.int_const ctx 11))
         (Context.int_const ctx 0));
    let w = Context.const ctx (Symbol.intern ".oxsmt.ite.0") in
    Session.assert_term s (Context.eq ctx w (Context.int_const ctx 20));
    Session.check_sat s)
;;

(* C2: lemma handles carry no session identity. A Lemma.t from session A seeded into B via
   instantiate is asserted under A's frame-selector int (colliding with B's base) -> B
   gets a constraint from a lemma it never stated -> wrong Unsat. Fix: bind the handle/cap
   to a session identity and reject a foreign handle. *)
let c2 () =
  expect_not_unsat "C2 cross-session-lemma-injection" (fun () ->
    let s_a = Session.create () in
    let ctx_a = Session.context s_a in
    let p_a = Session.declare_fun s_a "p" int_to_bool in
    let _a_a = Session.declare_const s_a "a" Sort.int in
    let l_a =
      Session.assert_lemma
        s_a
        ~qvars:[ "x", Sort.int ]
        ~build:(fun qv ->
          { Session.body = Context.app ctx_a p_a [ Qvar.to_term qv.(0) ]; triggers = [] })
    in
    let s_b = Session.create () in
    let ctx_b = Session.context s_b in
    let p_b = Session.declare_fun s_b "p" int_to_bool in
    let a_b = Context.const ctx_b (Session.declare_const s_b "a" Sort.int) in
    Session.assert_term s_b (Context.not_ ctx_b (Context.app ctx_b p_b [ a_b ]));
    let _l_b =
      Session.assert_lemma
        s_b
        ~qvars:[ "y", Sort.int ]
        ~build:(fun qv ->
          { Session.body = Context.eq ctx_b (Qvar.to_term qv.(0)) (Qvar.to_term qv.(0))
          ; triggers = []
          })
    in
    Session.instantiate s_b l_a [| a_b |];
    Session.check_sat s_b)
;;

let () =
  c1 ();
  c2 ();
  if !failures > 0
  then (
    Printf.printf "\n%d RED (wrong-Unsat present)\n" !failures;
    exit 1)
  else Printf.printf "\nboth PASS (no wrong-Unsat)\n"
;;
