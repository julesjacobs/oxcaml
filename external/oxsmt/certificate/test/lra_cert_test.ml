(* Real (LRA) certificate soundness under the OXSMT_LRA flip (riders #162 / #134).

   Two things this gate proves, batched with the LRA-flip suite:

   1. With the Real engine ENABLED, the certificate recorder + replay checker handle a
      live Real UNSAT solve SOUNDLY — the verdict is Valid / Valid-modulo (a conditional
      but sound leaf), never a wrong Valid and never a crash.

   2. The rider #134 "LRA x Farkas-witness collision" RE-CONFIRM, live: the Real path
      emits NO Farkas witness. The single Farkas-witness emission site (cdclt.ml,
      [T_conflict] with [Lia_farkas]) is guarded by [TCombined] (the Int theory); the Real
      theory ([TCombinedReal]) never calls [on_lia_conflict]. So no recorded Real theory
      leaf carries a [lia_witness], and [Checker.verify_lia_conflict]'s Int-only
      [Sort.int] guard (checker.ml: a negated non-integer [Le] premise is fail-closed
      [Invalid]) is never even reached on the live Real path — it stays as
      defense-in-depth. We assert the invariant directly: every recorded theory leaf from
      a Real solve has [lia_witness = None]. *)

let () = Unix.putenv "OXSMT_LRA" "1"

open Oxsmt_core
module Session = Oxsmt_interface.Session
module Recorder = Oxsmt_certificate.Recorder
module Checker = Oxsmt_certificate.Checker
module Cdclt = Oxsmt_interface.Cdclt

let () = if not (Lra_config.enabled ()) then failwith "OXSMT_LRA gate did not enable"
let checks = ref 0
let failures = ref 0

let check name condition =
  incr checks;
  if not condition
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let install rec_ s =
  Session.install_cert_trace s (Some (Recorder.trace rec_));
  Session.install_leaf_certificate_trace
    s
    (Some
       { Cdclt.on_theory_atom =
           (fun ~var ~atom -> Recorder.record_theory_atom rec_ ~var ~atom)
       ; on_euf_leaf = (fun ~clause -> Recorder.record_euf_leaf rec_ ~clause)
       ; on_dt_distinctness =
           (fun ~registry ~clause ~left ~right ->
             Recorder.record_dt_distinctness rec_ ~registry ~clause ~left ~right)
       ; on_lia_conflict =
           (fun ~premise_lits ~multipliers ->
             Recorder.record_lia_conflict rec_ ~premise_lits ~multipliers)
       })
;;

let real_var s name =
  Context.const (Session.context s) (Session.declare_const s name Sort.real)
;;

let verdict_sound = function
  | Checker.Valid | Checker.Valid_modulo_unchecked_steps -> true
  | Checker.Invalid _ | Checker.Unsupported _ -> false
;;

let verdict_str = function
  | Checker.Valid -> "Valid"
  | Checker.Valid_modulo_unchecked_steps -> "Valid_modulo"
  | Checker.Invalid m -> "Invalid(" ^ m ^ ")"
  | Checker.Unsupported m -> "Unsupported(" ^ m ^ ")"
;;

let no_farkas_witness (ev : Checker.events) =
  List.for_all
    (fun (e : Recorder.theory_event) -> e.Recorder.lia_witness = None)
    ev.Checker.theory
;;

(* Strict cycle x < y and y < x over the reals: unsat, closed by the Real theory. *)
let test_strict_cycle () =
  let s = Session.create () in
  let rec_ = Recorder.create () in
  install rec_ s;
  let ctx = Session.context s in
  let x = real_var s "lra_cert_x" in
  let y = real_var s "lra_cert_y" in
  Session.assert_term s (Context.lt ctx x y);
  Session.assert_term s (Context.lt ctx y x);
  check "strict-cycle: solve unsat" (Session.check_sat s = Session.Unsat);
  let ev = Checker.of_recorder rec_ ~assumptions:(Session.cert_assumptions s) in
  let v = Checker.check ev in
  check
    (Printf.sprintf "strict-cycle: cert verdict sound (got %s)" (verdict_str v))
    (verdict_sound v);
  check
    "strict-cycle: no Real Farkas witness recorded (#134 live re-confirm)"
    (no_farkas_witness ev)
;;

(* Equality-elimination shape (W1b-adjacent): x = y defines one real in terms of the
   other, then y < x contradicts it. Exercises the Real path through an equality
   assertion + a theory conflict; the certificate must stay sound. *)
let test_eq_then_conflict () =
  let s = Session.create () in
  let rec_ = Recorder.create () in
  install rec_ s;
  let ctx = Session.context s in
  let x = real_var s "lra_cert_ex" in
  let y = real_var s "lra_cert_ey" in
  Session.assert_term s (Context.eq ctx x y);
  Session.assert_term s (Context.lt ctx y x);
  check "eq-conflict: solve unsat" (Session.check_sat s = Session.Unsat);
  let ev = Checker.of_recorder rec_ ~assumptions:(Session.cert_assumptions s) in
  let v = Checker.check ev in
  check
    (Printf.sprintf "eq-conflict: cert verdict sound (got %s)" (verdict_str v))
    (verdict_sound v);
  check
    "eq-conflict: no Real Farkas witness recorded (#134 live re-confirm)"
    (no_farkas_witness ev)
;;

(* Three-variable transitive strict chain x < y, y < z, z < x: unsat over the reals. A
   larger Real conflict; still no Farkas witness, still sound. *)
let test_transitive_chain () =
  let s = Session.create () in
  let rec_ = Recorder.create () in
  install rec_ s;
  let ctx = Session.context s in
  let x = real_var s "lra_cert_tx" in
  let y = real_var s "lra_cert_ty" in
  let z = real_var s "lra_cert_tz" in
  Session.assert_term s (Context.lt ctx x y);
  Session.assert_term s (Context.lt ctx y z);
  Session.assert_term s (Context.lt ctx z x);
  check "chain: solve unsat" (Session.check_sat s = Session.Unsat);
  let ev = Checker.of_recorder rec_ ~assumptions:(Session.cert_assumptions s) in
  let v = Checker.check ev in
  check
    (Printf.sprintf "chain: cert verdict sound (got %s)" (verdict_str v))
    (verdict_sound v);
  check
    "chain: no Real Farkas witness recorded (#134 live re-confirm)"
    (no_farkas_witness ev)
;;

let () =
  test_strict_cycle ();
  test_eq_then_conflict ();
  test_transitive_chain ();
  Printf.printf "lra_cert_test: %d checks, %d failure(s)\n%!" !checks !failures;
  if !failures > 0 then exit 1
;;
