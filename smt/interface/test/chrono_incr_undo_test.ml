(* ADR-0014 S4.2 chrono incremental-undo DRIVER REDs (fix stage for review PIN
   78228a50ee). Two flag-ON soundness gaps the TCB review promoted to land blockers,
   reproduced against the REAL cdclt driver + a REAL Combined (EUF+LIA) theory — not a
   mock. Each drives the exact seam callbacks the SAT core invokes ([on_assign] /
   [on_chrono_rewind] / [check] / [reset_for_new_query]) via the {!Cdclt} test re-exports,
   so the check exercises the shipped driver logic, and observes the theory's own
   SAT/UNSAT verdict (LIA feasibility) and the driver's absolute-trail-indexed [ckpt_log]
   length.

   H1 (codex 2.1, WRONG-SAT hazard): a zero-removal chrono [cancel_until] passes
   [w = trail_n] (reachable when an already-true assumption opens a dummy decision level
   with no trail literal). Pre-fix, [w >= ckpt_log length] fell through to the [base_ckpt]
   fallback and REWOUND the theory to before-any-assertion while the Boolean trail
   retained everything — the theory forgets its constraints and a following Final can
   report Sat on an under-constrained theory. Fix: [w >= length] is a NO-OP (nothing
   at/after [w] was removed). RED: after the zero-removal rewind the theory must still see
   its conflict.

   H2 (codex 2.3, incremental index skew): [reset_for_new_query] cleared [ckpt_log] while
   the SAT core RETAINS its level-0 trail prefix across queries, so query-2 [on_assign]s
   landed at the wrong absolute index and a later [rewind w] mis-targeted (or hit the
   unsafe fallback). Fix: reset invalidates each retained entry to a [None] spacer but
   PRESERVES the count, keeping the log absolute-trail-indexed. RED: the retained-prefix
   count survives the reset, query-2 entries land at their true absolute index, and a
   rewind that drops the last query-2 literal restores exactly the intended sub-state.

   Both flags are set in-process (before any [Cdclt.create] forces the [incr_undo] lazy)
   so the test is correct regardless of the invoking env. Run via `make
   chrono-incr-undo-test`; also built by `make build`. Stdlib-only over the interface
   stack (I3 firewall). *)

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

type fx =
  { cdclt : Cdclt.t
  ; ctx : Context.t
  ; xs : Term.t array
  }

let make () =
  let env, cap = Env.create_with_cap () in
  let xsyms =
    Array.init 3 (fun i ->
      Env.declare_fun env (Printf.sprintf "x%d" i) (Rank.create [] Sort.int))
  in
  let ctx = Context.create env in
  let sat = Sat.create () in
  let budget = Budget.create () in
  let registry = ref Oxsmt_core.Datatype_defs.empty in
  let array_registry = ref Oxsmt_core.Array_defs.empty in
  let cdclt =
    Cdclt.create
      ctx
      env
      sat
      ~split_budget:10_000
      ~budget
      ~registry
      ~array_registry
      ~arithmetic_family:(ref Cdclt.None_seen)
      ~cap
  in
  { cdclt; ctx; xs = Array.map (Context.const ctx) xsyms }
;;

(* the atom [x_i <= c] *)
let le fx i c =
  Context.le
    fx.ctx
    (Context.linear_combination fx.ctx [ 1, fx.xs.(i) ] (-c))
    (Context.int_const fx.ctx 0)
;;

(* the atom [x_i >= c] as [-x_i + c <= 0] (a DISTINCT Le term from {!le}) *)
let ge fx i c =
  Context.le
    fx.ctx
    (Context.linear_combination fx.ctx [ -1, fx.xs.(i) ] c)
    (Context.int_const fx.ctx 0)
;;

(* Intern [atom], then drive the seam [on_assign] for its POSITIVE trail literal — exactly
   what the SAT core does when it places the literal on the trail (logs one [ckpt_log]
   entry and asserts to the theory under the flag). *)
let assign_true fx atom ~level =
  let v = Cdclt.intern_atom fx.cdclt atom in
  Cdclt.on_assign_for_test fx.cdclt (Sat.pos v) ~level;
  v
;;

let is_conflict = function
  | Sat.T_conflict _ -> true
  | _ -> false
;;

let is_consistent = function
  | Sat.T_consistent _ -> true
  | _ -> false
;;

(* ------------------------------------------------------------------ H1 *)
let test_h1_zero_removal_no_wipe () =
  let fx = make () in
  let _ = assign_true fx (le fx 0 0) ~level:0 (* x0 <= 0 *) in
  let _ = assign_true fx (ge fx 0 1) ~level:1 (* x0 >= 1 *) in
  (* flag-path sanity: [on_assign] logged both checkpoints (proves [incr_undo] is ON —
     with the flag off the RED would be vacuous). *)
  check
    "h1: flag path live (ckpt_log populated)"
    (Cdclt.ckpt_log_length_for_test fx.cdclt = 2);
  check
    "h1: pre-rewind Final = conflict (x0<=0 /\\ x0>=1 unsat)"
    (is_conflict (Cdclt.check_for_test fx.cdclt ~final:true));
  (* zero-removal chrono cancel_until: [w = trail_n = ckpt_log length]; nothing removed. *)
  let w = Cdclt.ckpt_log_length_for_test fx.cdclt in
  Cdclt.on_chrono_rewind_for_test fx.cdclt w;
  (* THE RED: pre-fix this wiped the theory to base (Final would flip to consistent — a
     would-be wrong Sat); post-fix it is a NO-OP and the conflict survives. *)
  check
    "h1: zero-removal rewind must NOT wipe live theory (Final still conflict)"
    (is_conflict (Cdclt.check_for_test fx.cdclt ~final:true));
  check
    "h1: zero-removal rewind preserves ckpt_log length"
    (Cdclt.ckpt_log_length_for_test fx.cdclt = w)
;;

(* ------------------------------------------------------------------ H2 *)
let test_h2_cross_query_index_alignment () =
  let fx = make () in
  (* Query 1: two consistent level-0 literals the SAT core RETAINS across the query
     boundary (x1<=5, x2<=5). *)
  let _ = assign_true fx (le fx 1 5) ~level:0 in
  let _ = assign_true fx (le fx 2 5) ~level:0 in
  check "h2: query-1 ckpt_log = 2" (Cdclt.ckpt_log_length_for_test fx.cdclt = 2);
  (* Query boundary: registry replaced -> reset. The SAT core keeps the 2 retained level-0
     literals on its trail, so the driver must keep [ckpt_log] the same length (absolute
     index alignment) rather than clear it. *)
  Cdclt.reset_for_new_query fx.cdclt;
  check
    "h2: reset preserves retained-trail alignment (ckpt_log length stays 2)"
    (Cdclt.ckpt_log_length_for_test fx.cdclt = 2);
  (* Query 2: new literals land at ABSOLUTE trail indices 2, 3. {C,D} is unsat, {C} sat. *)
  let _ = assign_true fx (le fx 0 0) ~level:0 (* C: x0 <= 0 *) in
  let _ = assign_true fx (ge fx 0 1) ~level:1 (* D: x0 >= 1 *) in
  check
    "h2: query-2 entries land at true absolute index (ckpt_log length 4)"
    (Cdclt.ckpt_log_length_for_test fx.cdclt = 4);
  check
    "h2: pre-rewind Final = conflict ({C,D} unsat)"
    (is_conflict (Cdclt.check_for_test fx.cdclt ~final:true));
  (* The core removes D at absolute trail index 3 (first_removed = 3), keeping the
     retained prefix + C. *)
  Cdclt.on_chrono_rewind_for_test fx.cdclt 3;
  (* THE RED: post-fix the log is index-aligned, so [rewind 3] targets the watermark just
     before D and the theory drops exactly D, holding [{C}] -> consistent. Pre-fix the
     cleared log made query-2's C/D land at index 0/1, so [rewind 3] over-shot the end and
     did nothing, leaving [{C,D}] -> a stale conflict citing a removed literal. *)
  check
    "h2: after removing D, Final = consistent (theory holds only C)"
    (is_consistent (Cdclt.check_for_test fx.cdclt ~final:true))
;;

let () =
  (* Arm the dark flag in-process BEFORE the first [Cdclt.create] forces the [incr_undo]
     lazy, so the test exercises the flag-ON driver path regardless of the invoking env. *)
  Unix.putenv "OXSMT_CHRONO" "1";
  Unix.putenv "OXSMT_CHRONO_INCR_UNDO" "1";
  test_h1_zero_removal_no_wipe ();
  test_h2_cross_query_index_alignment ();
  Printf.printf "chrono_incr_undo_test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
