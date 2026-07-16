(* Direct adapter-level test of the per-query CG-cut budget + its reset (task #53 H3).

   The [OXSMT_CG_MAX_CUTS] budget caps CG-cut ATTEMPTS per adapter instance so an
   unproductive lattice cut cannot dominate a query's wall. The review's H3 point: the cap
   should reset per QUERY. On the corpus it already does (fresh adapter per query); the
   residual is a persisting-theory incremental session, for which
   {!Lia_adapter.reset_cut_budget} is the mechanism. This test PROVES that mechanism
   directly, without the (frozen-interface- blocked) per-check-sat wiring:

   - drive an integer-INFEASIBLE, rationally-feasible, single-row-gcd-BLIND system (the
     multi-row lattice case only a CG/HNF cut sees) so every [check Final] is a cuttable
     Final that reaches the budgeted cut site;
   - each such Final bumps the throttle; on the throttle period an ATTEMPT is made and
     [cut_attempts] rises, until the budget caps it;
   - confirm the cap holds (attempts never exceed the budget), then [reset_cut_budget] and
     confirm [cut_attempts] returns to 0 AND further Finals make attempts again — i.e. the
     budget gate ([cg_attempts >= cg_max_cuts]) is re-opened.

   Run with OXSMT_CG_CUTS=1 (+ a small OXSMT_CG_MAX_CUTS) — the make target sets both. If
   CG is off, the cut site is never entered and the test SKIPS (reports so, exits 0),
   because [cg_cuts_on] is fixed at module load. *)

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

(* --- minimal adapter fixture (mirrors lia_adapter_test) --- *)
type fixture =
  { ctx : Context.t
  ; vars : Term.t array
  ; adapter : Lia_adapter.t
  ; alloc : Atom.allocator
  }

let make_fixture n =
  let env = Env.create () in
  let vsyms =
    Array.init n (fun i ->
      Env.declare_fun env (Printf.sprintf "x%d" i) (Rank.create [] Sort.int))
  in
  let ctx = Context.create env in
  { ctx
  ; vars = Array.map (Context.const ctx) vsyms
  ; adapter = Lia_adapter.create ctx env
  ; alloc = Atom.create_allocator ()
  }
;;

let assert_eq_lc fx coeffs rhs =
  (* Σ coeffs·x = rhs, coeffs by var index *)
  let pairs = List.map (fun (i, c) -> c, fx.vars.(i)) coeffs in
  let lhs = Context.linear_combination fx.ctx pairs 0 in
  let term = Context.eq fx.ctx lhs (Context.int_const fx.ctx rhs) in
  let atom = Atom.fresh fx.alloc in
  Lia_adapter.register_atom fx.adapter atom term;
  Lia_adapter.assert_lit fx.adapter (Lit.make atom true)
;;

(* one Final; is it a cuttable Final (Split/Lemma) rather than a terminal verdict? *)
let final_is_cuttable fx =
  match Lia_adapter.check fx.adapter Theory.Final with
  | Theory.Split _ | Theory.Lemma _ -> true
  | Theory.Sat | Theory.Conflict _ | Theory.Propagations _ -> false
;;

let cg_on =
  match Sys.getenv_opt "OXSMT_CG_CUTS" with
  | Some ("1" | "true" | "yes" | "on") -> true
  | _ -> false
;;

let () =
  print_endline "cut-budget self-test (task #53 H3 reset mechanism):";
  if not cg_on
  then
    print_endline
      "  SKIP: OXSMT_CG_CUTS not set — cut site inert at module load (run via `make \
       cut-budget-test`)"
  else (
    (* Multi-row ℤ-infeasible: x0 + 2·x1 = 0, 2·x0 + x1 = 1. ℚ-vertex (2/3, -1/3); every
       single-row gcd divides its rhs, so it is diophantine-blind and reaches the cut. *)
    let fx = make_fixture 2 in
    assert_eq_lc fx [ 0, 1; 1, 2 ] 0;
    assert_eq_lc fx [ 0, 2; 1, 1 ] 1;
    check "fresh adapter: cut_attempts = 0" (Lia_adapter.cut_attempts fx.adapter = 0);
    (* PHASE 1: drive cuttable Finals; the budget must cap the attempt count. *)
    let cuttable_seen = ref 0 in
    for _ = 1 to 60 do
      if final_is_cuttable fx then incr cuttable_seen
    done;
    let cap = Lia_adapter.cut_attempts fx.adapter in
    check
      "phase 1: every Final was cuttable (system stays lattice-infeasible)"
      (!cuttable_seen = 60);
    check "phase 1: at least one cut was ATTEMPTED (test non-vacuous)" (cap >= 1);
    (* a further burst must NOT exceed the cap (budget holds) *)
    for _ = 1 to 20 do
      ignore (final_is_cuttable fx : bool)
    done;
    check
      (Printf.sprintf "phase 1: attempts capped at the budget (stayed %d)" cap)
      (Lia_adapter.cut_attempts fx.adapter = cap);
    (* RESET: the per-query budget is restored. *)
    Lia_adapter.reset_cut_budget fx.adapter;
    check "reset: cut_attempts back to 0" (Lia_adapter.cut_attempts fx.adapter = 0);
    (* PHASE 2: the budget gate is re-opened — further cuttable Finals attempt again. *)
    for _ = 1 to 60 do
      ignore (final_is_cuttable fx : bool)
    done;
    check
      "phase 2: after reset, cuts are ATTEMPTED again (budget re-opened)"
      (Lia_adapter.cut_attempts fx.adapter >= 1);
    Printf.printf "    (budget cap observed = %d; reset re-opened it)\n" cap);
  Printf.printf "\ncut-budget self-test: %d checks, %d failure(s)\n" !checks !failures;
  if !failures > 0 then exit 1
;;
