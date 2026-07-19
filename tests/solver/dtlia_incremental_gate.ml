(* Incremental / free-Boolean DT+LIA gate (bugreport 03 scope limit: "incremental
   (push/pop) DT+LIA combination ... still fail closed").

   The DT+LIA combination decides mixed datatype+integer queries in the BATCH and PUSH/POP
   paths already. The residual gap this gate pins is narrower and NOT push/pop-specific:
   at an accepting DT [Sat], the constructor-tree checker model carried no truth value for
   a PURE-Boolean atom whose value lives only in the propositional skeleton — a free Bool
   constant that is ASSERTED, an uninterpreted nullary predicate, or (the common trigger)
   an assumption atom that every non-empty [check_sat_assuming] splices into the checked
   assertion set. The independent [Dt_model_check] reads such a nullary atom from the
   model env and fails closed when absent, so those otherwise-SAT problems degraded to a
   sound [unknown].

   The fix ([Session.complete_dt_bool_atoms], default ON, [OXSMT_DTLIA_BOOL_COMPLETE]=0
   opts out) completes the DT checker model with each such atom's value from the accepting
   SAT assignment. Gains-only and fail-closed: [Dt_model_check] still re-evaluates every
   assertion, so a completed value can only turn a model-check [unknown] into a checked
   [Sat], never a wrong [Sat].

   Each RED here fails (degrades to [unknown]) with the fix removed — run with
   [OXSMT_DTLIA_BOOL_COMPLETE=0] to see the pre-fix baseline. *)

module Session = Oxsmt_interface.Session
module Sort = Oxsmt_core.Sort
module Context = Oxsmt_core.Context
module Defs = Oxsmt_core.Datatype_defs

let failures = ref 0

let vstr = function
  | Session.Sat -> "sat"
  | Session.Unsat -> "unsat"
  | Session.Unknown -> "unknown"
;;

let expect name got want =
  if got = want
  then Printf.printf "  ok   %s: %s\n%!" name (vstr got)
  else (
    incr failures;
    Printf.printf "  FAIL %s: got %s, want %s\n%!" name (vstr got) (vstr want))
;;

let ctor_sym dt i = (List.nth dt.Defs.constructors i).Defs.sym

let sel_sym dt i j =
  (List.nth (List.nth dt.Defs.constructors i).Defs.selectors j).Defs.sym
;;

(* Tree = Node(left:Tree, key:Int, right:Tree) | Empty. A fresh datatype per session. *)
type tree_api =
  { ctx : Oxsmt_core.Context.t
  ; tree : Sort.t
  ; node : Oxsmt_core.Symbol.t
  ; empty : Oxsmt_core.Symbol.t
  ; key : Oxsmt_core.Symbol.t
  }

let make_tree s =
  let ctx = Session.context s in
  let tree = Sort.datatype_ (Session.declare_sort s "Tree") in
  let dtree =
    Session.declare_datatype
      s
      tree
      [ { Session.ctor_name = "Node"
        ; fields = [ "left", tree; "key", Sort.int; "right", tree ]
        }
      ; { Session.ctor_name = "Empty"; fields = [] }
      ]
  in
  { ctx
  ; tree
  ; node = ctor_sym dtree 0
  ; empty = ctor_sym dtree 1
  ; key = sel_sym dtree 0 1
  }
;;

(* [t = Node(Empty, k, Empty) /\ key t = 5] — a mixed datatype+integer SAT skeleton. *)
let assert_mixed_sat s a =
  let ic n = Context.int_const a.ctx n in
  let mk_empty = Context.app a.ctx a.empty [] in
  let t = Context.const a.ctx (Session.declare_const s "t" a.tree) in
  let k = Context.const a.ctx (Session.declare_const s "k" Sort.int) in
  let key_t = Context.app a.ctx a.key [ t ] in
  Session.assert_term
    s
    (Context.eq a.ctx t (Context.app a.ctx a.node [ mk_empty; k; mk_empty ]));
  Session.assert_term s (Context.eq a.ctx key_t (ic 5));
  key_t
;;

let csa1 s atom pol = (Session.check_sat_assuming s [ atom, pol ]).Session.verdict

(* R1: mixed DT+LIA SAT under check_sat_assuming with a trivial (unconstrained)
   assumption. RED pre-fix: the assumption atom is a pure Bool with no value in the DT
   tree model. *)
let r1_csa_trivial_assumption () =
  let s = Session.create () in
  let a = make_tree s in
  ignore (assert_mixed_sat s a : Oxsmt_core.Term.t);
  let g = Context.const a.ctx (Session.declare_const s "g" Sort.bool) in
  expect "R1 check_sat_assuming(g=true) on mixed SAT" (csa1 s g true) Session.Sat
;;

(* R2: mixed DT+LIA SAT in BATCH with a free Bool assertion. RED pre-fix: the free Bool
   [g] has no value in the DT tree model, so the checker fails closed on evaluating it. *)
let r2_batch_free_bool () =
  let s = Session.create () in
  let a = make_tree s in
  ignore (assert_mixed_sat s a : Oxsmt_core.Term.t);
  let g = Context.const a.ctx (Session.declare_const s "g" Sort.bool) in
  Session.assert_term s g;
  expect "R2 batch mixed SAT + free Bool assert" (Session.check_sat s) Session.Sat
;;

(* V1 (value discrimination): [g] is asserted true and gates a SATISFIED integer fact. The
   fix must complete [g]'s value as TRUE (its accepting assignment); a completion using a
   wrong/constant value would evaluate the assertion [g] to false and degrade to
   [unknown]. So [sat] here proves the genuine assignment is read, not a constant. *)
let v1_asserted_gate_true () =
  let s = Session.create () in
  let a = make_tree s in
  let key_t = assert_mixed_sat s a in
  let g = Context.const a.ctx (Session.declare_const s "g" Sort.bool) in
  Session.assert_term s g;
  Session.assert_term
    s
    (Context.implies a.ctx g (Context.le a.ctx (Context.int_const a.ctx 0) key_t));
  expect "V1 asserted g gates satisfied fact" (Session.check_sat s) Session.Sat
;;

(* V2 (assumption-polarity discrimination): [g -> (key t < 0 /\ key t > 0)] is a
   contradiction exactly when [g] holds. Assuming [g=false] must be SAT (vacuous
   implication — requires the completion to read g=false); assuming [g=true] must be UNSAT
   (the contradiction fires). A completion that ignored polarity, or read a constant,
   could not satisfy BOTH directions. The UNSAT leg also confirms the unsat path is
   unaffected (it never consults the completed model). *)
let v2_assumption_polarity () =
  let s = Session.create () in
  let a = make_tree s in
  let key_t = assert_mixed_sat s a in
  let g = Context.const a.ctx (Session.declare_const s "g" Sort.bool) in
  let z = Context.int_const a.ctx 0 in
  let contradiction =
    Context.and_ a.ctx [ Context.lt a.ctx key_t z; Context.lt a.ctx z key_t ]
  in
  Session.assert_term s (Context.implies a.ctx g contradiction);
  expect "V2a check_sat_assuming(g=false) vacuous" (csa1 s g false) Session.Sat;
  expect "V2b check_sat_assuming(g=true) contradiction" (csa1 s g true) Session.Unsat
;;

(* S1 (soundness guard): an asserted Boolean forces an integer fact that contradicts
   another assertion — UNSAT. The completion must not turn this into a wrong [sat]; UNSAT
   is derived by refutation and never reaches the completed model, so it stays UNSAT. *)
let s1_unsat_with_bool () =
  let s = Session.create () in
  let a = make_tree s in
  let ic n = Context.int_const a.ctx n in
  let mk_empty = Context.app a.ctx a.empty [] in
  let t = Context.const a.ctx (Session.declare_const s "t" a.tree) in
  let k = Context.const a.ctx (Session.declare_const s "k" Sort.int) in
  let key_t = Context.app a.ctx a.key [ t ] in
  Session.assert_term
    s
    (Context.eq a.ctx t (Context.app a.ctx a.node [ mk_empty; k; mk_empty ]));
  let g = Context.const a.ctx (Session.declare_const s "g" Sort.bool) in
  Session.assert_term s g;
  Session.assert_term s (Context.implies a.ctx g (Context.eq a.ctx key_t (ic 0)));
  Session.assert_term s (Context.eq a.ctx key_t (ic 9));
  expect
    "S1 asserted-bool-forced contradiction stays UNSAT"
    (Session.check_sat s)
    Session.Unsat
;;

(* M1 (multi-query no-staleness): TWO successive check_sat_assuming calls in ONE session
   on a SHARED Bool atom [g] with OPPOSITE polarity, each SAT but forcing a DIFFERENT key
   value ([g -> key t = 5], [not g -> key t = 7]). Call 1 commits a model with g=true;
   call 2 assumes g=false. The completion reads [g]'s value from [Sat.value], which each
   solve rebuilds from scratch ([save_model] clears + refills [saved_model] for every
   var), so call 2 must see g=FALSE — if it read call 1's stale g=true, [Dt_model_check]
   would evaluate call 2's spliced [not g] assumption to false and degrade to unknown.
   Both calls returning SAT (with the right key each time) pins that the injected value is
   never stale from a previous check_sat. The soundness-adjacent case the lead asked to
   hunt hardest. *)
let m1_multi_query_no_staleness () =
  let s = Session.create () in
  let a = make_tree s in
  let ic n = Context.int_const a.ctx n in
  let mk_empty = Context.app a.ctx a.empty [] in
  let t = Context.const a.ctx (Session.declare_const s "t" a.tree) in
  let k = Context.const a.ctx (Session.declare_const s "k" Sort.int) in
  let key_t = Context.app a.ctx a.key [ t ] in
  let g = Context.const a.ctx (Session.declare_const s "g" Sort.bool) in
  Session.assert_term
    s
    (Context.eq a.ctx t (Context.app a.ctx a.node [ mk_empty; k; mk_empty ]));
  Session.assert_term s (Context.implies a.ctx g (Context.eq a.ctx key_t (ic 5)));
  Session.assert_term
    s
    (Context.implies a.ctx (Context.not_ a.ctx g) (Context.eq a.ctx key_t (ic 7)));
  expect "M1a check_sat_assuming(g=true) -> key t=5" (csa1 s g true) Session.Sat;
  expect
    "M1b check_sat_assuming(g=false) -> key t=7 (no stale g)"
    (csa1 s g false)
    Session.Sat;
  expect "M1c check_sat_assuming(g=true) again (no stale sat)" (csa1 s g true) Session.Sat
;;

let () =
  Printf.printf "dtlia incremental / free-Boolean gate:\n%!";
  r1_csa_trivial_assumption ();
  r2_batch_free_bool ();
  v1_asserted_gate_true ();
  v2_assumption_polarity ();
  s1_unsat_with_bool ();
  m1_multi_query_no_staleness ();
  if !failures > 0
  then (
    Printf.printf "dtlia-incremental gate: %d failure(s)\n%!" !failures;
    exit 1)
  else Printf.printf "dtlia-incremental gate: all required checks passed\n%!"
;;
