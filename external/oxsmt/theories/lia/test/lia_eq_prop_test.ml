(* Focused OFF/ON tests for OXSMT_LIA_EQ_PROP. The equality target is registered but not
   asserted; active simplex bounds must propagate it with the minimal premise set, and
   push/pop must retract both the report bit and the adapter's lazy explanation. *)

open Oxsmt_core
open Oxsmt_lia

let checks = ref 0
let failures = ref 0

let check name condition =
  incr checks;
  if not condition
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let enabled = Array.length Sys.argv > 1 && String.equal Sys.argv.(1) "on"

type fixture =
  { env : Env.t
  ; ctx : Context.t
  ; x : Term.t
  ; adapter : Lia_adapter.t
  ; alloc : Atom.allocator
  }

let make_fixture () =
  let env = Env.create () in
  let x_sym = Env.declare_fun env "x" (Rank.create [] Sort.int) in
  let ctx = Context.create env in
  { env; ctx; x = Context.const ctx x_sym; adapter = Lia_adapter.create ctx env
  ; alloc = Atom.create_allocator ()
  }
;;

let int fx n = Context.int_const fx.ctx n
let eq fx a b = Context.eq fx.ctx a b
let le fx a b = Context.le fx.ctx a b

let register fx term =
  let atom = Atom.fresh fx.alloc in
  Lia_adapter.register_atom fx.adapter atom term;
  Lit.make atom true
;;

let assert_true fx term =
  let lit = register fx term in
  Lia_adapter.assert_lit fx.adapter lit;
  lit
;;

let propagate fx =
  match Lia_adapter.check fx.adapter Theory.Propagate with
  | Theory.Propagations lits -> lits
  | Theory.Conflict _ | Theory.Sat | Theory.Split _ | Theory.Lemma _ ->
    failwith "lia_eq_prop_test: expected a consistent propagation result"
;;

let has lit lits = List.exists (Lit.equal lit) lits

let same_lit_set a b =
  List.sort_uniq Lit.compare a = List.sort_uniq Lit.compare b
;;

let test_two_bound_positive () =
  let fx = make_fixture () in
  let target = register fx (eq fx fx.x (int fx 3)) in
  let upper = assert_true fx (le fx fx.x (int fx 3)) in
  let lower = assert_true fx (le fx (int fx 3) fx.x) in
  let propagated = propagate fx in
  check
    "two oriented bounds propagate x=3 iff enabled"
    (Bool.equal (has target propagated) enabled);
  if enabled
  then (
    let explanation = Lia_adapter.explain fx.adapter target in
    check
      "positive equality explanation contains exactly the two oriented bounds"
      (same_lit_set explanation.Explanation.premises [ lower; upper ]);
    check
      "positive equality explanation is tagged Lia_bound"
      (explanation.rule = Explanation.Rule_tag.Lia_bound))
;;

let test_one_bound_negative () =
  (let fx = make_fixture () in
   let target = register fx (eq fx fx.x (int fx 3)) |> Lit.negate in
   let excluding = assert_true fx (le fx fx.x (int fx 2)) in
   let propagated = propagate fx in
   check
     "one excluding upper bound propagates x<>3 iff enabled"
     (Bool.equal (has target propagated) enabled);
   if enabled
   then
     check
       "upper-excluded equality explanation is the single excluding bound"
       ((Lia_adapter.explain fx.adapter target).Explanation.premises = [ excluding ]));
  let fx = make_fixture () in
  let target = register fx (eq fx fx.x (int fx 3)) |> Lit.negate in
  let excluding = assert_true fx (le fx (int fx 4) fx.x) in
  let propagated = propagate fx in
  check
    "one excluding lower bound propagates x<>3 iff enabled"
    (Bool.equal (has target propagated) enabled);
  if enabled
  then
    check
      "lower-excluded equality explanation is the single excluding bound"
      ((Lia_adapter.explain fx.adapter target).Explanation.premises = [ excluding ])
;;

let test_non_entailing_bound () =
  let fx = make_fixture () in
  let target = register fx (eq fx fx.x (int fx 3)) in
  ignore (assert_true fx (le fx fx.x (int fx 3)) : Lit.t);
  let propagated = propagate fx in
  check "one non-excluding bound does not decide equality" (not (has target propagated));
  check
    "one non-excluding bound does not decide disequality"
    (not (has (Lit.negate target) propagated))
;;

let test_duplicate_source_is_one_premise () =
  let fx = make_fixture () in
  let x_plus_one = Context.linear_combination fx.ctx [ 1, fx.x ] 1 in
  let target = register fx (eq fx x_plus_one (int fx 4)) in
  let source = assert_true fx (eq fx fx.x (int fx 3)) in
  let propagated = propagate fx in
  check
    "an equivalent asserted equality propagates the distinct target iff enabled"
    (Bool.equal (has target propagated) enabled);
  if enabled
  then
    check
      "the same source on both simplex bounds is deduplicated in the reason"
      ((Lia_adapter.explain fx.adapter target).Explanation.premises = [ source ])
;;

let test_scaled_equality_target () =
  let fx = make_fixture () in
  let two_x = Context.linear_combination fx.ctx [ 2, fx.x ] 0 in
  let target = register fx (eq fx two_x (int fx 2)) in
  let upper = assert_true fx (le fx fx.x (int fx 1)) in
  let lower = assert_true fx (le fx (int fx 1) fx.x) in
  let propagated = propagate fx in
  check
    "primitive bounds propagate the GCD-equivalent target 2*x=2 iff enabled"
    (Bool.equal (has target propagated) enabled);
  if enabled
  then
    check
      "scaled equality keeps the two primitive oriented premises"
      (same_lit_set
         (Lia_adapter.explain fx.adapter target).Explanation.premises
         [ lower; upper ])
;;

let test_pop_and_rederive () =
  let fx = make_fixture () in
  let target = register fx (eq fx fx.x (int fx 3)) in
  Lia_adapter.push fx.adapter;
  ignore (assert_true fx (le fx fx.x (int fx 3)) : Lit.t);
  ignore (assert_true fx (le fx (int fx 3) fx.x) : Lit.t);
  let first = propagate fx in
  check
    "pushed bounds propagate equality iff enabled"
    (Bool.equal (has target first) enabled);
  Lia_adapter.pop fx.adapter 1;
  if enabled
  then
    check
      "pop removes the positive equality's lazy explanation"
      (match Lia_adapter.explain fx.adapter target with
       | _ -> false
       | exception Failure _ -> true);
  check "popped equality is not spuriously re-emitted" (not (has target (propagate fx)));
  Lia_adapter.push fx.adapter;
  let excluding = assert_true fx (le fx fx.x (int fx 2)) in
  let negative = Lit.negate target in
  let second = propagate fx in
  check
    "after pop, a fresh excluding bound rederives the opposite polarity iff enabled"
    (Bool.equal (has negative second) enabled);
  if enabled
  then
    check
      "rederived opposite polarity has the fresh post-pop reason"
      ((Lia_adapter.explain fx.adapter negative).Explanation.premises = [ excluding ])
;;

let test_checkpoint_rewind () =
  let fx = make_fixture () in
  let target = register fx (eq fx fx.x (int fx 3)) in
  let checkpoint = Lia_adapter.checkpoint fx.adapter in
  ignore (assert_true fx (le fx fx.x (int fx 3)) : Lit.t);
  ignore (assert_true fx (le fx (int fx 3) fx.x) : Lit.t);
  check
    "base-frame bounds propagate equality before checkpoint rewind iff enabled"
    (Bool.equal (has target (propagate fx)) enabled);
  Lia_adapter.rewind_to_checkpoint fx.adapter checkpoint;
  check
    "checkpoint rewind retracts the equality propagation"
    (not (has target (propagate fx)));
  if enabled
  then
    check
      "checkpoint rewind removes the equality's lazy explanation"
      (match Lia_adapter.explain fx.adapter target with
       | _ -> false
       | exception Failure _ -> true);
  let excluding = assert_true fx (le fx fx.x (int fx 2)) in
  let negative = Lit.negate target in
  check
    "checkpoint rewind re-arms both equality orientations iff enabled"
    (Bool.equal (has negative (propagate fx)) enabled);
  if enabled
  then
    check
      "post-rewind opposite polarity has the new reason"
      ((Lia_adapter.explain fx.adapter negative).Explanation.premises = [ excluding ])
;;

let test_euf_only_equality_does_not_enter_lia_model () =
  let fx = make_fixture () in
  let f_sym = Env.declare_fun fx.env "f" (Rank.create [ Sort.int ] Sort.int) in
  let f_x = Context.app fx.ctx f_sym [ fx.x ] in
  let f_zero = Context.app fx.ctx f_sym [ int fx 0 ] in
  let target = register fx (eq fx f_x f_zero) in
  check
    "an equality over arithmetic-unused UF terms is not propagated"
    (not (has target (propagate fx)));
  (match Lia_adapter.check fx.adapter Theory.Final with
   | Theory.Sat ->
     let model = Lia_adapter.model fx.adapter in
     check
       "registering an EUF-only equality does not add its applications to the LIA model"
       (Model.value model f_x = None && Model.value model f_zero = None)
   | Theory.Conflict _ | Theory.Propagations _ | Theory.Split _ | Theory.Lemma _ ->
     check "EUF-only equality leaves the empty arithmetic problem satisfiable" false)
;;

let test_pending_target_activates_when_last_leaf_becomes_arithmetic () =
  let fx = make_fixture () in
  let y_sym = Env.declare_fun fx.env "y" (Rank.create [] Sort.int) in
  let y = Context.const fx.ctx y_sym in
  let target = register fx (eq fx fx.x y) in
  ignore (assert_true fx (le fx fx.x (int fx 0)) : Lit.t);
  check
    "a target with one arithmetic-unused leaf stays pending"
    (not (has target (propagate fx)));
  let partial_model =
    match Lia_adapter.check fx.adapter Theory.Final with
    | Theory.Sat -> Lia_adapter.model fx.adapter
    | Theory.Conflict _ | Theory.Propagations _ | Theory.Split _ | Theory.Lemma _ ->
      failwith "lia_eq_prop_test: expected a model for the partial arithmetic problem"
  in
  check
    "pending target does not add its missing leaf to the LIA model"
    (Model.value partial_model fx.x <> None && Model.value partial_model y = None);
  let xy = assert_true fx (le fx fx.x y) in
  let yx = assert_true fx (le fx y fx.x) in
  let propagated = propagate fx in
  check
    "making the last leaf arithmetic-live activates and propagates x=y iff enabled"
    (Bool.equal (has target propagated) enabled);
  if enabled
  then
    check
      "late-activated equality has the two cross-variable bounds"
      (same_lit_set (Lia_adapter.explain fx.adapter target).Explanation.premises [ xy; yx ])
;;

let () =
  Printf.printf "lia equality propagation (%s):\n" (if enabled then "on" else "off");
  test_two_bound_positive ();
  test_one_bound_negative ();
  test_non_entailing_bound ();
  test_duplicate_source_is_one_premise ();
  test_scaled_equality_target ();
  test_pop_and_rederive ();
  test_checkpoint_rewind ();
  test_euf_only_equality_does_not_enter_lia_model ();
  test_pending_target_activates_when_last_leaf_becomes_arithmetic ();
  Printf.printf "lia_eq_prop_test: %d checks, %d failures\n" !checks !failures;
  if !failures <> 0 then exit 1
;;
