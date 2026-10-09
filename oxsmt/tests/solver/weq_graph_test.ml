(* Unit tests for the W0 dark weak-equivalence graph substrate (ADR-weakeq / DESIGN.md
   A12). The graph is exercised directly through its O6 abstract view — no Session, no Euf
   — so these pin the substrate's own invariants: the O9 index-sort-stability gate,
   store-edge permanence, equality-edge folding + Trail-undo on pop, deterministic path
   finding, and the self-check's incompleteness tripwire. The end-to-end dark-equivalence
   and O9-wrong-sat RED checks live in the corpus A/B (W0 freeze) and at W1 (where a lemma
   can actually fire). *)

module Weq_graph = Oxsmt_arr.Weq_graph
module Session = Oxsmt_interface.Session
module Context = Oxsmt_core.Context
module Sort = Oxsmt_core.Sort
module Term = Oxsmt_core.Term

let checks = ref 0
let failures = ref 0

let fail fmt =
  Printf.ksprintf
    (fun s ->
       incr failures;
       print_string ("  FAIL " ^ s ^ "\n"))
    fmt
;;

let expect_bool name got want =
  incr checks;
  if Bool.equal got want then () else fail "%s: got %b, want %b" name got want
;;

(* --- O9: the index-sort-stability gate --- *)

let test_index_sort_gate s =
  let dt = Sort.datatype_ (Session.declare_sort s "D") in
  let u = Sort.uninterpreted (Session.declare_sort s "U") in
  let ii = Weq_graph.index_sort_stably_infinite in
  expect_bool "O9: Int index is stably infinite" (ii Sort.int) true;
  expect_bool "O9: Uninterpreted index is stably infinite" (ii u) true;
  expect_bool "O9: Bool index is NOT stably infinite" (ii Sort.bool) false;
  expect_bool "O9: BitVec index is NOT stably infinite" (ii (Sort.bitvec 8)) false;
  expect_bool "O9: Datatype index is NOT stably infinite (may be finite)" (ii dt) false;
  expect_bool
    "O9: Array-sorted index is NOT stably infinite (excluded)"
    (ii (Sort.array_ ~index:Sort.int ~element:Sort.int))
    false;
  (* array_sort_admissible = array sort with a stably-infinite index *)
  expect_bool
    "O9: array over Int index is admissible"
    (Weq_graph.array_sort_admissible (Sort.array_ ~index:Sort.int ~element:Sort.int))
    true;
  expect_bool
    "O9: array over Bool index is inadmissible"
    (Weq_graph.array_sort_admissible (Sort.array_ ~index:Sort.bool ~element:Sort.int))
    false;
  expect_bool
    "O9: a non-array sort is inadmissible"
    (Weq_graph.array_sort_admissible Sort.int)
    false
;;

(* --- graph structure, backtracking, paths --- *)

(* A view whose equality is controllable; class_of/explain_equal are unused by the
   structural tests. *)
let view are_equal : Weq_graph.egraph_view =
  { Weq_graph.class_of = (fun _ -> 0); are_equal; explain_equal = (fun _ _ -> []) }
;;

let arr_int = Sort.array_ ~index:Sort.int ~element:Sort.int
let arr_bool_idx = Sort.array_ ~index:Sort.bool ~element:Sort.int

let mk_const s name sort =
  Context.const (Session.context s) (Session.declare_const s name sort)
;;

let test_store_edges s =
  let g = Weq_graph.create (view Term.equal) in
  let base = mk_const s "b_se" arr_int in
  let s1 = mk_const s "s1_se" arr_int in
  let i = mk_const s "i_se" Sort.int in
  Weq_graph.add_store_edge g ~store_term:s1 ~base ~index:i;
  expect_bool "store edge: s1 ~ base" (Weq_graph.weakly_equivalent g s1 base) true;
  expect_bool
    "store edge: symmetric base ~ s1"
    (Weq_graph.weakly_equivalent g base s1)
    true;
  expect_bool "reflexive: base ~ base" (Weq_graph.weakly_equivalent g base base) true;
  (match Weq_graph.find_path g base base with
   | Some [] -> incr checks
   | _ ->
     incr checks;
     fail "reflexive path should be empty");
  (* a path with one store edge, labelled by the store index *)
  match Weq_graph.find_path g s1 base with
  | Some [ Weq_graph.Store { index; _ } ] ->
    expect_bool "store path index term is i" (Term.equal index i) true
  | Some _ | None ->
    incr checks;
    fail "expected a single-store-edge path s1 -> base"
;;

let test_o9_untracked s =
  (* a Bool-index array is inadmissible (O9): its store edge is dropped, so the graph
     never relates the two arrays and no path can be found — the W1/W2 rules therefore
     never fire over a finite index sort (the substrate half of the O9 wrong-sat defence). *)
  let g = Weq_graph.create (view Term.equal) in
  let base = mk_const s "b_o9" arr_bool_idx in
  let s1 = mk_const s "s1_o9" arr_bool_idx in
  let i = mk_const s "i_o9" Sort.bool in
  Weq_graph.add_store_edge g ~store_term:s1 ~base ~index:i;
  expect_bool
    "O9: Bool-index store edge is NOT tracked (arrays stay unrelated)"
    (Weq_graph.weakly_equivalent g s1 base)
    false;
  (* query-side O9 guard (obligation 2): the reflexive zero-length path must ALSO be
     denied for a finite-index array, so a rule cannot fire over it via find_path a a =
     Some []. *)
  expect_bool
    "O9: reflexive query on a Bool-index array is denied (no zero-length path)"
    (Weq_graph.weakly_equivalent g base base)
    false;
  incr checks;
  match Weq_graph.find_path g base base with
  | None -> ()
  | Some _ -> fail "O9: find_path on a Bool-index array (reflexive) must be None"
;;

let test_equality_backtracking s =
  let g = Weq_graph.create (view Term.equal) in
  let a = mk_const s "a_bt" arr_int in
  let b = mk_const s "b_bt" arr_int in
  let base = mk_const s "base_bt" arr_int in
  let i = mk_const s "i_bt" Sort.int in
  (* a permanent store edge base ~ a *)
  Weq_graph.add_store_edge g ~store_term:a ~base ~index:i;
  expect_bool "pre-merge: a !~ b" (Weq_graph.weakly_equivalent g a b) false;
  Weq_graph.push g;
  Weq_graph.on_merge g a b;
  expect_bool "post-merge: a ~ b" (Weq_graph.weakly_equivalent g a b) true;
  expect_bool "post-merge: b ~ base (via a)" (Weq_graph.weakly_equivalent g b base) true;
  Weq_graph.pop g 1;
  expect_bool
    "post-pop: equality edge removed, a !~ b"
    (Weq_graph.weakly_equivalent g a b)
    false;
  expect_bool
    "post-pop: store edge SURVIVES, a ~ base"
    (Weq_graph.weakly_equivalent g a base)
    true;
  (* re-merge after pop must work again (eq_seen was un-recorded on pop) *)
  Weq_graph.push g;
  Weq_graph.on_merge g a b;
  expect_bool "re-merge after pop: a ~ b" (Weq_graph.weakly_equivalent g a b) true;
  Weq_graph.pop g 1
;;

let test_nested_pop s =
  let g = Weq_graph.create (view Term.equal) in
  let a = mk_const s "a_np" arr_int in
  let b = mk_const s "b_np" arr_int in
  let c = mk_const s "c_np" arr_int in
  Weq_graph.push g;
  Weq_graph.on_merge g a b;
  Weq_graph.push g;
  Weq_graph.on_merge g b c;
  expect_bool "nested: a ~ c" (Weq_graph.weakly_equivalent g a c) true;
  Weq_graph.pop g 1;
  expect_bool "nested pop1: a ~ b survives" (Weq_graph.weakly_equivalent g a b) true;
  expect_bool
    "nested pop1: a !~ c (b~c removed)"
    (Weq_graph.weakly_equivalent g a c)
    false;
  Weq_graph.pop g 1;
  expect_bool "nested pop2: a !~ b" (Weq_graph.weakly_equivalent g a b) false
;;

let test_self_check s =
  let a = mk_const s "a_sc" arr_int in
  let b = mk_const s "b_sc" arr_int in
  (* a view that CLAIMS a = b though the graph has no connecting edge: the self-check must
     catch the (simulated) merge-stream mis-fold. *)
  let g_bad =
    Weq_graph.create
      (view (fun x y ->
         Term.equal x y
         || (Term.equal x a && Term.equal y b)
         || (Term.equal x b && Term.equal y a)))
  in
  incr checks;
  (match Weq_graph.self_check g_bad [ a; b ] with
   | () -> fail "self_check: should have raised on unconnected same-e-class pair"
   | exception Failure _ -> ());
  (* now actually fold the merge: self-check passes *)
  Weq_graph.on_merge g_bad a b;
  incr checks;
  match Weq_graph.self_check g_bad [ a; b ] with
  | () -> ()
  | exception Failure _ -> fail "self_check: false positive on a properly connected pair"
;;

let () =
  let s = Session.create () in
  test_index_sort_gate s;
  test_store_edges s;
  test_o9_untracked s;
  test_equality_backtracking s;
  test_nested_pop s;
  test_self_check s;
  Printf.printf "Weq_graph test: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
