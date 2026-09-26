(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml generalize_datatype_demo.ml";
 { bytecode; }
*)
open Copy_spec
open Level_spec
open Generalize_spec
open Generalize_proofs
open Generalize_scheme_proofs
open Level_unifier_metadata

let run word =
  let refine_ state = Pref.empty () in
  let element_desc : desc = if word then Word else Var in
  let refine_ node = cell element_desc 2 in
  let refine_ step = Pref.alloc node state in
  let element = step.value in let state = step.state in
  let list_desc = List element in let refine_ node = cell list_desc 2 in
  let refine_ step = Pref.alloc node state in
  let root = step.value in let state = step.state in
  let h = ghost_ (Pref.own (borrow_ state)) in
  let empty = Empty in let rest = Entry (element, empty) in
  let pool = Entry (root, rest) in
  ghost_ (
    cell_def element_desc 2; cell_def list_desc 2;
    pool_scoped_def h empty; pool_scoped_def h rest; pool_scoped_def h pool);
  let scope : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem h x then source_ok h x else H.at h x === None})
      @ total ghost = ghost_ (fun x ->
    cell_def element_desc 2; cell_def list_desc 2;
    source_ok_def h x; ()) in
  let coverage : ((x : node Pref.t) @ immutable ->
      {u : unit | covered h 0 pool x}) @ total ghost = ghost_ (fun x ->
    cell_def element_desc 2; cell_def list_desc 2; at_level_def h x;
    listed_def empty x; listed_def rest x; listed_def pool x;
    covered_def h 0 pool x; ()) in
  ghost_ (
    let child = Tip element in let tree = Through (root, child) in
    bound_root_def child; bound_root_def tree;
    unfolded_def h child; unfolded_def h tree;
    pool_scoped_def h empty; pool_scoped_def h rest; pool_scoped_def h pool;
    scope element; scope root;
    scheme_valid h 0 pool coverage tree ());
  let state = Generalize.close h 0 pool state in
  let saved = ghost_ (Pref.own (borrow_ state)) in
  let saved_scope : ((x : node Pref.t) @ immutable ->
      {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})
      @ total ghost = ghost_ (fun x -> closed_scope h scope 0 pool x (); ()) in
  ghost_ (closed_observe h 0 pool root (); closed_at_def h saved 0 pool root);
  let out = Copy_algorithm.instantiate saved saved_scope 1 root state in
  let after = ghost_ (Pref.own (borrow_ out.#state)) in
  ghost_ (
    Copy_heap_proofs.target_allocated saved out.#epoch 1 out.#history root out.#value ();
    Copy_model_proofs.history_scope saved saved_scope out.#epoch 1 out.#history out.#value ();
    source_ok_def after out.#value);
  let copied = Pref.read out.#value (borrow_ out.#state) in
  match copied.desc with
  | List child ->
    let copied_element = Pref.read child (borrow_ out.#state) in
    (match copied_element.desc, copied_element.level with
     | Word, Finite 1 when word -> ()
     | Var, Finite 1 when not word -> ()
     | _ -> failwith "wrong generalized list instance");
    if Pref.equal child element then failwith "generic list element was shared"
  | _ -> failwith "generalized list changed constructor"

let () = run false; run true
