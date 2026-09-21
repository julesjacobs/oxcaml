(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compression_proofs.ml structure_spec.ml optimized_unifier_spec.ml optimized_metadata.ml copy_cleanup_spec.ml hm_type_proofs.ml hm_environment_proofs.ml hm_environment_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Level_spec
open Hm_environment_spec
open Hm_environment_proofs
module D = Hm_declarative
module T = Hm_type_proofs

let run () =
  let state = Pref.empty () in
  let v = cell Var 0 in let allocated = Pref.alloc v state in
  let p = allocated.value in let state = allocated.state in
  let h = ghost_ (Pref.own (borrow_ state)) in
  let s = Boundary p in let env = Bind (p, Empty) in
  let ts = Template_binding (s, No_templates) in
  ghost_ (
    let desc : desc = Var in cell_def desc 0; root_def s;
    template_def h s; finite_node_def h p;
    at_level_def h p; below_def h p 0; boundary_bound_def h 0 s;
    let empty = Empty in let no_templates = No_templates in
    env_at_def h 0 empty no_templates; env_at_def h 0 env ts);
  let next_value = cell Bool 2 in let allocated = Pref.alloc next_value state in
  let q = allocated.value in let state = allocated.state in
  let middle = ghost_ (Pref.own (borrow_ state)) in
  let pool = Generalize_spec.Entry (q, Generalize_spec.Empty) in
  ghost_ (
    let desc : desc = Bool in cell_def desc 2;
    source_ok_def middle q;
    let empty = Generalize_spec.Empty in
    Generalize_spec.pool_scoped_def middle empty;
    Generalize_spec.pool_scoped_def middle pool);
  let frame : ((x : node Pref.t) @ immutable ->
      {u : unit | protected_at h middle 0 x}) @ total ghost = ghost_ (fun x ->
    let desc : desc = Var in cell_def desc 0; source_ok_def h x;
    let u = () in let u = allocation_protected h q next_value 0 x (u) in u) in
  ghost_ (let u = () in env_transport h middle 0 frame env ts (u));
  let state : {t : Pref.token | Pref.own t === middle && Generalize_spec.pool_scoped middle pool} = state in
  let state = Generalize.close middle 0 pool state in
  let after = ghost_ (Pref.own (borrow_ state)) in
  let closed_frame : ((x : node Pref.t) @ immutable ->
      {u : unit | protected_at middle after 0 x}) @ total ghost = ghost_ (fun x ->
    let u = () in let u = close_protected middle 0 pool 0 x (u) in u) in
  ghost_ (let u = () in env_transport middle after 0 closed_frame env ts (u);
    frame p; closed_frame p; protected_trans h middle after 0 p (u);
    protected_at_def h after 0 p);
  let state : {t : Pref.token | H.mem (Pref.own t) p} = state in
  let v = Pref.read p (borrow_ state) in
  assert (v.level = Finite 0)

let () = run ()
