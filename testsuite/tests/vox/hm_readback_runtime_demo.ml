(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml level_finite_spec.ml hm_readback_runtime.ml hm_readback_runtime_demo.ml";
 { bytecode; }
*)
open Copy_spec
module F = Level_finite_spec
module U = Level_unifier_spec

let () =
  let state = Pref.empty () in
  let node = cell Bool 0 in
  ghost_ (cell_def Bool 0);
  let allocated = Pref.alloc node state in
  let p = allocated.value in
  let tree = ghost_ (F.Constant_tree p) in
  ghost_ (
    let h = Pref.own (borrow_ allocated.state) in
    F.tree_root_def tree; F.finite_def h tree;
    U.observe_def h p; F.readback_def tree);
  let ty = Hm_readback_runtime.read tree p (borrow_ allocated.state) in
  match ty with Boolean -> () | _ -> failwith "wrong Boolean readback"

let () =
  let state = Pref.empty () in
  let node = cell Var 0 in
  ghost_ (cell_def Var 0);
  let allocated = Pref.alloc node state in
  let p = allocated.value in
  let tree = ghost_ (F.Free p) in
  ghost_ (
    let h = Pref.own (borrow_ allocated.state) in
    F.tree_root_def tree; F.finite_def h tree;
    U.observe_def h p; F.readback_def tree);
  let ty = Hm_readback_runtime.read tree p (borrow_ allocated.state) in
  match ty with
  | Variable q -> if not (Pref.equal p q) then failwith "wrong free variable"
  | _ -> failwith "lost free variable"

let () =
  let state = Pref.empty () in
  let bool_node = cell Bool 0 in
  ghost_ (cell_def Bool 0);
  let boolean = Pref.alloc bool_node state in
  let p = boolean.value in
  let arrow_node = cell (Arrow (p, p)) 0 in
  ghost_ (cell_def (Arrow (p, p)) 0);
  let arrow = Pref.alloc arrow_node boolean.state in
  let q = arrow.value in
  let alias_node = cell (Link q) 0 in
  ghost_ (cell_def (Link q) 0);
  let alias = Pref.alloc alias_node arrow.state in
  let r = alias.value in
  let tree = ghost_ (
    let h = Pref.own (borrow_ alias.state) in
    let leaf = F.Constant_tree p in
    let branch = F.Branch (q, leaf, leaf) in
    let tree = F.Alias_tree (r, branch) in
    F.tree_root_def leaf; F.finite_def h leaf; U.observe_def h p;
    F.tree_root_def branch; F.finite_def h branch; U.observe_def h q;
    F.tree_root_def tree; F.finite_def h tree; U.observe_def h r;
    tree) in
  let ty = Hm_readback_runtime.read tree r (borrow_ alias.state) in
  match ty with
  | Function (Boolean, Boolean) -> ()
  | _ -> failwith "wrong shared arrow readback through alias"

let () =
  let state = Pref.empty () in
  let desc : desc = Word in
  let node = cell desc 0 in
  ghost_ (cell_def desc 0);
  let word = Pref.alloc node state in
  let p = word.value in
  let desc = List p in let node = cell desc 0 in
  ghost_ (cell_def desc 0);
  let list = Pref.alloc node word.state in
  let q = list.value in
  let desc = List q in let node = cell desc 0 in
  ghost_ (cell_def desc 0);
  let nested = Pref.alloc node list.state in
  let r = nested.value in
  let tree = ghost_ (
    let h = Pref.own (borrow_ nested.state) in
    let leaf = F.Word_tree p in
    let child = F.List_tree (q, leaf) in
    let tree = F.List_tree (r, child) in
    F.tree_root_def leaf; F.finite_def h leaf; U.observe_def h p;
    F.tree_root_def child; F.finite_def h child; U.observe_def h q;
    F.tree_root_def tree; F.finite_def h tree; U.observe_def h r;
    tree) in
  let ty = Hm_readback_runtime.read tree r (borrow_ nested.state) in
  match ty with
  | List_type (List_type Word64) -> ()
  | _ -> failwith "wrong nested word list readback"
