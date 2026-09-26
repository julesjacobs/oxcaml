(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml generalize_spec.ml generalize_proofs.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml hm_readback_runtime.ml level_unifier_datatype_demo.ml";
 { bytecode; }
*)
open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_unifier_proofs
open Level_unifier_metadata

module F = Level_finite_spec

let run mode =
  let state = Pref.empty () in
  let d_a = Var in let v_a = cell d_a 0 in
  let refine_ step = Pref.alloc v_a state in
  let a = step.value in let state = step.state in
  let d_w = Word in let v_w = cell d_w 4 in
  let refine_ step = Pref.alloc v_w state in
  let w = step.value in let state = step.state in
  let d_b = (if mode = 4 then Word else if mode = 6 then List w else Bool) in let v_b = cell d_b 4 in
  let refine_ step = Pref.alloc v_b state in
  let b = step.value in let state = step.state in
  let d_la = List (if mode = 5 then b else a) in let v_la = cell d_la 4 in
  let refine_ step = Pref.alloc v_la state in
  let la = step.value in let state = step.state in
  let d_lw = List (if mode = 6 then b else w) in let v_lw = cell d_lw 4 in
  let refine_ step = Pref.alloc v_lw state in
  let lw = step.value in let state = step.state in
  let d_nested = (if mode = 3 then Arrow (w, w) else List la) in let v_nested = cell d_nested 4 in
  let refine_ step = Pref.alloc v_nested state in
  let nested = step.value in let state = step.state in
  let h = ghost_ (Pref.own (borrow_ state)) in
  let active_all : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || active h x}) @ total ghost = ghost_ (fun x ->
    cell_def d_a 0; cell_def d_w 4; cell_def d_b 4; cell_def d_la 4; cell_def d_lw 4; cell_def d_nested 4;
    active_def h x; at_level_def h x; ()) in
  let scope : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || finite_scope h x}) @ total ghost = ghost_ (fun x ->
    cell_def d_a 0; cell_def d_w 4; cell_def d_b 4; cell_def d_la 4; cell_def d_lw 4; cell_def d_nested 4;
    active_all a; active_all w; active_all b; active_all la; active_all lw; active_all nested;
    finite_scope_def h x; source_ok_def h x; ()) in
  let unmarked : ((x : node Pref.t) @ immutable ->
      {u : unit | match H.at h x with None -> true | Some v -> not v.visited})
      @ total ghost = ghost_ (fun x ->
    cell_def d_a 0; cell_def d_w 4; cell_def d_b 4; cell_def d_la 4; cell_def d_lw 4; cell_def d_nested 4; ()) in
  let trees : ((x : node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === x &&
        (if H.mem h x then F.finite h t else observe h x === None)} @ immutable)
      @ total ghost = ghost_ (fun x ->
    cell_def d_a 0; cell_def d_w 4; cell_def d_b 4;
    cell_def d_la 4; cell_def d_lw 4; cell_def d_nested 4;
    let ta = F.Free a in let tw = F.Word_tree w in
    let tb = if mode = 4 then F.Word_tree b
      else if mode = 6 then F.List_tree (b, tw) else F.Constant_tree b in
    let te = if mode = 5 then tb else ta in let tl = F.List_tree (la, te) in
    let tr = if mode = 6 then tb else tw in let tu = F.List_tree (lw, tr) in
    let tn = if mode = 3 then F.Branch (nested, tw, tw)
      else F.List_tree (nested, tl) in
    observe_def h a; observe_def h w; observe_def h b;
    observe_def h la; observe_def h lw; observe_def h nested;
    F.tree_root_def ta; F.finite_def h ta;
    F.tree_root_def tw; F.finite_def h tw;
    F.tree_root_def tb; F.finite_def h tb;
    F.tree_root_def te; F.tree_root_def tl; F.finite_def h tl;
    F.tree_root_def tr; F.tree_root_def tu; F.finite_def h tu;
    F.tree_root_def tn; F.finite_def h tn;
    let t = if x === a then ta else if x === w then tw else if x === b then tb
      else if x === la then tl else if x === lw then tu
      else if x === nested then tn else F.Free x in
    F.tree_root_def t; observe_def h x; refine_ t) in
  let p, q, expected = match mode with
    | 0 -> la, lw, true
    | 1 -> a, la, false
    | 2 -> b, w, false
    | 3 -> lw, nested, false
    | 4 -> w, b, true
    | 5 -> la, lw, false
    | 6 -> nested, lw, true
    | 7 -> a, lw, true
    | _ -> a, nested, false in
  ghost_ (active_all p; active_all q);
  let out = Level_unifier.unify h scope unmarked p q state in
  if out.#ok <> expected then failwith "wrong datatype unification result";
  let after = ghost_ (Pref.own (borrow_ out.#state)) in
  ghost_ (
    unified_frame h p q out.#ok after out.#derivation a ();
    unified_frame h p q out.#ok after out.#derivation w ());
  let tree = ghost_ (Level_finite_proofs.unified_finite_at h (refine_ trees)
    p q out.#ok after out.#derivation a ()) in
  let ty = Hm_readback_runtime.read tree a (borrow_ out.#state) in
  (match mode, ty with
   | (0 | 6), Word64 | 7, List_type Word64 -> ()
   | (1 | 2 | 3 | 4 | 5 | 8), Variable _ -> ()
   | _ -> failwith "wrong type read back after datatype unification");
  let variable = Pref.read a (borrow_ out.#state) in
  if mode = 0 || mode = 6 then
    (match variable.desc with
     | Link target -> if not (Pref.equal target w) then failwith "wrong element binding"
     | _ -> failwith "element variable was not bound")
  else if mode = 7 then (
    (match variable.desc with
     | Link target -> if not (Pref.equal target lw) then failwith "wrong list binding"
     | _ -> failwith "variable was not bound to list");
    let word = Pref.read w (borrow_ out.#state) in
    match word.level with Finite 0 -> () | _ -> failwith "list child level was not lowered")
  else match variable.desc with Var -> () | _ -> failwith "unrelated variable changed"

let () = List.iter run [0; 1; 2; 3; 4; 5; 6; 7; 8]
