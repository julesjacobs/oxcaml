(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml generalize_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Level_spec
open Generalize_spec
open Generalize_proofs
open Generalize_scheme_proofs
open Level_unifier_metadata

let run shared =
  let refine_ state = Pref.empty () in
  let v = cell Bool 0 in let refine_ step = Pref.alloc v state in
  let boundary = step.value in let state = step.state in
  let v = cell Var 2 in let refine_ step = Pref.alloc v state in
  let a = step.value in let state = step.state in
  let b = if shared then a else boundary in
  let desc = Arrow (a, b) in let v = cell desc 2 in let refine_ step = Pref.alloc v state in
  let root = step.value in let state = step.state in
  let h = ghost_ (Pref.own (borrow_ state)) in
  let scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}) @ total ghost = ghost_ (fun x ->
    let d = Bool in cell_def d 0; let d = Var in cell_def d 2; cell_def desc 2;
    source_ok_def h x; let u = () in refine_ u) in
  let order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x}) @ total ghost = ghost_ (fun x ->
    let d = Bool in cell_def d 0; let d = Var in cell_def d 2; cell_def desc 2;
    at_level_def h a; at_level_def h b; below_def h a 2; below_def h b 2;
    children_below_def h desc 2; let d = Var in children_below_def h d 2; let d = Bool in children_below_def h d 0;
    ordered_def h x; let u = () in refine_ u) in
  let forest : ((x : node Pref.t) @ immutable -> {t : bounded | not (H.mem h x) || (bound_root t === x && unfolded h t)} @ immutable) @ total ghost = ghost_ (fun x ->
    let d = Bool in cell_def d 0; let d = Var in cell_def d 2; cell_def desc 2;
    let ta = Tip a in let tb = Tip boundary in let tc = if shared then ta else tb in let tr = Fork (root, ta, tc) in
    bound_root_def ta; bound_root_def tb; bound_root_def tc; bound_root_def tr;
    unfolded_def h ta; unfolded_def h tb; unfolded_def h tc; unfolded_def h tr;
    let t = if x === root then tr else if x === a then ta else if x === boundary then tb else Tip x in
    bound_root_def t; refine_ t) in
  let empty = Empty in let rest = Entry (a, empty) in let pool = Entry (root, rest) in
  ghost_ (scope a; scope root; pool_scoped_def h empty; pool_scoped_def h rest; pool_scoped_def h pool);
  let coverage : ((x : node Pref.t) @ immutable -> {u : unit | covered h 0 pool x}) @ total ghost = ghost_ (fun x ->
    let d = Bool in cell_def d 0; let d = Var in cell_def d 2; cell_def desc 2;
    at_level_def h x; listed_def pool x; listed_def rest x; listed_def empty x; covered_def h 0 pool x;
    let u = () in refine_ u) in
  let state : {t : Pref.token | Pref.own t === h && pool_scoped h pool} = refine_ state in
  let refine_ state = Generalize.close h 0 pool state in
  let saved = ghost_ (Pref.own (borrow_ state)) in
  let scope_saved : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = closed_scope h scope 0 pool x (refine_ u) in refine_ u) in
  let order_saved : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || ordered saved x}) @ total ghost = ghost_ (fun x ->
    coverage x; order x; let u = () in closed_observe h 0 pool x (refine_ u); closed_at_def h saved 0 pool x;
    if H.mem h x then (closed_ordered h 0 pool x (refine_ u); refine_ u) else refine_ u) in
  ghost_ (let refine_ tree = forest root in let u = () in scheme_valid h 0 pool coverage tree (refine_ u);
    closed_observe h 0 pool root (refine_ u); closed_at_def h saved 0 pool root);
  let p : {p : node Pref.t | H.mem saved p} = refine_ root in
  let depth = 1 in let depth : {n : int | n >= 0} = refine_ depth in
  let state : {t : Pref.token | Pref.own t === saved} = refine_ state in
  let refine_ copied = Copy_algorithm.instantiate saved scope_saved depth p state in
  let refine_ depth = depth in let refine_ p = p in
  let after_copy = ghost_ (Pref.own (borrow_ copied.#state)) in
  let d = ghost_ copied.#history in let epoch = ghost_ copied.#epoch in
  let copy_scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem after_copy x) || finite_scope after_copy x}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = Level_copy_proofs.copy_finite_scope saved scope_saved order_saved epoch depth d x (refine_ u) in refine_ u) in
  let unmarked_saved : ((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
    let d = Bool in cell_def d 0; let d = Var in cell_def d 2; cell_def desc 2;
    let u = () in closed_observe h 0 pool x (refine_ u);
    closed_at_def h saved 0 pool x; refine_ u) in
  let unmarked : ((x : node Pref.t) @ immutable ->
      {u : unit | match H.at after_copy x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = Copy_heap_proofs.history_unmarked saved unmarked_saved epoch depth d x (refine_ u) in refine_ u) in
  let result = copied.#value in
  let _models = ghost_ (
    let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
      if x === root then Function (Boolean, Boolean) else Boolean in
    let model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x}) @ total = fun x ->
      rho_def x; rho_def a; rho_def b; rho_def boundary;
      let d = Bool in cell_def d 0; let d = Var in cell_def d 2; cell_def desc 2;
      equation_def h rho x; let u = () in refine_ u in
    let choices : node Pref.t @ immutable total -> ty @ immutable total = fun _ -> Function (Boolean, Boolean) in
    let refine_ tree = forest root in let claim = true in
    let use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap (closed_heap h 0 pool) epoch depth d) tau x})) @ total ->
        (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
        {u : unit | tau result === interpret rho choices (scheme h 0 tree)} -> {u : unit | claim}) @ total =
      fun tau next equal fit -> let refine_ fit = fit in equal boundary; next result;
        let accept : ((eta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
          {u : unit | tau result === interpret tau eta (scheme h 0 tree)} -> {u : unit | claim}) @ total =
          fun _eta fit -> let refine_ fit = fit in let u = () in refine_ u in
        let u = () in let refine_ u = with_generalized_choices h 0 pool coverage epoch depth d tau next tree result (refine_ u) claim accept in refine_ u in
    let u = () in let refine_ u = with_generalized_instance h scope forest 0 pool coverage rho model choices epoch depth d tree result (refine_ u) claim use in ()) in
  ghost_ (let u = () in Copy_heap_proofs.target_allocated saved epoch depth d p result (refine_ u);
    Copy_heap_proofs.extends_def d d;
    Level_copy_proofs.target_active_at saved order_saved epoch depth d d p result (refine_ u);
    copy_scope result; finite_scope_def after_copy result;
    Copy_heap_proofs.history_at saved epoch depth d boundary (refine_ u);
    closed_observe h 0 pool boundary (refine_ u); closed_at_def h saved 0 pool boundary;
    let v = Bool in cell_def v 0; let level = Finite 0 in close_level_def 0 level;
    active_def after_copy boundary; at_level_def after_copy boundary);
  let state = copied.#state in let state : {t : Pref.token | H.mem (Pref.own t) result} = refine_ state in
  let refine_ result_node = Pref.read result (borrow_ state) in let refine_ state = state in
  assert (result_node.level = Finite 1);
  match result_node.desc with
  | Arrow (x, y) ->
    ghost_ (active_def after_copy x; active_def after_copy y);
    let refine_ equal = Pref.equal x y in assert (equal = shared);
    let state : {t : Pref.token | Pref.own t === after_copy && H.mem after_copy x && H.mem after_copy boundary
      && active after_copy x && active after_copy boundary} = refine_ state in
    let refine_ unified = Level_unifier.unify after_copy copy_scope unmarked x boundary state in
    assert unified.#ok;
    let final = ghost_ (Pref.own (borrow_ unified.#state)) in
    ghost_ (let u = () in let ok = unified.#ok in
      Level_unifier_proofs.unified_frame after_copy x boundary ok final unified.#derivation a (refine_ u);
      Copy_heap_proofs.history_grows saved epoch depth d a (refine_ u);
      closed_observe h 0 pool a (refine_ u); closed_at_def h saved 0 pool a);
    let state = unified.#state in let state : {t : Pref.token | H.mem (Pref.own t) a} = refine_ state in
    let refine_ original = Pref.read a (borrow_ state) in
    assert (original.desc = Var && original.level = Generic)
  | _ -> assert false

let () = run true; run false
